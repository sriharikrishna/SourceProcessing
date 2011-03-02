'''functions related to type/shape/generic inference:

    ...
'''

import re
import copy

from _Setup import *

from PyUtil.debugManager import DebugManager
from PyUtil.symtab import SymtabEntry

import fortStmts
from fortExp import App,NamedParam,Sel,Unary,Ops,is_const,is_id,_id_re,_flonum_re,_int_re,_logicon_set,_quote_set,Slice,Zslice,Lslice,Rslice,ArrayConstructor
from intrinsic import is_intrinsic, getNonStandard

class InferenceError(Exception):
   '''exception for ...'''
   def __init__(self,msg,lineNumber=None):
       self.msg  = msg
       self.lineNumber=lineNumber

   def __str__(self):
        errString='\nERROR: InferenceError: '
        if self.lineNumber is not None:
           errString+='at line '+str(self.lineNumber)+':'
        if self.msg: errString+=str(self.msg)
        return (errString)

def kw2type(s): return(fortStmts.kwtbl[s.lower()])
def lenfn(n): return [fortStmts._F77Len(str(n))]

_modhash = { fortStmts._Prec     : 0,
             fortStmts._Kind     : 1,
             fortStmts._ExplKind : 2,
             }

def __modCompare(m1,m2,addLength):
    'compare type modifiers'
    if not m1: return m2
    if not m2: return m1
    mm1 = m1[0]
    mm2 = m2[0]
    if (mm1.__class__ == mm2.__class__) and isinstance(mm1,fortStmts._TypeMod) :
        if mm1.mod >= mm2.mod: return m1
        return m2
    if isinstance(mm2,fortStmts._FLenMod) and isinstance(mm1,fortStmts._FLenMod) :
       # could have different ways of specifying length - easiest case is identical modifiers
       if mm1.len==mm2.len:
          return m1
       # one of them could have a "*" so we have to use that one
       if (mm1.len=='*'):
          return m1
       if (mm2.len=='*'):
          return m2
       # they could be integers
       try :
          le1=mm1.len
          if (isinstance(mm1.len,NamedParam) and mm1.len.myId.lower()=='len'):
             le1=mm1.len.myRHS
          le2=mm2.len
          if (isinstance(mm2.len,NamedParam) and mm2.len.myId.lower()=='len'):
             le2=mm2.len.myRHS
          if (addLength):
              return [fortStmts._F90Len(Ops('+',le1,le2))] 
          l1=int(le1)
          l2=int(le2)
          if l1>l2:
             return m1
          else :
             return m2
       # if they are not both integers there could be some parameter name etc.
       except ValueError:
          raise InferenceError('__modCompare: cannot compare length specifiers '+str(mm1.len)+' and '+str(mm2.len))
    if _modhash[c1] >= _modhash[c2]: return m1
    return m2

def __typeCompare(t1,t2,addLength):
    DebugManager.debug('inference.__typeCompare called on t1 = "'+str(t1)+'\tt2 = "'+str(t2)+'"')
    mergeit = dict(character=0,
                   logical=1,
                   integer=2,
                   real=3,
                   doubleprecision=4,
                   complex=5,
                   doublecomplex=6,
                   )

    if t1[0] == t2[0]:
        return(t1[0],__modCompare(t1[1],t2[1],addLength))

    if mergeit[t1[0].kw] > mergeit[t2[0].kw]: return t1

    return t2

def typemerge(lst,default,addLength=False):
    DebugManager.debug('inference.typemerge called on '+str(lst)+'...',newLine=False)
    if not lst: return default
    if len(lst) == 1: return lst[0]
    t1 = __typeCompare(lst[0],lst[1],addLength)
    for l in lst[2:]:
        t1 = __typeCompare(t1,l,addLength)
    DebugManager.debug(' result is '+str(t1))
    return t1

def constantType(e,lineNumber):
    kind_re = re.compile(r'_(\w+)')
    if _flonum_re.match(e):
        sep_re = re.compile(r'([^_]+)(_(\w+))?')
        v      = sep_re.match(e)
        ty     = 'd' in v.group(1).lower() and kw2type('doubleprecision') \
                                            or kw2type('real')
        kind   = v.group(2) and [fortStmts._Kind(v.group(3))] \
                             or []
        return (ty,kind)
    if _int_re.match(e):
        ty   = kw2type('integer')
        kind = kind_re.search(e)
        kind = kind and [fortStmts._Kind(kind.group(1))] \
                     or []
        return (ty,kind)
    if e.lower() in _logicon_set:
        return (kw2type('logical'),[])
    if e[0] in _quote_set:
        return (kw2type('character'),lenfn(len(e)-2))

def identifierType(anId,localSymtab,lineNumber):
    (symtabEntry,containingSymtab) = localSymtab.lookup_name_level(anId)
    # a type is known -> return it
    if symtabEntry and symtabEntry.type:
        returnType = symtabEntry.type
        DebugManager.debug('with symtab entry '+symtabEntry.debug(anId)+' -> returning type '+str(returnType))
    # an entry exists with no type -> try to type implicitly
    elif symtabEntry and symtabEntry.entryKind==SymtabEntry.InterfaceEntryKind:
       DebugManager.debug('with symtab entry'+symtabEntry.debug(anId)+' (is an interface name).')
       return None
    elif symtabEntry:
       #try local implicit typing
       implicitLocalType=containingSymtab.implicit[anId[0]]
       if implicitLocalType: # we handle the error condition below
           symtabEntry.enterType(containingSymtab.implicit[anId[0]])
       returnType = implicitLocalType
       DebugManager.warning('inference.identifierType: implicit typing: '+symtabEntry.typePrint()+' '+anId,lineNumber,DebugManager.WarnType.implicit)
    else: # no symtab entry -> try local implicit typing
       returnType = localSymtab.implicit[anId[0]]
       if (returnType):
           DebugManager.warning('inference.identifierType: implicit typing: '+SymtabEntry.ourTypePrint(returnType)+' '+anId,lineNumber,DebugManager.WarnType.implicit)
    if not returnType:
        raise InferenceError('inference.identifierType: No type could be determined for identifier "'+anId+'"',lineNumber)
    return returnType

def __intrinsicType(anIntrinsicApp,localSymtab,lineNumber):
    if anIntrinsicApp.head.lower() in ['aimag','alog','real']:
        return (fortStmts.RealStmt, [])
    elif anIntrinsicApp.head.lower() in ['int','idint','lbound','ubound','scan','shape','size']:
        return (fortStmts.IntegerStmt, [])
    elif anIntrinsicApp.head.lower() in ['dble','dfloat','dabs','dexp','dlog','dsqrt','dmod']:
        return (fortStmts.DoubleStmt, [])
    elif anIntrinsicApp.head.lower() == 'cmplx':
        return (fortStmts.ComplexStmt, [])
    elif anIntrinsicApp.head.lower() == 'repeat':
        return (fortStmts.CharacterStmt, [])
    elif anIntrinsicApp.head.lower() in ['lge','lgt','lle','llt']:
        return (fortStmts.LogicalStmt, [])
    #nonstandard ones: we check is_intrinsic before this is called.
    elif anIntrinsicApp.head.lower() in getNonStandard():
        return (fortStmts.IntegerStmt, [])
    else:
        return typemerge([expressionType(anArg,localSymtab,lineNumber) for anArg in anIntrinsicApp.args],
                         (None,None))

def genericFunctionType(aFunctionApp,localSymtab,lineNumber):
    # find the symbol:
    symtabEntry=localSymtab.lookup_name(aFunctionApp.head)
    # find a match for the signature:
    for sName in symtabEntry.genericInfo.resolvableTo.keys():
       signature=symtabEntry.genericInfo.resolvableTo[sName]
       # we don't cover optional arguments here - yet
       if len(signature)!=len(aFunctionApp.args):
          continue
       matched=True
       for formal,actual in zip(signature.keys(),aFunctionApp.args):
          if signature[formal]!=expressionType(actual,localSymtab,lineNumber) :
             matched=False
             break
       if (not matched):
          continue
       return localSymtab.lookup_name(sName).type
    raise InferenceError('inference.genericFunctionType: could not resolve generic "'+aFunctionApp.head+'"',lineNumber)

def appType(anApp,localSymtab,lineNumber):
    DebugManager.debug('inference.appType called on '+str(anApp)+'...',newLine=False)
    if isinstance(anApp.head,App): # example: matrix(3)(2:14)
        return appType(anApp.head,localSymtab,lineNumber)
    if isinstance(anApp.head,Sel):  # example type%member(1)
        return expressionType(anApp.head,localSymtab,lineNumber)
    returnType = None
    # intrinsics: do a type merge
    if is_intrinsic(anApp.head):
        returnType = __intrinsicType(anApp,localSymtab,lineNumber)
        DebugManager.debug(' It is an INTRINSIC of type '+str(returnType))
    # nonintrinsics: Look for it in the symbol table or for implicit type
    else:
        returnType = identifierType(anApp.head,localSymtab,lineNumber)
        if (returnType is None) :
           # this must be a generic
           returnType=genericFunctionType(anApp,localSymtab,lineNumber)
        DebugManager.debug(' It is an NONINTRINSIC of type '+str(returnType))
    return returnType

def selectionType(aSelectionExpression,localSymtab,lineNumber):
    DebugManager.debug('inference.SelectionType: determining type of selection expression '+str(aSelectionExpression)+' using symtab '+str(localSymtab))
    # lookup type of head
    dType=expressionType(aSelectionExpression.head,localSymtab,lineNumber)
    # lookup the projection type
    pType=identifierType(dType[1][0]+":"+aSelectionExpression.proj,localSymtab,lineNumber)
    return pType

def expressionType(anExpression,localSymtab,lineNumber):
    DebugManager.debug('inference.expressionType: determining type of expression '+str(anExpression)+'...',newLine=False)
    if isinstance(anExpression,str) and is_const(anExpression):
        rType=constantType(anExpression,lineNumber)
        DebugManager.debug(' it\'s a '+str(rType)+' constant')
        return rType
    elif isinstance(anExpression,str) and _id_re.match(anExpression):
        DebugManager.debug(' it\'s an IDENTIFIER')
        return identifierType(anExpression,localSymtab,lineNumber)
    elif isinstance(anExpression,Unary):
        DebugManager.debug(' it\'s a UNARY EXPRESSION')
        return expressionType(anExpression.exp,localSymtab,lineNumber)
    elif isinstance(anExpression,Ops):
        if (anExpression.op =='//'):
            return typemerge([expressionType(anExpression.a1,localSymtab,lineNumber),
                              expressionType(anExpression.a2,localSymtab,lineNumber)],
                              (None,None),True)
        else: 
            DebugManager.debug(' it\'s a BINARY EXPRESSION')
            return typemerge([expressionType(anExpression.a1,localSymtab,lineNumber),
                              expressionType(anExpression.a2,localSymtab,lineNumber)],
                              (None,None))
    elif isinstance(anExpression,App):
        DebugManager.debug(' it\'s an APPLICATION')
        return appType(anExpression,localSymtab,lineNumber)
    elif isinstance(anExpression,NamedParam):
        DebugManager.debug(' it\'s a NAMED PARAMETER')
        return expressionType(anExpression.myRHS,localSymtab,lineNumber)
    elif isinstance(anExpression,Sel):
       DebugManager.debug(' it\'s a SELECTION EXPRESSION')
       return selectionType(anExpression,localSymtab,lineNumber)
    elif isinstance(anExpression,Slice) :
       DebugManager.debug(' it\'s a SLICE EXPRESSION')
       return (fortStmts.IntegerStmt, [])
    elif isinstance(anExpression,fortStmts._NoInit):
       DebugManager.debug(' it\'s a NO INIT EXPRESSION')
       return expressionType(anExpression.lhs,localSymtab,lineNumber)
    elif isinstance(anExpression,fortStmts._ImplicitDoConstruct):
       DebugManager.debug(' it\'s a implicit DO')
       return expressionType(anExpression.object,localSymtab,lineNumber)
    elif isinstance(anExpression,ArrayConstructor): # look at the first element in the list, they have to be consistent
       DebugManager.debug(' it\'s an ARRAY CONSTRUCTOR')
       return expressionType(anExpression.valueList[0],localSymtab,lineNumber)
    else:
       raise InferenceError('inference.expressionType: No type could be determined for expression "'+str(anExpression)+'" (represented as '+repr(anExpression)+' )',lineNumber)

def canonicalTypeClass(typeClass,modList):
   if modList:
      if (typeClass==fortStmts.RealStmt):
         if (isinstance(modList[0],fortStmts._Kind)):
            if (modList[0].mod=='8'):
               return (True,fortStmts.DoubleStmt)
            if (modList[0].mod=='4'):
               return (True,fortStmts.RealStmt)
   else:
      return (True,typeClass)
   return (False,None)

def shapecompare(d1,d2):
    DebugManager.debug('inference.shapecompare called on d1 = "'+str(d1)+'\td2 = "'+str(d2)+'"')
    if d1 is None :
       return d2
    return d1

def shapemerge(lst,default):
    DebugManager.debug('inference.shapemerge called on '+str(lst)+'...',newLine=False)
    if not lst: return default
    if len(lst) == 1: return lst[0]
    d1 = shapecompare(lst[0],lst[1])
    for l in lst[2:]:
        d1 = shapecompare(d1,l)
    DebugManager.debug(' result is '+str(d1))
    return d1

def constantShape(e,lineNumber):
    kind_re = re.compile(r'_(\w+)')
    if _flonum_re.match(e):
       return None
    if _int_re.match(e):
       return None
    if e.lower() in _logicon_set:
       return None
    if e[0] in _quote_set:
       return None
    raise InferenceError('inference.constantShape: No shape could be determined for "'+e+'"',lineNumber)
    
def __identifierShape(anId,localSymtab,lineNumber):
    returnShape=None
    (symtabEntry,containingSymtab) = localSymtab.lookup_name_level(anId)
    if symtabEntry:
        returnShape = symtabEntry.dimensions
        dbgStr='with symtab entry '+symtabEntry.debug(anId)+' -> returning shape '
        if returnShape:
           dbgStr+=str(returnShape)
        else:
           dbgStr+='None'
        DebugManager.debug(dbgStr)
    return returnShape

def arrayReferenceShape(arrRefApp,localSymtab,lineNumber):
    DebugManager.debug('inference.arrayReferenceShape called on '+repr(arrRefApp)+'...',newLine=False)
    symtabEntry=None
    if isinstance(arrRefApp.head,Sel):
       theSel=arrRefApp.head
       # lookup type of head
       dType=expressionType(theSel.head,localSymtab,lineNumber)
       # lookup the projection
       (symtabEntry,containingSymtab) = localSymtab.lookup_name_level(dType[1][0]+":"+theSel.proj)
    else: 
       (symtabEntry,containingSymtab) = localSymtab.lookup_name_level(arrRefApp.head)
    dimensions=[]
    symDimIndex=0
    for index in arrRefApp.args:
       if isinstance(index,Zslice):
          dimensions.append(symtabEntry.dimensions[symDimIndex])
       if isinstance(index,Lslice):
          if (isinstance(symtabEntry.dimensions[symDimIndex],Ops)):
             uBound=symtabEntry.dimensions[symDimIndex].a2
          else:
             uBound=symtabEntry.dimensions[symDimIndex]
          dimensions.append(Ops(':',index.arg,uBound))
       if isinstance(index,Rslice):
          if (isinstance(symtabEntry.dimensions[symDimIndex],Ops)):
             lBound=symtabEntry.dimensions[symDimIndex].a1
          else:
             lBound='1'
          dimensions.append(Ops(':',lBound,index.arg))
       elif (isRangeExpression(index)):
          dimensions.append(index)
       symDimIndex+=1
    while symDimIndex<len(symtabEntry.dimensions):
       dimensions.append(symtabEntry.dimensions[symDimIndex])
    if (dimensions!=[]): 
       return tuple(dimensions)
    return None

def __intrinsicShape(anIntrinsicApp,localSymtab,lineNumber):
    if anIntrinsicApp.head.lower() in ['reshape','matmul']:
        raise InferenceError('inference.__intrinsicShape: not implemented for "'+anIntrinsicApp+'"',lineNumber)
    if anIntrinsicApp.head.lower() in ['maxval','minval','lge','lgt','lle','llt','scan','size','time']:
       return None
    else:
        return shapemerge([expressionShape(anArg,localSymtab,lineNumber) for anArg in anIntrinsicApp.args],
                         (None,None))

__ourMissingFuncDefs=set()
def __functionShape(aFunctionApp,localSymtab,lineNumber):
    class MapParams:
       
       def __init__(self,aFunctionApp,aSymtabEntry,lineNumber):
          self.argDict={}          
          for pos,arg in enumerate(aFunctionApp.args):
             if isinstance(arg,NamedParam):
                if (arg.myId in self.argDict.keys()):
                   raise InferenceError('formal parameter for actual named parameter: '+str(arg)+
                                        ' is already in the  mapping dictionary for positional parameter '+
                                        str(argDict[arg.myId][1])+' at position '+str(argDict[arg.myId][0]),lineNumber)
                namedParmFormalPosition=symtabEntry.funcFormalArgs.positionOf(arg.myId)
                if (namedParmFormalPosition in map(lambda l: l[0],self.argDict.values())):
                   raise InferenceError('formal parameter position '+namedParmFormalPosition+' for actual named parameter: '+str(arg)+
                                        ' is already in the  mapping dictionary as '+
                                        str(argDict[arg.myId][1]),lineNumber)
                self.argDict[arg.myId]=(symtabEntry.funcFormalArgs.positionOf(arg.myId),arg.myRHS)
             else:
                formalParm=symtabEntry.funcFormalArgs.nameByPosition(pos)
                if (formalParm in self.argDict.keys() or pos in map(lambda l: l[0],self.argDict.values())):
                   raise InferenceError('Formal parameter: '+formalParm+' for position '+str(pos)+
                                        ' is already in the  mapping dictionary with actual parameter value '+str(self.argDict[arg.myId][1]),lineNumber)
                self.argDict[formalParm]=(pos,arg)
          
       def replaceFormalWithActual(self,dimExpr):
          ''' this modifies dimExpr'''
          if  isinstance(dimExpr,Unary):
             dimExpr.exp=self.replaceFormalWithActual(dimExpr.exp)
          elif isinstance(dimExpr,Ops):
             dimExpr.a1=self.replaceFormalWithActual(dimExpr.a1)
             dimExpr.a2=self.replaceFormalWithActual(dimExpr.a2)
          elif isinstance(dimExpr,App):
             args=[]
             for arg in dimExpr.args:
                args.append(self.replaceFormalWithActual(arg))
             dimExpr.args=args
          elif isinstance(dimExpr,str) and is_id(dimExpr) and dimExpr in self.argDict.keys():
             dimExpr=copy.deepcopy(self.argDict[dimExpr][1])
          return dimExpr

    DebugManager.debug('inference.__functionShape called on '+str(aFunctionApp)+'...',newLine=False)
    # example: bbb(3)(2:14)
    if isinstance(aFunctionApp.head,App):
        return __functionShape(aFunctionApp.head,localSymtab,lineNumber)
    returnShape = None
    # intrinsics: do a shape merge
    if is_intrinsic(aFunctionApp.head):
        returnShape = __intrinsicShape(aFunctionApp,localSymtab,lineNumber)
        DebugManager.debug(' It is an INTRINSIC of shape '+str(returnShape))
    # non-intrinsics: Look for it in the symbol table
    else:
       (symtabEntry,containingSymtab) = localSymtab.lookup_name_level(aFunctionApp.head)
       if symtabEntry:
          returnShape = symtabEntry.dimensions
       if (returnShape is None) :
          # this may be a generic
          DebugManager.debug('inference.__genericFunctionShape called on '+str(aFunctionApp)+'...',newLine=False)
          specInfo=__genericResolve(aFunctionApp,localSymtab,lineNumber)
          if specInfo:
             symtabEntry=specInfo[1]
             returnShape=specInfo[1].dimensions
       DebugManager.debug(' It is an NONINTRINSIC of shape '+str(returnShape))
       if (aFunctionApp.args and symtabEntry is None):
          global __ourMissingFuncDefs
          if (not (aFunctionApp.head.lower() in __ourMissingFuncDefs)) :
              __ourMissingFuncDefs.add(aFunctionApp.head.lower())
              DebugManager.warning('no explicit definition for function "'+str(aFunctionApp.head)+'" called as "'+str(aFunctionApp)+'" found within the current compile unit.',lineNumber,DebugManager.WarnType.noDefinition)
       if (returnShape and aFunctionApp.args and symtabEntry and symtabEntry.funcFormalArgs):
         # the dimensions need to be checked for references to formal parameters
         aMapParams=MapParams(aFunctionApp,symtabEntry,lineNumber)
         returnShape=copy.deepcopy(returnShape) # don't want to modify the original
         for dim in returnShape:
            aMapParams.replaceFormalWithActual(dim)
    return returnShape

def selectionShape(aSelectionExpression,localSymtab,lineNumber):
    DebugManager.debug('inference.SelectionShape: determining shape of selection expression '+str(aSelectionExpression)+' using symtab '+str(localSymtab))
    # lookup type of head
    dType=expressionType(aSelectionExpression.head,localSymtab,lineNumber)
    # lookup the projection shape
    pShape=__identifierShape(dType[1][0]+":"+aSelectionExpression.proj,localSymtab,lineNumber)
    return pShape

def expressionShape(anExpression,localSymtab,lineNumber):
    DebugManager.debug('inference.expressionShape: determining shape of expression '+str(anExpression)+'...',newLine=False)
    if isinstance(anExpression,str) and is_const(anExpression):
        rShape=constantShape(anExpression,lineNumber)
        DebugManager.debug(' it\'s a '+str(rShape)+' constant')
        return rShape
    elif isinstance(anExpression,str) and _id_re.match(anExpression):
        DebugManager.debug(' it\'s an IDENTIFIER')
        return __identifierShape(anExpression,localSymtab,lineNumber)
    elif isinstance(anExpression,Unary):
        DebugManager.debug(' it\'s a UNARY EXPRESSION')
        return expressionShape(anExpression.exp,localSymtab,lineNumber)
    elif isinstance(anExpression,Ops):
        DebugManager.debug(' it\'s a BINARY EXPRESSION')
        return shapemerge([expressionShape(anExpression.a1,localSymtab,lineNumber),
                          expressionShape(anExpression.a2,localSymtab,lineNumber)],
                                   (None,None))
    elif isinstance(anExpression,App):
        DebugManager.debug(' it\'s an APPLICATION')
        if isArrayReference(anExpression,localSymtab,lineNumber):
           return arrayReferenceShape(anExpression,localSymtab,lineNumber)
        else : 
           return __functionShape(anExpression,localSymtab,lineNumber)
    elif isinstance(anExpression,NamedParam):
        DebugManager.debug(' it\'s a NAMED PARAMETER')
        return expressionShape(anExpression.myRHS,localSymtab,lineNumber)
    elif isinstance(anExpression,Sel):
        DebugManager.debug(' it\'s a SELECTION EXPRESSION')
        return selectionShape(anExpression,localSymtab,lineNumber)
    elif isinstance(anExpression,ArrayConstructor):  
        DebugManager.debug(' it\'s an ARRAY CONSTRUCTOR')
        return [len(anExpression.valueList)]
    else:
        raise InferenceError('inference.expressionShape: No shape could be determined for expression "'+str(anExpression)+'"',lineNumber)

def __genericResolve(aFunctionApp,localSymtab,lineNumber):
    ''' returns tuple (<specificName>,<symtabEntry>) for generic aFunctionApp '''
    # find the symbol:
    sName=aFunctionApp.head
    symtabEntry=localSymtab.lookup_name(aFunctionApp.head)
    if symtabEntry is None:
       # f77 style call, doesn't have to be in the symboltable
       return None
    if (symtabEntry.genericInfo is None
        or
        len(symtabEntry.genericInfo.resolvableTo)==0) : # not overloaded
       return (sName,symtabEntry)
    # find a match for the signature:
    for sName in symtabEntry.genericInfo.resolvableTo.keys():
       signature=symtabEntry.genericInfo.resolvableTo[sName]
       # we don't cover optional arguments here - yet
       if len(signature)!=len(aFunctionApp.args):
          DebugManager.debug('inference.__genericResolve signature length mismatch at specific '+
                             str(sName)+'('+','.join(signature.keys())+
                             ') for generic call '+
                             aFunctionApp.head+'('+','.join([str(arg) for arg in aFunctionApp.args]))
          continue
       matched=True
       for formal,actual in zip(signature.keys(),aFunctionApp.args):
          if signature[formal]!=expressionType(actual,localSymtab,lineNumber) :
             DebugManager.debug('inference.__genericResolve argument type mismatch for specific "'+
                                str(sName)+'" at formal "'+
                                str(formal)+'"('+str(signature[formal])+')'
                                ' for call to generic "'+
                                aFunctionApp.head+'" at actual "'+
                                str(actual)+'"('+ str(expressionType(actual,localSymtab,lineNumber))+')')
             matched=False
             break
          formalRank=0
          for typeAttr in signature[formal][1] :
             if isinstance(typeAttr,App) and typeAttr.head.lower()=='dimension' :
                formalRank=len(tuple(typeAttr.args))
                break
          actualRank=0
          actualShape=expressionShape(actual,localSymtab,lineNumber)
          if (actualShape):
             actualRank=len(actualShape)
          if formalRank!=actualRank:
             DebugManager.debug('inference.__genericResolve argument rank mismatch for specific "'+
                                str(sName)+'" at formal "'+
                                str(formal)+'"('+str(formalRank)+')'
                                ' for call to generic "'+
                                aFunctionApp.head+'" at actual "'+
                                str(actual)+'"('+ str(actualRank)+')')
             matched=False
             break
       if (not matched):
          continue
       return (sName,localSymtab.lookup_name(sName))
    raise InferenceError('inference.__genericResolve: could not resolve generic "'+aFunctionApp.head+'"',lineNumber)

def isRangeExpression(theExpression):
   return (isinstance(theExpression,Ops) and theExpression.op==':')

def __selSymtabName(aSel,localSymtab,lineNumber):
    ''' for aSel the name to be used for a symbol table lookup is "<type_name>:<member name>" '''
    # lookup type of head
    dType=expressionType(aSel.head,localSymtab,lineNumber)
    return dType[1][0]+":"+aSel.proj
    
def isArrayReference(theApp,localSymtab,lineNumber):
    if not isinstance(theApp,App):
        raise InferenceError('inference.isArrayReference: called on non-App object '+str(theApp),lineNumber)
    DebugManager.debug('inference.isArrayReference: Application Expression "'+str(theApp))
    lookupName=""
    if isinstance(theApp.head,Sel): # example type%member(1)
        lookupName=__selSymtabName(theApp.head,localSymtab,lineNumber)
    else:
        lookupName=theApp.head
    theSymtabEntry=localSymtab.lookup_name(lookupName)
    DebugManager.debug('inference.isArrayReference: symtab entry is '+str(theSymtabEntry))
    if not theSymtabEntry:
        return False
    # there has to be a symbol table entry for a variable
    DebugManager.debug('inference.isArrayReference for '+theSymtabEntry.debug(lookupName))
    if isinstance(theSymtabEntry.entryKind(),SymtabEntry.ProcedureEntryKind):
        return False
    if (not theSymtabEntry.dimensions or theSymtabEntry.dimensions == ()) and \
       (not theSymtabEntry.length or theSymtabEntry.length == 1):
        #  now we know that its NOT a scalar variable, but rather a function.  so we update the symbol table with this information.
        if (not theSymtabEntry.entryKind in [SymtabEntry.InterfaceEntryKind,SymtabEntry.StatementFunctionEntryKind]) : 
           DebugManager.debug('inference.isArrayReference: Application Expression "'+str(theApp)+\
                              '" for something that we thought was a scalar variable => assuming it\'s a function and updating the symbol table to reflect this')
           theSymtabEntry.enterEntryKind(SymtabEntry.FunctionEntryKind)
        return False
    return True

