'''functions related to type inference:

    ...
'''

import re

from _Setup import *

from PyUtil.debugManager import DebugManager
from PyUtil.symtab import SymtabEntry
from PyUtil.debugManager import DebugManager

import fortStmts
from fortExp import App,NamedParam,Sel,Unary,Ops,is_const,_id_re,_flonum_re,_int_re,_logicon_set,_quote_set
from intrinsic import is_intrinsic

class TypeInferenceError(Exception):
   '''exception for ...'''
   def __init__(self,msg,lineNumber=None):
       self.msg  = msg
       self.lineNumber = lineNumber

def kw2type(s): return(fortStmts.kwtbl[s.lower()])
def lenfn(n): return [fortStmts._F77Len(str(n))]

_modhash = { fortStmts._Prec     : 0,
             fortStmts._Kind     : 1,
             fortStmts._ExplKind : 2,
             }

def modcompare(m1,m2):
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
          l1=int(mm1.len)
          l2=int(mm2.len)
          if l1>l2:
             return m1
          else :
             return m2
       # if they are not both integers there could be some parameter name etc.
       except ValueError:
          raise TypeInferenceError('modcompare: cannot compare length specifiers '+mm1.len+' and '+mm2.len)
    if _modhash[c1] >= _modhash[c2]: return m1
    return m2

def typecompare(t1,t2):
    DebugManager.debug('typeInference.typecompare called on t1 = "'+str(t1)+'\tt2 = "'+str(t2)+'"')
    mergeit = dict(character=0,
                   logical=1,
                   integer=2,
                   real=3,
                   doubleprecision=4,
                   complex=5,
                   doublecomplex=6,
                   )

    if t1[0] == t2[0]:
        return(t1[0],modcompare(t1[1],t2[1]))

    if mergeit[t1[0].kw] > mergeit[t2[0].kw]: return t1

    return t2

def typemerge(lst,default):
    DebugManager.debug('typeInference.typemerge called on '+str(lst)+'...',newLine=False)
    if not lst: return default
    if len(lst) == 1: return lst[0]
    t1 = typecompare(lst[0],lst[1])
    for l in lst[2:]:
        t1 = typecompare(t1,l)
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
    elif symtabEntry and symtabEntry.entryKind==SymtabEntry.InterfaceEntryKind and symtabEntry.genericInfo:
       if len(symtabEntry.genericInfo.resolvableTo)==1:
          specificName=(symtabEntry.genericInfo.resolvableTo.keys())[0]
          returnType=identifierType(specificName,containingSymtab,lineNumber)
          DebugManager.debug('with symtab entry '+containingSymtab.lookup_name_level(specificName)[0].debug(specificName)+' -> returning type '+str(returnType))
       else :
          return None
    # an entry exists with no type -> try to type implicitly
    elif symtabEntry:
        symtabEntry.enterType(containingSymtab.implicit[anId[0]])
        returnType = symtabEntry.type
        DebugManager.debug('with symtab entry'+symtabEntry.debug(anId)+' (without type).  Implicit type (locally) is '+str(returnType))
        DebugManager.warning('typeInference.identifierType: [line '+str(lineNumber)+'] implicit typing (scope ='+str(containingSymtab)+') used for identifier "'+anId+'" type ="'+str(returnType)+'"')
    else: # no symtab entry -> try local implicit typing
        returnType = localSymtab.implicit[anId[0]]
        DebugManager.warning('typeInference.identifierType: [line '+str(lineNumber)+'] local scope implicit typing used for identifier "'+anId+'" type ="'+str(returnType)+'"')
        DebugManager.debug('with implicit type '+str(returnType))
    if not returnType:
        raise TypeInferenceError('typeInference.identifierType: No type could be determined for identifier "'+anId+'"',lineNumber)
    return returnType

def intrinsicType(anIntrinsicApp,localSymtab,lineNumber):
    if anIntrinsicApp.head.lower() in ['aimag','alog','real']:
        return (fortStmts.RealStmt, [])
    elif anIntrinsicApp.head.lower() in ['int','idint','size','lbound','ubound','shape']:
        return (fortStmts.IntegerStmt, [])
    elif anIntrinsicApp.head.lower() in ['dble','dabs','dexp','dlog','dsqrt','dmod']:
        return (fortStmts.DoubleStmt, [])
    elif anIntrinsicApp.head.lower() == 'cmplx':
        return (fortStmts.ComplexStmt, [])
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
    raise TypeInferenceError('typeInference.genericFunctionType: could not resolve generic "'+aFunctionApp.head+'"',lineNumber)
   

def functionType(aFunctionApp,localSymtab,lineNumber):
    DebugManager.debug('typeInference.functionType called on '+str(aFunctionApp)+'...',newLine=False)
    # example: bbb(3)(2:14)
    if isinstance(aFunctionApp.head,App):
        return functionType(aFunctionApp.head,localSymtab,lineNumber)
    returnType = None
    # intrinsics: do a type merge
    if is_intrinsic(aFunctionApp.head):
        returnType = intrinsicType(aFunctionApp,localSymtab,lineNumber)
        DebugManager.debug(' It is an INTRINSIC of type '+str(returnType))
    # nonintrinsics: Look for it in the symbol table or for implicit type
    else:
        returnType = identifierType(aFunctionApp.head,localSymtab,lineNumber)
        if (returnType is None) :
           # this must be a generic
           returnType=genericFunctionType(aFunctionApp,localSymtab,lineNumber)
        DebugManager.debug(' It is an NONINTRINSIC of type '+str(returnType))
    return returnType

def selectionType(aSelectionExpression,localSymtab,lineNumber):
    DebugManager.debug('typeInference.SelectionType: determining type of selection expression '+str(aSelectionExpression)+' using symtab '+str(localSymtab))
    # retrieve information for the derived type from the symbol table
    raise TypeInferenceError('typeInference.selectionType: called on "'+str(theApp)+'" (Not yet implemented)',lineNumber)

def expressionType(anExpression,localSymtab,lineNumber):
    DebugManager.debug('typeInference.expressionType: determining type of expression '+str(anExpression)+'...',newLine=False)
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
        DebugManager.debug(' it\'s a BINARY EXPRESSION')
        return typemerge([expressionType(anExpression.a1,localSymtab,lineNumber),
                          expressionType(anExpression.a2,localSymtab,lineNumber)],
                                   (None,None))
    elif isinstance(anExpression,App):
        DebugManager.debug(' it\'s an APPLICATION')
        return functionType(anExpression,localSymtab,lineNumber)
    elif isinstance(anExpression,NamedParam):
        DebugManager.debug(' it\'s a NAMED PARAMETER')
        return expressionType(anExpression.myRHS,localSymtab,lineNumber)
    elif isinstance(anExpression,Sel):
        DebugManager.debug(' it\'s a SELECTION EXPRESSION')
        return selectionType(anExpression.myRHS,lineNumber)
    else:
        raise TypeInferenceError('typeInference.expressionType: No type could be determined for expression "'+str(anExpression)+'"',lineNumber)

def isArrayReference(theApp,localSymtab,lineNumber):
    if not isinstance(theApp,App):
        raise TypeInferenceError('typeInference.isArrayReference: called on non-App object '+str(theApp),lineNumber)
    theSymtabEntry = localSymtab.lookup_name(theApp.head)
    if not theSymtabEntry:
        return False
    # there has to be a symbol table entry for a variable
    DebugManager.debug('typeInference.isArrayReference for '+theSymtabEntry.debug(theApp.head))
    if isinstance(theSymtabEntry.entryKind(),SymtabEntry.ProcedureEntryKind):
        return False
    if (not theSymtabEntry.dimensions or theSymtabEntry.dimensions == ()) and \
       (not theSymtabEntry.length or theSymtabEntry.length == 1):
#       now we know that its NOT a scalar variable, but rather a function.  so we update the symbol table with this information.
        if (theSymtabEntry.entryKind!=SymtabEntry.InterfaceEntryKind) : 
           DebugManager.debug('typeInference.isArrayReference: Application Expression "'+str(theApp)+\
                              '" for something that we thought was a scalar variable => assuming it\'s a function and updating the symbol table to reflect this')
           theSymtabEntry.enterEntryKind(SymtabEntry.FunctionEntryKind)
        return False
    return True

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
