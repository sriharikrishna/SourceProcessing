'''functions related to shape inference:

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
from typeInference import expressionType

class ShapeInferenceError(Exception):
   '''exception for ...'''
   def __init__(self,msg,lineNumber=None):
       self.msg  = msg
       self.lineNumber = lineNumber

def shapecompare(d1,d2):
    DebugManager.debug('shapeInference.shapecompare called on d1 = "'+str(d1)+'\td2 = "'+str(d2)+'"')
    if d1 is None :
       return d2
    return d1

def shapemerge(lst,default):
    DebugManager.debug('shapeInference.shapemerge called on '+str(lst)+'...',newLine=False)
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
    raise ShapeInferenceError('shapeInference.constantShape: No shape could be determined for "'+e+'"',lineNumber)
    

def identifierShape(anId,localSymtab,lineNumber):
    (symtabEntry,containingSymtab) = localSymtab.lookup_name_level(anId)
    # a shape is known -> return it
    if symtabEntry and symtabEntry.dimensions:
        returnShape = symtabEntry.dimensions
        DebugManager.debug('with symtab entry '+symtabEntry.debug(anId)+' -> returning shape '+str(returnShape))
    elif symtabEntry and symtabEntry.entryKind==SymtabEntry.InterfaceEntryKind and symtabEntry.genericInfo:
       if len(symtabEntry.genericInfo.resolvableTo)==1:
          specificName=(symtabEntry.genericInfo.resolvableTo.keys())[0]
          returnShape=identifierShape(specificName,containingSymtab,lineNumber)
          DebugManager.debug('with symtab entry '+containingSymtab.lookup_name_level(specificName)[0].debug(specificName)+' -> returning shape '+str(returnShape))
       else :
          return None
    else: # no symtab entry or an entry exists with no shape
       return None
    if not returnShape:
        raise ShapeInferenceError('shapeInference.identifierShape: No shape could be determined for identifier "'+anId+'"',lineNumber)
    return returnShape

def intrinsicShape(anIntrinsicApp,localSymtab,lineNumber):
    if anIntrinsicApp.head.lower() in ['reshape','matmul']:
        raise ShapeInferenceError('shapeInference.intrinsicShape: not implemented for "'+anIntrinsicApp+'"',lineNumber)
    else:
        return shapemerge([expressionShape(anArg,localSymtab,lineNumber) for anArg in anIntrinsicApp.args],
                         (None,None))

def genericFunctionShape(aFunctionApp,localSymtab,lineNumber):
    DebugManager.debug('shapeInference.genericFunctionShape called on '+str(aFunctionApp)+'...',newLine=False)
    # find the symbol:
    symtabEntry=localSymtab.lookup_name(aFunctionApp.head)
    if symtabEntry is None:
       # f77 style call
       return None
    if symtabEntry.genericInfo is None:
       return symtabEntry.dimensions
    if len(symtabEntry.genericInfo.resolvableTo)==0:
       return symtabEntry.dimensions
    # find a match for the signature:
    for sName in symtabEntry.genericInfo.resolvableTo.keys():
       signature=symtabEntry.genericInfo.resolvableTo[sName]
       # we don't cover optional arguments here - yet
       if len(signature)!=len(aFunctionApp.args):
          continue
       matched=True
       for formal,actual in zip(signature.keys(),aFunctionApp.args):
          if (signature[formal]!=expressionType(actual,localSymtab,lineNumber)) :
             DebugManager.debug('shapeInference.genericFunctionShape signature mismatch at '+str(aFunctionApp)+' for formal "'+str(formal)+'" ('+str(signature[formal])+'")!= actual "'+actual+'"('+ str(expressionType(actual,localSymtab,lineNumber))+')')
             matched=False
             break
          formalShape=None
          for typeAttr in signature[formal][1] :
             if isInstance(typeAttr,App) and typeAttr.head.lower()=='dimension' :
                formalShape=tuple(typeAttr.args)
                break
          actualShape=expressionShape(actual,localSymtab,lineNumber)
          if formalShape!=actualShape:
             DebugManager.debug('shapeInference.genericFunctionShape signature mismatch at '+str(aFunctionApp)+' for formal "'+formal+'" ('+str(formalShape)+'")!= actual "'+actual+'"('+ str(actualShape)+')') 
             matched=False
             break
       if (not matched):
          continue
       return localSymtab.lookup_name(sName).dimensions
    raise ShapeInferenceError('shapeInference.genericFunctionShape: could not resolve generic "'+aFunctionApp.head+'"',lineNumber)
   

def functionShape(aFunctionApp,localSymtab,lineNumber):
    DebugManager.debug('shapeInference.functionShape called on '+str(aFunctionApp)+'...',newLine=False)
    # example: bbb(3)(2:14)
    if isinstance(aFunctionApp.head,App):
        return functionShape(aFunctionApp.head,localSymtab,lineNumber)
    returnShape = None
    # intrinsics: do a shape merge
    if is_intrinsic(aFunctionApp.head):
        returnShape = intrinsicShape(aFunctionApp,localSymtab,lineNumber)
        DebugManager.debug(' It is an INTRINSIC of shape '+str(returnShape))
    # nonintrinsics: Look for it in the symbol table or for implicit shape
    else:
        returnShape = identifierShape(aFunctionApp.head,localSymtab,lineNumber)
        if (returnShape is None) :
           # this must be a generic
           returnShape=genericFunctionShape(aFunctionApp,localSymtab,lineNumber)
        DebugManager.debug(' It is an NONINTRINSIC of shape '+str(returnShape))
    return returnShape

def selectionShape(aSelectionExpression,localSymtab,lineNumber):
    DebugManager.debug('shapeInference.SelectionShape: determining shape of selection expression '+str(aSelectionExpression)+' using symtab '+str(localSymtab))
    # retrieve information for the derived shape from the symbol table
    raise ShapeInferenceError('shapeInference.selectionShape: called on "'+str(theApp)+'" (Not yet implemented)',lineNumber)

def expressionShape(anExpression,localSymtab,lineNumber):
    DebugManager.debug('shapeInference.expressionShape: determining shape of expression '+str(anExpression)+'...',newLine=False)
    if isinstance(anExpression,str) and is_const(anExpression):
        rShape=constantShape(anExpression,lineNumber)
        DebugManager.debug(' it\'s a '+str(rShape)+' constant')
        return rShape
    elif isinstance(anExpression,str) and _id_re.match(anExpression):
        DebugManager.debug(' it\'s an IDENTIFIER')
        return identifierShape(anExpression,localSymtab,lineNumber)
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
        return functionShape(anExpression,localSymtab,lineNumber)
    elif isinstance(anExpression,NamedParam):
        DebugManager.debug(' it\'s a NAMED PARAMETER')
        return expressionShape(anExpression.myRHS,localSymtab,lineNumber)
    elif isinstance(anExpression,Sel):
        DebugManager.debug(' it\'s a SELECTION EXPRESSION')
        return selectionShape(anExpression.myRHS,lineNumber)
    else:
        raise ShapeInferenceError('shapeInference.expressionShape: No shape could be determined for expression "'+str(anExpression)+'"',lineNumber)

def isArrayReference(theApp,localSymtab,lineNumber):
    if not isinstance(theApp,App):
        raise ShapeInferenceError('shapeInference.isArrayReference: called on non-App object '+str(theApp),lineNumber)
    theSymtabEntry = localSymtab.lookup_name(theApp.head)
    if not theSymtabEntry:
        return False
    # there has to be a symbol table entry for a variable
    DebugManager.debug('shapeInference.isArrayReference for '+theSymtabEntry.debug(theApp.head))
    if isinstance(theSymtabEntry.entryKind(),SymtabEntry.ProcedureEntryKind):
        return False
    if (not theSymtabEntry.dimensions or theSymtabEntry.dimensions == ()) and \
       (not theSymtabEntry.length or theSymtabEntry.length == 1):
#       now we know that its NOT a scalar variable, but rather a function.  so we update the symbol table with this information.
        if (theSymtabEntry.entryKind!=SymtabEntry.InterfaceEntryKind) : 
           DebugManager.debug('shapeInference.isArrayReference: Application Expression "'+str(theApp)+\
                              '" for something that we thought was a scalar variable => assuming it\'s a function and updating the symbol table to reflect this')
           theSymtabEntry.enterEntryKind(SymtabEntry.FunctionEntryKind)
        return False
    return True

def canonicalShapeClass(shapeClass,modList):
   if modList:
      if (shapeClass==fortStmts.RealStmt):
         if (isinstance(modList[0],fortStmts._Kind)):
            if (modList[0].mod=='8'):
               return (True,fortStmts.DoubleStmt)
            if (modList[0].mod=='4'):
               return (True,fortStmts.RealStmt)
   else:
      return (True,shapeClass)
   return (False,None)
