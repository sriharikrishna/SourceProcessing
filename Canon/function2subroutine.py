'''
Function to Subroutine conversion
'''

from _Setup import *
from optparse import OptionParser

from PyFort.fortUnit import Unit,fortUnitIterator
from PyUtil.debugManager import DebugManager
from PyUtil.symtab import Symtab,SymtabError
import PyFort.fortStmts as fs
import PyFort.fortExp as fe
import copy
import re

class FunToSubError(Exception):
    '''exception for errors that occur during the transformation of function unit definitions to subroutine definitions'''
    def __init__(self,msg):
        self.msg  = msg

name_init = 'oad_s_'

def createTypeDecl(type_kw,mod,outParam,lead):
    DebugManager.debug(10*'-'+'>'+'called function2subroutine.createTypeDecl ' \
                     + 'with type keyword "'+type_kw+'",' \
                     +' mod = "'+str(mod)+'",' \
                     +' outParam = "'+str(outParam)+'"')
    intentArg = fe.App('intent',['out'])
    try:
        newDecl = {
            'real': fs.RealStmt(mod,[intentArg],[outParam,],lead=lead),
            'complex': fs.ComplexStmt(mod,[intentArg],[outParam,],lead=lead),
            'integer': fs.IntegerStmt(mod,[intentArg],[outParam,],lead=lead),
            'logical': fs.LogicalStmt(mod,[intentArg],[outParam,],lead=lead),
            'doubleprecision': fs.DoubleStmt(mod,[intentArg],[outParam,],lead=lead),
            'doublecomplex': fs.DoubleCplexStmt(mod,[intentArg],[outParam,],lead=lead)
            }[type_kw]
    except KeyError:
        raise FunToSubError('Unrecognized type "'+type_kw+'"')

    return newDecl

def convertFunctionDecl(aDecl,oldFuncnewSubPairs):
    DebugManager.debug(10*'-'+'>'+'called function2subroutine.convertFunctionDecl ' \
                     + 'on declaration statement "'+str(aDecl)+'"' \
                     +' with oldFuncnewSubPairs = "'+str(oldFuncnewSubPairs)+'"')
    newDecl = copy.deepcopy(aDecl)
    modified = False
    if hasattr(newDecl,"_sons"):
        for aSon in newDecl.get_sons():
            theSon = getattr(newDecl,aSon)
            if isinstance(theSon,list):
                for anOldNewPair in oldFuncnewSubPairs :
                    if anOldNewPair[0] in theSon:
                        theSon.remove(anOldNewPair[0])
                        theSon.append(anOldNewPair[1])
                        modified = True
            else :
                for anOldNewPair in oldFuncnewSubPairs :
                    if theSon == anOldNewPair[0] :
                        setattr(newDecl,aSon,anOldNewPair[1])
                        modified = True
    return (newDecl,modified)

def convertFunctionStmt(functionStmt):
    DebugManager.debug(10*'-'+'>'+'called function2subroutine.convertFunctionStmt on '+str(functionStmt))
    if functionStmt.result is None:
        outParam = fs._NoInit(functionStmt.name.lower())
    else:
        outParam = fs._NoInit(functionStmt.result.lower())

    args = functionStmt.args
    args.append(outParam)
    name = name_init+functionStmt.name.lower()
    subroutineStmt = fs.SubroutineStmt(name,args,lead=functionStmt.lead)

    return (outParam,subroutineStmt)

def createResultDecl(functionStmt,outParam):
    DebugManager.debug(10*'-'+'>'+'called function2subroutine.createResultDecl ' \
                     + 'on function statement "'+str(functionStmt)+'"' \
                     +' with out parameter "'+str(outParam)+'"')
    if functionStmt.ty is not None:
        (type_name,mod) = functionStmt.ty
        newDecl = createTypeDecl(type_name.kw,mod,outParam,functionStmt.lead)
        return newDecl
    return None

def updateResultDecl(decl,outParam):
    DebugManager.debug(10*'-'+'>'+'called function2subroutine.updateResultDecl ' \
                     + 'on declaration statement "'+str(decl)+'",' \
                     +' outParam = "'+str(outParam)+'"')
    if (str(outParam) == decl) or \
           (hasattr(decl,'lhs') and (str(outParam) == decl.lhs)):
        return True
    else:
        return False

def updateTypeDecl(aDecl,outParam,declList):
    DebugManager.debug(10*'-'+'>'+'called function2subroutine.updateTypeDecl ' \
                     + 'on declaration statement "'+str(aDecl)+'",' \
                     +' outParam = "'+str(outParam)+'",' \
                     +' declList = "'+str(declList)+'"')
    resultDeclCreated = False
    if (len(aDecl.decls) == 1) and \
           updateResultDecl(aDecl.get_decls()[0],outParam):
        aDecl = createTypeDecl(aDecl.kw,aDecl.get_mod(),outParam,aDecl.lead)
        resultDeclCreated = True
    else:
        for decl in aDecl.get_decls():
            if updateResultDecl(decl,outParam):
                newDecl = createTypeDecl(aDecl.kw,aDecl.get_mod(),outParam,aDecl.lead)
                aDecl.decls.remove(decl)
                declList.append(newDecl)
                resultDeclCreated = True
    return (aDecl,resultDeclCreated)

def convertFunction(functionUnit,keepFunctionDecl=True):
    '''converts a function unit definition to a subroutine unit definition'''
    DebugManager.debug(10*'-'+'>'+'called function2subroutine.convertFunction ' \
                     + 'on function unit statement "'+str(functionUnit)+'",' \
                     +' with symtab "'+str(functionUnit.symtab)+'"')
    newSubUnit = Unit(parent=functionUnit.parent,fmod=functionUnit.fmod)
    (outParam,newSubUnit.uinfo) = convertFunctionStmt(functionUnit.uinfo)
    newSubUnit.cmnt = functionUnit.cmnt
    newSubUnit.contains = functionUnit.contains
    newSubUnit.ulist = functionUnit.ulist

    newList = []
    for subUnit in newSubUnit.ulist:
        # no need to process function statements, since they have already been
        # recursively processed in canon. => drop function stmts & keep other sub units
        if not isinstance(subUnit.uinfo,fs.FunctionStmt):
            newList.append(subUnit)
    newSubUnit.ulist = newList

    resultDecl = createResultDecl(functionUnit.uinfo,outParam)
    if resultDecl is not None:
        funTypeFound = True
    else:
        funTypeFound = False

    # iterate over decls for functionUnit
    for aDecl in functionUnit.decls:
        if not funTypeFound and isinstance(aDecl,fs.TypeDecl):
            (aDecl,funTypeFound) = updateTypeDecl(aDecl,outParam,newSubUnit.decls)
        newSubUnit.decls.append(aDecl)

    if resultDecl is not None:
        if len(functionUnit.decls) != 0:
            resultDecl.lead = aDecl.lead
        # append declaration for new out parameter
        newSubUnit.decls.append(resultDecl)
        
    # iterate over execs for functionUnit
    for anExec in functionUnit.execs:
        newSubUnit.execs.append(anExec)

    # iterate over end stmts for functionUnit
    for endStmt in functionUnit.end:
        newEndStmts = []
        if isinstance(endStmt,fs.EndStmt):
            newEndStmt=fs.EndStmt(lineNumber=endStmt.lineNumber,\
                                  label=endStmt.label,lead=endStmt.lead)
            newEndStmt.rawline="end subroutine "+newSubUnit.uinfo.name+'\n'
            newEndStmts.append(newEndStmt)
        else:
            newEndStmts.append(endStmt)
    newSubUnit.end = newEndStmts    

    return newSubUnit
