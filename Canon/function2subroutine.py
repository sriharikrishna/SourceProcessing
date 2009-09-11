'''
Function to Subroutine conversion
'''

from _Setup import *
from optparse import OptionParser

from PyFort.fortUnit import Unit,fortUnitIterator
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

def convertFunctionDecl(aDecl,oldFuncName,newFuncName):
    newDecl = copy.deepcopy(aDecl)
    modified = False
    if hasattr(newDecl,"_sons"):
        for aSon in newDecl.get_sons():
            theSon = getattr(newDecl,aSon)
            if isinstance(theSon,list):
                if oldFuncName in theSon:
                    theSon.remove(oldFuncName)
                    theSon.append(newFuncName)
                    modified = True
            elif theSon == oldFuncName:
                setattr(newDecl,aSon,newFuncName)
                modified = True
    return (newDecl,modified)

def convertFunctionStmt(functionStmt):
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
    if functionStmt.ty is not None:
        (type_name,mod) = functionStmt.ty
        newDecl = createTypeDecl(type_name.kw,mod,outParam,functionStmt.lead)
        return newDecl
    return None

def updateResultDecl(decl,outParam):
    if (str(outParam) == decl) or \
           (hasattr(decl,'lhs') and (str(outParam) == decl.lhs)):
        return True
    else:
        return False

def updateTypeDecl(aDecl,outParam,declList):
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

def convertFunction(functionUnit):
    '''converts a function unit definition to a subroutine unit definition'''
    newSubUnit = Unit(parent=functionUnit.parent,fmod=functionUnit.fmod)
    (outParam,newSubUnit.uinfo) = convertFunctionStmt(functionUnit.uinfo)

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
