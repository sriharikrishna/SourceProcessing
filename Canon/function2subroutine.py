'''
Function to Subroutine conversion
'''

from _Setup import *
from optparse import OptionParser

from PyFort.fortUnit import Unit,fortUnitIterator
from PyUtil.symtab import Symtab,SymtabError
import PyFort.fortStmts as fs
import PyFort.fortExp as fe
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
            'real': fs.RealStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            'complex': fs.ComplexStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            'integer': fs.IntegerStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            'logical': fs.LogicalStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            'doubleprecision': fs.DoubleStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            'doublecomplex': fs.DoubleCplexStmt(mod,[intentArg],[outParam,],lead=lead).flow()
            }[type_kw]
    except KeyError:
        raise FunToSubError('Unrecognized type "'+type_kw+'"')

    return newDecl

def convertFunctionDecl(aDecl,oldFuncName,newFuncName):
    if hasattr(aDecl,"_sons"):
        for aSon in aDecl._sons:
            theSon = getattr(aDecl,aSon)
            if isinstance(theSon,list):
                if oldFuncName in theSon:
                    theSon.remove(oldFuncName)
                    theSon.append(newFuncName)
            elif theSon == oldFuncName:
                setattr(aDecl,aSon,newFuncName)
        aDecl.flow()
    return aDecl

def convertFunctionStmt(functionStmt):
    if functionStmt.result is None:
        outParam = fs._NoInit(functionStmt.name.lower())
    else:
        outParam = fs._NoInit(functionStmt.result.lower())

    args = functionStmt.args
    args.append(outParam)
    name = name_init+functionStmt.name.lower()
    subroutineStmt = fs.SubroutineStmt(name,args,lead=functionStmt.lead).flow()

    return (outParam,subroutineStmt)

def createResultDecl(functionStmt,outParam):
    if functionStmt.ty is not None:
        (type_name,mod) = functionStmt.ty
        newDecl = createTypeDecl(type_name.kw,mod,outParam,functionStmt.lead+'  ')
        return newDecl
    return None

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
            for decl in aDecl.decls:
                if (str(outParam) == decl) or \
                       (hasattr(decl,'lhs') and (str(outParam) == decl.lhs)):
                    aDecl.decls.remove(decl)
                    aDecl.flow()
                    newDecl = createTypeDecl(aDecl.kw,aDecl.mod,outParam,aDecl.lead)
                    newSubUnit.decls.append(newDecl.flow())
                    funTypeFound = True
        newSubUnit.decls.append(aDecl)

    if resultDecl is not None:
        # append declaration for new out parameter
        newSubUnit.decls.append(resultDecl.flow())
        
    # iterate over execs for functionUnit
    for anExec in functionUnit.execs:
        anExec.flow()
        newSubUnit.execs.append(anExec)

    # iterate over end stmts for functionUnit
    for endStmt in functionUnit.end:
        newEndStmts = []
        if isinstance(endStmt,fs.EndStmt):
            newEndStmt=fs.EndStmt(endStmt.lineNumber,endStmt.label,endStmt.lead)
            newEndStmt.rawline=endStmt.lead+"end subroutine "+newSubUnit.uinfo.name+'\n'
            newEndStmts.append(newEndStmt)
        else:
            newEndStmts.append(endStmt)
    newSubUnit.end = newEndStmts    

    return newSubUnit
