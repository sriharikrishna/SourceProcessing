'''
Function to Subroutine conversion
'''

from _Setup import *
from optparse import OptionParser

from PyFort.fortUnit import Unit,fortUnitIterator
from PyUtil.symtab import Symtab,SymtabError
import PyFort.fortStmts as fs
import PyFort.fortExp as fe

class FunToSubError(Exception):
    '''exception for errors that occur during the transformation of function unit definitions to subroutine definitions'''
    def __init__(self,msg):
        self.msg  = msg

def convertFunction(functionUnit):
    '''converts a function unit definition to a subroutine unit definition'''
    newSubUnit = Unit(parent=functionUnit.parent,fmod=functionUnit.fmod)
    outParam = 'RES'
    args = functionUnit.uinfo.args
    args.append(outParam)
    name = 'oad_s_'+functionUnit.uinfo.name.lower()
    newSubUnit.uinfo = fs.SubroutineStmt(name,args,lead=functionUnit.uinfo.lead).flow()

    # iterate over decls for functionUnit
    lead = functionUnit.uinfo.lead+'  '
    for aDecl in functionUnit.decls:
        newSubUnit.decls.append(aDecl)
        if not isinstance(aDecl,fs.Comments):
            lead = aDecl.lead
    intentArg = fe.App('intent',['out'])
    (type_name,mod) = functionUnit.uinfo.ty

    # append declaration for new out parameter
    try:
        newDecl = {
            fs.RealStmt: fs.RealStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            fs.ComplexStmt: fs.ComplexStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            fs.IntegerStmt: fs.IntegerStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            fs.LogicalStmt: fs.LogicalStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            fs.DoubleStmt: fs.DoubleStmt(mod,[intentArg],[outParam,],lead=lead).flow(),
            fs.DoubleCplexStmt: fs.DoubleCplexStmt(mod,[intentArg],[outParam,],lead=lead).flow()
            }[type_name]
    except KeyError:
        raise FunToSubError('Unrecognized type declaration "'+type_name+'" for function "'+functionUnit.uinfo.name+'"')

    newSubUnit.decls.append(newDecl)

    # iterate over execs for functionUnit
    for anExec in functionUnit.execs:
        if isinstance(anExec,fs.AssignStmt) and \
               anExec.lhs.lower() == functionUnit.uinfo.name.lower():
            anExec.lhs = outParam
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
