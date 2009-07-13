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

def replaceSon(arg,oldResult,newResult):
    newSon = arg
    if isinstance(arg,fe.Sel):
        if arg.head == oldResult:
            newSon = fe.Sel(newResult,arg.proj)
        if arg.proj == oldResult:
            newSon = fe.Sel(newSon.head,newResult)
    elif isinstance(arg,fe.App):
        head = arg.head
        args = arg.args
        newArgs = []
        i = 0
        while i < len(arg.args):
            anArg = arg.args[i]
            if isinstance(anArg,fe.App) or isinstance(anArg,fe.Sel):
                newArgs.append(replaceSon(anArg,oldResult,newResult))
            else:
                if anArg == oldResult:
                    newArg = newResult
                    newArgs.append(newArg)
                else:
                    newArgs.append(anArg)
            i += 1
        if len(newArgs) != 0:
            args = newArgs
        if head == oldResult:
            head = newResult
        newSon = fe.App(head,newArgs)
    else:
        if arg== oldResult:
            newSon = newResult
        elif hasattr(arg,"_sons"):
            for aSon in arg._sons:
                theSon = getattr(arg,aSon)
                if theSon is None:
                    continue
                elif isinstance(theSon,list):
                    index = 0
                    while index < len(theSon):
                        rep_son = theSon[index]
                        newSon = replaceSon(rep_son,oldResult,newResult)
                        theSon[index] = newSon
                        index += 1
                else:
                    newSon = replaceSon(theSon,oldResult,newResult)
                    setattr(arg,aSon,newSon)
                newSon = arg
            
    return newSon

def replaceStr(execStr,oldResult,newResult):
    if isinstance(oldResult,fe.App):
        old = oldResult.head
    else:
        old = oldResult
    lno_replace = "[\w]"+old
    rno_replace = old+"[\w]"
    p = re.compile("("+rno_replace+"|"+lno_replace+")")
    p2 = re.compile(old)
    pats = p.finditer(execStr,re.IGNORECASE)
    prevEnd = 0; stopRep=len(execStr)
    for match in pats:
        (stopRep,end) = match.span()
        m2 = p.search(execStr[prevEnd:stopRep],re.IGNORECASE)
        if m2:
            prevEnd = m2.end()
        newstr = p2.sub(str(newResult),execStr[prevEnd:stopRep],re.IGNORECASE)
        execStr = execStr[:prevEnd]+\
                  newstr+\
                  execStr[(stopRep):]
        prevEnd = end
        stopRep = len(execStr)
    match = p.search(execStr[prevEnd:],re.IGNORECASE)
    if match:
        execStr = execStr[:match.end()]+\
                  p2.sub(str(newResult),execStr[match.end():],re.IGNORECASE)
    elif execStr[prevEnd:].lower() == old:
        execStr = execStr[:prevEnd]+str(newResult)
    else:
        execStr = execStr[:prevEnd]+\
                  p2.sub(str(newResult),execStr[prevEnd:],re.IGNORECASE)
    return execStr
    
def replaceResultVal(execStmt,oldResult,newResult):
    if isinstance(execStmt,fs.Comments):
        execStmt.rawline = \
                         replaceStr(execStmt.rawline,oldResult,newResult)
    elif isinstance(execStmt,fs.AssignStmt):
        lhs = replaceStr(str(execStmt.lhs),oldResult,newResult)
        rhs = replaceStr(str(execStmt.rhs),oldResult,newResult)
        execStmt = fs.AssignStmt(lhs,rhs,lead=execStmt.lead)
        execStmt.flow()
    elif isinstance(execStmt,fs.IOStmt):
        newItemList = []
        for item in execStmt.itemList:
            newItem=replaceStr(str(item),oldResult,newResult)
            newItemList.append(newItem)
        execStmt.itemList = newItemList
        execStmt.flow()
    else:
        execStmt = replaceSon(execStmt,oldResult,newResult)
        execStmt.rawline.strip()
        execStmt.flow()
    return execStmt

def convertFunction(functionUnit):
    '''converts a function unit definition to a subroutine unit definition'''
    newSubUnit = Unit(parent=functionUnit.parent,fmod=functionUnit.fmod)
    outParam = fs._NoInit('RES')
    args = functionUnit.uinfo.args
    args.append(outParam)
    name = 'oad_s_'+functionUnit.uinfo.name.lower()
    if functionUnit.uinfo.result is None:
        result = functionUnit.uinfo.name.lower()
    else:
        result =functionUnit.uinfo.result
    newSubUnit.uinfo = fs.SubroutineStmt(name,args,lead=functionUnit.uinfo.lead).flow()

    intentArg = fe.App('intent',['out'])
    funTypeFound = False
    lead = functionUnit.uinfo.lead+'  '
    if functionUnit.uinfo.ty is not None:
        (type_name,mod) = functionUnit.uinfo.ty
        funTypeFound = True
        
        # append declaration for new out parameter
        newDecl = createTypeDecl(type_name.kw,mod,outParam,lead)
        newSubUnit.decls.append(newDecl)

    # iterate over decls for functionUnit
    for aDecl in functionUnit.decls:
        if not funTypeFound and isinstance(aDecl,fs.TypeDecl):
            for decl in aDecl.decls:
                if functionUnit.uinfo.name == decl.lhs:
                    aDecl.decls.remove(decl)
                    aDecl.flow()
                    newDecl = createTypeDecl(aDecl.kw,aDecl.mod,outParam,lead)
                    newSubUnit.decls.append(newDecl.flow())
                    
        newSubUnit.decls.append(aDecl)
        if not isinstance(aDecl,fs.Comments):
            lead = aDecl.lead
        
    # iterate over execs for functionUnit
    for anExec in functionUnit.execs:
        if hasattr(anExec,"flow"):
            anExec.flow()
        newExec = replaceResultVal(anExec,result,outParam)
        newSubUnit.execs.append(newExec)

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
