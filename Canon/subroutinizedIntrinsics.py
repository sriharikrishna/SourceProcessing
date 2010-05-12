from PyUtil.debugManager import DebugManager

from PyFort.fortUnit import Unit
from PyFort.fortExp import App,Ops
import PyFort.fortStmts as fs
import PyFort.intrinsic as intrinsic
import PyFort.flow as flow
from PyUtil.symtab import Symtab 

class SubroutinizeError(Exception):
    '''exception for errors that occur during the subroutinization of intrinsics'''
    def __init__(self,msg):
        self.msg  = msg

_subroutinizable=['max','maxval','min','minval']

def shouldSubroutinize(aFunction):
    DebugManager.debug('subroutinizedIntrinsics.shouldSubroutinize called on "'+str(aFunction)+'"')
    return intrinsic.getGenericName(aFunction.head) in _subroutinizable

_requiredSubroutinizedIntrinsics=[]

def markRequired(key,typeClass):
    if ((key,typeClass) not in _requiredSubroutinizedIntrinsics):
        _requiredSubroutinizedIntrinsics.append((key,typeClass))

call_prefix  = 'oad_s_'
typeList=[fs.RealStmt,fs.DoubleStmt,fs.IntegerStmt]

def getModuleName():
    return 'OAD_intrinsics'

def makeName(intrName):
    return call_prefix + intrName     

def __makeNameImpl(intrName,typeClass):
    return makeName(intrName)+'_'+typeClass.kw[0]

def __skipThisType(onlyRequired,key,typeClass):
    return ((onlyRequired
             and
             (((key,typeClass) not in _requiredSubroutinizedIntrinsics)
              or 
              (Symtab.getRealTypeDefault()[0]==fs.DoubleStmt
               and
               typeClass==fs.RealStmt
               and
               (key, fs.DoubleStmt) in _requiredSubroutinizedIntrinsics)))
            or
            (not onlyRequired
             and
             Symtab.getRealTypeDefault()[0]==fs.DoubleStmt
             and
             typeClass==fs.RealStmt))
   

def makeSubroutinizedIntrinsics(onlyRequired):
    ''' this is incomplete '''
    keyList=_subroutinizable
    if onlyRequired:
        uniq=set([k for k,t in _requiredSubroutinizedIntrinsics])
        keyList=[k for k in uniq] 
    newUnit=Unit()
    newUnit.uinfo=fs.ModuleStmt(getModuleName())
    empty=True
    for k in keyList:
        nameList=[]
        newUnit.decls.append(fs.InterfaceStmt(call_prefix+k,lead='  '))
        for t in typeList:
            if (__skipThisType(onlyRequired,k,t)) : 
                continue
            nameList.append(__makeNameImpl(k,t))
            empty=False
        newUnit.decls.append(fs.ProcedureStmt('module',nameList,lead='    '))
        newUnit.decls.append(fs.EndInterfaceStmt(lead='  '))
    if not empty: 
        newUnit.decls.append(fs.ContainsStmt(lead='  '))
    for k in keyList:
        for t in typeList:
            if (__skipThisType(onlyRequired,k,t)) : 
                continue
            newSUnit=Unit()
            if (k in ['max','min']) : 
                makeSubroutinizedMaxOrMin(newSUnit,k,t,'    ')
            else:
                makeSubroutinizedMaxvalOrMinval(newSUnit,k,t,'    ')
            newUnit.ulist.append(newSUnit)            
    newUnit.end.append(fs.EndModuleStmt())
    return [newUnit]

def makeSubroutinizedMaxOrMin(newUnit,aKey,aTypeClass,indent):
    newUnit.uinfo=fs.SubroutineStmt(__makeNameImpl(aKey,aTypeClass),
                                    ["a","b","r"],
                                    lead=indent)
    newUnit.decls.append(aTypeClass(None,
                                   [App('intent',['in'])],
                                   'a',
                                   lead=indent+'  '))
    newUnit.decls.append(aTypeClass(None,
                                   [App('intent',['in'])],
                                   'b',
                                    lead=indent+'  '))
    newUnit.decls.append(aTypeClass(None,
                                   [App('intent',['out'])],
                                   'r',
                                   lead=indent+'  '))
    testExpr = (aKey=='max') and Ops('>','a','b') \
                             or Ops('<','a','b')
    newUnit.execs.append(fs.IfThenStmt(testExpr,lead=indent+'  '))
    newUnit.execs.append(fs.AssignStmt('r','a',lead=indent+'    '))
    newUnit.execs.append(fs.ElseStmt(lead=indent+'  '))
    newUnit.execs.append(fs.AssignStmt('r','b',lead=indent+'    '))
    newUnit.execs.append(fs.EndifStmt(lead=indent+'  '))
    newUnit.end.append(fs.EndSubroutineStmt(lead=indent))

def makeSubroutinizedMaxvalOrMinval(newUnit,aKey,aTypeClass,indent):
    newUnit.uinfo = fs.SubroutineStmt(__makeNameImpl(aKey,aTypeClass),
                                      ['a','l','r'],
                                      lead=indent)
    newUnit.decls.append(aTypeClass(None,
                                    [App('intent',['in'])],
                                    [fs._NoInit(fs.App('a',['l']))],
                                    lead=indent+'  '))
    newUnit.decls.append(fs.IntegerStmt(None,
                                        [App('intent',['in'])],
                                        ['l'],
                                        lead=indent+'  '))
    newUnit.decls.append(aTypeClass(None,
                                    [App('intent',['out'])],
                                    ['r'],
                                    lead=indent+'  '))
    newUnit.decls.append(fs.IntegerStmt(None,
                                        None,
                                        [fs._NoInit(App('i',['1']))],
                                        lead=indent+'  '))
    locVersion = (aKey=='maxval') and 'maxloc' \
                                   or 'minloc'
    newUnit.execs.append(fs.AssignStmt('i',
                                       App(locVersion,['a']),
                                       lead=indent+'    '))
    newUnit.execs.append(fs.AssignStmt('r',
                                       App('a',[App('i',['1'])]),
                                       lead=indent+'    '))
    newUnit.end.append(fs.EndSubroutineStmt(lead=indent))


