from PyUtil.debugManager import DebugManager

from PyFort.fortUnit import Unit
from PyFort.fortExp import App,Ops
import PyFort.fortStmts as fs
import PyFort.intrinsic as intrinsic
import PyFort.flow as flow

class SubroutinizeError(Exception):
    '''exception for errors that occur during the subroutinization of intrinsics'''
    def __init__(self,msg):
        self.msg  = msg

def shouldSubroutinize(aFunction):
    DebugManager.debug('subroutinizedIntrinsics.shouldSubroutinize called on "'+str(aFunction)+'"')
    return intrinsic.getGenericName(aFunction.head) in ('max','maxval','min','minval')

_requiredSubroutinizedIntrinsics=[]

def markRequired(key,typeClass):
    if ((key,typeClass) not in _requiredSubroutinizedIntrinsics):
        _requiredSubroutinizedIntrinsics.append((key,typeClass))

call_prefix  = 'oad_s_'

def makeName(intrName,typeClass):
    return call_prefix + intrName + (intrinsic.isPolymorphic(intrName) and '_'+typeClass.kw.lower()[0] or '')    

def makeSubroutinizedIntrinsics():
    ''' this is just a starter and currently works only for max/min '''
    subroutinizedIntrinsics=[]
    for (key,typeClass) in _requiredSubroutinizedIntrinsics:
        newUnit = Unit()
        if key in ('max','min'):
            makeSubroutinizedMaxOrMin(newUnit,key,typeClass)
        elif key in ('maxval','minval'):
            makeSubroutinizedMaxvalOrMinval(newUnit,key,typeClass)
        else:
            raise SubroutinizeError('subroutinizedIntrinsics.makeSubroutinizedIntrinsics: I DONT KNOW HOW TO SUBROUTINIZE FUNCTION WITH KEY '+key)
        subroutinizedIntrinsics.append(newUnit)
    return subroutinizedIntrinsics

def makeSubroutinizedMaxOrMin(newUnit,aKey,aTypeClass):
    newUnit.uinfo=fs.SubroutineStmt(makeName(aKey,aTypeClass),
                                    ["a","b","r"],
                                    lead=flow.formatStart)
    newUnit.decls.append(aTypeClass(None,
                                   [App('intent',['in'])],
                                   'a',
                                   lead=flow.formatStart+'  '))
    newUnit.decls.append(aTypeClass(None,
                                   [App('intent',['in'])],
                                   'b',
                                   lead=flow.formatStart+'  '))
    newUnit.decls.append(aTypeClass(None,
                                   [App('intent',['out'])],
                                   'r',
                                   lead=flow.formatStart+'  '))
    testExpr = (aKey=='max') and Ops('>','a','b') \
                             or Ops('<','a','b')
    newUnit.execs.append(fs.IfThenStmt(testExpr,lead=flow.formatStart+'  '))
    newUnit.execs.append(fs.AssignStmt('r','a',lead=flow.formatStart+'    '))
    newUnit.execs.append(fs.ElseStmt(lead=flow.formatStart+'  '))
    newUnit.execs.append(fs.AssignStmt('r','b',lead=flow.formatStart+'    '))
    newUnit.execs.append(fs.EndifStmt(lead=flow.formatStart+'  '))
    newUnit.end.append(fs.EndStmt(lead=flow.formatStart))

def makeSubroutinizedMaxvalOrMinval(newUnit,aKey,aTypeClass):
    newUnit.uinfo = fs.SubroutineStmt(makeName(aKey,aTypeClass),
                                      ['a','l','r'],
                                      lead=flow.formatStart)
    newUnit.decls.append(aTypeClass(None,
                                    [App('intent',['in'])],
                                    [fs._NoInit(App('a',['l']))],
                                    lead=flow.formatStart+'  '))
    newUnit.decls.append(fs.IntegerStmt(None,
                                        [App('intent',['in'])],
                                        ['l'],
                                        lead=flow.formatStart+'  '))
    newUnit.decls.append(aTypeClass(None,
                                    [App('intent',['out'])],
                                    ['r'],
                                    lead=flow.formatStart+'  '))
    newUnit.decls.append(fs.IntegerStmt(None,
                                        None,
                                        [fs._NoInit(App('i',['1']))],
                                        lead=flow.formatStart+'  '))
    locVersion = (aKey=='maxval') and 'maxloc' \
                                   or 'minloc'
    newUnit.execs.append(fs.AssignStmt('i',
                                       App(locVersion,['a']),
                                       lead=flow.formatStart+'    '))
    newUnit.execs.append(fs.AssignStmt('r',
                                       App('i',['1']),
                                       lead=flow.formatStart+'    '))
    newUnit.end.append(fs.EndStmt(lead=flow.formatStart))

