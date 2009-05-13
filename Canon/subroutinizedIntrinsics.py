from PyFort.flow import formatStart
from PyFort.fortUnit import Unit
from PyFort.fortExp import App,Ops
import PyFort.fortStmts as fs
import PyFort.intrinsic as intrinsic

class SubroutinizeError(Exception):
    '''exception for errors that occur during the subroutinization of intrinsics'''
    def __init__(self,msg):
        self.msg  = msg

def shouldSubroutinizeIntrinsic(aFunction):
    return intrinsic.getGenericName(aFunction.head) in ('max','maxval','min','minval')

_requiredSubroutinizedIntrinsics=[]

def requireSubroutinizedIntrinsic(key,typeClass):
    if ((key,typeClass) not in _requiredSubroutinizedIntrinsics):
        _requiredSubroutinizedIntrinsics.append((key,typeClass))

_call_prefix  = 'oad_s_'

def makeSubroutinizedIntrinsicName(intrName,typeClass):
    return _call_prefix + intrName + (intrinsic.isPolymorphic(intrName) and '_'+typeClass.kw.lower()[0] or '')    

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
    newUnit.uinfo=fs.SubroutineStmt(makeSubroutinizedIntrinsicName(aKey,aTypeClass),
                                    ["a","b","r"],
                                    lead=formatStart
                                   ).flow()
    newUnit.decls.append(aTypeClass(None,
                                   [App('intent',['in'])],
                                   'a',
                                   lead=formatStart+'  '
                                  ).flow())
    newUnit.decls.append(aTypeClass(None,
                                   [App('intent',['in'])],
                                   'b',
                                   lead=formatStart+'  '
                                  ).flow())
    newUnit.decls.append(aTypeClass(None,
                                   [App('intent',['out'])],
                                   'r',
                                   lead=formatStart+'  '
                                  ).flow())
    testExpr = (aKey=='max') and Ops('>','a','b') \
                             or Ops('<','a','b')
    newUnit.execs.append(fs.IfThenStmt(testExpr,lead=formatStart+'  ').flow())
    newUnit.execs.append(fs.AssignStmt('r','a',lead=formatStart+'    ').flow())
    newUnit.execs.append(fs.ElseStmt(lead=formatStart+'  ').flow())
    newUnit.execs.append(fs.AssignStmt('r','b',lead=formatStart+'    ').flow())
    newUnit.execs.append(fs.EndifStmt(lead=formatStart+'  ').flow())
    newUnit.end.append(fs.EndStmt(lead=formatStart).flow())

def makeSubroutinizedMaxvalOrMinval(newUnit,aKey,aTypeClass):
    newUnit.uinfo = fs.SubroutineStmt(makeSubroutinizedIntrinsicName(aKey,aTypeClass),
                                      ['a','l','r'],
                                      lead=formatStart
                                     ).flow()
    newUnit.decls.append(aTypeClass(None,
                                    [App('intent',['in'])],
                                    [fs._NoInit(App('a',['l']))],
                                    lead=formatStart+'  '
                                   ).flow())
    newUnit.decls.append(fs.IntegerStmt(None,
                                        [App('intent',['in'])],
                                        ['l'],
                                        lead=formatStart+'  '
                                       ).flow())
    newUnit.decls.append(aTypeClass(None,
                                    [App('intent',['out'])],
                                    ['r'],
                                    lead=formatStart+'  '
                                   ).flow())
    newUnit.decls.append(fs.IntegerStmt(None,
                                        None,
                                        [fs._NoInit(App('i',['1']))],
                                        lead=formatStart+'  '
                                       ).flow())
    locVersion = (aKey=='maxval') and 'maxloc' \
                                   or 'minloc'
    newUnit.execs.append(fs.AssignStmt('i',
                                       App(locVersion,['a']),
                                       lead=formatStart+'    '
                                      ).flow())
    newUnit.execs.append(fs.AssignStmt('r',
                                       App('i',['1']),
                                       lead=formatStart+'    '
                                      ).flow())
    newUnit.end.append(fs.EndStmt(lead=formatStart).flow())
