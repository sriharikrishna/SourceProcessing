from Setup    import *
from _Setup   import *
from unittest import *
from useparse import *

#from PyUtil.debugManager import DebugManager
#DebugManager.setVerbose(True)

from PyUtil.symtab import Symtab, SymtabEntry

from inference import expressionShape,constantShape,shapemerge
from fortStmts import IntegerStmt,RealStmt,DimensionStmt
from fortStmts import _Prec,_Kind,_F77Len
from fortExp   import Ops

class ShapeOpsExpressions(TestCase):
    'test shape inference for simple combinations of binary ops for constants and variables'

    def test0(self):
        'scalar * array'
        ae = self.assertEquals

        Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))
        theSymtab = Symtab()
        theSymtab.enter_name('x',SymtabEntry(SymtabEntry.VariableEntryKind, type=RealStmt))
        theSymtab.enter_name('y', SymtabEntry(SymtabEntry.VariableEntryKind,
                                              type=RealStmt,
                                              dimensions=tuple(['2','3'])))
        e1 = ep('x * y')
        ae(expressionShape(e1,theSymtab,lineNumber=0),
           (tuple(['2','3'])))

    def test1(self):
        'scalar * subscripted array'
        ae = self.assertEquals

        Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))
        theSymtab = Symtab()
        theSymtab.enter_name('x',SymtabEntry(SymtabEntry.VariableEntryKind, type=RealStmt))
        theSymtab.enter_name('y', SymtabEntry(SymtabEntry.VariableEntryKind,
                                              type=RealStmt,
                                              dimensions=tuple(['2','3'])))
        e1 = ep('x * y(1,:)')
        ae(expressionShape(e1,theSymtab,lineNumber=0),
           (tuple(['3'])))


    def test2(self):
        'scalar * fixed array segment'
        ae = self.assertEquals

        Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))
        theSymtab = Symtab()
        theSymtab.enter_name('x',SymtabEntry(SymtabEntry.VariableEntryKind, type=RealStmt))
        theSymtab.enter_name('y', SymtabEntry(SymtabEntry.VariableEntryKind,
                                              type=RealStmt,
                                              dimensions=tuple(['2','3'])))
        e1 = ep('x * y(1,1:2)')
        ae(str(expressionShape(e1,theSymtab,lineNumber=0)),
           str(tuple([Ops(':','1','2')])))

    def test3(self):
        'scalar * range w open ubound'
        ae = self.assertEquals

        Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))
        theSymtab = Symtab()
        theSymtab.enter_name('x',SymtabEntry(SymtabEntry.VariableEntryKind, type=RealStmt))
        theSymtab.enter_name('y', SymtabEntry(SymtabEntry.VariableEntryKind,
                                              type=RealStmt,
                                              dimensions=tuple(['2','3'])))
        e1 = ep('x * y(1,2:)')
        ae(str(expressionShape(e1,theSymtab,lineNumber=0)),
           str(tuple([Ops(':','2','3')])))

    def test4(self):
        'scalar * range w open lbound'
        ae = self.assertEquals

        Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))
        theSymtab = Symtab()
        theSymtab.enter_name('x',SymtabEntry(SymtabEntry.VariableEntryKind, type=RealStmt))
        theSymtab.enter_name('y', SymtabEntry(SymtabEntry.VariableEntryKind,
                                              type=RealStmt,
                                              dimensions=tuple(['2','3'])))
        e1 = ep('x * y(1,:2)')
        ae(str(expressionShape(e1,theSymtab,lineNumber=0)),
           str(tuple([Ops(':','1','2')])))

    def test5(self):
        'scalar * range w open lbound and dimension range'
        ae = self.assertEquals

        Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))
        theSymtab = Symtab()
        theSymtab.enter_name('x',SymtabEntry(SymtabEntry.VariableEntryKind, type=RealStmt))
        theSymtab.enter_name('y', SymtabEntry(SymtabEntry.VariableEntryKind,
                                              type=RealStmt,
                                              dimensions=tuple([Ops(':','2','4'),Ops(':','-1','2')])))
        e1 = ep('x * y(:3,1)')
        ae(str(expressionShape(e1,theSymtab,lineNumber=0)),
           str(tuple([Ops(':','2','3')])))

suite = asuite(ShapeOpsExpressions)

if __name__ == '__main__':
    sys.exit(runit(suite))

