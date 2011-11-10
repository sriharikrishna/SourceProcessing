from Setup    import *
from unittest import *
from useparse import *

from PyUtil.symtab import Symtab

from PyFort.inference import expressionType,_TypeContext,_kw2type,_lenfn 
from PyFort.fortStmts import LogicalStmt,CharacterStmt,IntegerStmt,RealStmt,DoubleStmt,ComplexStmt,DoubleCplexStmt
from PyFort.fortStmts import _Prec,_Kind,_F77Len

Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))
theSymtab = Symtab()


class TypeUtils(TestCase):
    '''Typing, and misc xform utilities
    '''
    def test1(self):
        '_kw2type'
        self.assertEquals(_kw2type('real'),RealStmt)
        self.assertEquals(_kw2type('doubleprecision'),DoubleStmt)
        self.assertEquals(_kw2type('integer'),IntegerStmt)
        self.assertEquals(_kw2type('logical'),LogicalStmt)

    def test2(self):
        '_lenfn'
        self.assertEquals(str(_lenfn(15)[0]),'*15')

class TypeConstants(TestCase):
    def test0(self):
        'constants - numerical values without modifiers'
        self.assertEquals(_TypeContext(0,theSymtab)._constantType(ep('3.787')),
                          (RealStmt,[]))
        self.assertEquals(_TypeContext(0,theSymtab)._constantType(ep('3.787D00')),
                          (DoubleStmt,[]))
        self.assertEquals(_TypeContext(0,theSymtab)._constantType(ep('3')),
                          (IntegerStmt,[]))

    def test1(self):
        'constants - numerical values with modifiers'

        (type,typeModList) = _TypeContext(0,theSymtab)._constantType(ep('3.787_foo'))
        typeMod = typeModList[0]
        self.assertEquals(type,RealStmt)
        self.assert_(isinstance(typeMod,_Kind))
        self.assertEquals(typeMod.mod,'foo')

        (type,typeModList) = _TypeContext(0,theSymtab)._constantType(ep('0_w2f__i8'))
        typeMod = typeModList[0]
        self.assertEquals(type,IntegerStmt)
        self.assert_(isinstance(typeMod,_Kind))
        self.assertEquals(typeMod.mod,'w2f__i8')

    def test2(self):
        'constants - logical values'
        self.assertEquals(_TypeContext(0,theSymtab)._constantType(ep('.true.')),
                          (LogicalStmt,[]))

    def test3(self):
        'constants - strings'
        (type,typeModList) = _TypeContext(0,theSymtab)._constantType(ep(r"'food'"))
        typeMod = typeModList[0]
        self.assertEquals(type,CharacterStmt)
        self.assert_(isinstance(typeMod,_F77Len))
        self.assertEquals(typeMod.len,'4')

class TypeOpsExpressions(TestCase):
    'test type inference for simple combinations of binary ops for constants and variables'

    def test0(self):
        'Typing of various binary operation expressions over constants and implicitly types variables'
        ae = self.assertEquals

        e1 = ep('x * y')
        ae(expressionType(e1,theSymtab,lineNumber=0),
           (RealStmt,[]))

        e1 = ep('5.11d0 * 4.77d0')
        ae(expressionType(e1,theSymtab,lineNumber=0),
           (DoubleStmt,[]))

        e1 = ep('i + 4')
        ae(expressionType(e1,theSymtab,lineNumber=0),
           (IntegerStmt,[]))

        e1 = ep('z + 5.11d0 * 4.77d0')
        ae(expressionType(e1,theSymtab,lineNumber=0),
           (DoubleStmt,[]))

        e1 = ep('x * 5.11d0 + i * 4.77')
        ae(expressionType(e1,theSymtab,lineNumber=0),
           (DoubleStmt,[]))

def _gt(decl):
    'generate type reps f decl strings, using parser'
    stmt = pps(decl + ' x')
    return (stmt.__class__,stmt.mod)

class TypeMerging(TestCase):
    'Merge types to find the supremum type'
    def test00(self):
        'Merge unmodified real with None'
        t = (RealStmt,[])
        self.assertEquals(_TypeContext(0,theSymtab)._typemerge([],
                                                              t),
                          t)

    def test01(self):
        'Merge modified real with None'
        t = (RealStmt,[_Prec('4')])
        self.assertEquals(_TypeContext(0,theSymtab)._typemerge([],
                                                                t),
                          t)

    def test1(self):
        'Merging of types in order to yield the correct supremum'
        ae = self.assertEquals

        t1 = _gt('real')
        t2 = _gt('real*4')
        t3 = _gt('real*8')
        t4 = _gt('double precision')
        t5 = _gt('complex')
        t6 = _gt('integer')

        ae(_TypeContext(0,theSymtab)._typemerge([],t1),t1)
        ae(_TypeContext(0,theSymtab)._typemerge([t2],t1),t2)
        ae(_TypeContext(0,theSymtab)._typemerge([t1,t1,t1],t2),t1)
        ae(_TypeContext(0,theSymtab)._typemerge([t1,t2,t1],t1),t2)
        ae(_TypeContext(0,theSymtab)._typemerge([t1,t2,t1,t3],t1),t3)
        ae(_TypeContext(0,theSymtab)._typemerge([t6,t6,t6,t1],t1),t1)
        ae(_TypeContext(0,theSymtab)._typemerge([t3,t4,],t1),t4)
        ae(_TypeContext(0,theSymtab)._typemerge([t1,t2,t3,t4,t5,t6],t1),t5)


suite = asuite(TypeUtils,TypeConstants,TypeOpsExpressions,TypeMerging)

if __name__ == '__main__':
    sys.exit(runit(suite))

