from Setup    import *
from _Setup   import *
from unittest import *
from useparse import *

from PyUtil.symtab import Symtab

from typeInference import expressionType,constantType,typemerge
from fortStmts import LogicalStmt,CharacterStmt,IntegerStmt,RealStmt,DoubleStmt,ComplexStmt,DoubleCplexStmt
from fortStmts import _Prec,_Kind,_F77Len

class TypeConstants(TestCase):
    def test0(self):
        'constants - numerical values without modifiers'
        self.assertEquals(constantType(ep('3.787')),
                          (RealStmt,[]))
        self.assertEquals(constantType(ep('3.787D00')),
                          (DoubleStmt,[]))
        self.assertEquals(constantType(ep('3')),
                          (IntegerStmt,[]))

    def test1(self):
        'constants - numerical values with modifiers'

        (type,typeModList) = constantType(ep('3.787_foo'))
        typeMod = typeModList[0]
        self.assertEquals(type,RealStmt)
        self.assert_(isinstance(typeMod,_Kind))
        self.assertEquals(typeMod.mod,'foo')

        (type,typeModList) = constantType(ep('0_w2f__i8'))
        typeMod = typeModList[0]
        self.assertEquals(type,IntegerStmt)
        self.assert_(isinstance(typeMod,_Kind))
        self.assertEquals(typeMod.mod,'w2f__i8')

    def test2(self):
        'constants - logical values'
        self.assertEquals(constantType(ep('.true.')),
                          (LogicalStmt,[]))

    def test3(self):
        'constants - strings'
        (type,typeModList) = constantType(ep(r"'food'"))
        typeMod = typeModList[0]
        self.assertEquals(type,CharacterStmt)
        self.assert_(isinstance(typeMod,_F77Len))
        self.assertEquals(typeMod.len,'4')

class TypeOpsExpressions(TestCase):
    'test type inference for simple combinations of binary ops for constants and variables'

    def test0(self):
        'Typing of various binary operation expressions over constants and implicitly types variables'
        ae = self.assertEquals

        Symtab.setTypeDefaults((fs.RealStmt,[]),(fs.IntegerStmt,[]))
        theSymtab = Symtab()

        e1 = ep('x * y')
        ae(expressionType(e1,theSymtab),
           (RealStmt,[]))

        e1 = ep('5.11d0 * 4.77d0')
        ae(expressionType(e1,theSymtab),
           (DoubleStmt,[]))

        e1 = ep('i + 4')
        ae(expressionType(e1,theSymtab),
           (IntegerStmt,[]))

        e1 = ep('z + 5.11d0 * 4.77d0')
        ae(expressionType(e1,theSymtab),
           (DoubleStmt,[]))

        e1 = ep('x * 5.11d0 + i * 4.77')
        ae(expressionType(e1,theSymtab),
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
        self.assertEquals(typemerge([],
                                    t),
                          t)

    def test01(self):
        'Merge modified real with None'
        t = (RealStmt,[_Prec('4')])
        self.assertEquals(typemerge([],
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

        ae(typemerge([],t1),t1)
        ae(typemerge([t2],t1),t2)
        ae(typemerge([t1,t1,t1],t2),t1)
        ae(typemerge([t1,t2,t1],t1),t2)
        ae(typemerge([t1,t2,t1,t3],t1),t3)
        ae(typemerge([t6,t6,t6,t1],t1),t1)
        ae(typemerge([t3,t4,],t1),t4)
        ae(typemerge([t1,t2,t3,t4,t5,t6],t1),t5)


suite = asuite(TypeConstants,TypeOpsExpressions,TypeMerging)

if __name__ == '__main__':
    runit(suite)

