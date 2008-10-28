from Setup    import *
from _Setup   import *
from unittest import *
from useparse import *

from PyUtil.symtab import Symtab

from typeInference import expressionType,typemerge
from fortStmts import IntegerStmt,RealStmt,DoubleStmt,ComplexStmt,DoubleCplexStmt,_Prec

class TypeOpsExpressions(TestCase):
    'test type inference for simple combinations of binary ops for constants and variables'

    def test0(self):
        'Typing of various binary operation expressions over constants and implicitly types variables'
        ae = self.assertEquals

        Symtab._default_int = (IntegerStmt,[])
        Symtab._default_real = (RealStmt,[])
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


suite = asuite(TypeOpsExpressions,TypeMerging)

if __name__ == '__main__':
    runit(suite)

