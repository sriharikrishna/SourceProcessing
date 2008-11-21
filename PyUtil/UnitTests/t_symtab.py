from Setup import *
from unittest import *
from symtab import Symtab

from PyFort.fortStmts import RealStmt,IntegerStmt

Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))

class C1(TestCase):
    def setUp(self):
        t = Symtab()
        t.enter_name('TOP','got top')

        ts = Symtab(t)
        ts.enter_name('fOo','local foo')
        self.ts = ts

    def test1(self):
        'local lookup'
        ts = self.ts
        ae(ts.lookup_name('foo'),'local foo')

    def test2(self):
        'remote lookup'
        ts = self.ts
        ae(ts.lookup_name('top'),'got top')

    def test3(self):
        'no entry'
        ts = self.ts
        a_(not ts.lookup_name('bar'))

s1 = asuite(C1)

suite = asuite(C1)

if __name__ == '__main__':
    runit(suite)
