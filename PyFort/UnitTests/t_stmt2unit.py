from Setup import *
from unittest      import *

from stmt2unit import *
from stmt2unit import _implicit

from unit_symtab import Symtab
from useparse import *

class U(object):
    def __init__(self):
        self.symtab = Symtab()

class _curr(object):
    def __init__(self):
        self.val = U()

class C1(TestCase):
    def test1(self):
        'implicit action'
        Symtab._default_real = (fs.RealStmt,[])
        Symtab._default_int  = (fs.IntegerStmt,[])

        cur = _curr()

        s1 = pps('implicit integer(special) (a-f)')
        sr = _implicit(s1,cur)
        v = cur.val.symtab
        s2 = pps('integer(special) a')
        t = (s2.__class__,s2.mod)
    
        t1 = v.lookup_type('foo')
        ae(repr(t1),repr(t))
        
        s2 = pps('real a')
        t = (s2.__class__,s2.mod)
    
        t1 = v.lookup_type('zoo')
        ae(repr(t1),repr(t))

s1 = makeSuite(C1)
suite = asuite(C1)

if __name__ == "__main__":
    runit(suite)

