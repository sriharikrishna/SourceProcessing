'''
Test the prog1 constructor
'''

from Setup import *
from _Setup import *
from unittest import *
from useparse import *
from cStringIO import StringIO

from prog1 import Prog1
import PyUtil.lexi_visitor as lv
import PyFort.fortStmts as fs
import PyFort.fortExp   as fe
import PyIR.mutable_tree as mt
from PP.xaifpp import val_deriv_m as valm
from PP.xaifpp import type_active_m as actm
from PP.xaifpp import add_active_module as addm

def normal(self,arg):
    arg.attach(self)

def exec_s(self,arg):
    self.mutate(self)
    arg.attach(self)

def noop(self,*a):
    pass

exp1 = mt.LexiMutator([(fe._Exp,noop),
                       (fe.App,valm),
                       ])
expm = lv.LexiVisitor([(fe._Exp,noop),
                       (fe.App,valm),
                       ],'mutator')

expp = lv.LexiPrimVisitor([(mt._Mutable_T,noop),
                           ],'prim_mutate')

vstp = lv.LexiPrimVisitor([(mt._Mutable_T,noop),
                           ],'prim_visit')

vstr = lv.LexiVisitor([(fs.GenStmt,normal),
                       (fs.Exec,exec_s),
                       (fs.DrvdTypeDecl,actm),
                       (fs.UseStmt,addm),
                       ],'build')

class C1(TestCase):
    def test1(self):
        'build simple prog1'
        ae = self.assertEquals
        a_ = self.assert_

        f1   = fpf(fname_t('t0.f'))
        vst2 = lv.MultiLexiVisitor(vstr,exp1)
        p1   = Prog1(vst2,f1.lines)

        ostr = StringIO()
        p1.printit(out=ostr)

        ostr = ostr.getvalue()
        okf  = file(fname_t('t0.f.ok'))
        ok   = okf.read()
        okf.close()
        ae(ok,ostr)

s1 = makeSuite(C1)
suite = asuite(C1)

if __name__ == "__main__":
    runit(suite)
