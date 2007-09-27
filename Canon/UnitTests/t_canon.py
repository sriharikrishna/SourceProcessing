from Setup import *
from _Setup import *
from canon import *
import Canon as cc
from unittest import *
from PyFort.fortContextFile import fortContextFile as fcf

'''
Tests for canonicalizer

'''
def hook1(self):
    if hasattr(self.toplev,'_scnt'):
        pass
    else:
        self.toplev._scnt = 0
        self.toplev.slice_undo = dict()

class C1(TestCase):
    def notest1(self):
        'read and context a fortran file'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fcf(fname_t('t0.f'),hook1)

        print e
        a_(True)

    def test1(self):
        'idemp rewrite using canon_lexi'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fcf(fname_t('t0.f'),hook1)
        set_verbose(False)
        f1rw = e.rewrite(canon_lexi)
        f1ok = fcf(fname_t('t0.f.ok'))
        ae(str(f1rw),str(f1ok))

    def test2(self):
        'canon of max using int consts w embedded kinds'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fcf(fname_t('int-const-w-kind.f90'),True,hook1)
        set_verbose(False)
        f1rw = e.rewrite(canon_lexi)
        f1ok = fcf(fname_t('int-const-w-kind.ok.f90'),True,hook1)
        ae(str(f1rw),str(f1ok))

    def test3(self):
        'canon + var decl of array of derived types'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fcf(fname_t('derived-type-arr.f90'),True,hook1)
        set_verbose(False)
        f1rw = e.rewrite(canon_lexi).rewrite(decl_lexi)
        f1ok = fcf(fname_t('derived-type-arr.ok.f90'),True,hook1)
        ae(str(f1rw),str(f1ok))

s1 = makeSuite(C1)

suite = asuite(C1)

if __name__ == "__main__":
    runit(suite)
