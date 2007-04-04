from Setup import *
from canon import *
from _Setup import *
from Canon import *
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

s1 = makeSuite(C1)

suite = asuite(C1)

if __name__ == "__main__":
    runit(suite)
