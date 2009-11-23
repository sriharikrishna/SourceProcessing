from Setup     import *

from unittest  import *
from fortFile  import *
from itertools import *
from _Setup    import *
from PyUtil.chomp     import chomp

import fortLine as fl

class C1(TestCase):
    def test1(self):
        'file gets lines'
        ff = Ffile.file(fname_t('f2.f'))
        a_(ff,'fortFile object is screwed')
        for l in ff.lines: print l

class file1(TestCase):
    def setUp(self):
        self.fname = fname_t('f1.f')
        self.ff    = open_t('f1.f')

    def tearDown(self):
        self.ff.close()

    def test1(self):
        '''Ffile string method returns the string rep of the file -- KNOWN TO FAIL (fortFile methods need to be updated for new rawline parsing)'''

        ff = self.ff
        out = open_t('f1.out.f')

        self.assertEquals(Ffile.file(self.fname).str(),''.join(out.readlines()))
        out.close()

    def test2(self):
        '''Ffile readlines method -- KNOWN TO FAIL (fortFile methods need to be updated for new rawline parsing)'''

        ff = self.ff
        out = open_t('f2.out.f')

        self.assertEquals(Ffile.file(self.fname).readlines(),out.readlines())
        out.close()
        
    def test3(self):
        '''Ffile iterlines method -- KNOWN TO FAIL (fortFile methods need to be updated for new rawline parsing)'''

        ff = self.ff
        ll = Ffile.file(self.fname).iterlines()

        for (v1,v2) in izip(ll,ff):
            self.assertEquals(v1,v2)

class fileops(TestCase):
    def setUp(self):

        fname     = fname_t('f1.f')
        f         = file(fname)
        self.ff   = Ffile.file(fname)
        self.fstr = f.read()
        f.close()

    def tearDown(self):
        pass

    def test1(self):
        '''write Ffile to a file -- KNOWN TO FAIL (fortFile methods need to be updated for new rawline parsing)'''
        import os

        fname = '__tmp.f'

        self.ff.write(fname)
        f = file(fname)
        
        self.assertEquals(self.fstr,f.read())

        f.close()
        os.remove(fname)

class heretst(TestCase):
    p1 = '''
      subroutine foo(x)
      x = x +
c
c embedded continuation lines
c
     &5 + 2 * x
c
c more embedded lines
c
     & + si
     &n(x+2.0)
      x = 5.0
      x
     & = 
     & 13.2
      end
'''
    p1    = preclip(p1)
    ff    = Ffile.here(p1)
    fname = fname_t('f1.f')
    f     = file(fname)

    def test1(self):
        "'here' docs -- KNOWN TO FAIL (fortFile methods need to be updated for new rawline parsing)"
        ff = heretst.ff
        f  = heretst.f
        ae = self.assertEquals
        for (here,ffi) in izip(f,ff.iterlines()):
            ae(here,ffi)

class maptest(TestCase):

    fname = fname_t('f2.f')

    def test1(self):
        'map operation, join short continuation lines -- KNOWN TO FAIL (fortFile methods need to be updated for new rawline parsing)'

        ae   = self.assertEquals
        ok   = open_t('f2.f.map_ok.1').read()
        ff   = Ffile.file(maptest.fname)
        lex1 = ((fl.cline,lambda l:[ chomp(l.rawline) ]),
                (fl.fline,lambda l:[ l.lead + l.line ]))
        res  = ''.join(l+'\n' for l in ff.map(lex1))
        ae(res,ok)

    def test2(self):
        'map operation, filter comments -- KNOWN TO FAIL (fortFile methods need to be updated for new rawline parsing)'

        ae = self.assertEquals
        ok = open_t('f2.f.map_ok.2').read()
        ff   = Ffile.file(maptest.fname)
        lex1 = ((fl.cline,lambda l:[]),
                (fl.fline,lambda l:[ l.lead + l.line ]))
        res  = ''.join(l+'\n' for l in ff.map(lex1))
        ae(res,ok)

s  = asuite(C1)
s1 = asuite(maptest)

suite = asuite(file1,fileops,heretst,maptest)


if __name__ == '__main__':
    sys.exit(runit(suite))


