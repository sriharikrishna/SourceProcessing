import Setup

from unittest  import *
from fortFile  import *
from itertools import *
from _Setup    import *
from PyUtil.chomp     import chomp

import fortLine as fl

class file1(TestCase):
    def setUp(self):
        self.fname = Setup.fname_t('f1.f')
        self.ff    = Setup.open_t('f1.f')

    def tearDown(self):
        self.ff.close()

    def test1(self):
        '''Ffile string method returns the string rep of the file'''

        ff = self.ff

        self.assertEquals(Ffile.file(self.fname).str(),''.join(ff.readlines()))

    def test2(self):
        '''Ffile readlines method'''

        ff = self.ff

        self.assertEquals(Ffile.file(self.fname).readlines(),ff.readlines())

    def test3(self):
        '''Ffile iterlines method'''

        ff = self.ff
        ll = Ffile.file(self.fname).iterlines()

        for (v1,v2) in izip(ll,ff):
            self.assertEquals(v1,v2)

class fileops(TestCase):
    def setUp(self):

        fname     = Setup.fname_t('f1.f')
        f         = file(fname)
        self.ff   = Ffile.file(fname)
        self.fstr = f.read()
        f.close()

    def tearDown(self):
        pass

    def test1(self):
        '''write Ffile to a file'''
        import os

        fname = '__tmp.f'

        self.ff.write(fname)
        f = file(fname)
        
        self.assertEquals(self.fstr,f.read())

        f.close()
        os.remove(fname)

class heretst(TestCase):
    import os.path

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
    p1    = p1[1:]
    ff    = Ffile.here(p1)
    fname = os.path.join(Setup.mypath,'Tfiles','f1.f')
    f     = file(fname)

    def test1(self):
        "'here' docs"
        ff = heretst.ff
        f  = heretst.f
        ae = self.assertEquals
        for (here,ffi) in izip(f,ff.iterlines()):
            ae(here,ffi)

class maptest(TestCase):

    fname = Setup.fname_t('f2.f')


    def test1(self):
        
        'map operation, join short continuation lines'

        ae   = self.assertEquals
        ok   = Setup.open_t('f2.f.map_ok.1').read()
        ff   = Ffile.file(maptest.fname)
        lex1 = ((fl.cline,lambda l:[ chomp(l.rawline) ]),
                (fl.fline,lambda l:[ l.line ]))
        res  = ''.join(l+'\n' for l in ff.map(lex1))
        ae(res,ok)

    def test2(self):
        'map operation, filter comments'

        ae = self.assertEquals
        ok = Setup.open_t('f2.f.map_ok.2').read()
        ff   = Ffile.file(maptest.fname)
        lex1 = ((fl.cline,lambda l:[]),
                (fl.fline,lambda l:[ l.line ]))
        res  = ''.join(l+'\n' for l in ff.map(lex1))
        ae(res,ok)

def s1():
    return makeSuite(maptest)

def suite():
    rv = makeSuite(file1)
    rv.addTest(makeSuite(fileops))
    rv.addTest(makeSuite(heretst))
    rv.addTest(makeSuite(maptest))

    return rv

def runSuite(s):
    TextTestRunner(verbosity=2).run(s)

if __name__ == '__main__':
    runSuite(suite())
