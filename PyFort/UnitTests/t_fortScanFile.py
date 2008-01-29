from Setup import *

from unittest     import *
from fortScanFile import *
from fortLine import fortLine
from itertools import *

class T2(TestCase):
    def test1(self):
        'map fortScanFile'

        f1  = fortScanFile(fname_t('f3.f'))
        a_(f1)
        
        lex = ((fortScanComment, lambda l: []),
               (fortScanLine,    lambda l: [l.scan[0]]))
        ok  = ['subroutine','x','x','x','open','format','do',
               'yy','enddo','end']
        for (v1,v2) in izip(f1.map(lex),chain(ok,repeat(None))):
            ae(v1,v2,"")

class T3(TestCase):
    '''free fmt scan from file
    NOTE: set fmt = 'free' in the line module
    '''
    def test1(self):
        ae  = self.assertEquals
        a_  = self.assert_
        a_(True)
        

suite = asuite(T2)
s = suite

if __name__ == '__main__':
    runit(suite)
