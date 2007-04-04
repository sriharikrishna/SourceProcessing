from Setup import *

from unittest     import *
from fortScanFile import *
from fortLine import fline_from_line
from itertools import *

class T1(TestCase):

    def test1(self):
        'fortScanFile scan string1'

        ae = self.assertEquals
        a_ = self.assert_

        p1 = '''
       f(i) = 'This is a '' str' // 'another string' // gl(3.14_www)
'''

        p1 = fortScanLine(fline_from_line(p1[1:]))
        ok = ['f', '(', 'i', ')', '=',
              "'This is a '' str'", '//',
              "'another string'", '//', 'gl', '(', '3.14_www', ')']

        ae(p1.scan,ok)
        a_(isinstance(p1,fortScanLine))

class T2(TestCase):
    def test1(self):
        'map fortScanFile'

        ae  = self.assertEquals

        f1  = fortScanFile(fname_t('f3.f'))
        
        lex = ((fortScanComment, lambda l: []),
               (fortScanLine,    lambda l: [l.scan[0]]))
        ok  = ['subroutine','x','x','x','open','format','do',
               'yy','enddo','end']
        for (v1,v2) in izip(f1.map(lex),chain(ok,repeat(None))):
            ae(v1,v2)
def s1():
    return makeSuite(T2)

suite = asuite(T1,T2)

if __name__ == '__main__':
    runit(suite)
