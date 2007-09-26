'''
Test the free format stuff by actually reading files
'''
from Setup     import *
from unittest  import *
from fortFile  import Ffile
from fortLine  import *

class T1(TestCase):

    def test1(self):
        'simple free format string'

        ae = self.assertEquals
        a_ = self.assert_
        l1 = preclip('''
  logical subroutine(x,y, &
      & z)
''')
        f1 = Ffile.here(l1,True)
        ll = list(f1.lines)
        ae(len(ll),1)
        a_(isinstance(ll[0],fline))
        ae(ll[0].line,'  logical subroutine(x,y,  z)')
        ae(ll[0].rawline,l1)

s1 = asuite(T1)
suite = asuite(T1)

if __name__ == '__main__':
    runit(suite)


