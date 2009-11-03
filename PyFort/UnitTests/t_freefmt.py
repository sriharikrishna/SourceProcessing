'''
Test the free format stuff by actually reading files
'''
from Setup     import *
from unittest  import *
from fortFile  import Ffile
from fortLine  import *
from fortStmts import *
from fortStmts import _F90ExplLen,_Star,_NoInit,_Kind,_ExplKind
from useparse  import *


class T1(TestCase):

    def test1(self):
        'simple free format string'

        ae = self.assertEquals
        a_ = self.assert_
        l1 = preclip('''
  logical subroutine(x,y, &
      & z)
''')
        flow.setInputFormat('free')
        flow.setOutputFormat('free')
        f1 = Ffile.here(l1,True)
        ll = list(f1.lines)
        ae(len(ll),1)
        a_(isinstance(ll[0],fline))
        ae(ll[0].line,'logical subroutine(x,y,  z)')
        ae(ll[0].rawline,'logical subroutine(x,y,  z)')

    def test2(self):
        'continued free format string with ! comments -- KNOWN TO FAIL (internal comments are not currently being preserved)'

        ae = self.assertEquals
        a_ = self.assert_
        l1 = preclip('''
  logical subroutine(x,y, &
      & z) ! test it
''')
        f1 = Ffile.here(l1,True)
        ll = list(f1.lines)
        ae(len(ll),1)
        a_(isinstance(ll[0],fline))
        ae(ll[0].line,'logical subroutine(x,y,  z) ')
        ae(ll[0].rawline,'logical subroutine(x,y,  z)! test it')

    def test3(self):
        'uncontinued free format string with ! comments -- KNOWN TO FAIL (internal comments are not currently being preserved)'

        ae = self.assertEquals
        a_ = self.assert_
        l1 = preclip('''
function foo(x,y) ! test it  
''')
        f1 = Ffile.here(l1,True)
        ll = list(f1.lines)
        ae(len(ll),1)
        a_(isinstance(ll[0],fline))
        ae(ll[0].line,'function foo(x,y) ')
        ae(ll[0].rawline,'function foo(x,y)! test it  ')

    def test4(self):
        'decl with ! comment'

        ae = self.assertEquals
        a_ = self.assert_
        l1 = 'real :: x(:) ! here comes the comment'
        f1 = Ffile.here(l1,True)
        ll = list(f1.lines)
        ae(repr(pps(ll[0].line)),repr(RealStmt([],[],[_NoInit(App('x',[':']))])))

s1 = asuite(T1)
suite = asuite(T1)

if __name__ == '__main__':
    sys.exit(runit(suite))
