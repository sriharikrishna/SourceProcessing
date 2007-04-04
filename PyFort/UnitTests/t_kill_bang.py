from Setup import *
from kill_bang import *
from unittest import *

class C1(TestCase):
    l1 = preclip(r'''
    str1 = 'This is a string'
''')
    l2 = preclip(r'''
    str1 = 'This is a string & !it has something in it'
''')
    l3 = preclip(r'''
          str1 = 'This is a string with & ! stuff &
''')
    l4 = preclip(r'''
       str1 = 'This is a string with & ! stuff &' &
''')
    l5 = preclip(r'''
       str1 = 'This is a string with & ! stuff &' & ! this is a comment
''')
    globals().update(locals())

    def test1(self):
        'no bang comments when embedded in strings'

        ae = self.assertEquals
        a_ = self.assert_
        for l in (l1,l2,l3,l4):
            (s,r) = kill_bang_comment(l)
            ae(s,l)
            ae(r,'')

    def test2(self):
        'bang comment after quoted string that contains & !'
        ae = self.assertEquals
        a_ = self.assert_
        (s,r) = kill_bang_comment(l5)
        ae(s,l5[:52])
        ae(r,'! this is a comment')

suite = asuite(C1)

if __name__ == '__main__':
    runit(suite)
    
