import Setup

from unittest  import *
from fixedfmt  import *
from cStringIO import StringIO

class preds(TestCase):
    s1 = '''
c This is a comment
'''
    s2 = '''

'''
    s3 = '''
      x(10) = y * 14
'''
    s4 = '''
     +x = y * 10 + 'qqq' // 'rrr'
'''
    s5 = '''
10    foo = sin(bar)
'''
    s6 = '''
c $openad foob bar glurg
'''
    s7 = '''
c $openad$ foob bar glurg
'''
    (s1,s2,s3,s4,s5,s6,s7) = [ ll[1:] for ll in (s1,s2,s3,s4,s5,s6,s7)]

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test1(self):
        '''s1 is a comment'''
        s1 = preds.s1
        self.assert_(comment_p(s1))

    def test2(self):
        '''s1 is not a stmt or continuation'''
        s1 = preds.s1
        self.assert_(not (stmt_p(s1) or cont_p(s1)))

    def test3(self):
        '''s2 (blank) is a comment and not a stmt nor a continuation'''
        s2 = preds.s2
        self.assert_(comment_p(s2) and not (stmt_p(s2) or cont_p(s2)))

    def test4(self):
        '''s3 is a stmt, and nothing else'''
        s3 = preds.s3
        self.assert_(stmt_p(s3) and not (comment_p(s3) or cont_p(s3)))

    def test5(self):
        '''s4 is a continuation, and nothing else'''
        s4 = preds.s4
        self.assert_(cont_p(s4) and not (comment_p(s4) or stmt_p(s4)))

    def test6(self):
        '''s5 is a stmt, and nothing else'''
        s5 = preds.s5
        self.assert_(stmt_p(s5) and not (comment_p(s5) or cont_p(s5)))

def s1():
    return makeSuite(fline_from_line_t)

def suite():
    
    rv = makeSuite(preds)

    return rv

def runSuite(s):
    TextTestRunner(verbosity=2).run(s)

if __name__ == "__main__":
    runSuite(suite())

