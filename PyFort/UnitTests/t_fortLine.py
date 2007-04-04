import Setup

from unittest  import *
from fortLine  import *
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
    (s1,s2,s3,s4,s5) = [ l[1:] for l in (s1,s2,s3,s4,s5)]

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

class flow(TestCase):

    s1 = '''
      x1(3:5) = 'This is an extremely long string that must be spread over several lines' // 'concatenated with another extreeeeeeeeemly long string'_foobar // 'one more string just to round things out ...'
'''
    s2 = '''
c This is a comment line that may be a little long, but that should be ok
'''
    s3 = '''
      x1(42,39,x**2 + ix * y + iz,44) = sin(cos(atan(foo,bar)))
'''
    (s1,s2,s3) = [x[1:] for x in (s1,s2,s3)]
    
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test1(self):
        '''flow a long line'''
        s1 = flow.s1
        correct = '''
      x1(3:5) = 'This is an extremely long string that must be spread ov
     +er several lines' // 'concatenated with another extreeeeeeeeemly l
     +ong string'_foobar // 'one more string just to round things out ..
     +.'
'''
        correct = correct[1:]
        self.assertEquals(flow_line(s1),correct)

    def test2(self):
        '''flow a comment gives no change'''
        s2 = flow.s2
        self.assertEquals(flow_line(s2),s2)

    def test3(self):
        '''flow a line shorter than 72 leaves line unchanged'''
        s3 = flow.s3
        self.assertEquals(flow_line(s3),s3)

class kill_bang(TestCase):
    s1 = "       x(ii,jj) = 'foo '' ' // 'bar' ! eol comment"
    s2 = "       x(ii,jj) = 'foo '' ' // 'bar'"
    s3 = "       x(ii,jj) = 'foo '' ' // 'bar ! eol comment"

    def test1(self):
        'kill bang comments from end of line'
        s1 = kill_bang.s1
        s2 = kill_bang.s2
        s3 = kill_bang.s3
        ae = self.assertEquals

        (l,c) = kill_bang_comment(s1)
        ae(l,"       x(ii,jj) = 'foo '' ' // 'bar' ")
        ae(c,"! eol comment")

        (l,c) = kill_bang_comment(s2)
        ae(l,s2)
        ae(c,'')

        (l,c) = kill_bang_comment(s3)
        ae(l,s3)
        ae(c,'')

class fline_t(TestCase):
    right = "       x = foo // 'bar  '' '    // ' some more string with ! in itfinishes the string'"
    p1 = '''
       x = foo // ! concatenation comment for fun

c Internal comment
c
     +'bar  '' ' ! another shout comment
     &   // ' some more string with ! in it
     *finishes the string'
'''
    p1 = p1[1:]
    p1f = StringIO(p1)
    stmt = pred(stmt_p)
    cmnt = pred(comment_p)
    cont = pred(cont_p)
    full = treat(seq(stmt,star(seq(star(cmnt),cont))),fline_from_asm)

    (fln1,rst) = full(buf_iter(p1f))

    def test1(self):
        'fline class, rawline,line,and internal_comments attributes'
        internal_c = '! concatenation comment for fun||c Internal comment|c|! another shout comment'
        ae = self.assertEquals
        aa = self.assert_

        fln1 = fline_t.fln1
        rst  = fline_t.rst

        aa(fln1.rawline == fline_t.p1)
        ae(fln1.line,fline_t.right)
        ae(list(rst),[])
        ae('|'.join(fln1.internal),internal_c)

    def test2(self):
        'cline class, rawline attribute, comment_list method'
        aa = self.assert_
        ae = self.assertEquals

        p1 = '''
   ! This is a comment
C
c Internal comment
c
* ! another !
d    a possible debugging line
'''
        p1   = p1[1:]
        p1f  = buf_iter(StringIO(p1))
        cmnt = pred(comment_p)
        cblk = treat(plus(cmnt),cline)

        (fln1,rst) = cblk(p1f)

        aa(fln1.rawline == p1)
        ae(fln1.comment_list(),['   ! This is a comment', 'C', 'c Internal comment', 'c', '* ! another !', 'd    a possible debugging line'])

class fline_from_line_t(TestCase):

    def test1(self):
        'convert a long line to an fline object'
        p1 = '''
       x = somefn('This is an extremely long string to be put on 1 line' // 'Another line')
'''
        p1 = p1[1:]
        ok = '''
       x = somefn('This is an extremely long string to be put on 1 line'
     + // 'Another line')
'''
        ok = ok[1:]
        t  = fline_from_line(p1)
        self.assert_(isinstance(t,fline))
        self.assertEquals(t.rawline,ok)

    def test2(self):
        'convert comment line to cline object'
        p1 = '''
c This is a comment
'''
        p1 = p1[1:]
        t  = fline_from_line(p1)
        self.assert_(isinstance(t,cline))
        self.assertEquals(t.rawline,p1)

def s1():
    return makeSuite(fline_from_line_t)

def suite():
    
    rv = makeSuite(preds)
    rv.addTest(makeSuite(flow))
    rv.addTest(makeSuite(kill_bang))
    rv.addTest(makeSuite(fline_t))
    rv.addTest(makeSuite(fline_from_line_t))

    return rv

def runSuite(s):
    TextTestRunner(verbosity=2).run(s)

if __name__ == "__main__":
    runSuite(suite())

