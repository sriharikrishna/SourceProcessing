from Setup import *

from unittest      import *
from fortParseFile import *
from itertools     import *
import fortStmts as fs
import fortExp   as fe

class C1(TestCase):
    def test1(self):
        'parsed file w default map function'

        ae = self.assertEquals
        a_ = self.assert_

        f1   = fortParseFile(fname_t('f3.f'))
        lexi = ()
        r    = [ l for l in f1.map(lexi) ]
        ok_s = (fs.Comments,
                fs.SubroutineStmt,
                fs.Comments,
                fs.AssignStmt,
                fs.Comments,
                fs.AssignStmt,
                fs.AssignStmt,
                fs.OpenStmt,
                fs.FormatStmt,
                fs.Comments,
                fs.DoStmt,
                fs.AssignStmt,
                fs.EnddoStmt,
                fs.Comments,
                fs.EndStmt,
                )
        ae(len(r),len(ok_s))
        for (l,c) in izip(r,ok_s):
            a_(isinstance(l,c))

    def test2(self):
        'parsed file w filtering map, + passing mutable arg to map'

        def asgn_fn(l,s,cl):
            cl[0] += 1
            return (s,repr(l.lhs))
        
        ae = self.assertEquals
        a_ = self.assert_

        f1  = fortParseFile(fname_t('f3.f'))
        tmp = [0]

        lexi = ((fs.GenStmt,lambda *l:[]),
                (fs.AssignStmt,asgn_fn))
        _s   = 'leading string'
        res  = list(f1.map(lexi,_s,tmp))
        ok   = [_s,"'x'",
                _s,"'x'",
                _s,"'x'",
                _s,repr(fe.App('yy',['i']))
                ]
        ae(res,ok)
        ae(tmp[0],4)

s1 = makeSuite(C1)

suite = asuite(C1)

if __name__ == "__main__":
    runit(suite)
