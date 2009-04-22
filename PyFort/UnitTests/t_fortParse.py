from Setup import *

from unittest      import *
from fortFile import Ffile
from fortParse import parse_stmt,parse_cmnt
from itertools     import *
import fortStmts as fs

class C1(TestCase):
    def test1(self):
        'parse file f3.f'

        f1   = Ffile.file(fname_t('f3.f'),free=False,c_action=parse_cmnt,s_action=parse_stmt)
        ok_s = (fs.Comments,
                fs.SubroutineStmt,
                fs.Comments,
                fs.AssignStmt,
                fs.Comments,
                fs.AssignStmt,
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
        for (l,c) in izip(f1.lines,ok_s):
            a_(isinstance(l,c))

s1 = makeSuite(C1)

suite = asuite(C1)

if __name__ == "__main__":
    runit(suite)
