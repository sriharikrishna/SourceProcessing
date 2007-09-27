from Setup import *
from unittest import *

from fortContextFile import *
from fortStmts       import comment_bl

'''
Tests for fortContextFile

'''
class C1(TestCase):
    def test1(self):
        'Empty file has "" as string rep'
        ae = self.assertEquals
        a_ = self.assert_
        ef = open_t('empty.f')
        e  = fortContext(ef)
        ef.close()
        ae(str(e),'')

    def test2(self):
        'empty object has "" as string rep'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextEmpty()
        ae(str(e),'')

    def test3(self):
        'extend empty with comment block works'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextEmpty()
        e.extend([comment_bl('This','is a','comment','')])
        
        f1  = open_t('comment_bl.f.ok')
        f1s = f1.read()
        f1.close()

        ae(str(e),f1s)
        
class C2(TestCase):
    def test1(self):
        'simple file f1'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextFile(fname_t('f1.f'))
        ae(len(e.lines),7)
        ae(str(e.lines[3]),'x = x + 5 + 2 * x + sin(x + 2.0)')

    def test2(self):
        'file w simple declarations'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextFile(fname_t('simple_decls.f'))
        (ty,mod) = e.lines[0].ctxt.lookup_type('x')
        ae(ty.kw_str,'double precision')
        a_(not mod)
        (ty,mod) = e.lines[0].ctxt.lookup_type('y')
        ae(ty.kw_str,'real')
        a_(mod)
        ae(str(mod[0]),'*8')
        dims = e.lines[0].ctxt.lookup_dims('x')
        a_(not dims)
        dims = e.lines[0].ctxt.lookup_dims('y')
        a_(dims)
        ae(len(dims),2)
        ae([str(d) for d in dims],['5','7'])

    def test3(self):
        'file w attr declarations'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextFile(fname_t('attr_decls.f'))
        (ty,mod) = e.lines[0].ctxt.lookup_type('x')
        ae(ty.kw_str,'double precision')
        a_(not mod)
        dims = e.lines[0].ctxt.lookup_dims('x')
        a_(dims)
        ae(len(dims),1)
        ae([str(d) for d in dims],['2'])
        (ty,mod) = e.lines[0].ctxt.lookup_type('y')
        ae(ty.kw_str,'real')
        a_(not mod)
        dims = e.lines[0].ctxt.lookup_dims('y')
        a_(not dims)
#        for l in e.lines: print l

    def test4(self):
        'file character decls + attrs'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextFile(fname_t('char_decls.f'))
        (ty,mod) = e.lines[0].ctxt.lookup_type('x')
        ae(ty.kw_str,'character')
        a_(mod)
        dims = e.lines[0].ctxt.lookup_dims('x')
        a_(dims)
        ae(len(dims),1)
        ae([str(d) for d in dims],['2'])
        (ty,mod) = e.lines[0].ctxt.lookup_type('y')
        ae(ty.kw_str,'character')
        a_(mod)
        dims = e.lines[0].ctxt.lookup_dims('y')
        a_(dims)
        ae([str(d) for d in dims],['2','4'])
#        for l in e.lines: print l

class C3(TestCase):
    def test1(self):
        'interface blocks stuff (using freefmt read)'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextFile(fname_t('intrfblk.f90'),True)
        (ty,mod) = e.lines[0].ctxt.lookup_type('x')
        ae(ty.kw_str,'integer')
        (ty,mod) = e.lines[0].ctxt.lookup_type('y')
        ae(ty.kw_str,'complex')
#        for l in e.lines: print repr(l)

    def test2(self):
        'change class of subroutine,function in interface block'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextFile(fname_t('intrfblk.f90'),True)
        a_(isinstance(e.lines[2],fs.IfPUstart))
        a_(isinstance(e.lines[5],fs.IfPUend))

    def test3(self):
        'ignore ifaceblk in JU test file'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextFile(fname_t('iblk.f'))
        a_(True)

    def test4(self):
        'a%b NOT a statement function'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextFile(fname_t('selarr.f90'),True)
        a_(isinstance(e.lines[2],fs.StmtFnStmt))
        a_(not isinstance(e.lines[4],fs.StmtFnStmt))
#        for l in e.lines: print repr(l)

class C4(TestCase):
    def test1(self):
        'derived types recording'
        ae = self.assertEquals
        a_ = self.assert_
        e  = fortContextFile(fname_t('drvd-types.f90'),True)
#        for l in e.lines: print repr(l)
        (ty,mod) = e.lines[0].ctxt.lookup_type('x')
        ae(ty.kw_str,'integer')
        (ty,mod) = e.lines[0].ctxt.lookup_type('y')
        ae(ty.kw_str,'complex')

s1 = makeSuite(C4)

suite = asuite(C1,C2,C3,C4)

'''
if __name__ == "__main__":
    runit(suite)
'''
