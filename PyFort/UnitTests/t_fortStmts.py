from Setup     import *
from unittest  import *

from fortStmts import *
from fortStmts import _Kind
from fortStmts import _Star
from fortStmts import _F90ExplLen,_NoInit
from useparse  import *

class C1(TestCase):
    '''Test that appropriate instances are created for
    some simple statements
    '''

    def test1(self):
        'simple real stmt'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'real  x(10),y,z'
        a_(isinstance(parse(scan(s)),RealStmt))

    def test2(self):
        'simple if stmt'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'if (x(11).EQ.y.OR.x(12).lt.(floob*(i**k))) goto 10'
        a_(isinstance(parse(scan(s)),IfStmt))

    def test3(self):
        'endif stmt as "end if"'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'end if'
        a_(isinstance(parse(scan(s)),EndifStmt))

    def test4(self):
        'endif stmt as "endif"'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'endif'
        a_(isinstance(parse(scan(s)),EndifStmt))

    def test5(self):
        'simple subroutine stmt'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'subroutine foo(x,y,z)'
        a_(isinstance(parse(scan(s)),SubroutineStmt))

class C2(TestCase):
    '''assignment statement'''

    def test1(self):
        'simple assignment statement'

        ae = self.assertEquals
        a_ = self.assert_

        s  = 'foo(5,7) = x.lt.y'
        v = parse(scan(s))
        ae(repr(v),
           repr(AssignStmt(App('foo',['5','7']),Ops('.lt.','x','y')))
           )

    def test2(self):
        'assignment w kw "if" as var'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'if(ix,iy) = sin(x)'
        v = parse(scan(s))
        ae(repr(v),
           repr(AssignStmt(App('if',['ix','iy']),App('sin',['x'])))
           )
    def test3(self):
        "selection(%) operator works on lhs"
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'x%v = 5'
        v  = parse(scan(s))
        ae(repr(v),
           repr(AssignStmt(Sel('x','v'),'5')))
    def test4(self):
        "complicated exp including selection(%) works on lhs"
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'x%v(1,2)%q(xx)%r = 5'
        v  = parse(scan(s))
        ae(repr(v),
           repr(AssignStmt(Sel(App(Sel(App(Sel('x','v'),['1', '2']),'q'),
                                   ['xx']),'r'),'5')))

class C3(TestCase):
    def test1(self):
        'create comment block'
        ae = self.assertEquals
        a_ = self.assert_
        c1 = comment_bl('This','is','a','comment')
        c2 = Comments('\n'.join(['c This','c is','c a','c comment'])+'\n')
        ae(c1.viz(),c2.viz())

class C4(TestCase):
    def test1(self):
        'assignment statement classes have have "_sons" attribute'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'if(ix,iy) = sin(x)'
        v = parse(scan(s))
        a_(hasattr(v,'_sons'))
        ae(v._sons,['lhs','rhs'])

    def test2(self):
        'if statement classes have have "_sons" attribute'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'if (x .eq. 5) goto 23'
        v = parse(scan(s))
        a_(hasattr(v,'_sons'))

class C5(TestCase):
    '''Test derived type
    '''
    def test1(self):
        'simple derived type declaration'
        ae = self.assertEquals
        a_ = self.assert_
        s = 'type(foo) :: x(10),y(len(kk),3),z,w'
        ps = pps(s)
        a_(isinstance(ps,DrvdTypeDecl))
        a_(not ps.attrs)
        ae(len(ps.decls),4)

    def test2(self):
        'simple derived type definition'
        ae = self.assertEquals
        a_ = self.assert_
        s = 'type bar'
        ps = pps(s)
        a_(isinstance(ps,DrvdTypeDefn))
        ae(ps.name,'bar')

    def test3(self):
        'derived type declaration with attributes'
        ae = self.assertEquals
        a_ = self.assert_
        s = 'type(foo),dimension(10),target :: x,y,z(30)'
        ps = pps(s)
        a_(isinstance(ps,DrvdTypeDecl),'instance check')
        ae(len(ps.attrs),2,'attr check')
        ae(len(ps.decls),3,'decls check')

    def test4(self):
        'empty interface definition'
        ae = self.assertEquals
        a_ = self.assert_
        s = 'interface'
        ps = pps(s)
        a_(isinstance(ps,InterfaceStmt),'instance check')
        ae(str(ps),s)

    def test5(self):
        'nonempty interface definition'
        ae = self.assertEquals
        a_ = self.assert_
        s = 'interface foo'
        ps = pps(s)
        a_(isinstance(ps,InterfaceStmt),'instance check')
        ae(str(ps),s)

class C6(TestCase):
    '''F90 style types
    '''
    def test1(self):
        'real with attributes'
        ae = self.assertEquals
        a_ = self.assert_
        s = 'real(3),dimension(aaa) :: x,y'
        ps = pps(s)
        a_(isinstance(ps,RealStmt))
        a_(ps.attrs)
        ae(len(ps.decls),2)

    def test2(self):
        'double precision with attributes'
        ae = self.assertEquals
        a_ = self.assert_
        s = 'double precision,pointer,intent(inout) :: y'
        ps = pps(s)
        a_(isinstance(ps,DoubleStmt))
        a_(ps.attrs)
        ae(len(ps.attrs),2)
        ae(str(ps.attrs[1]),str(App('intent',['inout'])))

    def test3(self):
        'string value of real stmt w attributes'
        ae = self.assertEquals
        a_ = self.assert_
        vv = RealStmt([_Kind('4')],[App('intent',['in']),'allocatable'],['x','y',])
        ae(str(vv),'real(4),intent(in),allocatable :: x,y')

    def test4(self):
        'string value of double stmt w attributes'
        ae = self.assertEquals
        a_ = self.assert_
        vv = DoubleStmt([],[App('intent',['in']),'allocatable'],['x','y',])
        ae(str(vv),'double precision,intent(in),allocatable :: x,y')

def _gt(decl):
    'generate type reps f decl strings, using parser'
    stmt = pps(decl + ' x')
    return (stmt.__class__,stmt.mod)

class C7(TestCase):
    '''Typing, and misc xform utilities
    '''
    def test1(self):
        'kw2type'
        ae = self.assertEquals
        a_ = self.assert_
        ae(kw2type('real'),RealStmt)
        ae(kw2type('doubleprecision'),DoubleStmt)
        ae(kw2type('integer'),IntegerStmt)
        ae(kw2type('logical'),LogicalStmt)

    def test2(self):
        'lenfn'
        ae = self.assertEquals
        a_ = self.assert_
        ae(str(lenfn(15)[0]),'*15')

    def test3(self):
        'typemerge'
        ae = self.assertEquals
        a_ = self.assert_

        t1 = _gt('real')
        t2 = _gt('real*4')
        t3 = _gt('real*8')
        t4 = _gt('double precision')
        t5 = _gt('complex')
        t6 = _gt('integer')

        ae(typemerge([],t1),t1)
        ae(typemerge([t2],t1),t2)
        ae(typemerge([t1,t1,t1],t2),t1)
        ae(typemerge([t1,t2,t1],t1),t2)
        ae(typemerge([t1,t2,t1,t3],t1),t3)
        ae(typemerge([t6,t6,t6,t1],t1),t1)
        ae(typemerge([t3,t4,],t1),t4)
        ae(typemerge([t1,t2,t3,t4,t5,t6],t1),t5)

class C8(TestCase):
    'more declarations, esp character statements'
    def test1(self):
        '''f77 style character decls
        '''
        ae = self.assertEquals
        a_ = self.assert_

        v = pps('character*5  foo,bar,baz(10)')
        a_(isinstance(v,CharacterStmt))
        a_(v.mod)
        ae(v.mod[0].len,'5')
        a_(not v.attrs)
        a_(v.decls)
        vars = [repr(i.lhs) for i in v.decls]
        for vv in ['foo','bar']: a_(repr(vv) in vars)
        a_(repr(App('baz',['10'])) in vars)

    def test2(self):
        '''f77 style character decls, w *(*) len
        '''
        ae = self.assertEquals
        a_ = self.assert_

        v = pps('character*(*)  foo,bar,baz(10)')
        a_(isinstance(v,CharacterStmt))
        a_(isinstance(v.mod[0].len,_Star))
        a_(not v.attrs)
        a_(v.decls)
        vars = [repr(i.lhs) for i in v.decls]
        for vv in ['foo','bar']: a_(repr(vv) in vars)
        a_(repr(App('baz',['10'])) in vars)

    def test3(self):
        '''f90 style decls, w len modifiers & attribs
        '''
        ae = self.assertEquals
        a_ = self.assert_

        ss = 'character(len=AAA+9),dimension(3) :: gack(*,*)*5,floob,foo*2,bar(2)'
        v = pps(ss)
        ae(repr(v),
           repr(CharacterStmt(
            [_F90ExplLen(Ops('+','AAA','9'))],
            [App('dimension',['3'])],
            [_NoInit(Ops('*',App('gack',['*', '*']),'5')),
             _NoInit('floob'),
             _NoInit(Ops('*','foo','2')),
             _NoInit(App('bar',['2']))])))

    def test4(self):
        '''double precision w attributes, and '*' dimensions
        '''
        ae = self.assertEquals
        a_ = self.assert_

        ss = pps('double precision,dimension(5) :: x,y(*,*,3),z')
        ae(repr(ss),
           repr(DoubleStmt([],[App('dimension',['5'])],
                           [_NoInit('x'),
                            _NoInit(App('y',['*', '*', '3'])),
                            _NoInit('z')])))
    def test5(self):
        '''star ranges (gawd)'''
        ae = self.assertEquals
        a_ = self.assert_

        ss = pps('double precision,dimension(5) :: x,y(*,2*x:*,3),z')
        ae(repr(ss),
           repr(DoubleStmt([],[App('dimension',['5'])],
                           [_NoInit('x'),
                            _NoInit(App('y',['*', Ops(':',Ops('*','2','x'),'*'), '3'])),
                            _NoInit('z')])))

s1    = makeSuite(C8)

suite = asuite(C1,C2,C3,C4,C5,C6,C7,C8)

if __name__ == '__main__':
    runit(suite)
