#!/usr/bin/env python

from Setup     import *
from unittest  import *

from fortStmts import *
from fortStmts import _F90ExplLen,_Star,_NoInit,_Kind,_ExplKind,_PointerInit
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
        ae(repr(pps(s)),repr(RealStmt([],[],[_NoInit(App('x',['10'])), _NoInit('y'), _NoInit('z')])))
        s  = 'real :: x(:)'
        ae(repr(pps(s)),repr(RealStmt([],[],[_NoInit(App('x',[':']))])))

    def test2(self):
        'simple if stmt'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'if (x(11).EQ.y.OR.x(12).lt.(floob*(i**k))) goto 10'
        a_(isinstance(pps(s),IfStmt))

    def test3(self):
        'endif stmt as "end if"'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'end if'
        a_(isinstance(pps(s),EndifStmt))

    def test4(self):
        'endif stmt as "endif"'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'endif'
        a_(isinstance(pps(s),EndifStmt))

    def test5(self):
        'simple subroutine stmt'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'subroutine foo(x,y,z)'
        a_(isinstance(pps(s),SubroutineStmt))


class C2(TestCase):
    '''assignment statement'''

    def test1(self):
        'simple assignment statement'

        ae = self.assertEquals
        a_ = self.assert_

        s  = 'foo(5,7) = x.lt.y'
        ae(repr(pps(s)),
           repr(AssignStmt(App('foo',['5','7']),Ops('.lt.','x','y')))
           )

    def test2(self):
        'assignment w kw "if" as var'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'if(ix,iy) = sin(x)'
        ae(repr(pps(s)),
           repr(AssignStmt(App('if',['ix','iy']),App('sin',['x'])))
           )

    def test3(self):
        "selection(%) operator works on lhs"
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'x%v = 5'
        ae(repr(pps(s)),
           repr(AssignStmt(Sel('x','v'),'5')))

    def test4(self):
        "complicated exp including selection(%) works on lhs"
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'x%v(1,2)%q(xx)%r = 5'
        ae(repr(pps(s)),
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

        v = pps('if(ix,iy) = sin(x)')
        a_(hasattr(v,'_sons'))
        ae(v._sons,['lhs','rhs'])

    def test2(self):
        'if statement classes have have "_sons" attribute'
        ae = self.assertEquals
        a_ = self.assert_

        s  = 'if (x .eq. 5) goto 23'
        a_(hasattr(pps(s),'_sons'))

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
        '''derived type declaration with empty array bounds'''
        s = 'type(OpenADTy_active) :: X(:)'
        r = DrvdTypeDecl(['(OpenADTy_active)'],[],[_NoInit(App('X',[':']))])
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

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

class C8(TestCase):
    'Arrays with attributes and asterisk dimension specifications'
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

class C9(TestCase):
    'end interface block stuff'
    def test1(self):
        '''end interface block'''
        ae = self.assertEquals
        a_ = self.assert_
        it = pps('end interface')
        a_(isinstance(it,EndInterfaceStmt))
        
    def test2(self):
        '''end other things (module, program, function, subroutine, block data)'''
        ae = self.assertEquals
        a_ = self.assert_
        chk = ['module','program','function','subroutine','block data']
        for l in chk:
            a_(isinstance(pps('end '+l),EndStmt))

class C10(TestCase):
    'check statement property predicates'
    def test1(self):
        'subroutine is both decl and ustart'
        s = pps('subroutine foo')
        a_(s.is_decl() and s.is_ustart())

class TestImplicitStmt(TestCase):
    '''Implicit statements'''
    def test1(self):
        'implicit character*10'
        s1 = 'implicit character*10 (b,d,f-h,l-n)'
        v = pps(s1)
        a_(isinstance(v,ImplicitStmt))
        ae(len(v.lst),1)
        ae(len(v.lst[0]),2)
        ae(len(v.lst[0][1]),4)
        l = v.lst[0][1]
        ae(str(l[0]).lower(),str(ep('b')).lower())
        ae(str(l[1]).lower(),str(ep('d')).lower())
        ae(str(l[2]).lower(),str(ep('f-h')).lower())
        ae(str(l[3]).lower(),str(ep('l-n')).lower())
        impl_typ = v.lst[0][0]
        proto    = pps('character*10 a')
        ae(impl_typ[0],proto.__class__)
        ae(repr(impl_typ[1]),repr(proto.mod))
        ae(kill_blanks(s1),kill_blanks(str(v)))

    def test2(self):
        'implicit character (no len)'
        s1 = 'implicit character (b,d,f-h,l-n)'
        v = pps(s1)
        a_(isinstance(v,ImplicitStmt))
        ae(len(v.lst),1)
        ae(len(v.lst[0]),2)
        ae(len(v.lst[0][1]),4)
        l = v.lst[0][1]
        ae(str(l[0]).lower(),str(ep('b')).lower())
        ae(str(l[1]).lower(),str(ep('d')).lower())
        ae(str(l[2]).lower(),str(ep('f-h')).lower())
        ae(str(l[3]).lower(),str(ep('l-n')).lower())
        impl_typ = v.lst[0][0]
        proto    = pps('character a')
        ae(impl_typ[0],proto.__class__)
        ae(repr(impl_typ[1]),repr(proto.mod))
        ae(kill_blanks(s1),kill_blanks(str(v)))

    def test3(self):
        'implicit real*4'
        s1 = 'implicit real*4 (a,f,h-k)'
        v = pps(s1)
        a_(isinstance(v,ImplicitStmt))
        ae(len(v.lst),1)
        ae(len(v.lst[0]),2)
        ae(len(v.lst[0][1]),3)
        l = v.lst[0][1]
        ae(str(l[0]).lower(),str(ep('a')).lower())
        ae(str(l[1]).lower(),str(ep('f')).lower())
        ae(str(l[2]).lower(),str(ep('h-k')).lower())
        impl_typ = v.lst[0][0]
        proto    = pps('real * 4 a')
        ae(impl_typ[0],proto.__class__)
        ae(repr(impl_typ[1]),repr(proto.mod))
        ae(kill_blanks(s1),kill_blanks(str(v)))

    def test4(self):
        'implicit integer (a,f)'
        s1 = 'implicit integer (a,f)'
        v = pps(s1)
        a_(isinstance(v,ImplicitStmt))
        ae(len(v.lst),1)
        ae(len(v.lst[0]),2)
        ae(len(v.lst[0][1]),2)
        l = v.lst[0][1]
        ae(str(l[0]).lower(),str(ep('a')).lower())
        ae(str(l[1]).lower(),str(ep('f')).lower())
        impl_typ = v.lst[0][0]
        proto    = pps('integer a')
        ae(impl_typ[0],proto.__class__)
        ae(repr(impl_typ[1]),repr(proto.mod))
        ae(kill_blanks(s1),kill_blanks(str(v)))

    def test6(self):
        'implicit real(RARB) (h,i,j)'
        s1 = 'implicit real(RARB) (h,i,j)'
        v = pps(s1)
        a_(isinstance(v,ImplicitStmt))
        ae(len(v.lst),1)

        ae(len(v.lst[0]),2)
        ae(len(v.lst[0][1]),3)
        l = v.lst[0][1]
        ae(str(l[0]).lower(),str(ep('h')).lower())
        ae(str(l[1]).lower(),str(ep('i')).lower())
        ae(str(l[2]).lower(),str(ep('j')).lower())
        impl_typ = v.lst[0][0]
        proto    = pps('real(RARB) a')
        ae(impl_typ[0],proto.__class__)
        ae(repr(impl_typ[1]),repr(proto.mod))

        ae(kill_blanks(s1),kill_blanks(str(v)))

    def test7(self):
        'implicit real(kind=FOOB) (b-d,f),real(RARB) (h,i,j)'
        s1 = 'implicit real(kind=FOOB) (b-d,f),real(RARB) (h,i,j)'
        v = pps(s1)
        a_(isinstance(v,ImplicitStmt))
        ae(len(v.lst),2)
        ae(len(v.lst[0]),2)
        ae(len(v.lst[0][1]),2)
        l = v.lst[0][1]
        ae(str(l[0]).lower(),str(ep('b-d')).lower())
        ae(str(l[1]).lower(),str(ep('f')).lower())
        impl_typ = v.lst[0][0]
        proto    = pps('real(kind=FOOB) a')
        ae(impl_typ[0],proto.__class__)
        ae(repr(impl_typ[1]),repr(proto.mod))

        ae(len(v.lst[1]),2)
        ae(len(v.lst[1][1]),3)
        l = v.lst[1][1]
        ae(str(l[0]).lower(),str(ep('h')).lower())
        ae(str(l[1]).lower(),str(ep('i')).lower())
        ae(str(l[2]).lower(),str(ep('j')).lower())
        impl_typ = v.lst[1][0]
        proto    = pps('real(RARB) a')
        ae(impl_typ[0],proto.__class__)
        ae(repr(impl_typ[1]),repr(proto.mod))

        ae(kill_blanks(s1),kill_blanks(str(v)))

class TestCharacterDecls(TestCase):
    'test character declaration statements'
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
        ae(repr(v).lower(),
           repr(CharacterStmt(
            [_F90ExplLen(Ops('+','aaa','9'))],
            [App('dimension',['3'])],
            [_NoInit(Ops('*',App('gack',['*', '*']),'5')),
             _NoInit('floob'),
             _NoInit(Ops('*','foo','2')),
             _NoInit(App('bar',['2']))])).lower())

    def test4(self):
        '''character type declaration with asterisk length specification'''
        s = 'character :: temp*16'
        r = CharacterStmt([],[],[_NoInit(Ops('*','temp','16'))])
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

class TestDimensionStmt(TestCase):
    '''Dimension statement'''
    def test1(self):
        'dimension m(4,4), v(1000)'
        self.assertEquals(repr(pps('dimension m(4,4), v(1000)')),
                          repr(DimensionStmt([App('m',['4', '4']),
                                              App('v',['1000'])])))

    def test2(self):
        'dimension helio (-3:3, 4, 3:9)'
        self.assertEquals(repr(pps('dimension helio (-3:3, 4, 3:9)')),
                          repr(DimensionStmt([App('helio',
                                                  [Ops(':',Umi('3'),'3'),
                                                   '4',
                                                   Ops(':','3','9')])])))

    def test3(self):
        'dimension a(hi, hi*3 + lo )'
        self.assertEquals(repr(pps('dimension a(hi, hi*3 + lo )')),
                          repr(DimensionStmt([App('a',
                                                  ['hi',
                                                   Ops('+',Ops('*','hi','3'),'lo')])])))

class TestDoStmt(TestCase):
    '''Do statements'''
    def test1(self):
        '''do i = 1,2'''
        s = 'do i = 1,2'
        r = DoStmt(None,None,'i','1','2',None)
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test2(self):
        '''do i = 1,2,1'''
        s = 'do i = 1,2,1'
        r = DoStmt(None,None,'i','1','2','1')
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test3(self):
        '''do i = floor(x),2'''
        s = 'do i = floor(x),2'
        r = DoStmt(None,None,'i',App('floor',['x']),'2',None)
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test4(self):
        '''do i = floor(x)+10,2,abs(y+2)'''
        s = 'do i = floor(x)+10,2,abs(y+2)'
        r = DoStmt(None,
                   None,
                   'i',
                   Ops('+',App('floor',['x']),'10'),
                   '2',
                   App('abs',[Ops('+','y','2')]))
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test5(self):
        '''do statement with label "do 40 i = 1,2"'''
        s = 'do 40 i = 1,2'
        r = DoStmt(None,'40','i','1','2',None)
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test6(self):
        '''do statement with label and stride "do 40 i = 1,2,1"'''
        s = 'do 40 i = 1,2,1'
        r = DoStmt(None,'40','i','1','2','1')
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test6(self):
        '''do statement with do name'''
        theString = 'l1000: do ic = 1,icm'
        theRepr = DoStmt('l1000',None,'ic','1','icm',None)
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

class TestWhileStmt(TestCase):
    '''While statements'''
    def test1(self):
        '''do while (1.lt.3)'''
        s = 'do while (1.lt.3)'
        r = WhileStmt(Ops('.lt.','1','3'))
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test2(self):
        '''do while (foo(x).and.p.eq.q)'''
        s = 'do while (foo(x).and.p.eq.q)'
        r = WhileStmt(Ops('.and.',
                          App('foo',['x']),
                          Ops('.eq.','p','q')))
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

class TestCallStmt(TestCase):
    '''Subroutine call statements'''
    def test1(self):
        '''Subroutine call with 1/2 named parameter arguments - FAILS right now, waiting for fix'''
        s = 'call foo(1,b = bar(x))'
        r = CallStmt('foo',['1',
                            NamedParam('b',App('bar','x'))])
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test2(self):
        '''Subroutine call with both named parameter arguments - FAILS right now, waiting for fix'''
        s = 'call foo(a = 1,b = bar(x))'
        r = CallStmt('foo',[NamedParam('a','1'),
                            NamedParam('b',App('bar','x'))])
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

class TestFunctionStmt(TestCase):
    '''Function statements'''
    def test0(self):
        '''function statement with no arguments'''
        s = 'function foo()'
        r = FunctionStmt(None,'foo',[],None)
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test1(self):
        '''function statement with no type'''
        s = 'function foo(x)'
        r = FunctionStmt(None,'foo',['x'],None)
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test2(self):
        '''function statement with type real (no modifier)'''
        s = 'real function foo(x)'
        r = FunctionStmt((RealStmt,[]),'foo',['x'],None)
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test3(self):
        '''function statement with type real (with modifier)'''
        s = 'real(kind = 16) function foo(x)'
        r = FunctionStmt((RealStmt,[_ExplKind('16')]),'foo',['x'],None)
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test4(self):
        '''function statement with result'''
        s = 'function foo(x) result(y)'
        r = FunctionStmt(None,'foo',['x'],'y')
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))
        
    def test5(self):
        '''function statement with type real (with modifier) and result specifier'''
        s = 'real(kind = 16) function foo(x) result(y)'
        r = FunctionStmt((RealStmt,[_ExplKind('16')]),'foo',['x'],'y')
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

class TestSelectCaseStmt(TestCase):
    '''select case statements'''
    def test0(self):
        '''select case statement with space'''
        s = 'select case (i)'
        r = SelectCaseStmt('i')
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test1(self):
        '''select case statement without space'''
        s = 'selectcase (i)'
        r = SelectCaseStmt('i')
        self.assertEquals(repr(pps(s)),repr(r))
#       self.assertEquals(s,str(r))

class TestCaseStmts(TestCase):
    '''case statements'''
    def test0(self):
        '''case default statement'''
        s = 'case default'
        r = CaseDefaultStmt()
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test1(self):
        '''case with single range'''
        s = 'case (1:3)'
        r = CaseRangeListStmt([Ops(':','1','3')])
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

    def test2(self):
        '''case with multiple ranges'''
        s = 'case (1:3,8:9)'
        r = CaseRangeListStmt([Ops(':','1','3'), Ops(':','8','9')])
        self.assertEquals(repr(pps(s)),repr(r))
        self.assertEquals(s,str(r))

class TestInterfaces(TestCase):
    '''stuff to do with interfaces'''

    def test1(self):
        'empty interface definition'
        ae = self.assertEquals
        a_ = self.assert_
        s = 'interface'
        ps = pps(s)
        a_(isinstance(ps,InterfaceStmt),'instance check')
        ae(str(ps),s)

    def test2(self):
        'nonempty interface definition'
        ae = self.assertEquals
        a_ = self.assert_
        s = 'interface foo'
        ps = pps(s)
        a_(isinstance(ps,InterfaceStmt),'instance check')
        ae(str(ps),s)

class TestUseStmts(TestCase):
    '''various forms of use statements, including use only and renames'''

    def test1(self):
        'plain use statement'
        theString = 'uSe s_lib'
        theRepr = UseAllStmt('s_lib',None,'uSe')
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

    def test2(self):
        'use statement with renames'
        theString = 'USE S_LIB, pressure=>x_pres,altiude=>x_alt'
        theRepr = UseAllStmt('S_LIB',
                             [_PointerInit('pressure','x_pres'), _PointerInit('altiude','x_alt')],
                             'USE')
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

    def test3(self):
        'use statement with only list'
        theString = 'use data_C, only: xn,xj,nthr,eb_grp,art'
        theRepr = UseOnlyStmt('data_C',['xn', 'xj', 'nthr', 'eb_grp', 'art'],
                              'use')
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

    def test4(self):
        'use statement with only list including renames'
        theString = 'use months, only: january=>jan,may,june=>jun'
        theRepr = UseOnlyStmt('months',
                              [_PointerInit('january','jan'), 'may', _PointerInit('june','jun')],
                              'use')
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

class TestPointerAssignStmt(TestCase):
    '''pointer assignment statements'''

    def test0(self):
        '''simple pointer assignment'''
        theString = 'a => b'
        theRepr = PointerAssignStmt('a','b')
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

    def test1(self):
        '''more complicated pointer assignment'''
        theString = 'xjtn => xj(:,:,iftg:)'
        theRepr = PointerAssignStmt('xjtn',App('xj',[Zslice(), Zslice(), Lslice('iftg')]))
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

class TestWhereStmt(TestCase):
    '''where statements'''

    def test0(self):
        '''simple where statement'''
        theString = 'where (1.0<=0.0) a(1:2) = b(1:2)'
        theRepr = WhereStmt(Ops('<=','1.0','0.0'),
                            AssignStmt(App('a',[Ops(':','1','2')]),
                                       App('b',[Ops(':','1','2')])))
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

    def test1(self):
        '''complicated where statement from SCALE'''
        theString = 'where (wd<=0.0) btn(:mm,ig) = (xntn(1,ig)/xnto(1,ig))*bto(:mm,ig)'
        theRepr = WhereStmt(Ops('<=','wd','0.0'),
                            AssignStmt(App('btn',[Rslice('mm'), 'ig']),
                            Ops('*',
                                ParenExp(Ops('/',
                                         App('xntn',['1', 'ig']),
                                         App('xnto',['1', 'ig']))),
                                App('bto',[Rslice('mm'), 'ig']))))
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

class TestIntegerStmt(TestCase):
    '''where statements'''

    def test0(self):
        '''integer array decl'''
        theString = 'integer :: NA(6)'
        theRepr = IntegerStmt([],[],[_NoInit(App('NA',['6']))])
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

    def test1(self):
        '''integer decl with dimension attr'''
        theString = 'integer,DIMENSION(6) :: NA'
        theRepr = IntegerStmt([],
                              [App('DIMENSION',['6'])],
                              [_NoInit('NA')])
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

class TestAllocateDeallocateStmts(TestCase):
    '''allocate and deallocate statementswhere statements'''
#   def test0(self):
#       'simple allocate stmt'
#       theString = 'allocate(a(2))'
#       theRepr = AllocateStmt([App('a',['2'])])
#       self.assertEquals(repr(pps(theString)),repr(theRepr))
#       self.assertEquals(theString,str(theRepr))

#   def test1(self):
#       'complicated allocate stmt from SCALE'
#       theString = 'allocate(aa(ip),at(im,30),ca(im),ch(im,isct),ct(im),cs(im),da(im,mm),db(im,mm),dc(im),ds(ip,mm),qg(igp),rav(im),sa(im,jt),sat(im,jt),sr(im),st(im),v(im),xna(im,jt),xnd(ip,mm),xne(im),xni(im,jt+1),xnn(im),xnr(im),abar(izm),rinnr(izm),rbar(izm),zon_vol(izm))'
#       theRepr = AllocateStmt([App('aa',['ip']), App('at',['im', '30']), App('ca',['im']), App('ch',['im', 'isct']), App('ct',['im']), App('cs',['im']), App('da',['im', 'mm']), App('db',['im', 'mm']), App('dc',['im']), App('ds',['ip', 'mm']), App('qg',['igp']), App('rav',['im']), App('sa',['im', 'jt']), App('sat',['im', 'jt']), App('sr',['im']), App('st',['im']), App('v',['im']), App('xna',['im', 'jt']), App('xnd',['ip', 'mm']), App('xne',['im']), App('xni',['im', Ops('+','jt','1')]), App('xnn',['im']), App('xnr',['im']), App('abar',['izm']), App('rinnr',['izm']), App('rbar',['izm']), App('zon_vol',['izm'])])
#       self.assertEquals(repr(pps(theString)),repr(theRepr))
#       self.assertEquals(theString,str(theRepr))

#   def test2(self):
#       'simple deallocate stmt'
#       theString = 'deallocate(a)'
#       theRepr = DeallocateStmt(['a'])
#       self.assertEquals(repr(pps(theString)),repr(theRepr))
#       self.assertEquals(theString,str(theRepr))

class TestGotoStmt(TestCase):
    '''goto statements'''

    def test0(self):
        '''goto without space'''
        theString = 'GoTo 100'
        theRepr = GotoStmt('100',gotoFormatStr='GoTo')
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

    def test1(self):
        '''goto with space'''
        theString = 'Go to 100'
        theRepr = GotoStmt('100',gotoFormatStr='Go to')
        self.assertEquals(repr(pps(theString)),repr(theRepr))
        self.assertEquals(theString,str(theRepr))

suite = asuite(C1,C2,C3,C4,C5,C6,C8,C9,C10,TestCharacterDecls,
                                           TestImplicitStmt,
                                           TestDimensionStmt,
                                           TestDoStmt,
                                           TestWhileStmt,
                                           TestCallStmt,
                                           TestFunctionStmt,
                                           TestSelectCaseStmt,
                                           TestCaseStmts,
                                           TestUseStmts,
                                           TestPointerAssignStmt,
                                           TestWhereStmt,
                                           TestIntegerStmt,
                                           TestAllocateDeallocateStmts,
                                           TestGotoStmt,
              )

if __name__ == '__main__':
    runit(suite)
