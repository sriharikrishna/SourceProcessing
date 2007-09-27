from Setup    import *
from _Setup   import *
from unittest import *
from useparse import *

from fortScan import scan1
from fortExp  import *
from fortExp  import _optbl
from fortExp  import _unary_ops

#def scan(s):
#    (v,rst) = scan1.scan(s)
#    return v

class C1(TestCase):
    def test01(self):
        'simple function app'

        ae = self.assertEquals
        a_ = self.assert_

        l1  = scan('f(x)')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),repr(App('f',['x'])))

    def test02(self):
        'function app with embedded function app'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('h(x,zzz(a,bbb),c)')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),repr(App('h',['x',App('zzz',['a','bbb']),'c'])))

    def test03(self):
        'simple constant'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('3.')
        (e1,dc)  = Exp(l1)
        ae(e1,'3.')

    def test04(self):
        'simple variable'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('x')
        (e1,dc)  = Exp(l1)
        ae(e1,'x')

    def test05(self):
        'simple no arg app'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('hfffhfhg_kk()')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),repr(App('hfffhfhg_kk',[])))

    def test06(self):
        'embedded apps, string constants, and no arg apps'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('g(hh(x0,x1),"Str",kk(1,xx("aa")),3,f())')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),
           repr(App('g',[App('hh',['x0', 'x1']),
                         '"Str"',
                         App('kk',['1',App('xx',['"aa"'])]),
                         '3',
                         App('f',[])]))
           )

    def test07(self):
        "embedded app leaves extra ')' at end"

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('h(x,zzz(a,bbb),c))')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),
           repr(App('h',['x',App('zzz',['a', 'bbb']), 'c']))
           )
        ae(dc,[')'])

    def test08(self):
        'simple unary minus inside app'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('h(-x,zzz)')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),
           repr(App('h',[Umi('x'), 'zzz']))
           )

    def test09(self):
        'app w embedded parenthesized expressions'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('hhh((kkk),(h3(x,y)))')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),
           repr(App('hhh',[ParenExp('kkk'),
                           ParenExp(App('h3',['x', 'y']))]))
           )

    def test10(self):
        'simple unary minus of a fp constant'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('-3.14')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),
           repr(Umi('3.14'))
           )

    def test11(self):
        'nested paren expressions, unary minus'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('-(h6(xxx,-y,(-2.2)))')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),
           repr(Umi(ParenExp(App('h6',['xxx',Umi('y'),
                                        ParenExp(Umi('2.2'))]))))
           )

    def test12(self):
        'constant w trailing - & paren'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('12.3-)')
        (e1,dc)  = Exp(l1)
        ae(e1,'12.3')
        ae(dc,['-',')'])

    def test13(self):
        'app w logical constant'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('f((.TRUE.),-5.2E22_w2f)')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),
           repr(App('f',[ParenExp('.true.'),Umi('5.2E22_w2f')]))
           )

    def test14(self):
        'simple arithmetic ops'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('h(x,y,z)-kkk')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),
           repr(Ops('-',App('h',['x', 'y', 'z']), 'kkk'))
           )

    def test15(self):
        'embedded arith ops as args to app'

        ae = self.assertEquals
        a_ = self.assert_
        l1  = scan('h(x*y,y:y+1,-zzz) * -hgh(2.1)')
        (e1,dc)  = Exp(l1)
        ae(repr(e1),
           repr(Ops('*',
                    App('h',[Ops('*','x','y'),
                             Ops(':','y',Ops('+','y','1')),
                             Umi('zzz')]),
                    Umi(App('hgh',['2.1']))))
           )

    def test16(self):
        'unary .not. handled'
        ae = self.assertEquals
        a_ = self.assert_

        l1      = scan('IF((K .GE. 2) .AND.(CALCADVECTION .AND.' + \
                       '(.NOT. IMPLICITADVECTION))) THEN')
        (e1,dc) = Exp(l1)
        ae(repr(e1),
           repr(App('IF',[Ops('.AND.',
                              ParenExp(Ops('.GE.','K','2')),
                              ParenExp(Ops('.AND.',
                                           'CALCADVECTION',
                                           ParenExp(Not('IMPLICITADVECTION'))
                                           ))
                              )]))
           )

    def test17(self):
        'unary + handled'
        ae = self.assertEquals
        a_ = self.assert_

        l1      = scan('+foo - bar * 3.14_w2f')
        (e1,dc) = Exp(l1)
        ae(repr(e1),
           repr(Ops('-',Upl('foo'),Ops('*','bar','3.14_w2f')))
           )

    def test18(self):
        'multiple apps'
        ae = self.assertEquals
        a_ = self.assert_

        l1      = 'foo(ii,kk)(j,k)()'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(App(App(App('foo',['ii','kk']),['j','k']),[])))
        ae(rm,[])

        l1      = 'foo(j,k)(3:xx+7)'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(App(App('foo',['j','k']),[Ops(':','3',Ops('+','xx','7'))])))
        ae(rm,[])

    def test19(self):
        'selection op,'
        ae = self.assertEquals
        a_ = self.assert_

        l1      = 'foo(ij,jk)%l'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(Sel(App('foo',['ij','jk']),'l')))
        ae(rm,[])

        l1      = 'foo(ij,jk)%l%mm%kkk'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(Sel(Sel(Sel(App('foo',['ij','jk']),'l'),'mm'),'kkk')))
        ae(rm,[])

        l1      = 'foo(ij,jk)%l%mm%kkk(xx+yy,ifoo,j)'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(App(Sel(Sel(Sel(App('foo',['ij','jk']),'l'),
                            'mm'),
                        'kkk'),
                    [Ops('+','xx','yy'), 'ifoo', 'j'])))
        ae(rm,[])

    def test20(self):
        'slicing options'
        ae = self.assertEquals
        a_ = self.assert_

        l1      = 's(3,foo(ii,jj)+10)'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(App('s',['3', Ops('+',App('foo',['ii', 'jj']),'10')])))
        ae(rm,[])

        l1      = 's(foo(ii,jj)+10 :)'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(App('s',[Lslice(Ops('+',App('foo',['ii', 'jj']),'10'))]))
           )
        ae(rm,[])

        l1      = 'xx(7,:foo(ii,jj)+10 ** 2)'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(App('xx',['7',
                          Rslice(Ops('+',App('foo',['ii','jj']),
                                     Ops('**','10','2')))]))
           )
        ae(rm,[])

        l1      = 't(:)'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(App('t',[Zslice()])))
        ae(rm,[])

        l1      = 't(a:b,f(x)%l : f(x)%u)'
        l1      = scan(l1)
        (e1,rm) = Exp(l1)
        ae(repr(e1),
           repr(App('t',[Ops(':','a','b'),
                         Ops(':',Sel(App('f',['x']),'l'),
                             Sel(App('f',['x']),'u'))])))
        ae(rm,[])

    def test21(self):
        ".eqv. and .neqv expressions"
        ae = self.assertEquals
        a_ = self.assert_

        l1      = scan('xxx .neqv. (yyy .eqv. zzz)')
        (e1,rm) = Exp(l1)

        ae(repr(e1),
           repr(Ops('.neqv.','xxx',ParenExp(Ops('.eqv.','yyy','zzz'))))
           )
        a_(not rm)

    def test22(self):
        'lvalue syntax'
        ae = self.assertEquals
        a_ = self.assert_

        (v,r) = lv_exp(scan('x%v'))
        ae(repr(v),
           repr(Sel('x','v')))
        a_(not r)

class C2(TestCase):
    'Test for all ops being covered'

    def test1(self):
        'all ops covered'
        ae = self.assertEquals
        a_ = self.assert_

        for op in list('+-*/:'):
            a_(is_op(op))

        for op in ('**','//',):
            a_(is_op(op))

        for op in [ '.' + t + '.' for t in ['and','or','lt','le',
                                            'gt','ge','eq','ne',
                                            'eqv','neqv']]:
            a_(is_op(op))

        for op in '< <= > >= == /='.split(): a_(is_op(op))

    def test2(self):
        'all unary ops covered'
        ae = self.assertEquals
        a_ = self.assert_

        for op in ['+','-','.not.']:
            a_(is_unary(op))

class C3(TestCase):
    'check that Exp backtrack works correctly for data stmts'

    def test1(self):
        'data statement backtrack'
        ae = self.assertEquals
        a_ = self.assert_

        s = 'DATA(A1(tmp0), tmp0 = 1, 5, 1) / 3.49999994039535522461D-01,' + \
            ' 6.00000023841857910156D-01, 1.0D00, 1.5D00, ' + \
            '1.39999997615814208984D00 /'

        (e1,dc) = Exp(scan(s))
        ae(e1,'DATA')
        ae(dc,['(', 'A1', '(', 'tmp0', ')',
               ',', 'tmp0', '=', '1', ',', '5', ',', '1', ')',
               '/', '3.49999994039535522461D-01', ',',
               '6.00000023841857910156D-01', ',', '1.0D00', ',',
               '1.5D00', ',', '1.39999997615814208984D00', '/'])

class C4(TestCase):
    'test binary operations from _optbl'
    pass

class C5(TestCase):
    'test unary operations from _unary_ops'
    pass

# support fns for typing

def kw2type(t): return t
def lenfn(n): return 'Len(%s)' % str(n)
def kindfn(s): return 'Kind(%s)' % str(s)
def nopoly(s): return False
def idchk(id):
    tbl = dict(x=('real',[]),
               y=('real',[]),
               z=('doubleprecision',[]),
               i=('integer',[]),
               l=('logical',[])
               )
    return tbl[id]

def tyc(t1,t2):
    code = dict(logical=0,
                integer=1,
                real   =2,
                doubleprecision=3,
                )
    if code[t1[0]] >= code[t2[0]]: return t1
    return t2

def typemerge(t1,oth):
    if not t1: return oth
    if len(t1) == 1: return t1[0]
    tt = tyc(t1[0],t1[1])
    for t in t1[2:]:
        tt = tyc(tt,t)
    return tt

class C6(TestCase):
    'test various type utilities'

    def test1(self):
        'test subst function'
        ae = self.assertEquals
        a_ = self.assert_

        def pred(e):
            return isinstance(e,App) and e.head == 'foo'

        def repl(e):
            return App('bar',e.args)

        e = ep('x * baz(123,.TRUE.,7+foo(1,1))')
        ae(repr(subst(e,pred,repl)),repr(ep('x * baz(123,.TRUE.,7+bar(1,1))')))

    def test2(self):
        'constant types'
        ae = self.assertEquals
        a_ = self.assert_

        ae(const_type(ep('3.787'),kw2type,lenfn,kindfn),('real',[]))
        ae(const_type(ep('3.787D00'),kw2type,lenfn,kindfn),('doubleprecision',[]))
        ae(const_type(ep('3.787_foo'),kw2type,lenfn,kindfn),('real',['Kind(foo)']))
        ae(const_type(ep('3'),kw2type,lenfn,kindfn),('integer',[]))
        ae(const_type(ep('0_w2f__i8'),kw2type,lenfn,kindfn),('integer', ['Kind(w2f__i8)']))
        ae(const_type(ep('.true.'),kw2type,lenfn,kindfn),('logical',[]))
        ae(const_type(ep(r"'food'"),kw2type,lenfn,kindfn),('character','Len(4)'))

    def test3(self):
        'exptype function'
        ae = self.assertEquals
        a_ = self.assert_
        e1 = ep('x * y')
        t1 = exptype(e1,idchk,kw2type,lenfn,kindfn,nopoly,typemerge)
        ae(t1,('real',[]))

        e1 = ep('5.11d0 * 4.77d0')
        t1 = exptype(e1,idchk,kw2type,lenfn,kindfn,nopoly,typemerge)
        ae(t1,('doubleprecision',[]))

        e1 = ep('i + 4')
        t1 = exptype(e1,idchk,kw2type,lenfn,kindfn,nopoly,typemerge)
        ae(t1,('integer',[]))

        e1 = ep('z + 5.11d0 * 4.77d0')
        t1 = exptype(e1,idchk,kw2type,lenfn,kindfn,nopoly,typemerge)
        ae(t1,('doubleprecision',[]))

        e1 = ep('x * 5.11d0 + i * 4.77')
        t1 = exptype(e1,idchk,kw2type,lenfn,kindfn,nopoly,typemerge)
        ae(t1,('doubleprecision',[]))

def __mkt2(o):
    def __t(self):
        v = ep('xxx %s yyy' % o)
        self.assertEquals(repr(v),
                          repr(Ops(o,'xxx','yyy')))
    __t.__doc__ = "op '%s' parses" % o
    return __t

__c = 1
for o in [x for (op,p) in _optbl for x in op]:
    setattr(C4,'test%d' % __c,__mkt2(o))
    __c += 1

s1    = makeSuite(C6)

suite = asuite(C1,C2,C3,C4,C6)

if __name__ == '__main__':
    runit(suite)
