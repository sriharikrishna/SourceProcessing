'''
Expression parser for fortran expressions:
  use rec decent
'''
from _Setup import *
from PyUtil.l_assembler import *
import fortScan  as fs

import re
from PyUtil.flatten import flatten
from op_prec import OpPrec
from PyIR.mutable_tree import _Mutable_T

_id_re     = re.compile(fs.id_re,re.I | re.X)
_flonum_re = re.compile(fs.flonum_re,re.I | re.X)
_int_re    = re.compile(fs.int_re,re.I | re.X)
_q_re      = re.compile(fs.q_re,re.I | re.X)
_qq_re     = re.compile(fs.qq_re,re.I | re.X)
_num_re    = re.compile(fs.flonum_re + '|' + fs.int_re,re.I | re.X)

_quote_set   = set(['"',"'"])
_logicon_set = set(['.true.','.false.'])
_optbl       = (([':']     ,1),
                (['.eqv.',
                  '.neqv.'],2),
                (['.or.']  ,3),
                (['.and.'] ,4),
                (['.le.',
                  '.lt.',
                  '.eq.',
                  '.ne.',
                  '.ge.',
                  '.gt.',
                  '<',
                  '<=',
                  '>',
                  '>=',
                  '==',
                  '/=']   ,5),
                (['//']   ,6),
                (['+','-'],7),
                (['*','/'],8),
                (['**']   ,9),
                )

_unary_ops   = ['-',
                '+',
                '.not.',
                ]

def is_op(op):
    '''
    Check to see if op is in the _optbl

    This routine is only necessary for
    unit test to check if all ops are covered
    '''
    op_l = op.lower()
    for (lst,prec) in _optbl:
        if op_l in lst:
            return True
    return False

class _Exp(_Mutable_T):
    'base class for Expression trees'
    _sons = []
    pass

class App(_Exp):
    'application expressions (ie f(x))'
    _sons = ['head','args']
    def __init__(self,head,args):
        self.head = head
        self.args = args

    def __repr__(self):
        return 'App(%s,%s)' % (repr(self.head),repr(self.args))

    def __str__(self):
        return '%s(%s)' % (str(self.head),
                           ','.join([str(l) for l in self.args]))
    def map(self,fn):
        return App(self.head,[fn(a) for a in self.args])

    def typeit(self,exptype,idchk,kw2type,lenfn,kindfn,poly,typemerge):
        headtype = idchk(self.head)
        if poly(self.head):
            return typemerge([exptype(l,idchk,kw2type,lenfn,kindfn,poly,typemerge) \
                              for l in self.args],
                             headtype)
        return headtype

class Sel(_Exp):
    'selection expressions like foo(i,j)%k'

    _sons = ['head']
    
    def __init__(self,head,proj):
        self.head = head
        self.proj = proj

    def __repr__(self):
        return 'Sel(%s,%s)' % (repr(self.head),repr(self.proj))

    def __str__(self):
        return '%s%%%s' % (str(self.head),str(self.proj))

    def map(self,fn):
        return Sel(fn(self.head),fn(self.proj))

class _AtomH(object):
    'helper class, captures the args of app or selection'

    def __init__(self,arglist):
        self.arglist = arglist

    def make(self):
        return (self.__class__.constr,self.arglist)

class _AppH(_AtomH):
    constr = App

class _SelH(_AtomH):
    constr = Sel

class Rslice(_Exp):

    _sons = ['arg']

    def __init__(self,e):
        self.arg = e
    def __repr__(self):
        return 'Rslice(%s)' % repr(self.arg)
    def __str__(self):
        return ':%s' % str(self.arg)

class Lslice(_Exp):

    _sons = ['arg']

    def __init__(self,e):
        self.arg = e
    def __repr__(self):
        return 'Lslice(%s)' % repr(self.arg)
    def __str__(self):
        return '%s:' % str(self.arg)

class Zslice(_Exp):
    def __init__(self): pass
    def __repr__(self):
        return 'Zslice()'
    def __str__(self):
        return ':'

class Unary(_Exp):
    _sons = ['exp']

    def typeit(self,exptype,idchk,kw2type,lenfn,kindfn,poly,typemerge):
        return exptype(self.exp,idchk,kw2type,lenfn,kindfn,poly,typemerge)
    
class Umi(Unary):
    'unary minus'
    def __init__(self,exp):
        self.exp = exp

    def __repr__(self):
        return 'Umi(%s)' % (repr(self.exp),)

    def __str__(self):
        return '-%s' % (str(self.exp),)

    def map(self,fn):
        return Umi(fn(self.exp))

class Upl(Unary):
    'unary plus'
    def __init__(self,exp):
        self.exp = exp

    def __repr__(self):
        return 'Upl(%s)' % (repr(self.exp),)

    def __str__(self):
        return '+%s' % (str(self.exp),)

    def map(self,fn):
        return Upl(fn(self.exp))

class Not(Unary):
    'unary not'

    def __init__(self,exp):
        self.exp = exp

    def __repr__(self):
        return 'Not(%s)' % (repr(self.exp),)

    def __str__(self):
        return '.not. %s' % (str(self.exp),)

    def map(self,fn):
        return Not(fn(self.exp))

class ParenExp(Unary):
    'parenthesized expression'
    def __init__(self,exp):
        self.exp = exp

    def __repr__(self):
        return 'ParenExp(%s)' % (repr(self.exp),)

    def __str__(self):
        return '(%s)' % (str(self.exp),)

    def map(self,fn):
        return ParenExp(fn(self.exp))

class Ops(_Exp):
    'some sequence of binops'
    _sons = ['a1','a2']

    def __init__(self,op,a1,a2):
        self.op = op.lower()
        self.a1 = a1
        self.a2 = a2
    
    def __repr__(self):
        return 'Ops(%s,%s,%s)' % (repr(self.op),repr(self.a1),repr(self.a2),)

    def __str__(self):
        return '%s %s %s' % (str(self.a1),
                             str(self.op),
                             str(self.a2))
    def map(self,fn):
        return Ops(self.op,fn(self.a1),fn(self.a2))

    def typeit(self,exptype,idchk,kw2type,lenfn,kindfn,poly,typemerge):
        return typemerge([exptype(self.a1,idchk,kw2type,lenfn,kindfn,
                                  poly,typemerge),
                          exptype(self.a2,idchk,kw2type,lenfn,kindfn,
                                  poly,typemerge)],'????')

def is_id(t):
    return _id_re.match(t)

def is_const(t):
    t1 = t.lower()
    return t1[0] in _quote_set or t1 in _logicon_set or _num_re.match(t1)

def _lc_const(t):
    t1 = t.lower()
    if t1 in _logicon_set:
        return t1
    return t

def is_unary(t):
    t1 = t.lower()
    return t1 in _unary_ops

def _squeeze(exp_list):
    ''' turn an expression list into an actual list
    with no embedded commas or lists
    '''
    (e1,rest) = exp_list
    return [e1] + [e2 for (dc,e2) in rest]

def _mkapp_e_r(a):
    'return either an Rslice or a Zslice object'
    (colon,lst) = a
    if lst:
        return Rslice(lst[0])
    else:
        return Zslice()

def _mkapp_e_l(a):
    'return either an Lslice or an unadorned Exp'
    (e,lst) = a
    if lst:
        return Lslice(e)
    else:
        return e


def _mkapp1(a):
    '''turn a recognized app into an App object'''
    (h,dc1,el,dc2) = a
    return App(h,_squeeze(el))

def _mkapp0(a):
    '''turn a recognized app with empty arglist into an App object'''
    (h,dc1,dc2) = a
    return App(h,[])

def _mkapp_ch(a):
    'turn a chain of apps into a single app'
    (h,lst) = a
    for l in lst:
        (cls,arg) = l.make()
        h = cls(h,arg)
    return h

def _mksel(a):
    'turn recognized selection into a Sel object'
    (h,dc,p) = a
    return Sel(h,p)

def _mkumi(um):
    'turn a recognized unary minus into a Umi object'
    (dc,e) = um
    return Umi(e)

def _mkupl(um):
    'turn a recognized unary plus into a Upl object'
    (dc,e) = upl
    return Upl(e)

def _mkunot(unot):
    (dc,e) = unot
    return Not(e)

_unary_tbl = {'+'    : Upl,
              '-'    : Umi,
              '.not.': Not,
              }

def _mkunary(e):
    (op,ee) = e
    return _unary_tbl[op.lower()](ee)

def _mkparen(parn):
    'turn a recognized parenthesized expression into a paren object'
    (dc,e,dc2) = parn
    return ParenExp(e)

def _mkexp(e):
    'convert a recognized exp into an Ops object'
    (a,oplst) = e
    for (op,a2) in oplst:
        a = Ops(op,a,_mkexp(a2))
    return a

id      = pred(is_id)
const   = pred(is_const)
const   = treat(const,_lc_const)
unary   = pred(is_unary)


def atom0(scan):
    '''eta expansion, since letrec unavail in python'''
#    return disj(app,id,const,unaryExp,paren)(scan)
    return disj(id,const,unaryExp,paren)(scan)


def atom(scan):

    _app1_trail = seq(lit('('),cslist(_appExp),lit(')'))
    _app1_trail = treat(_app1_trail,lambda a: _AppH(a[1]))

    _app0_trail = seq(lit('('),lit(')'))
    _app0_trail = treat(_app0_trail,lambda a: _AppH([]))

    _sel_trail  = seq(lit('%'),id)
    _sel_trail  = treat(_sel_trail,lambda a: _SelH(a[1]))

    _app_trail  = disj(_app1_trail,_app0_trail,_sel_trail)

    p0 = seq(atom0,star(_app_trail))
    p0 = treat(p0,_mkapp_ch)

    return p0(scan)

'''
FIXME:
   duplicated *_trail code, can't factor out easily due to fwd ref
'''

def lv_exp(scan):
    'proper syntax for an lvalue'

    _app1_trail = seq(lit('('),cslist(_appExp),lit(')'))
    _app1_trail = treat(_app1_trail,lambda a: _AppH(a[1]))

    _app0_trail = seq(lit('('),lit(')'))
    _app0_trail = treat(_app0_trail,lambda a: _AppH([]))

    _sel_trail  = seq(lit('%'),id)
    _sel_trail  = treat(_sel_trail,lambda a: _SelH(a[1]))

    _app_trail  = disj(_app1_trail,_app0_trail,_sel_trail)

    p0 = seq(id,star(_app_trail))
    p0 = treat(p0,_mkapp_ch)

    return p0(scan)

Exp = OpPrec(atom,_optbl,('**',))
Exp = treat(Exp,_mkexp)

ExpList   = seq(Exp,star(seq(lit(','),Exp)))
ExpList_l = treat(ExpList,_squeeze)

_appExpR = seq(lit(':'),zo1(Exp))
_appExpR = treat(_appExpR,_mkapp_e_r)

_appExpL = seq(Exp,zo1(lit(':')))
_appExpL = treat(_appExpL,_mkapp_e_l)

_appExp = disj(_appExpR,_appExpL)

app1      = seq(id,lit('('),ExpList,lit(')'))
app1      = treat(app1,_mkapp1)

app0      = seq(id,lit('('),lit(')'))
app0      = treat(app0,_mkapp0)

app       = disj(app1,app0)

unaryExp  = seq(unary,atom)
unaryExp  = treat(unaryExp,_mkunary)

paren     = seq(lit('('),Exp,lit(')'))
paren     = treat(paren,_mkparen)

# utility list
#
app_id    = disj(app,id)
app_id_l  = seq(app_id,star(seq(lit(','),app_id)))
app_id_l  = treat(app_id_l,_squeeze)

#
# simple subtree replacement function
#
def subst(a,pat,repl):
    if pat(a):
        return repl(a)
    elif isinstance(a,str):
        return a
    else:
        return a.map(lambda x:subst(x,pat,repl))

def const_type(e,kw2type,lenfn,kindfn):
    kind_re = re.compile(r'_(\w+)')
    if _flonum_re.match(e):
        sep_re = re.compile(r'([^_]+)(_(\w+))?')
        v      = sep_re.match(e)
        ty     = 'd' in v.group(1).lower() and 'doubleprecision' or \
                 'real'
        ty     = kw2type(ty)
        kind   = v.group(2) and [ kindfn(v.group(3))] or []
        return (ty,kind)
    if _int_re.match(e):
        ty   = kw2type('integer')
        kind = kind_re.search(e)
        kind = kind and [ kindfn(kind.group(1)) ] or []
        return (ty,kind)
    if e.lower() in _logicon_set:
        return (kw2type('logical'),[])
    if e[0] in _quote_set:
        return (kw2type('character'),[lenfn(len(e)-2)])

def exptype(e,idchk,kw2type,lenfn,kindfn,poly,typemerge):
    if isinstance(e,str) and is_const(e):
        return const_type(e,kw2type,kindfn,lenfn)
    if isinstance(e,str) and _id_re.match(e):
        return idchk(e)
    return e.typeit(exptype,idchk,kw2type,lenfn,kindfn,poly,typemerge)
