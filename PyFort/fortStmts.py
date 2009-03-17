#### FIXME: functions returning derived types !!! ###
#### function parsing in general: distinguish 'real function' (a var decl) from 'real function foo()'

'''
Various Fortran statement types and Fortran parsing functions
'''

from _Setup import *

from PyIR.mapper       import _Mappable
from PyIR.mutable_tree import _Mutable_T

from PyUtil.chomp        import chomp
from PyUtil.errors  import ParseError
from PyUtil.l_assembler import AssemblerException as ListAssemblerException

from fortExp      import *
from fixedfmt     import fixedfmt
import flow

class __FakeUnit(object):
    def __init__(self):
        self._in_iface = False

_non = __FakeUnit()

class _TypeMod(_Mutable_T):
    'modifier for type declaration'
    _sons = ['mod']

    def __init__(self,e):
        self.mod = e

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,repr(self.mod),)

    def __str__(self):
        return self.pat % str(self.mod)

class _Star(_Mutable_T):
    'Utility modifier type for character data'
    _sons = []
    def __init__(self):
        pass
    def __repr__(self):
        return '_Star()'
    def __str__(self):
        return '(*)'

class _FLenMod(_Mutable_T):
    'generic fortran char len type'
    _sons = ['len']
    def __init__(self,len):
        self.len = len
    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,repr(self.len))
    def __str__(self):
        return self.pat_ % str(self.len)
    
class _F90ExplLen(_FLenMod):
    'utility modifier for explicit len in F90 char data'
    pat_ = '(len=%s)'

class _F90Len(_FLenMod):
    'Utility modifier for character data, F90 style'
    pat_ = '(%s)'

    def _separate_implicit_list(self):
        app = self.len
        return ([],app)

class _F77Len(_FLenMod):
    'Utility modifier for character data, F77 style'
    pat_ = '*%s'
    def _separate_implicit_list(self):
        app = self.len
        return ([_F77Len(app.head)],app.args)

class _Prec(_TypeMod):
    pat = '*%s'

    def _separate_implicit_list(self):
        app = self.mod
        return ([_Prec(app.head)],app.args)

class _Kind(_TypeMod):
    pat = '(%s)'

    def _separate_implicit_list(self):
        l = self.mod
        return ([],self.mod)


class _ExplKind(_TypeMod):
    pat = '(kind = %s)'

prec = seq(lit('*'),Exp)
prec = treat(prec,lambda a:_Prec(a[1]))

# kind = seq(lit('('),cslist(Exp),lit(')')) #### KILL THIS??
kind = seq(lit('('),Exp,lit(')'))
kind = treat(kind,lambda a:_Kind(a[1]))

explKind = seq(lit('('),lit('kind'),lit('='),Exp,lit(')'))
explKind = treat(explKind,lambda a:_ExplKind(a[3]))

starmod  = seq(lit('('),lit('*'),lit(')'))
starmod  = treat(starmod,lambda l: _Star())

lenmod   = disj(Exp,starmod)
f77mod   = seq(lit('*'),lenmod)
f77mod   = treat(f77mod,lambda l: _F77Len(l[1]))

f90mod   = seq(lit('('),disj(lit('*'),Exp),lit(')'))
f90mod   = treat(f90mod,lambda l: _F90Len(l[1]))

explLen  = seq(lit('('),lit('len'),lit('='),Exp,lit(')'))

id_l     = treat(id,str.lower)
_typeid  = disj(lit('real'),
                lit('integer'),
                lit('logical'),
                lit('complex'),
                lit('doubleprecision'),
                lit('doublecomplex'),
                )

_dblp2   = seq(lit('double'),lit('precision'))
_dblp2   = treat(_dblp2,lambda l:'doubleprecision')

_dblp1   = lit('doubleprecision')

_dblp    = disj(_dblp2,_dblp1)

pstd = seq(_typeid,
         zo1(disj(prec,kind,explKind)),
         )

pchar = seq(lit('character'),
         zo1(disj(f77mod,f90mod,explLen)),
         )

type_pat = disj(pchar,pstd)

def _get_class(p):
    return (kwtbl[p[0].lower()],p[1])

type_pat_sem = treat(type_pat,_get_class)

def _name2class(name): return kwtbl[name.lower()]

def _ta_listify(asm):
    rv = []
    for (dc,item) in asm:
        rv.append(item)
    return rv

type_attr_list = star(seq(lit(','),Exp))
type_attr_list = treat(type_attr_list,_ta_listify)

class _Init(object):
    'general f90 init object'

class _NoInit(_Init):
    'no initialization'
    def __init__(self,lhs): self.lhs = lhs
    def __repr__(self): return '_NoInit(%s)' % repr(self.lhs)
    def __str__(self): return str(self.lhs)

class _PointerInit(_Init):
    'pointer initialization'

    def __init__(self,lhs,rhs):
        self.lhs = lhs
        self.rhs = rhs
    def __repr__(self):
        return '_PointerInit(%s,%s)' % (repr(self.lhs),
                                        repr(self.rhs))
    def __str__(self):
        return '%s=>%s' % (str(self.lhs),
                             str(self.rhs))

class _AssignInit(_Init):
    'normal assignment-style initialization'
    def __init__(self,lhs,rhs):
        self.lhs = lhs
        self.rhs = rhs
    def __repr__(self):
        return '_AssignInit(%s,%s)' % (repr(self.lhs),
                                       repr(self.rhs))
    def __str__(self):
        return '%s=%s' % (str(self.lhs),
                             str(self.rhs))

def _handle_init(asm):

    (lhs,init) = asm
    if init:
        init = init[0]
        if init[0] == '=>':
            return _PointerInit(lhs,init[1])
        else:
            return _AssignInit(lhs,init[1])
    else:
        return _NoInit(lhs)

init_spec = zo1(seq(disj(lit('=>'),lit('=')),Exp))

star_range = seq(Exp,lit(':'),lit('*'))
star_range = treat(star_range,lambda x: Ops(':',x[0],'*'))

var_w_dim = seq(id,zo1(seq(lit('('),cslist(disj(star_range,lit('*'),lit(':'),Exp)),lit(')'))))
var_w_dim = treat(var_w_dim,lambda x: x[1] and App(x[0],x[1][0][1]) or x[0])

decl_item = seq(var_w_dim,init_spec)
decl_item = treat(decl_item,_handle_init)

char_override  = seq(lit('*'),Exp)

char_decl_     = seq(var_w_dim,zo1(char_override))
char_decl_     = treat(char_decl_,lambda x: x[1] and Ops('*',x[0],x[1][0][1]) or x[0])

char_decl_item = seq(char_decl_,init_spec)
char_decl_item = treat(char_decl_item,_handle_init)

def typestr(raw):
    (tyid,mod) = raw
    return kwtbl[tyid].kw_str + (mod and str(mod[0]) or '')

def typestr2(raw):
    (tycls,mod) = raw
    return tycls.kw_str + (mod and str(mod[0]) or '')

class GenStmt(_Mappable,_Mutable_T):
    _sons = []

    def __init__(self,scan,lineNumber=0,label=False,lead=''):
        self.scan = scan
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead


    @classmethod
    def parse(cls,scan,lineNumber):
        return cls(scan,lineNumber)

    def is_exec(self,unit=_non): return False
    def is_decl(self,unit=_non): return False
    def is_ustart(self,unit=_non): return False
    def is_uend(self,unit=_non): return False
    def is_contains(self,unit=_non): return False
    def is_comment(self,unit=_non): return False

class Skip(GenStmt):
    def __init__(self):
        self.scan = []

class Comments(GenStmt):
    def __init__(self,rawline,lineNumber=0):
        self.rawline = rawline
        self.lineNumber = lineNumber

    def __repr__(self):
        return 'Comments(blk)'

    def viz(self):
        return 'Comments(%s)' % self.rawline

    def is_comment(self,unit=_non): return True

def comment_bl(*comlines):
    return Comments('\n'.join(['c '+ chomp(s) for s in comlines])+'\n')

class NonComment(GenStmt):

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                           ','.join([repr(aSon) for aSon in self._sons]))

    def flow(self):
        init = self.label and ' ' + ('%-4d' % self.label) + ' ' \
                           or ''
        self.rawline = flow.flow_line(init + self.lead + str(self))+'\n'
        return self

    def reflow(self):
        'remove rawline, so that later processing will regen'
        if hasattr(self,'rawline'): del self.rawline
        
    def same_level(self,parsed):
        parsed.label = False
        parsed.lead    = self.lead
        return parsed.flow()

    def indent(self,n):
        self.lead += ' ' * n
        return self.flow()

    def basic_line(self):
        self.label = False
        self.lead   = ''
        self.flow()
        return self

    def same(self,targ):
        self.same_level(targ)
        targ.lineNumber = self.lineNumber
        targ.label = self.label
        targ.lead = self.lead
        return targ.flow()

    def clone_fmt(self,src):
        self.label = False
        self.lead   = src.lead
        self.flow()
        return self

class Decl(NonComment):
    def is_decl(self,unit=_non): return True

def attrstr(l):
    if not l: return ''
    return ','+ ','.join([str(ll) for ll in l])

def ditemstr(l):
    return ','.join([str(ll) for ll in l])

class TypeDecl(Decl):
    '''
    The double colon "::" is optional when parsing, but we have chosen to always include it when printing
    '''
    ## This is the first appearance of the kw and kw_str members; the kw is a string with no spaces (like 'doubleprecision').
    kw = '__unknown__'
    ## the kw_str may have spaces (like with 'double precision').
    kw_str = kw
    mod = None
    decls = []

    @classmethod
    def parse(cls,scan,lineNumber):
        p0 = seq(type_pat,
                 type_attr_list,
                 zo1(lit('::')),
                 cslist(decl_item))
        (v,r) = p0(scan)
        ((typ,mod),attrs,dc,decls) = v
        return cls(mod,attrs,decls,lineNumber)

    def __init__(self,mod,attrs,decls,lineNumber=0,label=False,lead=''):
        self.mod   = mod
        self.attrs = attrs
        self.decls = decls
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return '%s(%s,%s,%s)' % (self.__class__.__name__,
                                 repr(self.mod),
                                 repr(self.attrs),
                                 repr(self.decls))

    def __str__(self):
        modstr = ''
        if self.mod:
            modstr = str(self.mod[0])
        attr_str = ''
        if self.attrs:
            attr_str = ','+','.join([str(a) for a in self.attrs])
        return '%s%s%s :: %s' % (self.__class__.kw_str,
                                 modstr,
                                 attr_str,
                                 ','.join([str(d) for d in self.decls]))

class DrvdTypeDecl(TypeDecl):
    '''
    Derived type declarations are treated as declarations of type "type,"
     with a modifier that is the name of the type.
    '''
    _sons = ['attrs','decls']
    kw     = 'type'
    kw_str = kw

    @staticmethod
    def parse(scan,lineNumber):
        p0 = seq(lit('type'),
                 lit('('),
                 id,
                 lit(')'),
                 type_attr_list,
                 zo1(lit('::')),
                 cslist(decl_item))
        p0 = treat(p0,lambda l: DrvdTypeDecl([l[1]+l[2]+l[3]],l[4],l[6],lineNumber))
        (v,r) = p0(scan)
        return v

class DrvdTypeDefn(Decl):
    '''
    derived type definition (start)
    '''
    kw_str = 'derivedDefn'
    kw     = kw_str

    def __init__(self,name,lineNumber=0,label=False,lead=''):
        self.name = name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'DrvdTypeDefn(%s)' % repr(self.name)

    def __str__(self):
        return 'type %s' % str(self.name)
    
    @staticmethod
    def parse(scan,lineNumber):
        p0    = treat(seq(lit('type'),id),lambda l: DrvdTypeDefn(l[1],lineNumber))
        (v,r) = p0(scan)
        return v

class InterfaceStmt(Decl):
    kw = 'interface'
    kw_str = 'interface'

    def __init__(self,l,lineNumber=0,label=False,lead=''):
        self.name = l
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'InterfaceStmt(%s)' % self.name

    def __str__(self):
        if self.name:
            return 'interface %s' % self.name
        else:
            return 'interface'

    @staticmethod
    def parse(scan,lineNumber):
        formInterfaceStmt = seq(lit('interface'),
                                zo1(id))
        ((interfaceKeyword,interfaceName),rest) = formInterfaceStmt(scan)
        name = interfaceName and interfaceName[0] \
                              or None
        return InterfaceStmt(name,lineNumber)

class TypePseudoStmt(GenStmt):
    '''
    type keyword signals *either* declaration or definition
    '''
    @staticmethod
    def parse(scan,lineNumber):
        if scan[1] == '(': return DrvdTypeDecl.parse(scan,lineNumber)
        return DrvdTypeDefn.parse(scan,lineNumber)

class PUstart(Decl):
    def is_decl(self,unit=_non):  return True
    def is_ustart(self,unit=_non): return True

class Exec(NonComment):
    ''' base class for all executable statements'''
    def is_exec(self,unit=_non): return True

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                           ','.join([repr(getattr(self,aSon)) for aSon in self._sons]))

class Leaf(Exec):
    "special Exec that doesn't have components"

    def __init__(self,lineNumber=0,label=False,lead='',*dc):
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return '%s()' % self.__class__.__name__

    def __str__(self):
        return '%s' % self.__class__.kw

class DeclLeaf(Decl):
    "special Decl that has no components"

    @classmethod
    def parse(cls,scan,lineNumber):
        return cls(lineNumber)

    def __init__(self,lineNumber=0,label=False,lead='',*dc,**dc2):
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead
    def __repr__(self): return '%s()' % self.__class__.__name__
    def __str__(self): return '%s' % self.__class__.kw_str

class PUend(Leaf):
    pass

class IfPUstart(DeclLeaf):
    kw_str = '(If)prog_unit_start'

class IfPUend(DeclLeaf):
    kw_str = '(If)prog_unit_end'

class BlockdataStmt(PUstart):
    pass

class CommonStmt(Decl):
    pass

class DataStmt(Decl):
    pass

class EndInterfaceStmt(DeclLeaf):
    'End of interface block'
    kw    = 'endinterface'
    kw_str = 'end interface'

class EndTypeStmt(DeclLeaf):
    'end of a type definition'
    kw     = 'endtype'
    kw_str = 'end type'

class VarAttrib(Decl):
    _sons  = ['vlist']

    @classmethod
    def parse(cls,scan,lineNumber):
        p0 = seq(lit(cls.kw),zo1(seq(lit('::'),cslist(id))))

        ((dc,vlist),r) = p0(scan)
        if vlist:
            vlist = vlist[0][1]

        return cls(vlist,lineNumber)

    def __init__(self,vlist,lineNumber=0,label=False,lead=''):
        self.vlist = vlist
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, repr(self.vlist))

    def __str__(self):
        s_key = self.kw_str
        rem   = ''
        if self.vlist:
            rem = ' :: %s' % ','.join([str(v) for v in self.vlist])
        return s_key+rem

class PrivateStmt(VarAttrib):
    kw     = 'private'
    kw_str = kw

class PublicStmt(VarAttrib):
    kw     = 'public'
    kw_str = kw

class ContainsStmt(DeclLeaf):
    kw     = 'contains'
    kw_str = kw

    def is_contains(self,unit=_non): return True
    def is_decl(self,unit=_non): return False
    
class ImplicitNone(DeclLeaf):
    kw     = 'implicitnone'
    kw_str = 'implicit none'

def _extract_imp_elts(type_pair):
    '''hack to extract the list of implicit letters from
    the absorbed type modifier in the case of type*Exp or
    type(Explist)
    '''
    (cls,m) = type_pair
    m = m[0]
    (nmod,implst) = m._separate_implicit_list()
    return ((cls,nmod),implst)

impelt1 = seq(type_pat_sem,lit('('),cslist(Exp),lit(')'))
impelt1f = treat(impelt1,lambda l: (l[0],l[2]))

impelt2 = type_pat_sem
impelt2f = treat(impelt2,_extract_imp_elts)

impelt = disj(impelt1,impelt2)
impeltf = disj(impelt1f,impelt2f)

class ImplicitStmt(Decl):
    _sons = ['lst']

    @staticmethod
    def parse(scan,lineNumber):
        p0 = seq(lit('implicit'),lit('none'))
        p0 = treat(p0,ImplicitNone)

        impelt1 = seq(type_pat_sem,lit('('),cslist(Exp),lit(')'))
        impelt1 = treat(impelt1,lambda l: (l[0],l[2]))

        impelt2 = type_pat_sem
        impelt2 = treat(impelt2,_extract_imp_elts)

        impelt = disj(impelt1,impelt2)

        p1 = seq(lit('implicit'),
                 cslist(impelt))

        p1 = treat(p1,lambda l:ImplicitStmt(l[1],lineNumber))

        (v,r) = disj(p0,p1)(scan)

        return v

    def __init__(self,lst,lineNumber=0,label=False,lead=''):
        self.lst  = lst
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'ImplicitStmt(%s)' % repr(self.lst)

    def __str__(self):

        def _helper(elt):
            (typ,explst) = elt
            return '%s (%s)' % (typestr2(typ),
                                ','.join([str(l).replace(' ','') \
                                          for l in explst]))
            
        return 'implicit %s' % ', '.join([_helper(e) for e in self.lst])

class EquivalenceStmt(Decl):
    pass

class ParameterStmt(Decl):
    _sons = ['namedParamList']
    kw = 'parameter'
    kw_str = kw

    @staticmethod
    def parse(scan,lineNumber):
        aNamedParam = seq(id,lit('='),Exp)
        p0 = seq(lit('parameter'),
                 lit('('),
                 cslist(aNamedParam),
                 lit(')'))
        p0 = treat(p0,lambda l:ParameterStmt(l[2],lineNumber))
        (v,r) = p0(scan)
        return v
   
    def __init__(self,namedParamList,lineNumber=0,label=False,lead=''):
        self.namedParamList = namedParamList
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'ParamterStmt(%s)' % ','.join([repr(aNamedParam) for aNamedParam in self.namedParamList])

    def __str__(self):
        return 'parameter (%s)' % ','.join([str(aNamedParam) for aNamedParam in self.namedParamList])

class SaveStmt(Decl):
    pass

class StmtFnStmt(Decl):
    _sons = ['args','body']

    def __init__(self,name,args,body,lineNumber=0,label=False,lead=''):
        self.name = name
        self.args = args
        self.body = body
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'StmtFnStmt(%s,%s,%s)' % (repr(self.name),
                                         repr(self.args),
                                         repr(self.body))
    def __str__(self):
        return '%s(%s) = %s' % (str(self.name),
                                ','.join([str(l) for l in self.args]),
                                str(self.body))

class ExternalStmt(Decl):
    _sons = ['procedureNames']
    kw = 'external'
    kw_str = kw

    @staticmethod
    def parse(scan,lineNumber):
        formExternalStmt = seq(lit('external'),
                               zo1(lit('::')),
                               cslist(id))
        ((externalKeyword,doubleColon,procedureNames),rest) = formExternalStmt(scan)
        return ExternalStmt(procedureNames,lineNumber)

    def __init__(self,procedureNames,lineNumber=0,label=False,lead=''):
        self.procedureNames = procedureNames
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return self.__class__.__name__+'('+repr(self.procedureNames)+')'

    def __str__(self):
        return self.kw+' '+','.join([str(aProcedureName) for aProcedureName in self.procedureNames])

class AllocatableStmt(Decl):
    _sons = ['lst']
    kw = 'allocatable'
    kw_str = kw

    @staticmethod
    def parse(scan,lineNumber):
        formAllocatableStmt = seq(lit('allocatable'),
                               zo1(lit('::')),
                               cslist(id))
        ((allocatableKeyword,doubleColon,lst),rest) = formAllocatableStmt(scan)
        return AllocatableStmt(lst,lineNumber)

    def __init__(self,lst,lineNumber=0,label=False,lead=''):
        self.lst = lst
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return self.__class__.__name__+'('+repr(self.lst)+')'

    def __str__(self):
        return self.kw+' '+','.join([str(aName) for aName in self.lst])

class CharacterStmt(TypeDecl):
    kw = 'character'
    kw_str = kw
    _sons  = ['mod','attrs','decls']

    @staticmethod
    def parse(scan,lineNumber):
        starmod  = seq(lit('('),lit('*'),lit(')'))
        starmod  = treat(starmod,lambda l: _Star())

        lenmod   = disj(Exp,starmod)
        f77mod   = seq(lit('*'),lenmod)
        f77mod   = treat(f77mod,lambda l: _F77Len(l[1]))

        f90mod   = seq(lit('('),disj(lit('*'),Exp),lit(')'))
        f90mod   = treat(f90mod,lambda l: _F90Len(l[1]))

        explLen  = seq(lit('('),lit('len'),lit('='),Exp,lit(')'))
        explLen  = treat(explLen,lambda l: _F90ExplLen(l[3]))
                
        p1 = seq(lit('character'),
                 zo1(disj(f77mod,f90mod,explLen)),type_attr_list,zo1(lit('::')),
                 cslist(char_decl_item))
        try: 
          ((dc,mod,attrs,dc1,decls),rest) = p1(scan)
        except AssemblerException,e:
          raise ParseError(lineNumber,scan,'character variable declaration')  

        return CharacterStmt(mod,attrs,decls,dc,lineNumber)

    def __init__(self,mod,attrs,decls,stmt_name='character',lineNumber=0,label=False,lead=''):
        self.mod   = mod
        self.decls = decls
        self.attrs = attrs
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'CharacterStmt(%s,%s,%s)' % (repr(self.mod),repr(self.attrs),repr(self.decls))

    def __str__(self):
        modstr = ''
        if self.mod:
            modstr = str(self.mod[0])
        
        attr_str = ''
        if self.attrs:
            attr_str = ','+','.join([str(a) for a in self.attrs])
            
        return '%s%s%s :: %s' % (self.stmt_name,modstr,
                                 attr_str,
                                 ','.join([str(d) for d in self.decls]))

class IntrinsicStmt(Decl):
    pass

class IncludeStmt(Decl):
    pass

class RealStmt(TypeDecl):
    kw = 'real'
    kw_str = kw

class ComplexStmt(TypeDecl):
    kw = 'complex'
    kw_str = kw

class IntegerStmt(TypeDecl):
    kw = 'integer'
    kw_str = kw

class LogicalStmt(TypeDecl):
    kw = 'logical'
    kw_str = kw

class DoubleStmt(TypeDecl):
    kw     = 'doubleprecision'
    kw_str = 'double precision'

class DoubleCplexStmt(TypeDecl):
    kw     = 'doublecomplex'
    kw_str = 'double complex'

class DimensionStmt(Decl):
    _sons = ['lst']

    @staticmethod
    def parse(scan,lineNumber):
        p1 = seq(lit('dimension'),
                 cslist(app))
        ((dc,lst),rest) = p1(scan)
        return DimensionStmt(lst,dc,lineNumber)

    def __init__(self,lst,stmt_name='dimension',lineNumber=0,label=False,lead=''):
        self.lst = lst
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,repr(self.lst))

    def __str__(self):
        return '%s %s' % (self.stmt_name,','.join([str(l) for l in self.lst]))

class IntentStmt(Decl):
    pass

class OptionalStmt(Decl):
    pass

class NamelistStmt(Decl):
    pass

class SubroutineStmt(PUstart):
    utype_name = 'subroutine'
    _sons = ['args']

    @staticmethod
    def parse(scan,lineNumber):
        p1 = seq(lit('subroutine'),
                 id,
                 zo1(seq(lit('('),cslist(id),lit(')')))
                 )
        ((dc,name,args),rst) = p1(scan)
        if args:
            (dc,args,dc1) = args[0]

        return SubroutineStmt(name,args,dc,lineNumber)

    def __init__(self,name,args,stmt_name='subroutine',lineNumber=0,label=False,lead=''):
        self.name = name
        self.args = args
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return '%s(%s,%s)' % (self.__class__.__name__,
                              repr(self.name),
                              repr(self.args))
    def __str__(self):
        return '%s %s(%s)' % (self.stmt_name,self.name,
                                      ','.join([str(d) for d in self.args]))

class ProgramStmt(PUstart):
    utype_name = 'program'

    @staticmethod
    def parse(scan,lineNumber):
        p1 = seq(lit('program'),
                 id)
        ((dc,name),rest) = p1(scan)
        return ProgramStmt(name,dc,lineNumber)

    def __init__(self,name,stmt_name='program',lineNumber=0,label=False,lead=''):
        self.name = name
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,repr(self.name))

    def __str__(self):
        return '%s %s' % (self.stmt_name,self.name)

class FunctionStmt(PUstart):
    utype_name = 'function'
    _sons = ['ty','args']

    @staticmethod
    def parse(scan,lineNumber):
        p1 = seq(zo1(type_pat_sem),
                 lit('function'),
                 id,
                 lit('('),
                 cslist(id),
                 lit(')'),
                 zo1(seq(lit('result'),
                         lit('('),
                         id,
                         lit(')'))))
        ((ty,dc,name,dc1,args,dc2,resultstuff),rest) = p1(scan)
        type = ty and ty[0] \
                   or None
        result = resultstuff and resultstuff[0][2] \
                              or None
        return FunctionStmt(type,name,args,result,lineNumber)

    def __init__(self,ty,name,args,result,lineNumber=0,label=False,lead=''):
        '''
        typ = None

        if ty:
            (type_name,mod) = ty[0]
            type_class = _name2class(type_name)
            typ        = (type_class,mod)
        '''
        self.ty = ty
        self.name = name
        self.args = args
        self.result = result
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        typeRepr = self.ty and '('+self.ty[0].__name__+','+repr(self.ty[1])+')' \
                            or None
        resultRepr = self.result and repr(self.result) \
                                  or None
        return 'FunctionStmt(%s,%s,%s,%s)' % (typeRepr,
                                              repr(self.name),
                                              repr(self.args),
                                              resultRepr)
    def __str__(self):
        typePrefix = self.ty and (typestr2(self.ty)+' ') \
                              or ''
        resultStr = self.result and ' result('+str(self.result)+')' \
                                 or ''
        return '%sfunction %s(%s)%s' % (typePrefix,
                                        str(self.name),
                                        ','.join([str(l) for l in self.args]),
                                        resultStr)

class ModuleStmt(PUstart):
    utype_name = 'module'
    @staticmethod
    def parse(scan,lineNumber):
        p1 = seq(lit('module'),
                 id)
        ((dc,name),rest) = p1(scan)
        return ModuleStmt(name,lineNumber)

    def __init__(self,name,lineNumber=0,label=False,lead=''):
        self.name = name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                              repr(self.name))
    def __str__(self):
        return 'module %s' % self.name

    pass

class UseStmt(Decl):
    @staticmethod
    def parse(scan,lineNumber):
        p1 = seq(lit('use'),id)
        ((dc,name),rest) = p1(scan)
        return UseStmt(name,dc,lineNumber)

    def __init__(self,name,stmt_name='use',lineNumber=0,label=False,lead=''):
        self.name = name
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'UseStmt(%s)' % repr(self.name)

    def __str__(self):
        return '%s %s' % (self.stmt_name,str(self.name))

class FormatStmt(Decl):
    pass

class EntryStmt(Decl):
    pass

class CallStmt(Exec):
    _sons = ['args']

    @staticmethod
    def parse(scan,lineNumber):
        prefix = seq(lit('call'),disj(app,id))
        ((dc,a),rst) = prefix(scan)
        if (isinstance(a,App)):
            return CallStmt(a.head,a.args,dc,lineNumber)
        else:
            return CallStmt(a,[],dc,lineNumber)

    def __init__(self,head,args,stmt_name='call',lineNumber=0,label=False,lead=''):
        self.head = head
        self.args = args
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'CallStmt(%s,%s)' % (repr(self.head),
                                    repr(self.args),)

    def __str__(self):
        return '%s %s(%s)' % (self.stmt_name,str(self.head),
                                ','.join([str(l) for l in self.args]))

class AssignStmt(Exec):
    _sons = ['lhs','rhs']

    @staticmethod
    def parse(scan,lineNumber):
#        lhs     = disj(app,id,)

        lhs     = lv_exp
        assign  = seq(lhs,lit('='),Exp)
        ((l,dc,r),rst) = assign(scan)
        return AssignStmt(l,r,lineNumber)

    def __init__(self,lhs,rhs,lineNumber=0,label=False,lead=''):
        self.lhs  = lhs
        self.rhs  = rhs
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'AssignStmt(%s,%s)' % (repr(self.lhs),repr(self.rhs))

    def __str__(self):
        return '%s = %s' % (str(self.lhs),str(self.rhs))

class OpenStmt(Exec):
    pass

class CloseStmt(Exec):
    pass

class ReadStmt(Exec):
    pass

class WriteStmt(Exec):
    pass

class PrintStmt(Exec):
    pass

class StopStmt(Exec):
    pass

class ReturnStmt(Leaf):
    kw = 'return'

class IfStmt(Exec):
    @staticmethod
    def parse(scan,lineNumber):
        prefix = seq(lit('if'),lit('('),Exp,lit(')'))
        ((dc,dc1,e,dc2),rest) = prefix(scan)
        if [l.lower() for l in rest] == ['then']:
            return IfThenStmt(e,dc,l,lineNumber)
        else:
            return IfNonThenStmt(e,_kw_parse(rest,lineNumber),dc,lineNumber)

class IfThenStmt(IfStmt):
    _sons = ['test']

    def __init__(self,e,stmt_name='if',stmt_name2='then',lineNumber=0,label=False,lead=''):
        self.test = e
        self.stmt_name = stmt_name
        self.stmt_name2 = stmt_name2
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'IfThenStmt(%s)' % (repr(self.test),)

    def __str__(self):
        return '%s (%s) %s' % (self.stmt_name,str(self.test),self.stmt_name2)

class IfNonThenStmt(IfStmt):
    _sons = ['test','stmt']

    def __init__(self,test,stmt,stmt_name='if',lineNumber=0,label=False,lead=''):
        self.test = test
        self.stmt = stmt
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'IfNonThenStmt(%s,%s)' % (repr(self.test),
                                         repr(self.stmt))

    def __str__(self):
        return '%s (%s) %s' % (self.stmt_name,str(self.test),str(self.stmt))

class ElseifStmt(Exec):
    _sons = ['test']
    kw = 'elseif'

    @staticmethod
    def parse(scan,lineNumber):
        prefix = seq(lit('elseif'),lit('('),Exp,lit(')'),lit('then'))

        ((dc0,dc1,e,dc2,dc3),rest) = prefix(scan)
        return ElseifStmt(e,dc0,dc3,lineNumber)

    def __init__(self,e,stmt_name='elseif',stmt_name2='then',lineNumber=0,label=False,lead=''):
        self.test = e
        self.stmt_name = stmt_name
        self.stmt_name2 = stmt_name2
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'ElseifStmt(%s)' % (repr(self.test),)

    def __str__(self):
        return '%s (%s) %s' % (self.stmt_name,repr(self.test),self.stmt_name2)
    
class ElseStmt(Leaf):
    kw = 'else'

class EndStmt(PUend):
    kw =  'end'

    def is_uend(self,unit=_non): return True
    def is_decl(self,unit=_non):
        return unit.val._in_iface
    def is_exec(self,unit=_non): return False


class EndPseudoStmt(GenStmt):
    @staticmethod
    def parse(scan,lineNumber):
        if len(scan) >= 2 and scan[1].lower() == 'interface':
            return EndInterfaceStmt.parse(scan,lineNumber)
        return EndStmt.parse(scan,lineNumber)

class EndifStmt(Leaf):
    kw = 'endif'

class DoStmt(Exec):
    #FIXME: optional construct name and comma are not handled
    '''
    [do-construct-name :] do [label] [,] &
      scalar-integer-variable-name = scalar-integer-expression , scalar-integer-expression [, scalar-integer-expression]
    '''
    _sons = ['doLabel','loopVar','loopStart','loopEnd','loopStride']
    kw = 'do'

    @staticmethod
    def parse(scan,lineNumber):
        formDoStmt = seq(lit('do'),
                         zo1(int),
                         id,
                         lit('='),
                         Exp,
                         lit(','),
                         Exp,
                         zo1(seq(lit(','),
                                 Exp)))
        try:
            ((theDoKeyword,doLabel,loopVar,equals,loopStart,comma1,loopEnd,loopStride),rest) = formDoStmt(scan)
        except ListAssemblerException,e:
            raise ParseError(lineNumber,scan,'Do statement')
        loopStride = loopStride and loopStride[0][1] \
                                 or None
        doLabel = doLabel and doLabel[0] \
                           or None
        return DoStmt(doLabel,loopVar,loopStart,loopEnd,loopStride,theDoKeyword,lineNumber)

    def __init__(self,doLabel,loopVar,loopStart,loopEnd,loopStride,stmt_name='do',lineNumber=0,label=False,lead=''):
        self.doLabel = doLabel
        self.loopVar = loopVar
        self.loopStart = loopStart
        self.loopEnd = loopEnd
        self.loopStride = loopStride
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __str__(self):
        loopStrideString = self.loopStride and ','+str(self.loopStride) \
                                            or ''
        doLabelString = self.doLabel and str(self.doLabel)+' ' \
                                      or ''
        return '%s %s%s = %s,%s%s' % (self.stmt_name,doLabelString,str(self.loopVar),str(self.loopStart),str(self.loopEnd),loopStrideString)

class WhileStmt(Exec):
    #FIXME: optional construct name, label, and comma are not handled
    '''
    [do-construct-name : ] DO [ label ] [ , ] while ( scalar-logical-expression )
    '''
    _sons = ['testExpression']
    kw = 'do while'

    @staticmethod
    def parse(scan,lineNumber):
        formWhileStmt = seq(lit('dowhile'),
                            lit('('),
                            Exp,
                            lit(')'))
        ((theDoWhileKeyword,openPeren,theTestExpression,closePeren),rest) = formWhileStmt(scan)
        return WhileStmt(theTestExpression,theDoWhileKeyword,lineNumber)

    def __init__(self,testExpression,stmt_name='do while',lineNumber=0,label=False,lead=''):
        self.testExpression = testExpression
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __str__(self):
        return '%s (%s)' % (self.stmt_name,str(self.testExpression))

class EnddoStmt(Leaf):
    kw = 'enddo'

class ContinueStmt(Leaf):
    kw = 'continue'

class SelectCaseStmt(Exec):
    #FIXME: optional case construct name 
    '''
    [case-construct-name :] select case (case-expression)
    '''
    _sons = ['caseExpression']
    kw = 'selectcase'
    kw_str = 'select case'

    @staticmethod
    def parse(scan,lineNumber):
        formSelectCaseStmt = seq(lit('selectcase'),
#                                 lit('case'),
                                 lit('('),
                                 Exp,
                                 lit(')'))
        try:
            ((selectCaseKeyword,openPeren,caseExpression,closePeren),rest) = formSelectCaseStmt(scan)
        except ListAssemblerException,e:
            raise ParseError(lineNumber,scan,'Select Case statement')
        return SelectCaseStmt(caseExpression,selectCaseKeyword,lineNumber)

    def __init__(self,caseExpression,stmt_name='select case',lineNumber=0,label=False,lead=''):
        self.caseExpression = caseExpression
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __str__(self):
        return '%s (%s)' % (self.stmt_name,str(self.caseExpression))

class EndSelectCaseStmt(Leaf):
    #FIXME: optional case construct name 
    '''
    end select [case-construct-name]
    '''
    kw = 'endselect'
    kw_str = 'end select'

class CaseDefaultStmt(Exec):
    #FIXME: optional case construct name 
    '''
    case default [case-construct-name]
    '''
    _sons = []
    kw = 'casedefault'
    kw_str = 'case default'

    @staticmethod
    def parse(scan,lineNumber):
        formCaseDefaultStmt = seq(lit('casedefault'))
        try:
            ((caseDefaultKeyword),rest) = formCaseDefaultStmt(scan)
        except ListAssemblerException,e:
            raise ParseError(lineNumber,scan,'case default statement')
        return CaseDefaultStmt(lineNumber)

    def __init__(self,lineNumber=0,label=False,lead=''):
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __str__(self):
        return 'case default'

class CaseRangeListStmt(Exec):
    #FIXME: optional case construct name 
    '''
    case (case-value-range-list) [case-construct-name]
    '''
    _sons = ['caseRangeList']
    kw = 'case'
    kw_str = 'case'

    @staticmethod
    def parse(scan,lineNumber):
        formCaseRangeListStmt = seq(lit('case'),
                                    lit('('),
                                    cslist(Exp),
                                    lit(')'))
        try:
            ((caseKeyword,openPeren,caseRangeList,closePeren),rest) = formCaseRangeListStmt(scan)
        except ListAssemblerException,e:
            raise ParseError(lineNumber,scan,'case range list statement')
        return CaseRangeListStmt(caseRangeList,caseKeyword,lineNumber)

    def __init__(self,caseRangeList,stmt_name='case',lineNumber=0,label=False,lead=''):
        self.caseRangeList = caseRangeList
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __str__(self):
        return '%s (%s)' % (self.stmt_name,','.join([str(range) for range in self.caseRangeList]))

class GotoStmt(Exec):
    pass

class AllocateStmt(Exec):
    _sons = ['arg']
    kw = 'allocate'
    kw_str = 'allocate'

    @staticmethod
    def parse(scan,lineNumber):
        allocStmt = seq(lit('allocate'),
                        lit('('),
                        Exp,
                        lit(')'))
        try:
            ((allocKeyword,openParen,allocArg,closeParen),rest) = allocStmt(scan)
        except ListAssemblerException,e:
            raise ParseError(lineNumber,scan,'alloc statement')
        return AllocateStmt(allocArg,allocKeyword,lineNumber)

    def __init__(self,arg,stmt_name='allocate',lineNumber=0,label=False,lead=''):
        self.arg = arg
        self.stmt_name = stmt_name
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead

    def __repr__(self):
        return 'AllocateStmt(%s)' % (repr(self.arg),)

    def __str__(self):
        return '%s(%s)' % (self.stmt_name,str(self.arg))

kwtbl = dict(blockdata       = BlockdataStmt,
             common          = CommonStmt,
             logical         = LogicalStmt,
             data            = DataStmt,
             doubleprecision = DoubleStmt,
             doublecomplex   = DoubleCplexStmt,
             implicit        = ImplicitStmt,
             equivalence     = EquivalenceStmt,
             parameter       = ParameterStmt,
             private         = PrivateStmt,
             public          = PublicStmt,
             type            = TypePseudoStmt,
             interface       = InterfaceStmt,
             contains        = ContainsStmt,
             save            = SaveStmt,
             goto            = GotoStmt,
             external        = ExternalStmt,
             allocatable     = AllocatableStmt,
             character       = CharacterStmt,
             intrinsic       = IntrinsicStmt,
             include         = IncludeStmt,
             real            = RealStmt,
             integer         = IntegerStmt,
             dimension       = DimensionStmt,
             complex         = ComplexStmt,
             subroutine      = SubroutineStmt,
             program         = ProgramStmt,
             function        = FunctionStmt,
             module          = ModuleStmt,
             use             = UseStmt,
             format          = FormatStmt,
             entry           = EntryStmt,
             call            = CallStmt,
             open            = OpenStmt,
             close           = CloseStmt,
             read            = ReadStmt,
             write           = WriteStmt,
             stop            = StopStmt,
             elseif          = ElseifStmt,
             endif           = EndifStmt,
             end             = EndStmt,
             endinterface    = EndInterfaceStmt,
             endtype         = EndTypeStmt,
             endmodule       = EndStmt,
             endprogram      = EndStmt,
             endfunction     = EndStmt,
             endsubroutine   = EndStmt,
             endblockdata    = EndStmt,
             do              = DoStmt,
             enddo           = EnddoStmt,
             dowhile         = WhileStmt,
             selectcase      = SelectCaseStmt,
             endselect       = EndSelectCaseStmt,
             casedefault     = CaseDefaultStmt,
             case            = CaseRangeListStmt,
             intent          = IntentStmt,
             optional        = OptionalStmt,
             allocate        = AllocateStmt,
             )

for kw in ('if','continue','return','else','print'):
    kwtbl[kw] = globals()[kw.capitalize() + 'Stmt']
    
lhs    = disj(app,id)
assign = seq(lhs,lit('='),Exp)


import kw_multi

def sqz(n,mutable):
    'return the squeezed kw, and as a side effect, change the scan'

    rv = ''.join([l.lower() for l in mutable[0][0:n]])
    mutable[0][0:n] = [rv]
    return rv

_types = ('real',
          'integer',
          'logical',
          'complex',
          'character',
          'doubleprecision',
          'doublecomplex',
          )

def parse(scan,lineNumber):
    try:
        return AssignStmt.parse(scan,lineNumber)

    except AssemblerException:
        lscan = tuple([ l.lower() for l in scan ])
        kw3g  = kw_multi.kw3.get
        kw2g  = kw_multi.kw2.get
        kw = len(scan) >=3 and kw3g(lscan[0:3]) and sqz(3,[scan]) or \
             len(scan) >=2 and kw2g(lscan[0:2]) and sqz(2,[scan]) or \
             lscan[0]
        if kw in _types and 'function' in lscan:
            kw = 'function'

        if not kwtbl.get(kw):
            raise ParseError(lineNumber,scan,None)
        return kwtbl.get(kw).parse(scan,lineNumber)

#
# alias so that stmts like if, etc can call the above routine
#
_kw_parse = parse

'''
if __name__ == '__main__':
    from _Setup.testit import *
'''
