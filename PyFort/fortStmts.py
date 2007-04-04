'''
Various Fortran statement types and Fortran parsing functions
'''

from _Setup import *
from fortExp      import *
from PyUtil.l_assembler  import *
from PyUtil.chomp        import chomp
from fortLine     import flow_line
from PyIR.mapper       import _Mappable
from PyIR.mutable_tree import _Mutable_T
from PyUtil.errors  import ParseError


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

class _F90Len(_Mutable_T):
    'Utility modifier for character data, F90 style'
    _sons = ['len']
    def __init__(self,len):
        self.len = len
    def __repr__(self):
        return '_F90Len(%s)' % repr(self.len)
    def __str__(self):
        return '(%s)' % str(self.len)

class _F77Len(_Mutable_T):
    'Utility modifier for character data, F77 style'
    _sons = ['len']

    def __init__(self,len):
        self.len = len
    def __repr__(self):
        return '_F77Len(%s)' % repr(self.len)
    def __str__(self):
        return '*%s' % str(self.len)

class _Prec(_TypeMod):
    pat = '*%s'

class _Kind(_TypeMod):
    pat = '(%s)'

class _ExplKind(_TypeMod):
    pat = '(kind = %s)'

prec = seq(lit('*'),Exp)
prec = treat(prec,lambda a:_Prec(a[1]))

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
         zo1(disj(f77mod,f90mod)),
         )

type_pat = disj(pchar,pstd)

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

decl_item = seq(Exp,init_spec)
decl_item = treat(decl_item,_handle_init)

def typestr(raw):
    (tyid,mod) = raw
    return kwtbl[tyid].kw_str + (mod and str(mod[0]) or '')

def typestr(raw):
    (tyid,mod) = raw
    return kwtbl[tyid].kw_str + (mod and str(mod[0]) or '')

class GenStmt(_Mappable,_Mutable_T):
    _sons = []

    def __init__(self,scan):
        self.scan = scan

    def __repr__(self):
        return '%s(args)' % self.__class__.__name__

    @classmethod
    def parse(cls,scan):
        return cls(scan)

class Skip(GenStmt):
    def __init__(self):
        self.scan = []

class Comments(GenStmt):
    def __init__(self,rawline):
        self.rawline = rawline
    def __repr__(self):
        return 'Comments(blk)'
    def viz(self):
        return 'Comments(%s)' % self.rawline

def comment_bl(*comlines):
    return Comments('\n'.join(['c '+ chomp(s) for s in comlines])+'\n')

class NonComment(GenStmt):

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, self.scan[0])

    def flow(self):
        lineno = self.lineno
        if lineno:
            init = ' ' + ('%-4d' % lineno) + ' '
        else:
            init = ' ' * 6
        self.rawline = flow_line(init + self.lead + str(self))+'\n'
        return self

    def reflow(self):
        'remove rawline, so that later processing will regen'
        if hasattr(self,'rawline'): del self.rawline
        
    def same_level(self,parsed):
        parsed.lineno  = False
        parsed.lead    = self.lead
        return parsed.flow()

    def indent(self,n):
        self.lead += ' ' * n
        return self.flow()

    def basic_line(self):
        self.lineno = False
        self.lead   = ''
        self.flow()
        return self

    def same(self,targ):
        self.same_level(targ)
        targ.ctxt = self.ctxt
        return targ

    def clone_fmt(self,src):
        self.lineno = False
        self.lead   = src.lead
#        self.ctxt   = src.ctxt
        self.flow()
        return self

class Marker(NonComment):
    def __init__(self):
        pass

    def __str__(self):
        return '!! %s (marker)' % self.ptxt

    def __repr__(self):
        return '%s()' % self.__class__.__name__

class LastDecl(Marker):
    ptxt = 'last declaration'

class FirstExec(Marker):
    ptxt = 'first executable follows'

class Decl(NonComment):
    pass

def attrstr(l):
    if not l: return ''
    return ','+ ','.join([str(ll) for ll in l])

def ditemstr(l):
    return ','.join([str(ll) for ll in l])

class TypeDecl(Decl):
    kw = '__unknown__'
    kw_str = kw
    mod = None
    decls = []

    @classmethod
    def parse(cls,scan):
        p0 = seq(type_pat,type_attr_list,zo1(lit('::')),
                 cslist(decl_item))

        (v,r) = p0(scan)

### FIXME: '::' is optional, so set flag to indicate presence ###
#
        ((typ,mod),attrs,dc,decls) = v
        return cls(mod,attrs,decls)

    def __init__(self,mod,attrs,decls):
        self.mod   = mod
        self.attrs = attrs
        self.decls = decls

    def __repr__(self):
        return '%s(%s,%s,%s)' % (self.__class__.__name__,
                                 repr(self.mod),
                                 repr(self.attrs),
                                 repr(self.decls),)
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

class DrvdTypeDecl(Decl):
    _sons = ['attrs','decls']
    
    kw     = 'derivedDcl'
    kw_str = kw
    def __init__(self,name,attrs,dc,decls):
        self.name  = name
        self.attrs = attrs
        self.decls = decls
        self.dblc  = bool(dc)

    def __repr__(self):
        return 'DrvdTypeDecl(%s,%s,%s)' % (repr(self.name),
                                           repr(self.attrs),
                                           repr(self.decls))
    def __str__(self):
        return 'type(%s)%s %s%s' % (str(self.name),
                                     attrstr(self.attrs),
                                     self.dblc and ':: ' or '',
                                     ditemstr(self.decls))

    @staticmethod
    def parse(scan):
        p0 = seq(lit('type'),lit('('),id,lit(')'),
                 type_attr_list,zo1(lit('::')),cslist(decl_item))
        p0 = treat(p0,lambda l: DrvdTypeDecl(l[2],l[4],l[5],l[6]))

        (v,r) = p0(scan)
        return v

class DrvdTypeDefn(Decl):
    '''
    derived type definition (start)
    '''
    kw_str = 'derivedDefn'
    kw     = kw_str

    def __init__(self,name):
        self.name = name

    def __repr__(self):
        return 'DrvdTypeDefn(%s)' % repr(self.name)

    def __str(self):
        return 'type %s' % str(self.name)
    
    @staticmethod
    def parse(scan):
        p0    = treat(seq(lit('type'),id),lambda l: DrvdTypeDefn(l[1]))
        (v,r) = p0(scan)
        return v

class InterfaceStmt(Decl):
    kw = 'interface'
    kw_str = 'interface'

    def __init__(self,l):
        self.name = l

    def __repr__(self):
        return 'InterfaceStmt(%s)' % self.name

    def __str__(self):
        if self.name:
            return 'interface %s' % self.name[0]
        else:
            return 'interface'

    @staticmethod
    def parse(scan):
        p0    = treat(seq(lit('interface'),zo1(id)),
                      lambda l:InterfaceStmt(l[1]))

        (v,r) = p0(scan)
        return v

class TypePseudoStmt(GenStmt):
    '''
    type keyword signals *either* declaration or definition
    '''
    @staticmethod
    def parse(scan):
        if scan[1] == '(': return DrvdTypeDecl.parse(scan)
        return DrvdTypeDefn.parse(scan)

class PUstart(Decl):
    pass

class Exec(NonComment):
    pass

class Leaf(Exec):
    "special Exec that doesn't have components"

    def __init__(self,*dc):
        pass

    def __repr__(self):
        return '%s()' % self.__class__.__name__

    def __str__(self):
        return '%s' % self.__class__.kw

class DeclLeaf(Decl):
    "special Decl that has no components"

    @classmethod
    def parse(cls,scan):
        return cls()

    def __init__(self,*dc,**dc2): pass
    def __repr__(self): return '%s()' % self.__class__.__name__
    def __str__(self): return '%s' % self.__class__.kw_str

class PUend(Leaf):
    pass

class BlockdataStmt(PUstart):
    pass

class CommonStmt(Decl):
    pass

class DataStmt(Decl):
    pass

class VarAttrib(Decl):
    @classmethod
    def parse(cls,scan):
        p0 = seq(lit(cls.kw),zo1(seq(lit('::'),cslist(id))))

        ((dc,vlist),r) = p0(scan)
        if vlist:
            vlist = vlist[0][1]

        return cls(vlist)

    def __init__(self,vlist):
        self.vlist = vlist
        _sons  = ['vlist']

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
    
class ImplicitNone(DeclLeaf):
    kw     = 'implicitnone'
    kw_str = 'implicit none'

class ImplicitStmt(Decl):
    _sons = ['lst']

    @staticmethod
    def parse(scan):
        p0 = seq(lit('implicit'),lit('none'))
        p0 = treat(p0,ImplicitNone)

        impelt = seq(type_pat,lit('('),ExpList_l,lit(')'))
        impelt = treat(impelt,lambda l: (l[0],l[2]))

        p1 = seq(lit('implicit'),
                 cslist(impelt))

        p1 = treat(p1,lambda l:ImplicitStmt(l[1]))

        (v,r) = disj(p0,p1)(scan)

        return v

    def __init__(self,lst):
        self.lst  = lst

    def __repr__(self):
        return 'ImplicitStmt(%s)' % repr(self.lst)

    def __str__(self):

        def _helper(elt):
            (typ,explst) = elt
            return '%s (%s)' % (typestr(typ),
                                ','.join([str(l).replace(' ','') \
                                          for l in explst]))
            
        return 'implicit %s' % ', '.join([_helper(e) for e in self.lst])

class EquivalenceStmt(Decl):
    pass

class ParameterStmt(Decl):
    pass

class SaveStmt(Decl):
    pass

class StmtFnStmt(Decl):
    _sons = ['args','body']

    def __init__(self,name,args,body):
        self.name = name
        self.args = args
        self.body = body

    def __repr__(self):
        return 'StmtFnStmt(%s,%s,%s)' % (repr(self.name),
                                         repr(self.args),
                                         repr(self.body))
    def __str__(self):
        return '%s(%s) = %s' % (str(self.name),
                                ','.join([str(l) for l in self.args]),
                                str(self.body))

class ExternalStmt(Decl):
    pass

class CharacterStmt(TypeDecl):
    kw = 'character'
    kw_str = kw
    @staticmethod
    def parse(scan):
        starmod  = seq(lit('('),lit('*'),lit(')'))
        starmod  = treat(starmod,lambda l: _Star())

        lenmod   = disj(Exp,starmod)
        f77mod   = seq(lit('*'),lenmod)
        f77mod   = treat(f77mod,lambda l: _F77Len(l[1]))

        f90mod   = seq(lit('('),disj(lit('*'),Exp),lit(')'))
        f90mod   = treat(f90mod,lambda l: _F90Len(l[1]))
                
        p1 = seq(lit('character'),
                 zo1(disj(f77mod,f90mod)),
                 app_id_l)
        try: 
          ((dc,mod,decls),rest) = p1(scan)
        except AssemblerException,e:
          raise ParseError(scan,'character variable declaration')  

        return CharacterStmt(mod,decls)

    def __init__(self,mod,decls):
        self.mod   = mod
        self.decls = decls
        _sons  = ['mod','decls']

    def __repr__(self):
        return 'CharacterStmt(%s,%s)' % (repr(self.mod),repr(self.decls))

    def __str__(self):
        modstr = ''
        if self.mod:
            modstr = str(self.mod[0])
        
        return 'character%s %s' % (modstr,
                                   ','.join([str(d) for d in self.decls]))

class IntrinsicStmt(Decl):
    pass

class IncludeStmt(Decl):
    pass

class BasicTypeDecl(TypeDecl):
    @classmethod
    def parse(cls,scan):
        prec = seq(lit('*'),Exp)
        prec = treat(prec,lambda a:_Prec(a[1]))

        kind = seq(lit('('),Exp,lit(')'))
        kind = treat(kind,lambda a:_Kind(a[1]))

        explKind = seq(lit('('),lit('kind'),lit('='),Exp,lit(')'))
        explKind = treat(explKind,lambda a:_ExplKind(a[3]))

        p1 = seq(lit(cls.kw),
                 star(disj(prec,kind,explKind)),
                 star(lit('::')),
                 app_id_l)

        ((dc,mod,dc1,decls),rest) = p1(scan)
        return cls(mod,decls)

    def __init__(self,mod,decls):
        self.mod   = mod
        self.decls = decls
        _sons  = ['mod','decls']

    def __repr__(self):
        return '%s(%s,%s)' % (self.__class__.__name__,
                              repr(self.mod),
                              repr(self.decls))
    def __str__(self):
        modtype = self.kw

        if self.mod:
            modtype += str(self.mod[0])

        return '%s %s' %(modtype,
                         ','.join([str(d).replace('()','') \
                                   for d in self.decls]))

# class RealStmt(BasicTypeDecl):
class RealStmt(TypeDecl):
    kw = 'real'
    kw_str = kw

# class ComplexStmt(BasicTypeDecl):
class ComplexStmt(TypeDecl):
    kw = 'complex'
    kw_str = kw

# class IntegerStmt(BasicTypeDecl):
class IntegerStmt(TypeDecl):
    kw = 'integer'
    kw_str = kw

# class LogicalStmt(BasicTypeDecl):
class LogicalStmt(TypeDecl):
    kw = 'logical'
    kw_str = kw

class F77Type(TypeDecl):
    '''
    These types do not have kinds or modifiers
    '''
    @classmethod
    def parse(cls,scan):
        p1 = seq(lit(cls.kw),
                 star(lit('::')),
                 app_id_l)

        ((dc,dc1,decls),rest) = p1(scan)
        return cls([],decls)

    def __init__(self,mod,decls):
        'same interface as other type decls, but mod is always empty'
        self.mod   = []
        self.decls = decls
        _sons  = ['decls']

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                           repr(self.decls))

    def __str__(self):
        return '%s  %s' % (self.__class__.kw_str,
                           ','.join([str(d) for d in self.decls]))

# class DoubleStmt(F77Type):
class DoubleStmt(TypeDecl):
    kw     = 'doubleprecision'
    kw_str = 'double precision'

class DoubleCplexStmt(F77Type):
    kw     = 'doublecomplex'
    kw_str = 'double complex'

class DimensionStmt(BasicTypeDecl):
    _sons = ['lst']

    @staticmethod
    def parse(scan):
        p1 = seq(lit('dimension'),
                 cslist(app))
        ((dc,lst),rest) = p1(scan)
        return DimensionStmt(lst)

    def __init__(self,lst):
        self.lst = lst

    def __repr__(self):
        return 'dimension(%s)' % repr(self.lst)

    def __str__(self):
        return 'dimension %s' % ','.join([str(l) for l in self.lst])

class NamelistStmt(Decl):
    pass

class SubroutineStmt(PUstart):
    utype_name = 'subroutine'
    _sons = ['args']

    @staticmethod
    def parse(scan):
        p1 = seq(lit('subroutine'),
                 id,
                 zo1(seq(lit('('),cslist(id),lit(')')))
                 )
        ((dc,name,args),rst) = p1(scan)
        if args:
            (dc,args,dc1) = args[0]

        return SubroutineStmt(name,args)

    def __init__(self,name,args):
        self.name = name
        self.args = args

    def __repr__(self):
        return '%s(%s,%s)' % (self.__class__.__name__,
                              repr(self.name),
                              repr(self.args))
    def __str__(self):
        return 'subroutine %s(%s)' % (self.name,
                                      ','.join([str(d) for d in self.args]))

class ProgramStmt(PUstart):
    utype_name = 'program'

    @staticmethod
    def parse(scan):
        p1 = seq(lit('program'),
                 id)
        ((dc,name),rest) = p1(scan)
        return ProgramStmt(name)

    def __init__(self,name):
        self.name = name

    def __repr__(self):
        return '%s(%s,%s)' % (self.__class__.__name__,
                              repr(self.name))
    def __str__(self):
        return 'program %s' % self.name

class FunctionStmt(PUstart):
    utype_name = 'function'
    _sons = ['ty','args']

    @staticmethod
    def parse(scan):
        p1 = seq(zo1(type_pat),
                 lit('function'),
                 id,
                 lit('('),cslist(id),lit(')'),
             )

        ((ty,dc,name,dc1,args,dc2),rest) = p1(scan)
        return FunctionStmt(ty,name,args)

    def __init__(self,ty,name,args):
        self.ty   = ty
        self.name = name
        self.args = args

    def __repr__(self):
        return 'FunctionStmt(%s,%s,%s)' % (repr(self.name),
                                           repr(self.ty),
                                           repr(self.args))
    def __str__(self):
        ty = self.ty
        typeprefix = ty and (typestr(ty[0])+' ') or ''
        return '%sfunction %s(%s)' % (typeprefix,
                                      str(self.name),
                                      ','.join([str(l) for l in self.args]))

class ModuleStmt(PUstart):
    utype_name = 'module'
    @staticmethod
    def parse(scan):
        p1 = seq(lit('module'),
                 id)
        ((dc,name),rest) = p1(scan)
        return ModuleStmt(name)

    def __init__(self,name):
        self.name = name

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                              repr(self.name))
    def __str__(self):
        return 'module %s' % self.name

    pass

class UseStmt(Decl):
    @staticmethod
    def parse(scan):
        p1 = seq(lit('use'),id)
        ((dc,name),rest) = p1(scan)

        return UseStmt(name)

    def __init__(self,name):
        self.name = name

    def __repr__(self):
        return 'UseStmt(%s)' % repr(self.name)

    def __str__(self):
        return 'use %s' % str(self.name)

class FormatStmt(Decl):
    pass

class EntryStmt(Decl):
    pass

class CallStmt(Exec):
    _sons = ['args']

    @staticmethod
    def parse(scan):
        prefix = seq(lit('call'),disj(app,id))
        ((dc,a),rst) = prefix(scan)
        if (isinstance(a,App)):
            return CallStmt(a.head,a.args)
        else:
            return CallStmt(a,[])

    def __init__(self,head,args):
        self.head = head
        self.args = args

    def __repr__(self):
        return 'CallStmt(%s,%s)' % (repr(self.head),
                                    repr(self.args),)

    def __str__(self):
        return 'call %s(%s)' % (str(self.head),
                                ','.join([str(l) for l in self.args]))

class AssignStmt(Exec):
    _sons = ['lhs','rhs']

    @staticmethod
    def parse(scan):
#        lhs     = disj(app,id,)

        lhs     = lv_exp
        assign  = seq(lhs,lit('='),Exp)
        ((l,dc,r),rst) = assign(scan)
        return AssignStmt(l,r)

    def __init__(self,lhs,rhs):
        self.lhs  = lhs
        self.rhs  = rhs

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
    def parse(scan):
        prefix = seq(lit('if'),lit('('),Exp,lit(')'))
        ((dc,dc1,e,dc2),rest) = prefix(scan)
        if [l.lower() for l in rest] == ['then']:
            return IfThenStmt(e)
        else:
            return IfNonThenStmt(e,_kw_parse(rest))

class IfThenStmt(IfStmt):
    _sons = ['test']

    def __init__(self,e):
        self.test = e

    def __repr__(self):
        return 'IfThenStmt(%s)' % (repr(self.test),)

    def __str__(self):
        return 'if ( %s ) then' % (str(self.test),)

class IfNonThenStmt(IfStmt):

    _sons = ['e','stmt']

    def __init__(self,e,stmt):
        self.test = e
        self.stmt = stmt

    def __repr__(self):
        return 'IfNonThenStmt(%s,%s)' % (repr(self.test),
                                         repr(self.stmt))

    def __str__(self):
        return 'if ( %s ) %s' % (str(self.test),str(self.stmt))

class ElseifStmt(Exec):
    _sons = ['test']

    @staticmethod
    def parse(scan):
        prefix = seq(lit('elseif'),lit('('),Exp,lit(')'),lit('then'))

        ((dc0,dc1,e,dc2,dc3),rest) = prefix(scan)
        return ElseifStmt(e)

    def __init__(self,e):
        self.test = e

    def __repr__(self):
        return 'ElseifStmt(%s)' % (repr(self.test),)

    def __str__(self):
        return 'elseif (%s) then' % (repr(self.test),)
    
class ElseStmt(Leaf):
    kw = 'else'

class EndStmt(PUend):
    kw =  'end'

class EndifStmt(Leaf):
    kw = 'endif'

class DoStmt(Exec):
    pass

class EnddoStmt(Leaf):
    kw = 'enddo'

class ContinueStmt(Leaf):
    kw = 'continue'

class SelectStmt(Exec):
    pass

class WhileStmt(Exec):
    pass

class GotoStmt(Exec):
    pass

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
             endmodule       = EndStmt,
             endprogram      = EndStmt,
             endfunction     = EndStmt,
             endsubroutine   = EndStmt,
             endblockdata    = EndStmt,
             do              = DoStmt,
             enddo           = EnddoStmt,
             select          = SelectStmt,
             dowhile         = WhileStmt,
             )

for kw in ('if','continue','return','else','print'):
    kwtbl[kw] = globals()[kw.capitalize() + 'Stmt']
    
lhs    = disj(app,id)
assign = seq(lhs,lit('='),Exp)

def mkassign(a):
    'make an assignment statement object from a recognized scan'
    (lhs,dc,rhs) = a
    return AssignStmt(lhs,rhs)

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

def parse(scan):
    try:
        return AssignStmt.parse(scan)

    except AssemblerException:
        lscan = tuple([ l.lower() for l in scan ])
        kw3g  = kw_multi.kw3.get
        kw2g  = kw_multi.kw2.get
        kw = len(scan) >=3 and kw3g(lscan[0:3]) and sqz(3,[scan]) or \
             len(scan) >=2 and kw2g(lscan[0:2]) and sqz(2,[scan]) or \
             lscan[0]
        if kw in _types and 'function' in lscan:
            kw = 'function'

#        cls = kwtbl.get(kw) or GenStmt
        cls = kwtbl.get(kw) or NonComment
        return cls.parse(scan)
#
# alias so that stmts like if, etc can call the above routine
#
_kw_parse = parse

#
# Type helper routines
#

def kw2type(s): return(kwtbl[s.lower()])
def lenfn(n): return [_F77Len(str(n))]
def poly(s):
    return s.lower() in ('max',
                         'min',
                         )

_modhash = { _Prec     : 0,
             _Kind     : 1,
             _ExplKind : 2,
             }

def modcompare(m1,m2):
    'compare type modifiers'
    if not m1: return m2
    if not m2: return m1
    mm1 = m1[0]
    mm2 = m2[0]
    c1  = mm1.__class__
    c2  = mm2.__class__

    if c1 == c2:
        if mm1.mod >= mm2.mod: return m1
        return m2

    if _modhash[c1] >= _modhash[c2]: return m1
    return m2

def typecompare(t1,t2):
    mergeit = dict(character=0,
                   logical=1,
                   integer=2,
                   real=3,
                   doubleprecision=4,
                   complex=5,
                   doublecomplex=6,
                   )

#    print 't1 = ',t1,'t2 = ',t2
    if t1[0] == t2[0]:
        return(t1[0],modcompare(t1[1],t2[1]))

    if mergeit[t1[0].kw] > mergeit[t2[0].kw]: return t1

    return t2

def typemerge(lst,default):
    if not lst: return default
    if len(lst) == 1: return lst[0]
    t1 = typecompare(lst[0],lst[1])
    for l in lst[2:]:
        t1 = typecompare(t1,l)
    return t1
