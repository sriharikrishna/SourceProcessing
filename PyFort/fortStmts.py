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

class _DimensionSpecifier(object):
    '''specifier for the dimension attribute in type declarations
       (currently not used dimension statements)'''

    form = seq(lit('dimension'),
               lit('('),
               cslist(disj(lit('*'),
                           Exp)),
               lit(')'))
    form = treat(form,lambda x: App(x[0],x[2]))

typeAttributeExpression = disj(_DimensionSpecifier.form,
                               Exp)

type_attr_list = star(seq(lit(','),
                          typeAttributeExpression))
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
        return '%s => %s' % (str(self.lhs),
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
        return '%s = %s' % (str(self.lhs),
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

upper_bound_range = seq(lit(':'),Exp)
upper_bound_range = treat(upper_bound_range, lambda x: Ops(':','',x[1]))

lower_bound_range = seq(Exp,lit(':'))
lower_bound_range = treat(lower_bound_range, lambda x: Ops(':',x[0],''))

var_w_dim = seq(id,zo1(seq(lit('('),cslist(disj(star_range,lit('*'),lit(':'),lower_bound_range,Exp)),lit(')'))))
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

    def __init__(self,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        """initializes a generic statement object
          formatting (leading whitespace or line breaks)
        lineNumber: contains the line number the statement was
          originally on
        label: contains a statement's numeric label, if there was one
        lead:contains the line lead, excluding the 6 leading spaces
          for fixed format (if input was in fixed format)
        internal: a list of internal comments
        accessed: determines whether or not the statement has been accessed and
          potentially modified, since the rawline is only updated
          when a statement is accessed
        """
        self.rawline = ''
        self.lineNumber = lineNumber
        self.label = label
        self.lead = lead
        self.internal = internal
        self.rest = rest
        self.modified = False

    @classmethod
    def parse(cls,ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        return cls(scan,lineNumber)

    def is_exec(self,unit=_non): return False
    def is_decl(self,unit=_non): return False
    def is_ustart(self,unit=_non): return False
    def is_uend(self,unit=_non): return False
    def is_contains(self,unit=_non): return False
    def is_comment(self,unit=_non): return False

    def __str__(self):
        return self.rawline

    def set_rawline(self,newRawline):
        self.rawline = newRawline

    def get_rawline(self):
        '''updates the rawline and returns it'''
        return self.rawline.strip()

    def get_sons(self):
        return self._sons

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

    def flow(self):
        """formats a comment for printing, depending on the format
        (free or fixed) of the output, while preserving all leading
        or trailing blank lines"""
        if self.rawline.strip() == '':
            return self.rawline
        formattedOutput = ''
        lead_len = len(self.rawline) - len(self.rawline.lstrip())
        if lead_len > 0:
            num_lines = self.rawline[:lead_len].count('\n')
            formattedOutput += '\n'*num_lines
        lines = self.rawline.strip().splitlines()
        if (flow.outputFormat == 'free'):
            for line in lines:
                formattedOutput += '!'+flow.flow_comment(line[1:])
        else:
            for line in lines:
                formattedOutput += 'C'+flow.flow_comment(line[1:])

        end_len = len(self.rawline)-len(self.rawline.rstrip())
        if end_len > 0:
            num_lines = self.rawline[len(self.rawline)-end_len+1:].count('\n')
            formattedOutput += '\n'*num_lines
        return formattedOutput

    def is_comment(self,unit=_non): return True

def comment_bl(*comlines):
    return Comments('\n'.join(['c '+ chomp(s) for s in comlines])+'\n')

class NonComment(GenStmt):

    def __init__(self,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        GenStmt.__init__(self,lineNumber,label,lead)
        self.internal = internal
        self.rest = rest
        self.rawline = str(self)
        self.modified = False
            
    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                           ','.join([repr(aSon) for aSon in self._sons]))

    def __str__(self):
        return self.__class__.kw_str+''.join(self.internal)

    def flow(self):
        """formats a statement for printing by concatenating the label
        string, the lead, the rawline (updated if necessary) and six
        leading spaces (if fixed format), and adding the necessary line
        breaks (depending on fixed or free format output)"""
        labelStr = self.label and ' ' + ('%-4d' % self.label) + ' ' \
                               or ''
        if (flow.outputFormat=='fixed') and len(labelStr) != 6:
            labelStr = labelStr + (6 - len(labelStr)) * ' '
            formattedOutput = flow.flow_line(labelStr + self.lead + self.get_rawline()) + '\n'
        else:
            formattedOutput = flow.flow_line(labelStr + self.lead + self.get_rawline()) + '\n'
        return formattedOutput

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

    def get_sons(self):
        '''returns the list of sons for this statement, and also marks the statement as accessed.'''
        return self._sons

    def set_son(self,theSon,newSon):
        oldSon = getattr(self,theSon)
        if newSon is not oldSon:
            setattr(self,theSon,newSon)
            self.modified = True

    def get_rawline(self):
        '''returns the rawline for this statement.
           If the statement has been changed (indicated by the flag self.modified), then the rawline is updated before being returned.'''
        if self.modified:
            old_rawline = self.rawline
            self.rawline = str(self)
            for string in self.rest:
                if string.lower() not in self.rawline.lower():
                    raise ParseError(self.lineNumber,self.rawline,"The parser dropped the substring "+string+" from the rawline. The original statement was '"+old_rawline+"'")
        self.modified = False
        return self.rawline.strip()

class Decl(NonComment):
    def __init__(self,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        NonComment.__init__(self,lineNumber,label,lead,internal,rest)

    def is_decl(self,unit=_non): return True

def attrstr(l):
    if not l: return ''
    return ','+ ','.join([str(ll) for ll in l])

def ditemstr(l):
    return ','.join([str(ll) for ll in l])

def itemstr(l):
    return ''.join([str(ll) for ll in l])

class TypeDecl(Decl):
    '''
    The double colon "::" is optional when parsing, but we have chosen to always include it when printing
    '''
    ## This is the first appearance of the kw and kw_str members; the kw is a string with no spaces (like 'doubleprecision').
    kw = '__unknown__'
    ## the kw_str may have spaces (like with 'double precision').
    kw_str = ''
    mod = None
    decls = []

    @classmethod
    def parse(cls,ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p0 = seq(type_pat,
                 type_attr_list,
                 zo1(lit('::')),
                 cslist(decl_item))
        (v,r) = p0(scan)
        ((typ,mod),attrs,dc,decls) = v
        return cls(mod,attrs,decls,lineNumber,rest=r)

    def __init__(self,mod,attrs,decls,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.mod   = mod
        self.attrs = attrs
        self.decls = decls
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

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
                                 ','.join([str(d) for d in self.decls]))\
                                 +''.join(self.internal)

    def get_mod(self):
        return self.mod

    def set_mod(self,newMod):
        if self.mod != newMod:
            self.mod = newMod
            self.modified = True

    def get_attrs(self):
        return self.attrs

    def set_attrs(self,newAttrs):
        self.attrs = newAttrs
        self.modified = True
    
    def get_decls(self):
        return self.decls

    def set_decls(self,newDecls):
        self.decls = newDecls
        self.modified = True

class DrvdTypeDecl(TypeDecl):
    '''
    Derived type declarations are treated as declarations of type "type,"
     with a modifier that is the name of the type.
    '''
    _sons = ['attrs','decls']
    kw     = 'type'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p0 = seq(lit('type'),
                 lit('('),
                 id,
                 lit(')'),
                 type_attr_list,
                 zo1(lit('::')),
                 cslist(decl_item))
        p0 = treat(p0,lambda l: DrvdTypeDecl([l[1]+l[2]+l[3]],l[4],l[6],lineNumber=lineNumber))
        (v,r) = p0(scan)
        v.rest=r
        return v

class DrvdTypeDefn(Decl):
    '''
    derived type definition (start)
    '''
    kw_str = 'derivedDefn'
    kw     = kw_str

    def __init__(self,name,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.name = name
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'DrvdTypeDefn(%s)' % repr(self.name)

    def __str__(self):
        return 'type %s' % str(self.name)+''.join(self.internal)
    
    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p0    = treat(seq(lit('type'),zo1(lit('::')),id),lambda l: DrvdTypeDefn(l[2],lineNumber=lineNumber))
        (v,r) = p0(scan)
        v.rest = r
        return v

class InterfaceStmt(Decl):
    kw = 'interface'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formInterfaceStmt = seq(lit('interface'),
                                zo1(id))
        ((interfaceKeyword,interfaceName),rest) = formInterfaceStmt(scan)
        name = interfaceName and interfaceName[0] \
                              or None
        return InterfaceStmt(name,lineNumber,rest)

    def __init__(self,l,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.name = l
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'InterfaceStmt(%s)' % self.name

    def __str__(self,whitespace=False):
        if self.name:
            return 'interface %s' % self.name+''.join(self.internal)
        else:
            return 'interface'+''.join(self.internal)

    def get_name(self):
        return self.name

    def set_name(self,newName):
        if self.name != newName:
            self.name = newName
            self.modified = True

class ProcedureStmt(Decl):
    kw = 'procedure'
    kw_str = kw
    _sons = ['procedureList']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formprocedureStmt = seq(zo1(lit('module')),    #0 - module keyword (optional)
                                lit(ProcedureStmt.kw), #1
                                cslist(id))            #2 - procedureList
        formprocedureStmt = treat(formprocedureStmt, lambda x : ProcedureStmt(x[0] and True or False, x[2],lineNumber))
        (theParsedStmt,rest) = formprocedureStmt(scan)
        theParsedStmt.rest = rest
        return theParsedStmt

    def __init__(self,hasModuleKeyword,procedureList,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.hasModuleKeyword = hasModuleKeyword
        self.procedureList = procedureList
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return '%s(%s,%s)' % (self.__class__.__name__,
                              repr(self.hasModuleKeyword),
                              repr(self.procedureList))

    def __str__(self):
        moduleKeywordStr = self.hasModuleKeyword and 'module ' \
                                                  or ''
        return '%s%s %s' % (moduleKeywordStr,
                            self.__class__.kw_str,
                            ','.join([str(aProcedureItem) for aProcedureItem in self.procedureList]))\
                           +''.join(self.internal)


class TypePseudoStmt(GenStmt):
    '''
    type keyword signals *either* declaration or definition
    '''
    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        if scan[1] == '(': return DrvdTypeDecl.parse(scan,lineNumber)
        return DrvdTypeDefn.parse(scan,lineNumber)

class PUstart(Decl):
    def is_decl(self,unit=_non):  return True
    def is_ustart(self,unit=_non): return True

class PointerStmt(Decl):
    kw = 'pointer'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(PointerStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return PointerStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

class TargetStmt(Decl):
    kw = 'target'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(TargetStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return TargetStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

class Exec(NonComment):
    ''' base class for all executable statements'''
    def __init__(self,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        NonComment.__init__(self,lineNumber,label,lead,internal,rest)
        
    def is_exec(self,unit=_non): return True

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                           ','.join([repr(getattr(self,aSon)) for aSon in self._sons]))

class Leaf(Exec):
    "special Exec that doesn't have components"

    @classmethod
    def parse(cls,ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        return cls(lineNumber)

    def __init__(self,lineNumber=0,label=False,lead='',internal=[],rest=[],*dc):
        Exec.__init__(self,lineNumber,label,lead,internal,rest)
            
    def __repr__(self):
        return '%s()' % self.__class__.__name__

class DeclLeaf(Decl):
    "special Decl that has no components"

    @classmethod
    def parse(cls,ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        return cls(lineNumber)

    def __init__(self,lineNumber=0,label=False,lead='',internal=[],rest=[],*dc,**dc2):
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self): return '%s()' % self.__class__.__name__

class PUend(Leaf):

    def __init__(self,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Leaf.__init__(self,lineNumber,label,lead,internal,rest)

class IfPUstart(DeclLeaf):
    kw_str = '(If)prog_unit_start'

class IfPUend(DeclLeaf):
    kw_str = '(If)prog_unit_end'

class BlockdataStmt(PUstart):
    kw = 'blockdata'
    kw_str = 'block data'
    _sons = ['name']

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(BlockdataStmt.kw_str), # 0 = stmt_name
                   id)                        # 1 = blockdataname
        ((stmt_name,blockdataname),rest) = form(scan)
        return BlockdataStmt(blockdataname,lineNumber=lineNumber,rest=rest)

    def __init__(self,blockdataname,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.name = blockdataname
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

class CommonStmt(Decl):
    kw = 'common'
    kw_str = kw
    _sons = ['declList']

    @classmethod
    def parse(cls,ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        stmt = seq(lit(CommonStmt.kw),lit('/'),id,lit('/'),cslist(Exp))
        ([common,slash1,name,slash2,declList],rm) = stmt(scan)
        return cls(name,declList,lineNumber,rest=rm)

    def __init__(self,name,declList=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.declList = declList
        self.name = name
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return self.kw + '/%s/ %s' % \
              (self.name,','.join(str(item) for item in self.declList))+ \
              ''.join(self.internal)

class _ImplicitDoConstruct(object):
    '''implicit do construct for DATA statements'''
    # data-implied-do object is one of
    #  array-element
    #  scalar-structure-component
    #  data-implied-do
    form = seq(lit('('),         # 0
               app,              # 1 = app
               lit(','),         # 2
               LoopControl.form, # 3 = loopControl
               lit(')'))         # 4
    form = treat(form, lambda x: _ImplicitDoConstruct(x[1],x[3]))

    #  form of data-implied-do:
    # ( data-implied-do-object-list , named-scalar-integer-variable = scalar-integer-expression , scalar-integer-expression [ , scalar-integer-expression ] )
    form = seq(lit('('),         # 0
               disj(app,         # 1 = object
                    form),
               lit(','),         # 2
               LoopControl.form, # 3 = loopControl
               lit(')'))         # 4
    form = treat(form, lambda x: _ImplicitDoConstruct(x[1],x[3]))

    def __init__(self,object,loopControl):
        self.object = object
        self.loopControl = loopControl

    def __str__(self):
        return '(%s, %s)' % (str(self.object),str(self.loopControl))

    def __repr__(self):
        return '%s(%s,%s)' % (self.__class__.__name__,repr(self.object),repr(self.loopControl))


class DataStmt(Decl):
    kw = 'data'
    kw_str = kw
    _sons = ['objectList','valueList']

    @staticmethod
    def parse(scan,lineNumber):
        # FIXME we don't cover the full range of possibilities.  In particular, here is an incomplete list of the issues:
        #  - there can be an entire comma-separated list of object-value pairs
        #  - The values should all be constants, and NOT general expressions:
        #      '1-1' is no good, but '-1' is.  It's just that '-1' doesnt match as a constant by the scanner (it matches as a unary expression)
        #      the definition of "const" can't be fixed easily due to scanner particulars (sometimes we want the '-' and '1' to be separate tokens, sometimes not)
        #  - we don't cover the optional repeat-factor for the value items (though this is handled by the fact that the values are parsed as expressions.  see previous item)

            # form of data-value:
            # [ repeat-factor * ] data-constant
                # data-constant is one of
                #  scalar-constant
                #  scalar-constant-subobject
                #  signed-integer-literal-constant
                #  signed-real-literal-constant
                #  null-initialization
                #  structure-constructor

        # form of DATA statement:
        # DATA data-statement-object-list / data-value-list / [ [ , ] data-statement-object-list / data-value-list / ] ...
            # form of data object is one of
            #  variable
            #  data-implied-do
        formDataStmt = seq(lit(DataStmt.kw),        # 0 = stmt_name
                           cslist(disj(_ImplicitDoConstruct.form, # 1 = objectList (variable or implicit do construct)
                                  id)),
                           lit('/'),           #
                           cslist(Exp),        # 3 = valueList
                           lit('/'))           #
        formDataStmt = treat(formDataStmt, lambda x: DataStmt(x[1],x[3],stmt_name=x[0],lineNumber=lineNumber))
        (theParsedStmt,rest) = formDataStmt(scan)
        theParsedStmt.rest=rest
        return theParsedStmt

    def __init__(self,objectList,valueList,stmt_name=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.objectList = objectList
        self.valueList = valueList
        self.stmt_name = stmt_name
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        # put a space after the data keyword iff the first object is a variable
        spaceStr = isinstance(self.objectList[0],str) and ' ' or ''
        return '%s%s%s / %s /' % (self.stmt_name,
                                  spaceStr,
                                  ', '.join([str(anObject) for anObject in self.objectList]),
                                  ', '.join([str(aValue) for aValue in self.valueList]))\
                                  +''.join(self.internal)

    def __repr__(self):
        return self.__class__.__name__ + \
               '(' + \
               ','.join([repr(aSon) for aSon in (self.objectList,self.valueList,self.stmt_name)]) + \
               ')'

class EndInterfaceStmt(DeclLeaf):
    'End of interface block'
    kw    = 'endinterface'
    kw_str = 'end interface'

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(EndInterfaceStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return EndInterfaceStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        DeclLeaf.__init__(self,lineNumber,label,lead,internal,rest)

class EndTypeStmt(DeclLeaf):
    'end of a type definition'
    kw     = 'endtype'
    kw_str = 'end type'

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(EndTypeStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return EndTypeStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        DeclLeaf.__init__(self,lineNumber,label,lead,internal,rest)    

class VarAttrib(Decl):
    _sons  = ['vlist']

    @classmethod
    def parse(cls,ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p0 = seq(lit(cls.kw),zo1(seq(lit('::'),cslist(id))))

        ((dc,vlist),r) = p0(scan)
        if vlist:
            vlist = vlist[0][1]
            return cls(vlist,lineNumber,rest=r)
        else:
            return cls(r,lineNumber,rest=r)

    def __init__(self,vlist,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.vlist = vlist
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, repr(self.vlist))

    def __str__(self):
        s_key = self.kw_str
        rem   = ''
        if self.vlist:
            rem = ' :: %s' % ','.join([str(v) for v in self.vlist])
        return s_key+rem+''.join(self.internal)

class PrivateStmt(VarAttrib):
    kw     = 'private'
    kw_str = kw
    _sons = ['vlist']
    
    def __init__(self,vlist,lineNumber=0,internal=[],rest=[]):
        VarAttrib.__init__(self,vlist,lineNumber,internal,rest)

class PublicStmt(VarAttrib):
    kw     = 'public'
    kw_str = kw
    _sons = ['vlist']

    def __init__(self,vlist,lineNumber=0,internal=[],rest=[]):
        VarAttrib.__init__(self,vlist,lineNumber,internal,rest)

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
    kw = 'implicit'
    kw_str = kw
    _sons = ['lst']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p0 = seq(lit(ImplicitStmt.kw),lit('none'))
        p0 = treat(p0,ImplicitNone)

        impelt1 = seq(type_pat_sem,lit('('),cslist(Exp),lit(')'))
        impelt1 = treat(impelt1,lambda l: (l[0],l[2]))

        impelt2 = type_pat_sem
        impelt2 = treat(impelt2,_extract_imp_elts)

        impelt = disj(impelt1,impelt2)

        p1 = seq(lit(ImplicitStmt.kw),
                 cslist(impelt))

        p1 = treat(p1,lambda l:ImplicitStmt(l[1],lineNumber))

        (v,r) = disj(p0,p1)(scan)
        v.rest = r
        if v.kw is 'implicit':
            type = []
            for item in v.rest:
                if item is '(' or len(item) < 2:
                    break
                else:
                    type.append(item)
            v.const_list = v.rest[len(type):]
            type = ' '.join(item.lower() for item in type)
            v.type = type.strip()
        return v

    def __init__(self,lst,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.lst  = lst 
        self.type = ''
        self.const_list = []
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'ImplicitStmt(%s)' % repr(self.lst)

    def __str__(self):

        def _helper(elt):
            (typ,explst) = elt
            return '%s (%s)' % (typestr2(typ),
                                ','.join([str(l).replace(' ','') \
                                          for l in explst]))+''.join(self.internal)
            
        return 'implicit %s' % ', '.join([_helper(e) for e in self.lst])\
            +self.type+''.join(self.const_list)\
            +''.join(self.internal)

class EquivalenceStmt(Decl):
    kw = 'equivalence'
    _sons = ['nlists']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        nlist = seq(lit('('),
                    cslist(Exp),
                    lit(')'))
        stmt = seq(lit(EquivalenceStmt.kw),cslist(nlist))
        ([equivalence,nlists],rm) = stmt(scan)
        return EquivalenceStmt(nlists,lineNumber,rest=rm)

    
    def __init__(self,nlists,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.nlists = nlists
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        declStrList = []
        for nlist in self.nlists:
            declStrList.append('('+','.join(str(item) for item in nlist[1])+')')
        return '%s %s' % (self.kw,','.join(declStrList))
        
aNamedParam = seq(id,lit('='),Exp)

class ParameterStmt(Decl):
    _sons = ['namedParamList']
    kw = 'parameter'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p0 = seq(lit(ParameterStmt.kw),
                 lit('('),
                 cslist(aNamedParam),
                 lit(')'))
        p0 = treat(p0,lambda l:ParameterStmt(l[2],lineNumber))
        (v,r) = p0(scan)
        v.rest=r
        return v
   
    def __init__(self,namedParamList,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.namedParamList = namedParamList
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'ParamterStmt(%s)' % ','.join([repr(aNamedParam) for aNamedParam in self.namedParamList])

    def __str__(self):
        return 'parameter (%s)' % ','.join([itemstr(aNamedParam) for aNamedParam in self.namedParamList])+''.join(self.internal)

class SaveStmt(Decl):
    'Save statement'
    kw    = 'save'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(SaveStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return SaveStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

class StmtFnStmt(Decl):
    _sons = ['args','body']

    def __init__(self,name,args,body,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.name = name
        self.args = args
        self.body = body
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'StmtFnStmt(%s,%s,%s)' % (repr(self.name),
                                         repr(self.args),
                                         repr(self.body))
    def __str__(self):
        return '%s(%s) = %s' % (str(self.name),
                                ','.join([str(l) for l in self.args]),
                                str(self.body))\
                                +''.join(self.internal)

    def get_name(self):
        return self.name

    def set_name(self,newName):
        if self.name != newName:
            self.name = newName
            self.modified = True

    def get_args(self):
        return self.args

    def set_args(self,newArgs):
        self.args = newArgs
        self.modified = True

    def get_body(self):
        return self.body

    def set_body(self,newBody):
        if self.body != newBody:
            self.body = newBody
            self.modified = True

class ExternalStmt(Decl):
    _sons = ['procedureNames']
    kw = 'external'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formExternalStmt = seq(lit(ExternalStmt.kw),
                               zo1(lit('::')),
                               cslist(id))
        ((externalKeyword,doubleColon,procedureNames),rest) = formExternalStmt(scan)
        return ExternalStmt(procedureNames,lineNumber,rest=rest)

    def __init__(self,procedureNames,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.procedureNames = procedureNames
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return self.__class__.__name__+'('+repr(self.procedureNames)+')'

    def __str__(self):
        return self.kw+' '+','.join([str(aProcedureName)
                                     for aProcedureName in
                                     self.procedureNames])\
                                     +''.join(self.internal)

class AllocatableStmt(Decl):
    _sons = ['lst']
    kw = 'allocatable'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formAllocatableStmt = seq(lit(AllocatableStmt.kw),
                               zo1(lit('::')),
                               cslist(id))
        ((allocatableKeyword,doubleColon,lst),rest) = formAllocatableStmt(scan)
        return AllocatableStmt(lst,lineNumber,rest=rest)

    def __init__(self,lst,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.lst = lst
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return self.__class__.__name__+'('+repr(self.lst)+')'

    def __str__(self):
        return self.kw+' '+','.join([str(aName) for aName in self.lst])\
               +''.join(self.internal)

class CharacterStmt(TypeDecl):
    kw = 'character'
    kw_str = kw
    _sons  = ['mod','attrs','decls']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        starmod  = seq(lit('('),lit('*'),lit(')'))
        starmod  = treat(starmod,lambda l: _Star())

        lenmod   = disj(Exp,starmod)
        f77mod   = seq(lit('*'),lenmod)
        f77mod   = treat(f77mod,lambda l: _F77Len(l[1]))

        f90mod   = seq(lit('('),disj(lit('*'),Exp),lit(')'))
        f90mod   = treat(f90mod,lambda l: _F90Len(l[1]))

        explLen  = seq(lit('('),
                       lit('len'),
                       lit('='),
                       disj(Exp,
                            lit('*')),
                       lit(')'))
        explLen  = treat(explLen,lambda l: _F90ExplLen(l[3]))
                
        p1 = seq(lit(CharacterStmt.kw),
                 zo1(disj(f77mod,f90mod,explLen)),type_attr_list,zo1(lit('::')),
                 cslist(char_decl_item))
        try: 
          ((dc,mod,attrs,dc1,decls),rest) = p1(scan)
        except AssemblerException,e:
          raise ParseError(lineNumber,scan,'character variable declaration')  

        return CharacterStmt(mod,attrs,decls,dc,lineNumber,rest=rest)

    def __init__(self,mod,attrs,decls,stmt_name=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.stmt_name = stmt_name
        TypeDecl.__init__(self,mod,attrs,decls,lineNumber,label,lead,internal,rest)

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
                                 ','.join([str(d) for d in self.decls]))\
                                 +''.join(self.internal)

class IntrinsicStmt(Decl):
    'Intrinsic stmt'
    kw    = 'intrinsic'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(IntrinsicStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return IntrinsicStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

class IncludeStmt(Decl):
    'Include stmt'
    kw    = 'include'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(IncludeStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return IncludeStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

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
    kw = 'dimension'
    kw_str = kw
    _sons = ['lst']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p1 = seq(lit(DimensionStmt.kw),
                 zo1(lit('::')),
                 cslist(app))
        ((dc,sep,lst),rest) = p1(scan)
        return DimensionStmt(lst,dc,lineNumber,rest=rest)

    def __init__(self,lst,stmt_name=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.lst = lst
        self.stmt_name = stmt_name
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,repr(self.lst))

    def __str__(self):
        return '%s %s' % (self.stmt_name,','.join([str(l) for l in self.lst]))\
               +''.join(self.internal)

    def get_lst(self):
        return self.lst

    def set_lst(self,newLst):
        self.lst = newLst
        self.modified = True

class IntentStmt(Decl):
    'Intent stmt'
    kw    = 'intent'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(IntentStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return IntentStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

class OptionalStmt(Decl):
    'Optional stmt'
    kw    = 'optional'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(OptionalStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return OptionalStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

class NamelistStmt(Decl):
    'Namelist stmt'
    kw    = 'namelist'
    kw_str = kw
    _sons = ['namVarListPairs']

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        namVarListPair = seq(lit('/'),
                             id,         # 1 = namlist
                             lit('/'),
                             cslist(id)) # 3 = varlist
        namVarListPair = treat(namVarListPair, lambda x:(x[1],x[3]))

        form = seq(lit(NamelistStmt.kw_str), # 0 = stmt_name
                   star(namVarListPair))     # 1 = namVarListPairs
        ((name,namVarListPairs),rest) = form(scan)
        return NamelistStmt(namVarListPairs,stmt_name=name,lineNumber=lineNumber,rest=rest)

    def __init__(self,namVarListPairs,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.namVarListPairs = namVarListPairs
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        nameVarGroups = ''
        for pair in self.namVarListPairs:
            pair = '/%s/ %s' % (pair[0],','.join(pair[1]))
            if nameVarGroups == '':
                nameVarGroups = pair
            else:
                nameVarGroups += ' '+pair
        return '%s %s' % (self.kw_str,nameVarGroups)

class SubroutineStmt(PUstart):
    kw = 'subroutine'
    kw_str = kw
    utype_name = kw
    _sons = ['args']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p1 = seq(lit(SubroutineStmt.kw),
                 id,
                 zo1(seq(lit('('),cslist(id),lit(')')))
                 )
        ((dc,name,args),rst) = p1(scan)
        if args:
            (dc,args,dc1) = args[0]

        return SubroutineStmt(name,args,lineNumber=lineNumber,rest=rst)

    def __init__(self,name,args,stmt_name=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.name = name
        self.args = args
        self.stmt_name = stmt_name
        PUstart.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return '%s(%s,%s)' % (self.__class__.__name__,
                              repr(self.name),
                              repr(self.args))
    def __str__(self):
        return '%s %s(%s)' % (self.stmt_name,self.name,
                                      ','.join([str(d) for d in self.args]))\
                                      +''.join(self.internal)

class ProgramStmt(PUstart):
    kw = 'program'
    kw_str = kw
    utype_name = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p1 = seq(lit(ProgramStmt.kw),
                 id)
        ((dc,name),rest) = p1(scan)
        return ProgramStmt(name,dc,lineNumber=lineNumber,rest=rest)

    def __init__(self,name,stmt_name=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.name = name
        self.stmt_name = stmt_name 
        PUstart.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,repr(self.name))

    def __str__(self):
        return '%s %s' % (self.stmt_name,self.name)+''.join(self.internal)

class FunctionStmt(PUstart):
    kw = 'function'
    kw_str = kw
    utype_name = kw
    _sons = ['ty','args']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p1 = seq(zo1(type_pat_sem),
                 lit(FunctionStmt.kw),
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
        return FunctionStmt(type,name,args,result,lineNumber,rest=rest)

    def __init__(self,ty,name,args,result,lineNumber=0,label=False,lead='',internal=[],rest=[]):
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
        PUstart.__init__(self,lineNumber,label,lead,internal,rest)

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
                                        resultStr)\
                                        +''.join(self.internal)

class ModuleStmt(PUstart):
    kw = 'module'
    kw_str = kw
    utype_name = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        p1 = seq(lit(ModuleStmt.kw),
                 id)
        ((dc,name),rest) = p1(scan)
        return ModuleStmt(name,lineNumber,rest=rest)

    def __init__(self,name,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.name = name 
        PUstart.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                              repr(self.name))
    def __str__(self):
        return 'module %s' % self.name+''.join(self.internal)

class UseStmt(Decl):
    kw     = 'use'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        # forms of renameItem: (we currently support only the first)
        # [ local-name => ] module-entity-name
        # [ OPERATOR ( local-defined-operator ) => ] OPERATOR ( module-defined-operator )
        formRenameItem = seq(id,
                             lit('=>'),
                             id)
        formRenameItem = treat(formRenameItem, lambda x: _PointerInit(x[0],x[2]))

        # use statement with NO ONLY: (we currently don't support module nature)
        # USE [[ , module-nature ] :: ] module-name [ , rename-list ]
        formUseAllStmt = seq(lit(UseStmt.kw),
                             id,
                             zo1(seq(lit(','),
                                     cslist(formRenameItem))))
        formUseAllStmt = treat(formUseAllStmt, lambda x: UseAllStmt(x[1],x[2] and x[2][0][1] or None,x[0],lineNumber))

        # forms of onlyItem:
        # generic-name
        # OPERATOR ( module-defined-operator )    (not currently supported)
        # ASSIGNMENT ( = )                        (not currently supported)
        # dtio-generic-spec                       (not currently supported)
        # module-entity-name
        # rename
        formOnlyItem = disj(formRenameItem,
                            id)

        # use statement WITH ONLY: (we currently don't support module nature)
        # USE [[ , module-nature ] :: ] module-name , ONLY : [ only-list ]
        formUseOnlyStmt = seq(lit(UseStmt.kw),
                              id,
                              lit(','),
                              lit('only'),
                              lit(':'),
                              cslist(formOnlyItem))
        formUseOnlyStmt = treat(formUseOnlyStmt, lambda x: UseOnlyStmt(x[1],x[5],x[0],lineNumber))

        (theParsedStmt,rest) = disj(formUseOnlyStmt,formUseAllStmt)(scan)
        theParsedStmt.rest = rest
        return theParsedStmt

class UseAllStmt(UseStmt):
    _sons  = ['renameList']

    def __init__(self,moduleName,renameList,stmt_name=UseStmt.kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.moduleName = moduleName
        self.renameList = renameList
        self.stmt_name = stmt_name
        UseStmt.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        renameListStr = self.renameList and ', '+\
                        ','.join([str(aRenameItem)
                                  for aRenameItem in self.renameList]) \
                                         or ''
        return self.stmt_name+' '+str(self.moduleName)+renameListStr\
               +''.join(self.internal)

class UseOnlyStmt(UseStmt):
    _sons  = ['onlyList']

    def __init__(self,moduleName,onlyList,stmt_name=UseStmt.kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.moduleName = moduleName
        self.onlyList = onlyList
        self.stmt_name = stmt_name
        UseStmt.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return self.stmt_name+' '+str(self.moduleName)+', only: '+\
               ','.join([str(anOnlyItem) for anOnlyItem in self.onlyList])\
               +''.join(self.internal)

class EntryStmt(Decl):
    'Entry stmt'
    kw    = 'entry'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(EntryStmt.kw)) # 0 = stmt_name
        (id,rest) = form(scan)
        return EntryStmt(lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Decl.__init__(self,lineNumber,label,lead,internal,rest)

class ExitStmt(Exec):
    kw = 'exit'
    kw_str = kw
    _sons = ['optionalDoConstructName']

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(ExitStmt.kw), # 0 = stmt_name
                   zo1(id))          # 1 = optionalDoConstructName
        form = treat(form, lambda x: ExitStmt(x[1] and x[1][0] or None,
                                              x[0]))
        (theParsedStmt,rest) = form(scan)
        theParsedStmt.rest = rest
        return theParsedStmt 

    def __init__(self,optionalDoConstructName,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.optionalDoConstructName = optionalDoConstructName
        self.stmt_name = stmt_name
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self) :
        optionalDoConstructStr = self.optionalDoConstructName and ' '+self.optionalDoConstructName \
                                                               or '' 
        return '%s%s' % (self.stmt_name,optionalDoConstructStr)+''.join(self.internal)


class EnddoStmt(Exec):
    kw = 'enddo'
    kw_str = 'end do'
    _sons = ['doConstructName']

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(EnddoStmt.kw), # 0 = stmt_name
                   zo1(id))           # 1 = doConstructName
        form = treat(form, lambda x: EnddoStmt(x[1] and x[1][0] or None))
        (theParsedStmt,rest) = form(scan)
        theParsedStmt.rest = rest
        return theParsedStmt 

    def __init__(self,doConstructName,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.doConstructName = doConstructName
        self.stmt_name = stmt_name
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self) :
        optionalDoConstructStr = self.doConstructName and ' '+self.doConstructName \
                                                       or '' 
        return '%s %s' % (self.stmt_name,optionalDoConstructStr)+''.join(self.internal)


class CycleStmt(Exec) :
    kw = 'cycle'
    kw_str = kw
    _sons = ['doConstructName']

    @staticmethod
    def parse(ws_scan,lineNumber) :
        scan = filter(lambda x: x != ' ',ws_scan)
        form = seq(lit(CycleStmt.kw), # 0 = stmt_name
                   zo1(id))           # 1 = doConstructName
        form = treat(form, lambda x: CycleStmt(x[1] and x[1][0] or None,
                                               x[0]))
        (theParsedStmt,rest) = form(scan)
        theParsedStmt.rest = rest
        return theParsedStmt 

    def __init__(self,doConstructName,stmt_name=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.doConstructName = doConstructName
        self.stmt_name = stmt_name
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self) :
        optionalDoConstructStr = self.doConstructName and ' '+self.doConstructName \
                                                       or '' 
        return '%s%s' % (self.stmt_name,optionalDoConstructStr)+''.join(self.internal)


class CallStmt(Exec):
    kw = 'call'
    kw_str = kw
    _sons = ['args']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        prefix = seq(lit(CallStmt.kw),disj(app,id))
        ((dc,a),rst) = prefix(scan)
        if (isinstance(a,App)):
            return CallStmt(a.head,a.args,dc,lineNumber,rest=rst)
        else:
            return CallStmt(a,[],dc,lineNumber,rest=rst)

    def __init__(self,head,args,stmt_name=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.head = head
        self.args = args
        self.stmt_name = stmt_name
        Exec.__init__(self,lineNumber,label,lead,internal,rest)
        
    def __repr__(self):
        return 'CallStmt(%s,%s)' % (repr(self.head),
                                    repr(self.args),)

    def __str__(self):
        return '%s %s(%s)' % (self.stmt_name,str(self.head),
                                ','.join([str(l) for l in self.args]))\
                                +''.join(self.internal)

    def get_head(self):
        return self.head

    def set_head(self,newHead):
        if self.head != newHead:
            self.head = newHead
            self.modified = True

    def get_args(self):
        return self.args

    def set_args(self,newArgs):
        self.args = newArgs
        self.modified = True

class AssignStmt(Exec):
    _sons = ['lhs','rhs']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        if (not '=' in scan):
            raise AssemblerException(scan,lineNumber)
        lhsEq   = seq(lv_exp,lit('='))
        ((l,dc),rst) = lhsEq(scan)
        ((r),rst) = Exp(rst)
        return AssignStmt(l,r,lineNumber,rest=rst)

    def __init__(self,lhs,rhs,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.lhs  = lhs
        self.rhs  = rhs
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'AssignStmt(%s,%s)' % (repr(self.lhs),repr(self.rhs))

    def __str__(self):
        return '%s = %s' % (str(self.lhs),str(self.rhs))+''.join(self.internal)

    def get_lhs(self):
        return self.lhs

    def set_lhs(self,newLhs):
        if lhs != newLhs:
            self.lhs = newLhs
            self.modified = True

    def get_rhs(self):
        return self.rhs

    def set_rhs(self,newRhs):
        if rhs != newRhs:
            self.rhs = newRhs
            self.modified = True

class PointerAssignStmt(Exec):
    _sons = ['lhs','rhs']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formPointerAssignStmt = seq(lv_exp,
                                    lit('=>'),
                                    Exp)
        ((lhs,assignSymbol,rhs),rst) = formPointerAssignStmt(scan)
        return PointerAssignStmt(lhs,rhs,lineNumber,rest=rst)

    def __init__(self,lhs,rhs,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.lhs  = lhs
        self.rhs  = rhs
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'PointerAssignStmt(%s,%s)' % (repr(self.lhs),repr(self.rhs))

    def __str__(self):
        return '%s => %s' % (str(self.lhs),str(self.rhs))+''.join(self.internal)

class OpenStmt(Exec):
    kw = 'open'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        unitSpec = seq(zo1(seq(lit('unit'),lit('='))),
                       disj(int,id))
        unitSpec = treat(unitSpec, lambda x: x[-1])
        formOpenStmt = seq(lit(OpenStmt.kw),
                           lit('('),
                           unitSpec,
                           zo1(seq(lit(','),
                                   cslist(disj(seq(lit('access'),lit('='),Exp),
                                               seq(lit('action'),lit('='),Exp),
                                               seq(lit('blank'),lit('='),Exp),
                                               seq(lit('blocksize'),lit('='),Exp),
                                               seq(lit('err'),lit('='),Exp),
                                               seq(lit('file'),lit('='),Exp),
                                               seq(lit('form'),lit('='),Exp),
                                               seq(lit('iostat'),lit('='),Exp),
                                               seq(lit('mode'),lit('='),Exp),
                                               seq(lit('position'),lit('='),Exp),
                                               seq(lit('recl'),lit('='),Exp),
                                               seq(lit('share'),lit('='),Exp),
                                               seq(lit('status'),lit('='),Exp),
                                               seq(lit('iofocus'),lit('='),Exp),
                                               seq(lit('title'),lit('='),Exp))))),
                           lit(')'))
        
        ((stmt_name,lparen,unitspec,params,rparen),rst) = formOpenStmt(scan)
        return OpenStmt(unitspec,params,lineNumber,rest=rst)

    def __init__(self,unitspec,params=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.unitspec = unitspec
        if len(params) > 0:
            self.params = params[0][1]
        else: self.params = []
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        if self.params == []:
            return '%s (%s)' % (self.kw,self.unitspec)            
        paramlist = []
        for param in self.params:
            paramlist.append(''.join(str(elt) for elt in param))
        return '%s (%s)' % (self.kw,self.unitspec+','+','.join(paramlist))

class CloseStmt(Exec):
    kw = 'close'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        unitSpec = seq(zo1(seq(lit('unit'),lit('='))),
                       disj(int,id))
        unitSpec = treat(unitSpec, lambda x: x[-1])
        formCloseStmt = seq(lit(CloseStmt.kw),
                           lit('('),
                           unitSpec,
                           zo1(seq(lit(','),
                                   cslist(disj(seq(lit('err'),lit('='),Exp),
                                               seq(lit('iostat'),lit('='),Exp),
                                               seq(lit('status'),lit('='),Exp))))),
                           lit(')'))
        
        ((stmt_name,lparen,unitspec,params,rparen),rst) = formCloseStmt(scan)
        return CloseStmt(unitspec,params,lineNumber,rest=rst)

    def __init__(self,unitspec,params=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.unitspec = unitspec
        if len(params) > 0:
            self.params = params[0][1]
        else: self.params = []
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        if self.params == []:
            return '%s (%s)' % (self.kw,self.unitspec)            
        paramlist = []
        for param in self.params:
            paramlist.append(''.join(str(elt) for elt in param))
        return '%s (%s)' % (self.kw,self.unitspec+','+','.join(paramlist))


class IOStmt(Exec):

    def __init__(self,stmt_name,ioCtrlSpecList,itemList=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.stmt_name = stmt_name
        self.ioCtrlSpecList=ioCtrlSpecList
        self.itemList=itemList
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def get_itemList(self):
        return self.itemList

    def set_itemList(self,newItemList):
        self.itemList = newItemList
        self.modified = True

class SimpleSyntaxIOStmt(IOStmt):

    @staticmethod
    def parse(ws_scan,lineNumber,kw,SubClass):
        scan = filter(lambda x: x != ' ',ws_scan)
        io_stmt = seq(lit(kw),disj(lit('*'),Exp),lit(','),cslist(Exp))
        ([kw,format,comma,itemList],rest) = io_stmt(scan)
        return SubClass(kw,format,itemList,lineNumber,rest=rest)

    def __init__(self,stmt_name,format,itemList=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        IOStmt.__init__(self,stmt_name,[format],itemList,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return '%s %s,%s' % (self.stmt_name,self.ioCtrlSpecList[0],\
                             ','.join([str(item) for item in self.itemList]))\
                             +''.join(self.internal)

class PrintStmt(SimpleSyntaxIOStmt):
    kw = 'print'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        return SimpleSyntaxIOStmt.parse(scan,lineNumber,PrintStmt.kw, PrintStmt)

    def __init__(self,kw,format,itemList,lineNumber=0,internal=[],rest=[]):
        SimpleSyntaxIOStmt.__init__(self,kw,format,itemList,lineNumber,internal,rest)

class ComplexSyntaxIOStmt(IOStmt):

    @staticmethod
    def parse(ws_scan,lineNumber,kw,SubClass):
        scan = filter(lambda x: x != ' ',ws_scan)
        io_stmt = seq(lit(kw),
                      lit('('),
                      cslist(disj(lit('*'),NamedParmExpWithStar,Exp)),
                      lit(')'),
                      cslist(Exp))
        ([kw,lbracket,ioCtrlSpecList,rbracket,itemList],rest) = io_stmt(scan)

        return SubClass(kw,ioCtrlSpecList,itemList,lineNumber=lineNumber,rest=rest)

    def __init__(self,stmt_name,ioCtrlSpecList,itemList=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        IOStmt.__init__(self,stmt_name,ioCtrlSpecList,itemList,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return '%s(%s) %s' % (self.stmt_name,
                              ','.join([str(ioCtrl)
                                        for ioCtrl in self.ioCtrlSpecList]),
                              ','.join([str(item) for item in self.itemList]))\
                              +''.join(self.internal)

class SimpleReadStmt(SimpleSyntaxIOStmt):
    ''' the version that only has format but not a full ioCtrlSpecList; its parse method
    is only called as a fallback on failure of ReadStmt.parse '''
    kw = 'read'
    kw_str = kw
    
    @staticmethod
    def parse(ws_scan,lineNumber):
        return SimpleSyntaxIOStmt.parse(ws_scan,lineNumber,SimpleReadStmt.kw, SimpleReadStmt)

    def __init__(self,kw,format,itemList,lineNumber=0,internal=[],rest=[]):
        SimpleSyntaxIOStmt.__init__(self,kw,format,itemList,lineNumber,internal,rest)

class ReadStmt(ComplexSyntaxIOStmt):
    kw = 'read'
    kw_str = kw
    _sons = ['ioCtrlSpecList','itemList']
    
    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        try : 
            return ComplexSyntaxIOStmt.parse(scan,lineNumber,ReadStmt.kw,ReadStmt)
        except ListAssemblerException,e:
            return SimpleReadStmt.parse(scan,lineNumber)

    def __init__(self,stmt_name=kw,ioCtrlSpecList=[],itemList=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        IOStmt.__init__(self,stmt_name,ioCtrlSpecList,itemList,lineNumber,label,lead,internal,rest)
    
class WriteStmt(ComplexSyntaxIOStmt):
    kw = 'write'
    kw_str = kw
    _sons = ['ioCtrlSpecList','itemList']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        return ComplexSyntaxIOStmt.parse(scan,lineNumber,WriteStmt.kw,WriteStmt)

    # rest is a temp. fix. implicit loops in WriteStmts should be
    # fully parsed in ComplexSyntaxIOStmt
    def __init__(self,stmt_name=kw,ioCtrlSpecList=[],itemList=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        ComplexSyntaxIOStmt.__init__(self,stmt_name,ioCtrlSpecList,itemList,lineNumber,label,lead,internal,rest)

class FormatStmt(Exec):
    kw = 'format'
    kw_str = kw

#     @staticmethod
#     def parse(ws_scan,lineNumber):
#         scan = filter(lambda x: x != ' ',ws_scan)
#         formFormatStmt = seq(lit(FormatStmt.kw),
#                              lit('('),
#                              cslist(Exp),
#                              lit(')'))
#         ((stmt_name,lparen,editlist,rparen),rst) = formFormatStmt(scan)
#         return FormatStmt(editlist,lineNumber)
    
#     def __init__(self,editlist,lineNumber=0,label=False,lead='',internal=[],rest=[]):
#         self.editlist = editlist
#         Exec.__init__(self,lineNumber,label,lead,internal,rest)
        
#     def __str__(self):
#         return '%s(%s)' % (self.kw_str,','.join(self.editlist))

class StopStmt(Exec):
    kw = 'stop'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formStopStmt = seq(lit(StopStmt.kw),
                           zo1(disj(const,int)))
        ((stmt_name,msg),rst) = formStopStmt(scan)
        if len(msg) > 0:
            return StopStmt(msg[0],lineNumber)
        else:
            return StopStmt(lineNumber=lineNumber)

    def __init__(self,msg='',lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.msg = msg
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return '%s %s' % (self.kw,self.msg)

class ReturnStmt(Leaf):
    kw = 'return'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formReturnStmt = seq(lit(ReturnStmt.kw),
                             zo1(int))
        ((stmt_name,ordinal),rst) = formReturnStmt(scan)
        if len(ordinal) > 0:
            return ReturnStmt(ordinal[0],lineNumber)
        return ReturnStmt(lineNumber=lineNumber)

    def __init__(self,ordinal=-1,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.ordinal = ordinal
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        if self.ordinal >= 0:
            return '%s %d' % (self.kw,self.ordinal)
        else:
            return self.kw

class IfStmt(Exec):
    kw = 'if'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        prefix = seq(lit(IfStmt.kw),lit('('),Exp,lit(')'))
        ((ifLit,dc1,test,dc2),rest) = prefix(scan)
        index = -1
        while index < (len(scan)-len(rest)):
            index = ws_scan[index+1:].index(rest[0])+(index+1)
        if [l.lower() for l in rest] == ['then']:
            return IfThenStmt(test,ifLit,ws_scan[index],lineNumber,rest=rest)
        else:
            return IfNonThenStmt(test,_kw_parse(ws_scan[index:],lineNumber),ifLit,lineNumber,rest=rest)

class IfThenStmt(IfStmt):
    _sons = ['test']

    def __init__(self,test,ifFormatStr=IfStmt.kw,thenFormatStr='then',lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.test = test
        self.ifFormatStr = ifFormatStr
        self.thenFormatStr = thenFormatStr
        IfStmt.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'IfThenStmt(%s)' % (repr(self.test),)

    def __str__(self):
        return '%s (%s) %s' % (self.ifFormatStr,str(self.test),self.thenFormatStr)\
               +''.join(self.internal)

class IfNonThenStmt(IfStmt):
    _sons = ['test','stmt']

    def __init__(self,test,stmt,ifFormatStr=IfStmt.kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.test = test
        self.stmt = stmt
        self.ifFormatStr = ifFormatStr
        IfStmt.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'IfNonThenStmt(%s,%s)' % (repr(self.test),
                                         repr(self.stmt))

    def __str__(self):
        return '%s (%s) %s' % (self.ifFormatStr,str(self.test),str(self.stmt))\
               +''.join(self.internal)


class ElseifStmt(Exec):
    kw = 'elseif'
    kw_str = kw
    _sons = ['test']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        prefix = seq(lit(ElseifStmt.kw),lit('('),Exp,lit(')'),lit('then'))

        ((dc0,dc1,e,dc2,dc3),rest) = prefix(scan)
        return ElseifStmt(e,dc0,dc3,lineNumber,rest=rest)

    def __init__(self,e,stmt_name=kw,stmt_name2='then',lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.test = e
        self.stmt_name = stmt_name
        self.stmt_name2 = stmt_name2
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __repr__(self):
        return 'ElseifStmt(%s)' % (repr(self.test),)

    def __str__(self):
        return '%s (%s) %s' % (self.stmt_name,str(self.test),self.stmt_name2)\
               +''.join(self.internal)
    
class ElseStmt(Leaf):
    kw = 'else'
    kw_str = kw

class WhereStmt(Exec):
    ''' WHERE ( logical-expression ) array-assignment-statement'''
    kw = 'where'
    kw_str = kw
    _sons = ['conditional','assignment']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        lhs = lv_exp

        formAssign = seq(lv_exp,lit('='),Exp)
        formAssign = treat(formAssign,lambda x:AssignStmt(x[0],x[2],lineNumber=lineNumber))

        formWhereStmt = seq(lit(WhereStmt.kw),
                            lit('('),
                            Exp,
                            lit(')'),
                            zo1(formAssign))
        ((whereKW,oPeren,conditional,cPeren,assignment),rest) = formWhereStmt(scan)
        assignment = assignment and assignment[0] \
                                 or None
        return WhereStmt(conditional,assignment,lineNumber,rest=rest)

    def __init__(self,conditional,assignment,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.conditional = conditional
        self.assignment = assignment
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        assignStr = self.assignment and ' '+str(self.assignment) \
                                     or ''
        return '%s (%s)%s' % (self.kw,
                               str(self.conditional),
                               assignStr)\
                               +''.join(self.internal)

class ElsewhereStmt(Leaf):
    kw = 'elsewhere'
    kw_str = kw

class EndWhereStmt(Leaf):
    kw = 'endwhere'
    kw_str = 'end where'

class EndifStmt(Leaf):
    kw = 'endif'
    kw_str = 'end if'

class EndStmt(PUend):
    kw = 'end'
    kw_str = kw

    def is_uend(self,unit=_non): return True
    def is_decl(self,unit=_non):
        return unit.val._in_iface
    def is_exec(self,unit=_non): return False

    def __init__(self,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        PUend.__init__(self,lineNumber,label,lead,internal,rest)
        
class ComplexEndStmt(EndStmt):

    @staticmethod
    def parse(ws_scan,lineNumber,kw,SubClass):
        scan = filter(lambda x: x != ' ',ws_scan)
        formEndStmt = seq(lit(kw),
                          zo1(id))
        ((stmt_name,name),rst) = formEndStmt(scan)
        if name == []:
            return SubClass(lineNumber=lineNumber)
        else:
            return SubClass(name[0],lineNumber)

    def __init__(self,name='',lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.name = name
        EndStmt.__init__(self,lineNumber,label,lead,internal,rest)
        

    def __str__(self):
        return '%s %s' % (self.kw_str,self.name)
    

class EndPseudoStmt(GenStmt):
    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        if len(scan) >= 2 and scan[1].lower() == 'interface':
            return EndInterfaceStmt.parse(scan,lineNumber,rest=rest)
        return EndStmt.parse(scan,lineNumber,rest=rest)


class EndModuleStmt(ComplexEndStmt):
    kw =  'endmodule'
    kw_str = 'end module'

    @staticmethod
    def parse(ws_scan,lineNumber):
        return ComplexEndStmt.parse(ws_scan,lineNumber,EndModuleStmt.kw,EndModuleStmt)

    def __init__(self,name='',lineNumber=0,label=False,lead='',internal=[],rest=[]):
        ComplexEndStmt.__init__(self,name,lineNumber,label,lead,internal,rest)

class EndProgramStmt(ComplexEndStmt):
    kw    = 'endprogram'
    kw_str = 'end program'

    @staticmethod
    def parse(ws_scan,lineNumber):
        return ComplexEndStmt.parse(ws_scan,lineNumber,EndProgramStmt.kw,EndProgramStmt)

    def __init__(self,name='',lineNumber=0,label=False,lead='',internal=[],rest=[]):
        ComplexEndStmt.__init__(self,name,lineNumber,label,lead,internal,rest)

class EndFunctionStmt(ComplexEndStmt):
    'End of function block'
    kw    = 'endfunction'
    kw_str = 'end function'

    @staticmethod
    def parse(ws_scan,lineNumber):
        return ComplexEndStmt.parse(ws_scan,lineNumber,EndFunctionStmt.kw,EndFunctionStmt)

    def __init__(self,name='',lineNumber=0,label=False,lead='',internal=[],rest=[]):
        ComplexEndStmt.__init__(self,name,lineNumber,label,lead,internal,rest)

class EndSubroutineStmt(ComplexEndStmt):
    'End of subroutine block'
    kw    = 'endsubroutine'
    kw_str = 'end subroutine'

    @staticmethod
    def parse(ws_scan,lineNumber):
        return ComplexEndStmt.parse(ws_scan,lineNumber,EndSubroutineStmt.kw,EndSubroutineStmt)

    def __init__(self,name='',lineNumber=0,label=False,lead='',internal=[],rest=[]):
        ComplexEndStmt.__init__(self,name,lineNumber,label,lead,internal,rest)

class DoStmt(Exec):
    #FIXME: optional comma isn't handled
    '''
    [do-construct-name :] do [label] [loop control]
    '''
    kw = 'do'
    kw_str = kw
    _sons = ['doName','doLabel','loopControl']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formDoName = seq(id,
                         lit(':'))
        formDoName = treat(formDoName, lambda x: x[0])
        formDoStmt = seq(zo1(formDoName),        # 0 = doName
                         lit(DoStmt.kw),         # 1 = theDoKeyword
                         zo1(int),               # 2 = doLabel
                         zo1(LoopControl.form))  # 3 = loopControl
        formDoStmt = treat(formDoStmt, lambda x: DoStmt(x[0] and x[0][0] or None,
                                                        x[2] and x[2][0] or None,
                                                        x[3] and x[3][0] or None,
                                                        x[1]))
        (theParsedStmt,rest) = formDoStmt(scan)
        theParsedStmt.rest = rest
        return theParsedStmt 

    def __init__(self,doName,doLabel,loopControl,doFormatStr=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.doName = doName
        self.doLabel = doLabel
        self.loopControl = loopControl
        self.doFormatStr = doFormatStr
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        doNameString = self.doName and str(self.doName)+': ' \
                                    or ''
        doLabelString = self.doLabel and ' '+str(self.doLabel) \
                                      or ''
        loopControlString = self.loopControl and ' '+str(self.loopControl) \
                                              or ''
        return '%s%s%s%s' % (doNameString,self.doFormatStr,doLabelString,loopControlString)\
               +''.join(self.internal)


class WhileStmt(Exec):
    #FIXME: optional construct name, label, and comma are not handled
    '''
    [do-construct-name : ] DO [ label ] [ , ] while ( scalar-logical-expression )
    '''
    _sons = ['testExpression']
    kw = 'dowhile'
    kw_str = 'do while'

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formWhileStmt = seq(lit(WhileStmt.kw),
                            lit('('),
                            Exp,
                            lit(')'))
        ((theDoWhileKeyword,openPeren,theTestExpression,closePeren),rest) = formWhileStmt(scan)
        return WhileStmt(theTestExpression,theDoWhileKeyword,lineNumber,rest=rest)

    def __init__(self,testExpression,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.testExpression = testExpression
        self.stmt_name = stmt_name 
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return 'do while (%s)' % str(self.testExpression)+''.join(self.internal)


class ContinueStmt(Leaf):
    kw = 'continue'
    kw_str = kw


class SelectCaseStmt(Exec):
    #FIXME: optional case construct name 
    '''
    [case-construct-name :] select case (case-expression)
    '''
    _sons = ['caseExpression']
    kw = 'selectcase'
    kw_str = 'select case'

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formSelectCaseStmt = seq(lit(SelectCaseStmt.kw),
                                 lit('('),
                                 Exp,
                                 lit(')'))
        try:
            ((selectCaseKeyword,openPeren,caseExpression,closePeren),rest) = formSelectCaseStmt(scan)
        except ListAssemblerException,e:
            raise ParseError(lineNumber,scan,'Select Case statement')
        return SelectCaseStmt(caseExpression,selectCaseKeyword,lineNumber,rest=rest)

    def __init__(self,caseExpression,stmt_name=kw_str,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.caseExpression = caseExpression
        self.stmt_name = stmt_name
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return '%s (%s)' % (self.stmt_name,str(self.caseExpression))+''.join(self.internal)

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
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formCaseDefaultStmt = seq(lit(CaseDefaultStmt.kw))
        try:
            ((caseDefaultKeyword),rest) = formCaseDefaultStmt(scan)
        except ListAssemblerException,e:
            raise ParseError(lineNumber,scan,'case default statement')
        return CaseDefaultStmt(lineNumber,rest=rest)

    def __init__(self,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return 'case default'+''.join(self.internal)

class CaseRangeListStmt(Exec):
    #FIXME: optional case construct name 
    '''
    case (case-value-range-list) [case-construct-name]
    '''
    _sons = ['caseRangeList']
    kw = 'case'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formRangeItem = disj(lower_bound_range,
                             upper_bound_range,
                             Exp)
        formCaseRangeListStmt = seq(lit(CaseRangeListStmt.kw),
                                    lit('('),
                                    cslist(formRangeItem),
                                    lit(')'))
        try:
            ((caseKeyword,openPeren,caseRangeList,closePeren),rest) = formCaseRangeListStmt(scan)
        except ListAssemblerException,e:
            raise ParseError(lineNumber,scan,'case range list statement')
        return CaseRangeListStmt(caseRangeList,caseKeyword,lineNumber,rest=rest)

    def __init__(self,caseRangeList,stmt_name=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.caseRangeList = caseRangeList
        self.stmt_name = stmt_name
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return '%s (%s)' % (self.stmt_name,
                            ','.join([str(range) for range in self.caseRangeList]))\
                            +''.join(self.internal)

class GotoStmt(Exec):
    kw = 'goto'
    kw_str = 'go to'
    _sons = ['targetLabel']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        noSpace = seq(lit(GotoStmt.kw),
                      int)

        withSpace = seq(lit('go'),
                        lit('to'),
                        int)
        withSpace = treat(withSpace, lambda x: (x[0]+' '+x[1],x[2]))

        ((gotoFormatStr,targetLabel),rest) = disj(withSpace,noSpace)(scan)
        return GotoStmt(targetLabel,gotoFormatStr,lineNumber,rest=rest)

    def __init__(self,targetLabel,gotoFormatStr=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.targetLabel = targetLabel
        self.gotoFormatStr = gotoFormatStr
        Exec.__init__(self,lineNumber,label,lead,internal,rest)
        
    def __str__(self):
        return self.gotoFormatStr+' '+self.targetLabel+''.join(self.internal)

class AllocateStmt(Exec):
    '''
    For the time being, we do not require proper parsing of allocate and deallocate statements.
    We have commented out this (incomplete) implementation of the parsing, because it covers many but not all of the cases.
    '''
    kw = 'allocate'
    kw_str = kw
    _sons = ['argList','statVariable']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formAllocateStmt = seq(lit(AllocateStmt.kw), # 0 = allocateFormatStr
                               lit('('),             # 1
                               cslist(Exp),          # 2 = argList
                               lit(')'))
        formAllocateStmt = treat(formAllocateStmt, lambda x: AllocateStmt(x[2],allocateFormatStr=x[0],lineNumber=lineNumber))
        if scan[-4].lower() == 'stat':
            newScan = scan[0:-5]
            newScan.append(')')
            (theParsedStmt,rest) = formAllocateStmt(newScan)
            theParsedStmt.statFormatStr = scan[-4]
            theParsedStmt.statVariable = scan[-2]
            theParsedStmt.rest = rest
            return theParsedStmt
        else:
            (theParsedStmt,rest) = formAllocateStmt(scan)
            theParsedStmt.rest = rest
            return theParsedStmt

    def __init__(self,argList,statVariable=None,statFormatStr='stat',allocateFormatStr=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.argList = argList
        self.statVariable = statVariable
        self.allocateFormatStr = allocateFormatStr
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        statVarStr = self.statVariable and ',stat='+self.statVariable \
                                   or ''
        return '%s(%s%s)' % (self.allocateFormatStr,
                             ','.join([str(arg) for arg in self.argList]),
                             statVarStr)\
                             +''.join(self.internal)

class DeallocateStmt(Exec):
    kw = 'deallocate'
    kw_str = kw
    _sons = ['argList']

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formDeallocateStmt = seq(lit(DeallocateStmt.kw),
                                 lit('('),
                                 cslist(Exp),
                                 lit(')'))
        ((deallocKeyword,oParen,argList,cParen),rest) = formDeallocateStmt(scan)
        return DeallocateStmt(argList,deallocKeyword,lineNumber,rest=rest)

    def __init__(self,argList,deallocateFormatStr=kw,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.argList = argList
        self.deallocateFormatStr = deallocateFormatStr
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return '%s(%s)' % (self.kw,','.join([str(arg) for arg in self.argList]))\
               +''.join(self.internal)

class InquireStmt(Exec):
    kw = 'inquire'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        unitSpec = seq(zo1(seq(lit('unit'),lit('='))),
                       int)
        formInquireStmt = seq(lit(InquireStmt.kw),
                              lit('('),
                              zo1(unitSpec),
                              zo1(lit(',')),
                              zo1(cslist(disj(seq(lit('access'),lit('='),Exp),
                                               seq(lit('binary'),lit('='),Exp),
                                               seq(lit('blank'),lit('='),Exp),
                                               seq(lit('blocksize'),lit('='),Exp),
                                               seq(lit('direct'),lit('='),Exp),
                                               seq(lit('err'),lit('='),Exp),
                                               seq(lit('exist'),lit('='),Exp),
                                               seq(lit('file'),lit('='),Exp),
                                               seq(lit('form'),lit('='),Exp),
                                               seq(lit('formatted'),lit('='),Exp),
                                               seq(lit('iostat'),lit('='),Exp),
                                               seq(lit('iofocus'),lit('='),Exp),
                                               seq(lit('mode'),lit('='),Exp),
                                               seq(lit('name'),lit('='),Exp),
                                               seq(lit('named'),lit('='),Exp),
                                               seq(lit('nextrec'),lit('='),Exp),
                                               seq(lit('number'),lit('='),Exp),
                                               seq(lit('opened'),lit('='),Exp),
                                               seq(lit('recl'),lit('='),Exp),
                                               seq(lit('sequential'),lit('='),Exp),
                                               seq(lit('share'),lit('='),Exp),
                                               seq(lit('unformatted'),lit('='),Exp)))),
                           lit(')'))
        
        ((stmt_name,lparen,unitspec,comma,params,rparen),rst) = formInquireStmt(scan)
        return InquireStmt(unitspec,params,lineNumber,rest=rst)

    def __init__(self,unitspec,params=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.unitspec = unitspec
        if len(params) > 0:
            self.params = params[0]
        else: self.params = []
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        if isinstance(self.unitspec,list):
            unitspec = ''.join(self.unitspec)
        else:
            unitspec = self.unitspec
        if self.params == []:
            return '%s (%s)' % (self.kw,unitspec)            
        paramlist = []
        for param in self.params:
            paramlist.append(''.join(str(elt) for elt in param))
        if unitspec == '':
            return '%s (%s)' % (self.kw,','.join(paramlist))
        return '%s (%s)' % (self.kw,unitspec+','+','.join(paramlist))

class NullifyStmt(Exec):
    kw = 'nullify'
    kw_str = kw
    
    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        formNullifyStmt = seq(lit(NullifyStmt.kw),
                              lit('('),
                              cslist(Exp),
                              lit(')'))
        ((stmt_name,lparen,ptrObjList,rparen),rst) = formNullifyStmt(scan)
        return NullifyStmt(ptrObjList,lineNumber,rest=rst)

    def __init__(self,ptrObjList,lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.ptrObjList = ptrObjList
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return '%s (%s)' % (self.kw,','.join(str(item) for item in self.ptrObjList))
        

class BackspaceStmt(Exec):
    kw = 'backspace'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        unitSpec = seq(zo1(seq(lit('unit'),lit('='))),
                       int)
        formBackspaceStmt = seq(lit(BackspaceStmt.kw),
                                lit('('),
                                unitSpec,
                                zo1(seq(lit(','),
                                        cslist(disj(seq(lit('err'),lit('='),Exp),
                                                    seq(lit('iostat'),lit('='),Exp))))),
                                lit(')'))
        
        ((stmt_name,lparen,unitspec,params,rparen),rst) = formBackspaceStmt(scan)
        return BackspaceStmt(unitspec,params,lineNumber,rest=rst)

    def __init__(self,unitspec,params=[],lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.unitspec = unitspec
        if len(params) > 0:
            self.params = params[0][1]
        else: self.params = []
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        if isinstance(self.unitspec,list):
            unitspec = ''.join(self.unitspec)
        else:
            unitspec = self.unitspec
        if self.params == []:
            return '%s (%s)' % (self.kw,unitspec)            
        paramlist = []
        for param in self.params:
            paramlist.append(''.join(str(elt) for elt in param))
        return '%s (%s)' % (self.kw,unitspec+','+','.join(paramlist))

class RewindStmt(Exec):
    kw = 'rewind'
    kw_str = kw

    @staticmethod
    def parse(ws_scan,lineNumber):
        scan = filter(lambda x: x != ' ',ws_scan)
        try:
            parenUnitSpec = seq(lit('('),
                                 id,
                                 lit(')'))
            formRewindStmt = seq(lit(RewindStmt.kw),
                                 disj(id,parenUnitSpec))
            ((rewindKeyword,unitSpec),rest) = formRewindStmt(scan)
            return RewindStmt(unitSpec,lineNumber=lineNumber)
        except:
            formUnitSpec = seq(lit('unit'),
                               lit('='),
                               id)
            formErrLabel = seq(lit(','),
                               lit('err'),
                               lit('='),
                               int)
            formIOCheck = seq(lit(','),
                              lit('iostat'),
                              lit('='),
                              int)
            formRewindStmt = seq(lit(RewindStmt.kw), # 0
                                 lit('('),           # 1
                                 formUnitSpec,       # 2
                                 zo1(formErrLabel),  # 3
                                 zo1(formIOCheck),   # 4
                                 lit(')'))           # 5
            formRewindStmt = treat(formRewindStmt, lambda x: RewindStmt(x[2],
                                                                        x[3] or None,
                                                                        x[4] or None,
                                                                        lineNumber))
            (theParsedStmt,rest) = formRewindStmt(scan)
            theParsedStmt.rest = rest
            return theParsedStmt
    def __init__(self,unitSpec,errLabel='',IOCheck='',lineNumber=0,label=False,lead='',internal=[],rest=[]):
        self.unitSpec = unitSpec
        self.errLabel = errLabel
        self.IOCheck = IOCheck
        Exec.__init__(self,lineNumber,label,lead,internal,rest)

    def __str__(self):
        return '%s %s%s%s' % (self.kw,self.unitSpec,self.errLabel,self.IOCheck)+\
               ''.join(self.internal)


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
             procedure       = ProcedureStmt,
             module          = ModuleStmt,
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
             endmodule       = EndModuleStmt,
             endprogram      = EndProgramStmt,
             endfunction     = EndFunctionStmt,
             endsubroutine   = EndSubroutineStmt,
             endblockdata    = EndStmt,
             endwhere        = EndWhereStmt,
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
             deallocate      = DeallocateStmt,
             inquire         = InquireStmt,
             nullify         = NullifyStmt,
             backspace       = BackspaceStmt,
             namelist        = NamelistStmt,
             )

for kw in ('if','continue','return','else','print','use','cycle','exit','rewind','where','elsewhere','format','pointer','target'):
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

def parse(ws_scan,lineNumber):
    scan = filter(lambda x: x != ' ',ws_scan)
    try:
        parsed = AssignStmt.parse(scan,lineNumber)
        parsed.rawline = ''.join(ws_scan).strip()
        return parsed
    except AssemblerException:
        lscan = tuple([ l.lower() for l in scan ])
        kw3g  = kw_multi.kw3.get
        kw2g  = kw_multi.kw2.get
        kw = len(scan) >=3 and kw3g(lscan[0:3]) and sqz(3,[scan]) or \
             len(scan) >=2 and kw2g(lscan[0:2]) and sqz(2,[scan]) or \
             lscan[0]
        if kw in _types and 'function' in lscan:
            kw = 'function'
        # special case for module procedure statements:
        elif (kw == 'module') and (lscan[1] == 'procedure') :
            kw = 'procedure'

        if not kwtbl.get(kw):
            if '=>' in scan:
                try:
                    parsed = PointerAssignStmt.parse(scan,lineNumber)
                    parsed.rawline = ''.join(ws_scan).strip()
                    return parsed
                except ListAssemblerException,e:
                    raise ParseError(lineNumber,scan,PointerAssignStmt,'l_assembler error:'+e.msg+' remainder:'+str(e.rest))
            elif ('do' in scan) and (':' in scan):
                try:
                    parsed = DoStmt.parse(scan,lineNumber)
                    parsed.rawline = ''.join(ws_scan).strip()
                    return parsed
                except ListAssemblerException,e:
                    raise ParseError(lineNumber,scan,DoStmt,'l_assembler error:'+e.msg+' remainder:'+str(e.rest))
            else:
                raise ParseError(lineNumber,scan,None)
        else:
            try:
                parsed = kwtbl.get(kw).parse(ws_scan,lineNumber)
                parsed.rawline = ''.join(ws_scan).strip()
                return parsed
            except Exception:
                try:
                    parsed = kwtbl.get(kw).parse(scan,lineNumber)
                    parsed.rawline = ''.join(ws_scan).strip()
                    return parsed
                except ListAssemblerException,e:
                    raise ParseError(lineNumber,scan,kwtbl.get(kw),'l_assembler error:'+e.msg+' remainder:'+str(e.rest))

#
# alias so that stmts like if, etc can call the above routine
#
_kw_parse = parse

'''
if __name__ == '__main__':
    from _Setup.testit import *
'''
