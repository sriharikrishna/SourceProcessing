'''
Turn a parsed line into a context line
'''

from _Setup import *
import fortParseFile as fpf
import fortStmts     as fs
import fortExp       as fe
import PyIR.mapper   as mapper
from   PyIR.mapper   import _Map

from   PyUtil.caselessDict import caselessDict

'''
def _ident(self,*args,**kw):
    return [self]
'''

class Struct(object):
    '''
    Structural object: a dynamic scratchpad for adding
    whatever state is needed
    '''
    def __init__(self):
        pass

_sym_init = dict(typeof=None,
                 mod=(),
                 dims=(),
                 vclass='local',
                 lngth=None,
                 kw_str='????')

class SymEntry(object):
    def __init__(self,**kw):
        self.__dict__.update(_sym_init)
        self.__dict__.update(kw)

default_sym = SymEntry(typeof=None,
                       kw_str='????',
                       dims=(),
                       mod=None,
                       vclass=None,
                       lngth=None
                       )

class Context(object):
    'Line context structure'

    def __init__(self,toplev,hook=mapper.noop):
        self.toplev     = toplev
        self.uname      = '__dummy__'
        self.utype      = None
#        self.vars       = dict()
        self.vars       = caselessDict()
        self._getnew    = False
        self._seekmarks = False
        self.implicit   = dict()
        for l in 'abcdefghopqrstuvwxyz':
            self.implicit[l] = (fs.RealStmt,[])
        for l in 'ijklmn':
            self.implicit[l] = (fs.IntegerStmt,[])
        hook(self)

    def lookup_var(self,v):
        if v in self.vars:
            return self.vars[v]
        return default_sym

    def lookup_type(self,v):
        try:
            return self.vars[v].typeof
        except KeyError, AttributeError:
            return self.implicit[v[0].lower()]

    def lookup_dims(self,v):
        try:
            return self.vars[v].dims
        except KeyError, AttributeError:
            return ()

    def lookup_lngth(self,v):
        try:
            return self.vars[v].lngth
        except KeyError, AttributeError:
            return False

def nextunit(line,ctxtm):
    '''process end-of-unit, including recording modules in the
    toplevel module table
    '''
    ctxt = ctxtm[0]
    ctxt._getnew = True
    if ctxt.utype == 'module':
        ctxt.toplev.modules[ctxt.uname] = ctxt
    return line

_showparse = False
def newunit(line,ctxtm):
    'start unit: set up decls, etc'
    import sys

    ctxt = ctxtm[0]
    ctxt.utype = line.__class__.utype_name
    ctxt.uname = line.name
    ctxt.retntype = None
    ctxt._seekmarks = True
    if _showparse:
        print >> sys.stderr,'Reading and parsing unit',ctxt.uname
    return line

def fnunit(line,ctxtm):
    'function unit needs to record return type for function'
    dc = newunit(line,ctxtm)
    ctxt = ctxtm[0]
    ctxt.utype = 'function'
    ctxt.retntype = line.ty
    if line.ty:
        (ty,mod) = line.ty[0]
        ty       = fs.kwtbl[ty.lower()]
        ty       = (ty,mod)

        ctxt.vars[line.name] = SymEntry(typeof=ty,dims=(),external=True)
    return line

def typesep(dd,default_dims):
    '''return name and dimensions for a given decl entry
    a type declaration will either be a simple var (string)
    or an App expression
    '''
    _helper = lambda e: isinstance(e,str) and (e,default_dims) or (e.head,tuple(e.args))

    d = dd.lhs

    if isinstance(d,fe.Ops): return _helper(d.a1)
    return _helper(d)

def default_dims(attrs_list):
    for a in attrs_list:
        if isinstance(a,fe.App) and a.head == 'dimension':
            return tuple(a.args)
    return ()

def typedecl(line,ctxtm):
    'type declaration -- record type in symbol table'

    ctxt = ctxtm[0]

    typeof  = line.__class__
    kw_str  = line.kw_str
    mod     = line.mod
    lngth   = kw_str == 'character' and (mod and mod[0] or 1)
    dflt_d  = default_dims(line.attrs)
    for d in line.decls:
        (name,dims)     = typesep(d,dflt_d)
        if name in ctxt.vars:
            ctxt.vars[name].typeof = typeof
            ctxt.vars[name].mod    = mod
        else:
            ctxt.vars[name] = SymEntry(typeof=(typeof,mod),
                                       kw_str=kw_str,
                                       dims=dims,
                                       vclass='local',
                                       lngth=lngth)
    return line

def dimen(line,ctxtm):
    ctxt = ctxtm[0]

    for d in line.decls:
        (name,dims) = (d.head,d.args)
        if name in ctxt.vars:
            ctxt.vars[name].dims = dims
        else:
            (typeof,kw_str,mod) = ctxt.typeof(name)
            ctxt.vars[name] = SymEntry(typeof,kw_str,mod,dims)

    return line

def assgn(line,ctxtm):
    '''Check assignment statement to see if it is a statement
    function.
    '''
    ctxt = ctxtm[0]
    lhs  = line.lhs
    look = ctxt.lookup_var
    if isinstance(lhs,fe.App) and not look(lhs.head).dims:
        ret = fs.StmtFnStmt(lhs.head,lhs.args,line.rhs)
        ret.rawline = line.rawline
        ret.lineno  = line.lineno
        ret.lead    = line.lead
        return ret

    return line

def use_module(line,ctxtm):
    '''For a use statement, grab the module out of the toplevel
    module table (or warn if module is unavailable
    '''
    from sys import stderr 
    ctxt    = ctxtm[0]
    modules = ctxt.toplev.modules
    mod     = line.name
    if mod not in modules:
      if mod not in ctxt.toplev.missingModules:
         print >> stderr,'Warning: unknown module named:',mod
         ctxt.toplev.missingModules.append(mod)
      return line

    modvars = modules[mod].vars
    ctxt.vars.update(modvars)
    vcstr = 'module:'+mod
    for v in modvars:
        ctxt.vars[v].vclass = vcstr
    
    return line

def implicit(line,ctxtm):
    '''Set up the implicit table
    '''
    ctxt = ctxtm[0]

    letters = 'abcdefghijklmnopqrstuvwxyz'

    for (t,tlst) in line.lst:
        (t_id,mod) = t
        typecls    = fs.kwtbl[t_id]
        tval = (typecls,mod)
        for exp in tlst:
            if isinstance(exp,str):
                ctxt.implicit[exp.lower()] = tval
            else:
                for l in letters[ \
                    letters.find(exp.a1.lower()) : \
                    letters.find(exp.a2.lower())+1]:
                    ctxt.implicit[l] = tval

    return line

ctxt_lexi = [(fs.PUend,         nextunit),
             (fs.PUstart,       newunit),
             (fs.FunctionStmt,  fnunit),
             (fs.TypeDecl,      typedecl),
             (fs.DimensionStmt, dimen),
             (fs.AssignStmt,    assgn),
             (fs.UseStmt,       use_module),
             (fs.ImplicitStmt,  implicit),
             ]

class fortContext(_Map):
    '''create a list of contextLine objects:
    At the moment, this is just a list of parsed lines
    with an additional attribute called 'ctxt' has
    been added to the parseLine object
    '''

    def __init__(self,line_iter):
        'initialize by adding the lines, 1 at a time via iterator'
        self.lines = []
        for l in line_iter:
            self.lines.append(l)

    def __str__(self):
        op = [l.rawline for l in self.lines if not isinstance(l,fs.Marker)]
        return ''.join(op)

    def rewrite(self,lexi,*args,**kws):
        return fortContext(self.map(lexi,*args,**kws))

    def extend(self,iter):
        self.lines.extend(list(iter))

    def printit(self,showmarks=False,out=None):
        for l in self.lines:
            if showmarks:
                if isinstance(l,fs.LastDecl):
                    l.rawline = 'c** Insert Decls BEFORE this line\n'
                elif isinstance(l,fs.FirstExec):
                    l.rawline = 'c** Next Line is FIRST exectuable\n'
                else:
                    pass
            else:
                if isinstance(l,fs.Marker): continue
            if out:
                print >> out,l.rawline,
            else:
                print l.rawline,

    def write(self,fname,showmarks=False):
        ff = open(fname,'w')
        self.printit(showmarks,ff)
        ff.close()


def _gen_context(parse_iter,hook=mapper.noop):
    '''
    The workhorse routine:
      from a list of un contexted but parsed lines, process each line,
      creating or modifying the context object according to
      the supplied lexi
    '''
    toplev         = Struct()
    toplev.units   = dict()
    toplev.modules = dict()
    toplev.missingModules = []

    ctxt         = Context(toplev,hook)

    ctxt_mutable = [ctxt]

    hold = []
    is_decl_prev = False
    decl_lead = ''
    for l in parse_iter.map1(ctxt_lexi,ctxt_mutable):
        ctxt   = ctxt_mutable[0]
        l.ctxt = ctxt
        if ctxt._seekmarks:
            if isinstance(l,fs.Exec):
                l.ctxt._seekmarks = False
                is_prev_decl = False
                marker = fs.LastDecl()
                marker.lead = decl_lead
                marker.ctxt = ctxt
                yield marker
                for ll in hold: yield ll
                hold = []
                marker = fs.FirstExec()
                marker.lead = l.lead
                marker.ctxt = ctxt
                yield marker
            elif is_decl_prev and isinstance(l,fs.Comments):
                hold.append(l)
                continue
            elif is_decl_prev and isinstance(l,fs.Decl):
                for ll in hold: yield ll
                hold = []
            else:
                is_decl_prev = isinstance(l,fs.Decl)
                decl_lead    = l.lead
        yield l
        if l.ctxt._getnew:
            _new = Context(toplev,hook)
            del l.ctxt._getnew
            hold = []
            is_decl_prev = False
            decl_lead    = ''
            ctxt_mutable[0] = _new

def fortContextFile(fname,hook=mapper.noop):
    'from a file name create a fortContext object'

    return fortContext(_gen_context(fpf.fortParseFileIter(fname),hook))

def fortContextEmpty(hook=mapper.noop):
    return fortContext([])

def fortContextExtend(iter,hook=mapper.noop):
    self.lines.extend
