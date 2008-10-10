'''add methods to fort stmts to build units
'''

#from _Setup import *

import _Setup

import fortStmts     as fs
import fortExp       as fe
import sym_entries   as SE
from unit_symtab import Symtab

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

def _typedecl(stmt,curr):
    'type declaration -- record type in symbol table'

#    print 'stmt = ',stmt
    unit    = curr.val
#    print 'unit = ',unit
    vtype   = (stmt.__class__,stmt.mod)
    kw_str  = stmt.kw_str
    lngth   = kw_str == 'character' and (stmt.mod and stmt.mod[0] or 1)
    dflt_d  = default_dims(stmt.attrs)
    symtab = unit.symtab
#    print 'symtab in typedecl = ',symtab
    for d in stmt.decls:
        (name,dims)     = typesep(d,dflt_d)
#        print 'about to lookup name |',name,'| using ',symtab
        entry = symtab.lookup_name(name)
#        print 'entry from symtab = ',entry
        if entry:
            entry.enter_type(vtype)
        else:
            entry = SE.var(vtype,
                           dims=dims,
                           origin='local',
                           lngth=lngth)
            symtab.enter_name(name,entry)
    return stmt

def _assign2stmtfn(s,cur):
    'convert assign stmt s to stmtfn, and enter in unit symtab'
#    print 'converting ',s,' to stmt fn'
    unit = cur.val
    lhs = s.lhs
    rv = fs.StmtFnStmt(lhs.head,lhs.args,s.rhs,s.lineNumber,s.label,s.lead)
    rv.rawline = s.rawline

    entry      = SE.stmtfn(lhs.args,s.rhs)
    unit.symtab.enter_name(lhs.head,entry)

    return rv

def _is_stmt_fn(s,cur):
    'determine if assignment s is a statement function, based on "unit" symtab'
#    print 'checking assignment ',s,' for stmt fn'
    lhs  = s.lhs
    look = cur.val.symtab.lookup_dims

    return isinstance(lhs,fe.App) and isinstance(lhs.head,str) and not look(lhs.head)

def _use_module(s,cur):
    'incorporate the used module symbol table into the current unit symbol table'

    module_unit = cur.module_handler.get_module(s.name)
    if module_unit:
        cur.val.symtab.update_w_module(module_unit)
    else:
        print 'WARNING: module %s not found' % s.name

    return s

def _make_fn_entry(self,lcl):
    return SE.func(self.name,self.args,lcl,self.ty)

def _make_subr_entry(self,lcl):
    return SE.subr(self.name,self.args,lcl)

def _unit_entry(self,cur):
    '''enter a subroutine or function into:
       1. The local symtab for the object
       2. The unit symtab
       3. The parent of the unit (if there is one)
    '''
    name  = self.name
    cur_stab = cur.val.symtab
    entry = self.make_unit_entry(cur_stab)
    cur_stab.enter_name(name,entry)
    if cur_stab.parent:
        cur_stab.parent.enter_name(name,entry)

    return self

def _implicit(self,cur):
    '''Set up the implicit table
    '''
    ctxt = cur.val

#    if ctxt._in_iface:
#        return line

    letters = 'abcdefghijklmnopqrstuvwxyz'

    for (tval,tlst) in self.lst:
        for exp in tlst:
            if isinstance(exp,str):
                ctxt.symtab.implicit[exp] = tval
            else:
                for l in letters[ \
                    letters.find(exp.a1) : \
                    letters.find(exp.a2)+1]:
                    ctxt.symtab.implicit[l] = tval

    return self

def _implicit_none(self,cur):
    cur.val.symtab.implicit_none()
    return self

def _unit_iface_action(self,cur):
    unit = cur.val
    name = self.name
    lcl = Symtab(unit.symtab)
    entry = self.make_unit_entry(lcl)
    lcl.enter_name(name,entry)
    unit.symtab.enter_name(name,entry)
    unit.symtab = lcl
    return self

def _end_unit_iface_action(self,cur):
    unit = cur.val
    unit.symtab = unit.symtab.parent
    return self

def _iface(self,cur):
    ctxt = cur.val
    ctxt._in_iface = True
    return self

def _end_iface(self,cur):
    ctxt._in_iface = False
    return self

fs.GenStmt.unit_action        = lambda s,*rest,**kw: s
fs.GenStmt.unit_entry         = lambda s,*rest,**kw: s

fs.SubroutineStmt.unit_entry      = _unit_entry
fs.SubroutineStmt.make_unit_entry = _make_subr_entry
fs.SubroutineStmt.unit_action     = _unit_iface_action

fs.FunctionStmt.unit_entry      = _unit_entry
fs.FunctionStmt.make_unit_entry = _make_fn_entry
fs.FunctionStmt.unit_action     = _unit_iface_action

fs.AssignStmt.is_decl         = _is_stmt_fn
fs.AssignStmt.unit_action     = _assign2stmtfn

fs.TypeDecl.unit_action       = _typedecl

fs.UseStmt.unit_action        = _use_module

fs.ImplicitNone.unit_action   = _implicit_none
fs.ImplicitStmt.unit_action   = _implicit

fs.InterfaceStmt.unit_action  = _iface

fs.EndInterfaceStmt.unit_action = _end_iface

'''
if __name__ == '__main__':
    from _Setup.testit import *
    from unit_symtab import Symtab
    from _Setup.testit import scan

    def pps(s):
        return fs.parse(scan(s))

    class U(object):
        def __init__(self):
            self.symtab = Symtab()

    class _curr(object):
        def __init__(self):
            self.val = U()

    Symtab._default_real = (fs.RealStmt,[])
    Symtab._default_int  = (fs.IntegerStmt,[])

    cur = _curr()

    s1 = pps('implicit integer(special) (a-f)')
    sr = _implicit(s1,cur)
    v = cur.val.symtab
    print 'lookup foo in implicit symtab = %s' % (v.lookup_type('foo'),)
    print 'lookup zoo in implicit symtab = %s' % (v.lookup_type('zoo'),)
'''
