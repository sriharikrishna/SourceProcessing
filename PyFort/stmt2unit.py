'''add methods to fort stmts to build units
'''

from _Setup import *

from PyUtil.symtab import Symtab,SymtabEntry

import fortStmts     as fs
import fortExp       as fe

#TODO: handle derived type entries

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

def _processTypedeclStmt(aTypeDeclStmt,curr):
    'type declaration -- record type in symbol table'
#   print 'stmt2unit._processTypedeclStmt: called for',aTypeDeclStmt 
    localSymtab = curr.val.symtab
    newType = (aTypeDeclStmt.__class__,aTypeDeclStmt.mod)
    newLength = aTypeDeclStmt.kw_str == 'character' and (aTypeDeclStmt.mod and aTypeDeclStmt.mod[0] \
                                                                            or 1)
    dflt_d  = default_dims(aTypeDeclStmt.attrs)
#   print '\nstmt2unit._typedecl: [stmt = "'+str(stmt)+'" (line number',stmt.lineNumber,')',\
#                                  ',\tstmt.decls = "'+str(stmt.decls)+'"',\
#                                  ',\tunit =',unit,\
#                                  ',\tvtype =',vtype,\
#                                  ',\tsymtab in typedecl =',symtab,']'
    for aDecl in aTypeDeclStmt.decls:
        (name,newDimensions) = typesep(aDecl,dflt_d)
        theSymtabEntry = localSymtab.lookup_name(name)
        # already in symtab -> check for conflicts, then add new information
        if theSymtabEntry:
#           print '\tdecl "'+str(aDecl)+'" already has SymtabEntry '+str(theSymtabEntry.debug(name))
            #FIXME: at what LEVEL should this new information be added?
            # choices are 1-local level, or 2-level in which the entry resides
            theSymtabEntry.enterType(newType)
            theSymtabEntry.enterDimensions(newDimensions)
        # no symtab entry -> create one
        else:
            newSymtabEntry = SymtabEntry(SymtabEntry.GenericEntryKind,
                                         type=newType,
                                         dimensions=newDimensions,
                                         length=newLength,
                                         origin='local')
#           print '\tdecl "'+str(aDecl)+'" NOT already present in symbol table -- adding '+str(newSymtabEntry.debug(name))
            localSymtab.enter_name(name,newSymtabEntry)
    return aTypeDeclStmt

#def _processDeclStmt(aDeclStmt,curr):
#    print 'called _processDeclStmt on',aDeclStmt
#    return aDeclStmt

def _processDimensionStmt(aDimensionStmt,curr):
    localSymtab = curr.val.symtab
    print 'called _processDimensionStmt on "'+str(aDimensionStmt)+'" with localSymtab',localSymtab
    for anApp in aDimensionStmt.lst:
        theSymtabEntry = localSymtab.lookup_name(anApp.head)
        if theSymtabEntry:
            print '\tvariable "'+anApp.head+'" already present in symbol table as '+theSymtabEntry.debug(anApp.head)
            theSymtabEntry.enterDimensions(tuple(anApp.args))
        else:
            newSymtabEntry = SymtabEntry(SymtabEntry.VariableEntryKind,
                                         dimensions=tuple(anApp.args),
                                         origin='local')
            print '\tvariable "'+anApp.head+'" NOT already present in symbol table -- adding '+newSymtabEntry.debug(anApp.head)
            localSymtab.enter_name(anApp.head,newSymtabEntry)
    return aDimensionStmt

def _processExternalStmt(anExternalStmt,curr):
    localSymtab = curr.val.symtab
#   print 'stmt2unit._processExternalStmt: called on "'+str(anExternalStmt)+'" with localSymtab',localSymtab
    for aProcedureName in anExternalStmt.procedureNames:
        theSymtabEntry = localSymtab.lookup_name(aProcedureName)
        if not theSymtabEntry:
            newSymtabEntry = SymtabEntry(SymtabEntry.ProcedureEntryKind,
                                         origin='external')
            localSymtab.enter_name(aProcedureName,newSymtabEntry)
#           print '\tprocedure NOT already present in symbol table -- adding '+newSymtabEntry.debug(aProcedureName)
#       else:
#           print '\tprocedure already has SymtabEntry'+theSymtabEntry.debug(aProcedureName)
    return anExternalStmt

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
    look = cur.val.symtab.lookupDimensions

    return isinstance(lhs,fe.App) and isinstance(lhs.head,str) and not look(lhs.head)

def _use_module(s,cur):
    'incorporate the used module symbol table into the current unit symbol table'

    module_unit = cur.module_handler.get_module(s.name)
    if module_unit:
        cur.val.symtab.update_w_module(module_unit)
    else:
        print 'WARNING: module %s not found' % s.name

    return s

def _makeFunctionEntry(self,localSymtab):
    return SymtabEntry(SymtabEntry.FunctionEntryKind)

def _makeSubroutineEntry(self,localSymtab):
    return SymtabEntry(SymtabEntry.SubroutineEntryKind)

def _unit_entry(self,cur):
    '''enter a subroutine or function into:
       1. The local symtab for the object
       2. The unit symtab
       3. The parent of the unit (if there is one)
    '''
    currentSymtab = cur.val.symtab
    entry = self.make_unit_entry(currentSymtab)
    currentSymtab.enter_name(self.name,entry)
    if currentSymtab.parent:
        currentSymtab.parent.enter_name(self.name,entry)
    return self

def _implicit(self,cur):
    '''Set up the implicit table
    '''
    currentUnit = cur.val
#   if currentUnit._in_iface:
#       return line

    letters = 'abcdefghijklmnopqrstuvwxyz'

    for (tval,tlst) in self.lst:
        for exp in tlst:
            if isinstance(exp,str):
                currentUnit.symtab.implicit[exp] = tval
            else:
                for l in letters[ \
                    letters.find(exp.a1) : \
                    letters.find(exp.a2)+1]:
                    currentUnit.symtab.implicit[l] = tval

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

def _beginInterface(self,cur):
    print 'called _beginInterface on ',self
    currentUnit = cur.val
    currentUnit._in_iface = True
    # make a symbol table local to the interface
    interfaceSymtab = Symtab(currentUnit.symtab)
    # for named interfaces, add a symbol table entry to its symtab AND the enclosing unit
    if self.name:
        entry = SE.InterfaceEntry(self.name,interfaceSymtab)
        currentUnit.symtab.enter_name(self.name,entry)
        interfaceSymtab.enter_name(self.name,entry)
    # switch to the interface's symbol table for the duration of the interface
    currentUnit.symtab = interfaceSymtab
    print '\tEntering interface "'+str(self.name)+'", with symbol table "'+str(interfaceSymtab)+'"'
    return self

def _endInterface(self,cur):
    currentUnit = cur.val
    currentUnit._in_iface = False
    # swtch back to the symbol table for the unit
    currentUnit.symtab = currentUnit.symtab.parent
    print '\tLeaving interface and restoring unit symbol table "'+str(currentUnit.symtab)+'"\n'
    return self

fs.GenStmt.unit_action        = lambda s,*rest,**kw: s
fs.GenStmt.unit_entry         = lambda s,*rest,**kw: s

fs.SubroutineStmt.unit_entry      = _unit_entry
fs.SubroutineStmt.make_unit_entry = _makeSubroutineEntry
fs.SubroutineStmt.unit_action     = _unit_iface_action

fs.FunctionStmt.unit_entry      = _unit_entry
fs.FunctionStmt.make_unit_entry = _makeFunctionEntry
fs.FunctionStmt.unit_action     = _unit_iface_action

fs.AssignStmt.is_decl         = _is_stmt_fn
fs.AssignStmt.unit_action     = _assign2stmtfn

fs.DimensionStmt.unit_action = _processDimensionStmt

fs.ExternalStmt.unit_action = _processExternalStmt


fs.TypeDecl.unit_action       = _processTypedeclStmt

fs.UseStmt.unit_action        = _use_module

fs.ImplicitNone.unit_action   = _implicit_none
fs.ImplicitStmt.unit_action   = _implicit

fs.InterfaceStmt.unit_action  = _beginInterface

fs.EndInterfaceStmt.unit_action = _endInterface

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
