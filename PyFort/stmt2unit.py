'''add methods to fort stmts to build units
'''

from _Setup import *

from PyUtil.symtab import Symtab,SymtabEntry
from PyUtil.debugManager import DebugManager

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
    localSymtab = curr.val.symtab
    DebugManager.debug('[Line '+str(aTypeDeclStmt.lineNumber)+']: stmt2unit._processTypedeclStmt('+str(aTypeDeclStmt)+') with symboltable '+str(localSymtab))
    newType = (aTypeDeclStmt.__class__,aTypeDeclStmt.mod)
    newLength = aTypeDeclStmt.kw_str == 'character' and (aTypeDeclStmt.mod and aTypeDeclStmt.mod[0] \
                                                                            or 1)
    dflt_d  = default_dims(aTypeDeclStmt.attrs)
    for aDecl in aTypeDeclStmt.decls:
        (name,newDimensions) = typesep(aDecl,dflt_d)
        theSymtabEntry = localSymtab.lookup_name_local(name)
        if theSymtabEntry: # already in symtab -> enter new information (taking exception to any conflicts)
            DebugManager.debug('\tTypeDecl "'+str(aDecl)+'" already present in local symbol table as '+str(theSymtabEntry.debug(name)))
            theSymtabEntry.enterType(newType,aTypeDeclStmt.lineNumber)
            theSymtabEntry.enterDimensions(newDimensions,aTypeDeclStmt.lineNumber)
        else: # no symtab entry -> create one
            newSymtabEntry = SymtabEntry(SymtabEntry.GenericEntryKind,
                                         type=newType,
                                         dimensions=newDimensions,
                                         length=newLength,
                                         origin='local')
            DebugManager.debug('\tdecl "'+str(aDecl)+'" NOT already present in symbol table -- adding '+str(newSymtabEntry.debug(name)))
            localSymtab.enter_name(name,newSymtabEntry)
    return aTypeDeclStmt

#def _processDeclStmt(aDeclStmt,curr):
#    print 'called _processDeclStmt on',aDeclStmt
#    return aDeclStmt

def _processDimensionStmt(aDimensionStmt,curr):
    localSymtab = curr.val.symtab
    DebugManager.debug('[Line '+str(aDimensionStmt.lineNumber)+']: stmt2unit._processDimensionStmt('+str(aDimensionStmt)+') with symbol table '+str(localSymtab))
    for anApp in aDimensionStmt.lst:
        theSymtabEntry = localSymtab.lookup_name_local(anApp.head)
        if theSymtabEntry:
            DebugManager.debug('\tvariable "'+anApp.head+'" already present in local symbol table as '+theSymtabEntry.debug(anApp.head))
            theSymtabEntry.enterDimensions(tuple(anApp.args),aDimensionStmt.lineNumber)
        else:
            newSymtabEntry = SymtabEntry(SymtabEntry.VariableEntryKind,
                                         dimensions=tuple(anApp.args),
                                         origin='local')
            DebugManager.debug('\tvariable "'+anApp.head+'" NOT already present in symbol table -- adding '+newSymtabEntry.debug(anApp.head))
            localSymtab.enter_name(anApp.head,newSymtabEntry)
    return aDimensionStmt

def _processExternalStmt(anExternalStmt,curr):
    localSymtab = curr.val.symtab
    DebugManager.debug('[Line '+str(anExternalStmt.lineNumber)+']: stmt2unit._processExternalStmt: called on "'+str(anExternalStmt)+'" with localSymtab '+str(localSymtab))
    for aProcedureName in anExternalStmt.procedureNames:
        theSymtabEntry = localSymtab.lookup_name(aProcedureName)
        if not theSymtabEntry:
            newSymtabEntry = SymtabEntry(SymtabEntry.ProcedureEntryKind,
                                         origin='external')
            localSymtab.enter_name(aProcedureName,newSymtabEntry)
            DebugManager.debug('\tprocedure NOT already present in symbol table -- adding '+newSymtabEntry.debug(aProcedureName))
        else:
            DebugManager.debug('\tprocedure already has SymtabEntry'+theSymtabEntry.debug(aProcedureName))
            # if the entry has a type, we know it's a function
            newEntryKind = theSymtabEntry.type and SymtabEntry.FunctionEntryKind \
                                                or SymtabEntry.ProcedureEntryKind
            theSymtabEntry.enterEntryKind(newEntryKind,anExternalStmt.lineNumber)
    return anExternalStmt

def _assign2stmtfn(anAssignmentStmt,curr):
    'convert assign stmt to stmtfn, and enter in unit symtab'
    DebugManager.debug('[Line '+str(anAssignmentStmt.lineNumber)+']: converting '+str(anAssignmentStmt)+' to stmt fn')
    newStmtFn = fs.StmtFnStmt(anAssignmentStmt.lhs.head,
                              anAssignmentStmt.lhs.args,
                              anAssignmentStmt.rhs,
                              anAssignmentStmt.lineNumber,
                              anAssignmentStmt.label,
                              anAssignmentStmt.lead)
    newStmtFn.rawline = anAssignmentStmt.rawline
    newSymtabEntry = SymtabEntry(SymtabEntry.StatementFunctionEntryKind,
                                 origin='local')
    curr.val.symtab.enter_name(anAssignmentStmt.lhs.head,newSymtabEntry)
    return newStmtFn

def _is_stmt_fn(s,cur):
    'determine if assignment s is a statement function, based on "unit" symtab'
    DebugManager.debug('checking assignment '+str(s)+' for stmt fn')
    lhs  = s.lhs
    look = cur.val.symtab.lookupDimensions

    return isinstance(lhs,fe.App) and isinstance(lhs.head,str) and not look(lhs.head)

reportedMissingModules=set()
def _use_module(s,cur):
    '''
    incorporate the used module symbol table into the current unit symbol table
    issue a warning if the module has not been seen yet
    '''
    module_unit = cur.module_handler.get_module(s.name)
    if module_unit:
        cur.val.symtab.update_w_module(module_unit)
    else:
	global reportedMissingModules
        if not (s.name.lower() in reportedMissingModules) :
            reportedMissingModules.add(s.name.lower())
            DebugManager.warning('definition for module '+s.name+' not seen in the input.',s.lineNumber)
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
    DebugManager.debug('[Line '+str(self.lineNumber)+']: stmt2unit._unit_entry() for '+str(self)+': with symtab '+str(currentSymtab)+' with parent symtab '+str(currentSymtab.parent))
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

def _beginInterface(anInterfaceStmt,cur):
    cur.val._in_iface = True
    localSymtab = cur.val.symtab
    # make a symbol table local to the interface
    interfaceSymtab = Symtab(localSymtab)
    DebugManager.debug('[Line '+str(anInterfaceStmt.lineNumber)+']: stmt2unit._beginInterface('+str(anInterfaceStmt)+'): creating interface symtab '+str(interfaceSymtab)+' with parent symtab '+str(localSymtab))
    sys.stderr.flush()
    # for named interfaces, add a symbol table entry to its symtab AND the enclosing unit
    if anInterfaceStmt.name:
        newSymtabEntry = SymtabEntry(SymtabEntry.InterfaceEntryKind,
                                     origin='local')
        localSymtab.enter_name(anInterfaceStmt.name,newSymtabEntry)
        interfaceSymtab.enter_name(anInterfaceStmt.name,newSymtabEntry)
    # switch to the interface's symbol table for the duration of the interface
    cur.val.symtab = interfaceSymtab
    DebugManager.debug('\tEntering interface "'+str(anInterfaceStmt.name)+'", with symbol table '+str(interfaceSymtab))
    return anInterfaceStmt

def _endInterface(anEndInterfaceStmt,cur):
    currentUnit = cur.val
    DebugManager.debug('[Line '+str(anEndInterfaceStmt.lineNumber)+']: stmt2unit._beginInterface('+str(anEndInterfaceStmt)+'): reverting from symtab '+str(currentUnit.symtab)+' to parent symtab '+str(currentUnit.symtab.parent))
    currentUnit._in_iface = False
    # switch back to the symbol table for the unit
    currentUnit.symtab = currentUnit.symtab.parent
    return anEndInterfaceStmt

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
