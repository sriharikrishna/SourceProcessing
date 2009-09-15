'''add methods to fort stmts to build units
'''

from _Setup import *

from PyUtil.symtab import Symtab,SymtabEntry,SymtabError
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
        if isinstance(a,fe.App) and a.head.lower() == 'dimension':
            return tuple(a.args)
    return ()

def _processTypedeclStmt(aTypeDeclStmt,curr):
    'type declaration -- record type in symbol table'
    localSymtab = curr.val.symtab
    newType = (aTypeDeclStmt.__class__,aTypeDeclStmt.mod)
    newLength = None
    dflt_d  = default_dims(aTypeDeclStmt.attrs)
    DebugManager.debug('[Line '+str(aTypeDeclStmt.lineNumber)+']: stmt2unit._processTypedeclStmt('+str(aTypeDeclStmt)+') with default dimensions '+str(dflt_d))
    for aDecl in aTypeDeclStmt.decls:
        DebugManager.debug('\tProcessing decl '+repr(aDecl)+' ... ',newLine=False)
        (name,newDimensions) = typesep(aDecl,dflt_d)
        try:
            # set the length for character statements
            if (aTypeDeclStmt.kw_str == 'character'):
                newLength = aTypeDeclStmt.mod and aTypeDeclStmt.mod[0] \
                                               or 1
                # extract the name and length for character declarations such as "character foo*7"
                if (isinstance(aDecl,fs._NoInit) or isinstance(aDecl,fs._AssignInit)):
                    if (isinstance(aDecl.lhs,fe.Ops) and aDecl.lhs.op == '*'):
                        DebugManager.debug('(recognized as a character statement with asterisk length specification) ... ',newLine=False)
                        name = aDecl.lhs.a1
                        newLength = aDecl.lhs.a2
            theSymtabEntry = localSymtab.lookup_name_local(name)
            if theSymtabEntry: # already in symtab -> enter new information (taking exception to any conflicts)
                DebugManager.debug('decl "'+str(aDecl)+'" already present in local symbol table as '+str(theSymtabEntry.debug(name)))
                theSymtabEntry.enterType(newType)
                theSymtabEntry.enterDimensions(newDimensions)
                theSymtabEntry.enterLength(newLength)
                # for function/subroutine entries, also update this information in the parent symbol table
                #if isinstance(theSymtabEntry.entryKind,SymtabEntry.ProcedureEntryKind):
                if localSymtab.parent and theSymtabEntry.entryKind in (SymtabEntry.FunctionEntryKind,SymtabEntry.SubroutineEntryKind):
                    replacementParentSymtabEntry = localSymtab.replicateEntry(name,str(curr.val.uinfo)+':'+name)
                    localSymtab.parent.enter_name(name,replacementParentSymtabEntry)
                    DebugManager.debug('[Line '+str(aTypeDeclStmt.lineNumber)+']: new PARENT unit symtab entry '+replacementParentSymtabEntry.debug(name))
            else: # no symtab entry -> create one
                newSymtabEntry = SymtabEntry(SymtabEntry.GenericEntryKind,
                                             type=newType,
                                             dimensions=newDimensions,
                                             length=newLength,
                                             origin='local')
                DebugManager.debug('decl "'+str(aDecl)+'" NOT already present in symbol table => adding '+str(newSymtabEntry.debug(name)))
                localSymtab.enter_name(name,newSymtabEntry)
        except SymtabError,e: # add lineNumber and symbol name to any SymtabError we encounter
            e.lineNumber = e.lineNumber or aTypeDeclStmt.lineNumber
            e.symbolName = e.symbolName or name
            raise e
    return aTypeDeclStmt

def _processDimensionStmt(aDimensionStmt,curr):
    localSymtab = curr.val.symtab
    DebugManager.debug('[Line '+str(aDimensionStmt.lineNumber)+']: stmt2unit._processDimensionStmt('+str(aDimensionStmt)+') with symbol table '+str(localSymtab))
    for anApp in aDimensionStmt.lst:
        try:
            theSymtabEntry = localSymtab.lookup_name_local(anApp.head)
            if theSymtabEntry:
                DebugManager.debug('\tvariable "'+anApp.head+'" already present in local symbol table as '+theSymtabEntry.debug(anApp.head))
                theSymtabEntry.enterDimensions(tuple(anApp.args))
            else:
                newSymtabEntry = SymtabEntry(SymtabEntry.VariableEntryKind,
                                             dimensions=tuple(anApp.args),
                                             origin='local')
                DebugManager.debug('\tvariable "'+anApp.head+'" NOT already present in symbol table -- adding '+newSymtabEntry.debug(anApp.head))
                localSymtab.enter_name(anApp.head,newSymtabEntry)
        except SymtabError,e: # add lineNumber and symbol name to any SymtabError we encounter
            e.lineNumber = e.lineNumber or aDimensionStmt.lineNumber
            e.symbolName = e.symbolName or anApp.head
            raise e
    return aDimensionStmt

def _processExternalStmt(anExternalStmt,curr):
    localSymtab = curr.val.symtab
    DebugManager.debug('[Line '+str(anExternalStmt.lineNumber)+']: stmt2unit._processExternalStmt: called on "'+str(anExternalStmt)+'" with localSymtab '+str(localSymtab))
    for aProcedureName in anExternalStmt.procedureNames:
        try:
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
                theSymtabEntry.enterEntryKind(newEntryKind)
        except SymtabError,e: # add lineNumber and symbol name to any SymtabError we encounter
            e.lineNumber = e.lineNumber or anExternalStmt.lineNumber
            e.symbolName = e.symbolName or aProcedureName
            raise e
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
def _use_module(aUseStmt,cur):
    '''
    incorporate the used module symbol table into the current unit symbol table
    issue a warning if the module has not been seen yet
    '''
    DebugManager.debug('[Line '+str(aUseStmt.lineNumber)+']: stmt2unit._use_module() for '+str(aUseStmt)+': with symtab '+str(cur.val.symtab)+' and parent symtab '+str(cur.val.symtab.parent))
    module_unit = cur.module_handler.get_module(aUseStmt.moduleName)
    if module_unit:
        try:
            if isinstance(aUseStmt,fs.UseAllStmt):
                cur.val.symtab.update_w_module_all(module_unit,aUseStmt.renameList)
            elif isinstance(aUseStmt,fs.UseOnlyStmt):
                cur.val.symtab.update_w_module_only(module_unit,aUseStmt.onlyList)
        except KeyError,e:
            raise SymtabError('error when updating a symbol table according to use statement '+str(aUseStmt),
                              symbolName=None,
                              entry=None,
                              lineNumber=aUseStmt.lineNumber)
    else:
	global reportedMissingModules
        if not (aUseStmt.moduleName.lower() in reportedMissingModules) :
            reportedMissingModules.add(aUseStmt.moduleName.lower())
            DebugManager.warning('definition for module '+aUseStmt.moduleName+' not seen in the input.',aUseStmt.lineNumber)
    return aUseStmt

def _makeFunctionEntry(aFunctionStmt,localSymtab):
    return SymtabEntry(SymtabEntry.FunctionEntryKind,aFunctionStmt.ty)

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
    DebugManager.debug('[Line '+str(self.lineNumber)+']: new unit symtab entry '+entry.debug(self.name))
    if currentSymtab.parent:
        parentSymtabEntry = currentSymtab.replicateEntry(self.name,str(cur.val.uinfo)+self.name)
        currentSymtab.parent.enter_name(self.name,parentSymtabEntry)
        DebugManager.debug('[Line '+str(self.lineNumber)+']: new PARENT unit symtab entry '+parentSymtabEntry.debug(self.name))
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

def _beginProcedureUnit(aProcedureDeclStmt,cur):
    localSymtab = Symtab(cur.val.symtab)
    DebugManager.debug('[Line '+str(aProcedureDeclStmt.lineNumber)+']: stmt2unit._beginProcedureUnit:' \
                      +' called for procedure statement "'+str(aProcedureDeclStmt)+'"' \
                      +' changing from current symtab "'+str(cur.val.symtab)+'"' \
                      +' to local symtab "'+str(localSymtab)+'"')
    entry = aProcedureDeclStmt.make_unit_entry(localSymtab)
    localSymtab.enter_name(aProcedureDeclStmt.name,entry)
    cur.val.symtab.enter_name(aProcedureDeclStmt.name,entry)
    cur.val.symtab = localSymtab
    return aProcedureDeclStmt

def _endProcedureUnit(anEndProcedureStmt,cur):
    if cur.val.symtab.parent :
        DebugManager.debug('[Line '+str(anEndProcedureStmt.lineNumber)+']: stmt2unit._endProcedureUnit:' \
                          +' called on "'+str(anEndProcedureStmt)+'"' \
                          +' ACTION: reverting from symtab "'+str(cur.val.symtab)+'"' \
                                       +' to parent symtab "'+str(cur.val.symtab.parent)+'"')
        cur.val.symtab = cur.val.symtab.parent
    else :
        raise SymtabError('stmt2unit._endProcedureUnit('+str(anEndProcedureStmt)+'):' \
                         +' cannot revert from symtab "'+str(cur.val.symtab)+'"' \
                         +' to parent symtab, because there is no parent symtab.',
                          symbolName=None,
                          entry=None,
                          lineNumber=anEndProcedureStmt.lineNumber)
    return anEndProcedureStmt

def _beginInterface(anInterfaceStmt,cur):
    currentUnit = cur.val
    currentUnit._in_iface = True
    DebugManager.debug('[Line '+str(anInterfaceStmt.lineNumber)+']: stmt2unit._beginInterface('+str(anInterfaceStmt)+')')
    return anInterfaceStmt

def _endInterface(anEndInterfaceStmt,cur):
    currentUnit = cur.val
    currentUnit._in_iface = False
    DebugManager.debug('[Line '+str(anEndInterfaceStmt.lineNumber)+']: stmt2unit._endInterface('+str(anEndInterfaceStmt)+')')
    return anEndInterfaceStmt

fs.GenStmt.unit_action        = lambda s,*rest,**kw: s
fs.GenStmt.unit_entry         = lambda s,*rest,**kw: s

fs.SubroutineStmt.unit_entry      = _unit_entry
fs.SubroutineStmt.make_unit_entry = _makeSubroutineEntry
fs.SubroutineStmt.unit_action     = _beginProcedureUnit
fs.EndSubroutineStmt.unit_action  = _endProcedureUnit

fs.FunctionStmt.unit_entry      = _unit_entry
fs.FunctionStmt.make_unit_entry = _makeFunctionEntry
fs.FunctionStmt.unit_action     = _beginProcedureUnit
fs.EndFunctionStmt.unit_action  = _endProcedureUnit

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
