'''add methods to fort stmts to build units
'''

import string
from _Setup import *

from PyUtil.symtab import Symtab,SymtabEntry,SymtabError, GenericInfo
from PyUtil.debugManager import DebugManager

import fortStmts     as fs
import fortExp       as fe

#TODO: handle derived type entries

def typesep(dd,default_dims):
    '''return name and dimensions for a given decl entry
    a type declaration will either be a simple var (string)
    or an App expression
    '''
    _helper = lambda e: isinstance(e,str) and (e,default_dims) or (e.head,e.args)

    d = dd.lhs

    if isinstance(d,fe.Ops): return _helper(d.a1)
    return _helper(d)

def default_dims(attrs_list):
    for a in attrs_list:
        if isinstance(a,fe.App) and a.head.lower() == 'dimension':
            return a.args
    return None

def _beginDrvdTypeDefn(aDrvdTypeDefn,curr):
    'derived type definition -- record type in symbol table and set the name on the unit'
    localSymtab = curr.val.symtab
    theSymtabEntry = localSymtab.lookup_name_local(aDrvdTypeDefn.name)
    isPrivate = False
    curr.val._in_drvdType=aDrvdTypeDefn.name
    if theSymtabEntry: # already in symtab, shouldn't happen  
        theSymtabEntry.enterEntryKind(SymtabEntry.DerivedTypeEntryKind)
    else :
        newSymtabEntry = SymtabEntry(SymtabEntry.DerivedTypeEntryKind,
                                     type=None,
                                     dimensions=None,
                                     length=None,
                                     origin='local',
                                     isPrivate=isPrivate)
        DebugManager.debug('defn "'+str(aDrvdTypeDefn)+'" NOT already present in symbol table => adding '+str(newSymtabEntry.debug(aDrvdTypeDefn.name)))
        localSymtab.enter_name(aDrvdTypeDefn.name,newSymtabEntry)
    return aDrvdTypeDefn

def _endDrvdTypeDefn(aEndDrvdTypeDefnStmt,curr):
    'derived type definition end  -- unset the name on the unit'
    curr.val._in_drvdType=None
    return aEndDrvdTypeDefnStmt

_commonPrefix='common:'

def _processTypedeclStmt(aTypeDeclStmt,curr):
    'type declaration -- record type in symbol table'
    localSymtab = curr.val.symtab
    newType = (aTypeDeclStmt.__class__,aTypeDeclStmt.mod)
    newLength = None
    dflt_d  = default_dims(aTypeDeclStmt.attrs)
    DebugManager.debug('[Line '+str(aTypeDeclStmt.lineNumber)+']: stmt2unit._processTypedeclStmt('+str(aTypeDeclStmt)+') with default dimensions '+str(dflt_d))
    isPrivate = False
    inDrvdTypeDefn=curr.val._in_drvdType
    for anAttribute in aTypeDeclStmt.attrs :
        if isinstance(anAttribute,str) and anAttribute.lower() == 'private' :
            isPrivate = True
    for aDecl in aTypeDeclStmt.decls:
        DebugManager.debug('\tProcessing decl '+repr(aDecl)+' ... ',newLine=False)
        (name,newDimensions) = typesep(aDecl,dflt_d)
        if inDrvdTypeDefn:
            name=inDrvdTypeDefn+":"+name
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
                if (theSymtabEntry.dimensions and (newDimensions is None)):
                    pass
                else:
                    theSymtabEntry.enterDimensions(newDimensions)
                theSymtabEntry.enterLength(newLength)
                if inDrvdTypeDefn:
                    theSymtabEntry.enterDrvdTypeName(inDrvdTypeDefn)
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
                                             origin='local',
                                             isPrivate=isPrivate)
                if inDrvdTypeDefn:
                    newSymtabEntry.enterDrvdTypeName(inDrvdTypeDefn)
                DebugManager.debug('decl "'+str(aDecl)+'" NOT already present in symbol table => adding '+str(newSymtabEntry.debug(name)))
                localSymtab.enter_name(name,newSymtabEntry)
            unitSymbolEntry,sTable=localSymtab.lookup_name_level(curr.val.name())
            if (unitSymbolEntry and unitSymbolEntry.entryKind==SymtabEntry.FunctionEntryKind and  unitSymbolEntry.genericInfo and unitSymbolEntry.genericInfo.genericName):
                genericSymbolEntry=localSymtab.lookup_name(unitSymbolEntry.genericInfo.genericName)
                if (genericSymbolEntry is None):
                    raise SymtabError('cannot find generic '+unitSymbolEntry.genericInfo.genericName+' for specific '+curr.val.name()+' entry: '+unitSymbolEntry.debug(curr.val.name()))
                if (genericSymbolEntry.genericInfo is None or (not curr.val.name().lower() in genericSymbolEntry.genericInfo.resolvableTo)):
                    raise SymtabError('no info available for specific '+curr.val.name()+' in generic entry: '+genericSymbolEntry.debug(unitSymbolEntry.genericInfo.genericName))
                argsTypeDict=localSymtab.lookup_name(unitSymbolEntry.genericInfo.genericName).genericInfo.resolvableTo[curr.val.name().lower()]
                if (argsTypeDict is None):
                    raise SymtabError('no arguments set for specific '+curr.val.name()+' under generic '+ unitSymbolEntry.genericInfo.genericName+' entry: '+localSymtab.lookup_name(unitSymbolEntry.genericInfo.genericName).debug(unitSymbolEntry.genericInfo.genericName))
                if name.lower() in argsTypeDict:
                    argsTypeDict[name.lower()]=localSymtab.lookup_name_local(name).type
                    DebugManager.debug('recorded type in '+str(id(argsTypeDict))+str(argsTypeDict))
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

def _processCommonStmt(aCommonStmt,curr):
    localSymtab = curr.val.symtab
    DebugManager.debug('['+':'+str(aCommonStmt.lineNumber)+']: stmt2unit._processCommonStmt: called on "'+repr(aCommonStmt)+'" with localSymtab '+str(localSymtab) +" implicit "+str(localSymtab.implicit))
    for aDecl in aCommonStmt.declList:
        try:
            aDeclName=""
            if isinstance(aDecl,fe.App):
                aDeclName=aDecl.head
            else:
                aDeclName=aDecl
            theSymtabEntry = localSymtab.lookup_name(aDeclName)
            if not theSymtabEntry:
                newSymtabEntry = SymtabEntry(SymtabEntry.VariableEntryKind,
                                             origin=_commonPrefix+aCommonStmt.name)
                localSymtab.enter_name(aDeclName,newSymtabEntry)
                if isinstance(aDecl,fe.App):
                    newSymtabEntry.enterDimensions(aDecl.args)
                DebugManager.debug('\tcommon variable NOT already present in symbol table -- adding '+newSymtabEntry.debug(aDeclName))
            else:
                DebugManager.debug('\tcommon variable already has SymtabEntry'+theSymtabEntry.debug(aDeclName))
                theSymtabEntry.enterEntryKind(SymtabEntry.VariableEntryKind)
                if isinstance(aDecl,fe.App):
                    theSymtabEntry.enterDimensions(aDecl.args)
                theSymtabEntry.updateOrigin(_commonPrefix+aCommonStmt.name)
        except SymtabError,e: # add lineNumber and symbol name to any SymtabError we encounter
            e.lineNumber = e.lineNumber or aCommonStmt.lineNumber
            e.symbolName = e.symbolName or aDeclName
            raise e
    return aCommonStmt

def _assign2stmtfn(anAssignmentStmt,curr):
    'convert assign stmt to stmtfn, and enter in unit symtab'
    DebugManager.debug('[Line '+str(anAssignmentStmt.lineNumber)+']: converting '+str(anAssignmentStmt)+' to stmt fn')
    newStmtFn = fs.StmtFnStmt(anAssignmentStmt.lhs.head,
                              anAssignmentStmt.lhs.args,
                              anAssignmentStmt.rhs,
                              lineNumber=anAssignmentStmt.lineNumber,
                              label=anAssignmentStmt.label,
                              lead=anAssignmentStmt.lead)
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
        DebugManager.debug('updating '+str(cur.val.symtab)+') with module "'+aUseStmt.moduleName+'", which has unit '+str(module_unit),newLine=False)
        try:
            if isinstance(aUseStmt,fs.UseAllStmt):
                DebugManager.debug(' where we are using ALL')
                cur.val.symtab.update_w_module_all(module_unit,aUseStmt.renameList)
            elif isinstance(aUseStmt,fs.UseOnlyStmt):
                DebugManager.debug(' where we are using ONLY '+str(aUseStmt.onlyList))
                cur.val.symtab.update_w_module_only(module_unit,aUseStmt.onlyList)
        except KeyError,e:
            raise SymtabError('KeyError for key "'+str(e)+'"' \
                             +' while updating '+cur.val.symtab.debug() \
                             +' according to use statement "'+str(aUseStmt)+'"',
                              symbolName=str(e),
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
    if (currentSymtab.parent and (self.name in currentSymtab.parent.ids))  :
        # this must be the definition of a previously  declared module procedure
        mpSymTabEntry=currentSymtab.parent.ids[self.name]
        if (mpSymTabEntry.entryKind!=SymtabEntry.ProcedureEntryKind
            or
            mpSymTabEntry.genericInfo is None):
            raise SymtabError('parent symbol is not a module procedure')
        entry = self.make_unit_entry(currentSymtab)
        mpSymTabEntry.entryKind=entry.entryKind
        mpSymTabEntry.type=entry.type
        entry.genericInfo=mpSymTabEntry.genericInfo
        currentSymtab.enter_name(self.name,entry)
        # if it is a function  - collect argument information
        if (isinstance(self,fs.FunctionStmt)) :
            genSymTabEntry=currentSymtab.parent.lookup_name(mpSymTabEntry.genericInfo.genericName)
            if (genSymTabEntry is None):
                raise SymtabError('cannot find generic with name '+mpSymTabEntry.genericInfo.genericName)
            argsTypeDict={}
            for arg in self.args:
                argsTypeDict[arg.lower()]=None # don't know the type yet
            genSymTabEntry.genericInfo.resolvableTo[self.name.lower()]=argsTypeDict
            DebugManager.debug('\tstmt2unit._unit_entry(): argsTypeDict : '+str(id(argsTypeDict)))
            DebugManager.debug('\tstmt2unit._unit_entry() parent symboltable entry: '+mpSymTabEntry.debug(self.name)+' \n\t\tgeneric entry '+genSymTabEntry.debug(mpSymTabEntry.genericInfo.genericName)+' with argstypedict: '+str(id(argsTypeDict))+'\n\t\tself entry: '+entry.debug(self.name))
        else :
            DebugManager.debug('\tstmt2unit._unit_entry() parent symboltable entry '+mpSymTabEntry.debug(self.name)+' self entry '+entry.debug(self.name))
    else: 
        entry = self.make_unit_entry(currentSymtab)
        currentSymtab.enter_name(self.name,entry)
        DebugManager.debug('[Line '+str(self.lineNumber)+']: new unit symtab entry '+entry.debug(self.name))
        if currentSymtab.parent:
            parentSymtabEntry = currentSymtab.replicateEntry(self.name,str(cur.val.uinfo)+self.name)
            currentSymtab.parent.enter_name(self.name,parentSymtabEntry)
            DebugManager.debug('[Line '+str(self.lineNumber)+']: new PARENT unit symtab entry '+parentSymtabEntry.debug(self.name))
    DebugManager.debug('[Line '+str(self.lineNumber)+']: stmt2unit._unit_entry() for '+str(self)+': with symtab '+str(currentSymtab)+' with parent symtab '+str(currentSymtab.parent))
    if (isinstance(self,fs.FunctionStmt)): 
        cur.val._in_functionDecl=self
    return self

def _unit_exit(self,cur):
    '''exit a subroutine or function
    '''
    if cur.val._in_functionDecl:
        theSymtabEntry=cur.val.symtab.lookup_name(cur.val._in_functionDecl.name)
        if (theSymtabEntry.type is None and cur.val._in_functionDecl.result):
            # try to get the tupe from the result symbol
            theResultEntry=cur.val.symtab.lookup_name(cur.val._in_functionDecl.result)
            if (theResultEntry):
                theSymtabEntry.enterType(theResultEntry.type)
                if cur.val.symtab.parent:  # update the copy in the parent
                    cur.val.symtab.parent.lookup_name(cur.val._in_functionDecl.name).enterType(theResultEntry.type)
        cur.val._in_functionDecl=None         
    return self

def _implicit(self,cur):
    '''update the implicit table
    '''
    alphabet=string.ascii_lowercase
    for (type_spec,letter_spec) in self.lst: 
        for e in letter_spec:
            if isinstance(e,fe.Ops): # something like 'q-t' for which we get Ops('-','q','t')
                for letter in alphabet[ord(e.a1.lower())-ord(alphabet[0]):ord(e.a2.lower())-ord(alphabet[0])+1]:
                    cur.val.symtab.implicit[letter] = type_spec
            else:
                cur.val.symtab.implicit[e] = type_spec
    DebugManager.debug('[Line '+str(self.lineNumber)+']: stmt2unit._implicit() implicit table is now '+str(cur.val.symtab.implicit)+str(cur.val.symtab))
    return self

def _implicit_none(self,cur):
    cur.val.symtab.implicit_none()
    DebugManager.debug('[Line '+str(self.lineNumber)+']: stmt2unit._implicit_none() implicit table is now '+str(cur.val.symtab.implicit)+str(cur.val.symtab))
    return self

def _beginProcedureUnit(aProcedureDeclStmt,cur):
    '''
    called for function/subroutine statements within an interface block
    '''
    localSymtab = Symtab(cur.val.symtab)
    DebugManager.debug('[Line '+str(aProcedureDeclStmt.lineNumber)+']: stmt2unit._beginProcedureUnit:' \
                      +' called for '+aProcedureDeclStmt.__class__.__name__+': "'+str(aProcedureDeclStmt)+'"' \
                      +' changing from current symtab "'+str(cur.val.symtab)+'"' \
                      +' to local symtab "'+str(localSymtab)+'"')
    entry = aProcedureDeclStmt.make_unit_entry(localSymtab)
    localSymtab.enter_name(aProcedureDeclStmt.name,entry)
    cur.val.symtab.enter_name(aProcedureDeclStmt.name,entry)
    cur.val.symtab = localSymtab
    if (isinstance(aProcedureDeclStmt,fs.FunctionStmt)): 
        cur.val._in_functionDecl=aProcedureDeclStmt
    return aProcedureDeclStmt

def _endProcedureUnit(anEndProcedureStmt,cur):
    '''
    called for function/subroutine end statements within an interface block
    '''
    if cur.val._in_functionDecl:
        theSymtabEntry=cur.val.symtab.lookup_name(cur.val._in_functionDecl.name)
        if (theSymtabEntry.type is None and cur.val._in_functionDecl.result):
            # try to get the tupe from the result symbol
            theResultEntry=cur.val.symtab.lookup_name(cur.val._in_functionDecl.name)
            if (theResultEntry):
                theSymtabEntry.enterType(theResultEntry.type)
        cur.val._in_functionDecl=None         
    if cur.val.symtab.parent :
        DebugManager.debug('[Line '+str(anEndProcedureStmt.lineNumber)+']: stmt2unit._endProcedureUnit:' \
                          +' called on "'+str(anEndProcedureStmt)+'"' \
                          +' ACTION: on unit '+str(cur.val)+' reverting from symtab "'+str(cur.val.symtab)+'"' \
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
    if (anInterfaceStmt.name): 
        cur.val.symtab.enter_name(anInterfaceStmt.name,SymtabEntry(SymtabEntry.InterfaceEntryKind))
    currentUnit = cur.val
    currentUnit._in_iface = True
    DebugManager.debug('[Line '+str(anInterfaceStmt.lineNumber)+']: stmt2unit._beginInterface('+str(anInterfaceStmt)+')')
    if (anInterfaceStmt.name): 
        # collect all the procedurenames in a mock symtab...
        localSymtab = Symtab(cur.val.symtab)
        cur.val.symtab = localSymtab
    # local attribute added on to convey the name to _endInterface
    cur.val.ifName=anInterfaceStmt.name
    return anInterfaceStmt

def _processProcedureStmt(aProcedureStmt,curr):
    localSymtab = curr.val.symtab
    DebugManager.debug('[Line '+str(aProcedureStmt.lineNumber)+']: stmt2unit._processProcedureStmt: called on "'+str(aProcedureStmt)+'" with localSymtab '+str(localSymtab))
    for aProcedureName in aProcedureStmt.procedureList:
        try:
            theSymtabEntry = localSymtab.lookup_name(aProcedureName)
            if not theSymtabEntry:
                newSymtabEntry = SymtabEntry(SymtabEntry.ProcedureEntryKind)
                localSymtab.enter_name(aProcedureName,newSymtabEntry)
                DebugManager.debug('\t_processProcedureStmt: module procedure NOT already present in symbol table -- adding '+newSymtabEntry.debug(aProcedureName))
            else:
                DebugManager.debug('\t_processProcedureStmt: module procedure already has SymtabEntry'+theSymtabEntry.debug(aProcedureName))
                # if the entry has a type, we know it's a function
                newEntryKind = theSymtabEntry.type and SymtabEntry.FunctionEntryKind \
                                                    or SymtabEntry.ProcedureEntryKind
                theSymtabEntry.enterEntryKind(newEntryKind)
        except SymtabError,e: # add lineNumber and symbol name to any SymtabError we encounter
            e.lineNumber = e.lineNumber or aProcedureStmt.lineNumber
            e.symbolName = e.symbolName or aProcedureName
            raise e
    return aProcedureStmt

def _endInterface(anEndInterfaceStmt,cur):
    # get all the procedurenames from the mock symtab...
    mockSymtab=cur.val.symtab
    ifName=cur.val.ifName
    if (ifName) : 
        cur.val.symtab = cur.val.symtab.parent
        theSymtabEntry = cur.val.symtab.lookup_name(ifName)
        for name in mockSymtab.ids.keys():
            theSymtabEntry.addResolveName(name)
        for name in mockSymtab.ids.keys():
            try:
                theSymtabEntry = cur.val.symtab.lookup_name(name)
                if not theSymtabEntry:
                    newSymtabEntry = SymtabEntry(SymtabEntry.ProcedureEntryKind)
                    newSymtabEntry.genericInfo=GenericInfo()
                    newSymtabEntry.genericInfo.genericName=ifName.lower()
                    cur.val.symtab.enter_name(name,newSymtabEntry)
                    DebugManager.debug('\tmodule procedure NOT already present in symbol table -- adding '+newSymtabEntry.debug(name))
                else:
                    DebugManager.debug('\tmodule procedure already has SymtabEntry'+theSymtabEntry.debug(name))
                    # if the entry has a type, we know it's a function
                    newEntryKind = theSymtabEntry.type and SymtabEntry.FunctionEntryKind \
                                                        or SymtabEntry.ProcedureEntryKind
                    theSymtabEntry.enterEntryKind(newEntryKind)
                    if (theSymtabEntry.genericInfo):
                        if(theSymtabEntry.genericInfo.genericName!=ifName.lower()):
                            raise SymtabError('mismatched generic name')
                    else :
                        newSymtabEntry.genericInfo=GenericInfo()
                        newSymtabEntry.genericInfo.genericName=ifName.lower()
            except SymtabError,e: # add lineNumber and symbol name to any SymtabError we encounter
                e.lineNumber = e.lineNumber or aProcedureStmt.lineNumber
                e.symbolName = e.symbolName or aProcedureName
                raise e
    currentUnit = cur.val
    currentUnit._in_iface = False
    DebugManager.debug('[Line '+str(anEndInterfaceStmt.lineNumber)+']: stmt2unit._endInterface('+str(anEndInterfaceStmt)+')')
    return anEndInterfaceStmt

fs.GenStmt.unit_action            = lambda s,*rest,**kw: s
fs.GenStmt.unit_entry             = lambda s,*rest,**kw: s
fs.GenStmt.unit_exit              = lambda s,*rest,**kw: s

fs.SubroutineStmt.unit_entry      = _unit_entry
fs.SubroutineStmt.make_unit_entry = _makeSubroutineEntry
fs.SubroutineStmt.unit_action     = _beginProcedureUnit
fs.EndSubroutineStmt.unit_action  = _endProcedureUnit
fs.EndSubroutineStmt.unit_exit    = _unit_exit

fs.FunctionStmt.unit_entry        = _unit_entry
fs.FunctionStmt.make_unit_entry   = _makeFunctionEntry
fs.FunctionStmt.unit_action       = _beginProcedureUnit
fs.EndFunctionStmt.unit_action    = _endProcedureUnit
fs.EndFunctionStmt.unit_exit      = _unit_exit

fs.AssignStmt.is_decl         = _is_stmt_fn
fs.AssignStmt.unit_action     = _assign2stmtfn

fs.DimensionStmt.unit_action = _processDimensionStmt

fs.ExternalStmt.unit_action  = _processExternalStmt

fs.TypeDecl.unit_action       = _processTypedeclStmt

fs.CommonStmt.unit_action     = _processCommonStmt

fs.UseStmt.unit_action        = _use_module

fs.ImplicitNone.unit_action   = _implicit_none
fs.ImplicitStmt.unit_action   = _implicit

fs.InterfaceStmt.unit_action  = _beginInterface
fs.ProcedureStmt.unit_action  = _processProcedureStmt
fs.EndInterfaceStmt.unit_action = _endInterface

fs.DrvdTypeDefn.unit_action = _beginDrvdTypeDefn 
fs.EndDrvdTypeDefn.unit_action = _endDrvdTypeDefn 
