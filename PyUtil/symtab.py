'''Hierachical symbol table objects =
   Linked list of caselessDicts
'''

from _Setup import *

from PyUtil.caselessDict import caselessDict as cDict
from PyUtil.debugManager import DebugManager

from PyFort.fortStmts import _PointerInit

class SymtabError(Exception):
    def __init__(self,msg,aSymtabEntry=None,lineNumber=0):
        self.msg = msg
        self.entry = aSymtabEntry
        self.lineNumber = lineNumber

class Symtab(object):
    @staticmethod
    def setTypeDefaults(defaultReal=None,defaultInt=None):
        Symtab._default_real = defaultReal
        Symtab._default_int = defaultInt

    def __init__(self,parent=None):
        self.ids    = cDict()
        self.parent = parent
        self.default_implicit()

    def default_implicit(self):
        if self.parent:
            self.implicit = self.parent.implicit
            return

        self.implicit = cDict()
        for l in '_abcdefghopqrstuvwxyz':
            self.implicit[l] = self._default_real
        for l in 'ijklmn':
            self.implicit[l] = self._default_int
    
    def implicit_none(self):
        for l in '_abcdefghijklmnopqrstuvwxyz':
            self.implicit[l] = None

    def lookup_name_local(self,name):
        '''check for name in the local symbol table exclusively (do not go up to the parent symbol table)'''
        if name in self.ids:
            return self.ids[name]
        else:
            return None

    def lookup_name_level(self,name):
        S = self
        while S:
            D = S.ids
            if name in D:
                return (D[name],S)
            S = S.parent
        return (None,None)

    def lookup_name(self,name):
        (entry,dc) = self.lookup_name_level(name)
        return entry

    def enter_name(self,name,val):
        DebugManager.debug('Symtab.enter_name: entering '+str(name)+' = '+str(val)+' in '+str(self))
        self.ids[name] = val

    def lookupDimensions(self,name):
        DebugManager.debug('Symtab.lookupDimensions: called on '+str(name))
        entry = self.lookup_name(name)
        if entry: return entry.lookupDimensions()
        return None

    def lookupType(self,name):
        (entry,level) = self.lookup_name_level(name)
        if entry: return entry.lookupType(level.implicit[name[0]])
        return self.implicit[name[0]]

    def update_w_module_all(self,aModuleUnit,renameList):
        'update self with all ids from module "unit" symtab, making sure to enter local names whenever there are renames. module name = "name"'
        # go through everything in the module and add it to the local symbol table, being sure to check for it in the rename list
        for aKey in aModuleUnit.symtab.ids.keys():
            noRename = True
            if renameList:
                for aRenameItem in renameList:
                    if aRenameItem == aKey:
                        # add the local name to the symbol table
                        self.ids[aRenameItem] = aModuleUnit.symtab.ids[aKey]
                        noRename = False
            if noRename:
                self.ids[aKey] = aModuleUnit.symtab.ids[aKey]
            self.ids[aKey].origin = 'module:'+aModuleUnit.name()

    def update_w_module_only(self,aModuleUnit,onlyList):
        'update self with the subset of ids from module "unit" symtab specified in onlyList. module name = "name"'
        for anOnlyItem in onlyList:
            if isinstance(anOnlyItem,_PointerInit):
                self.ids[anOnlyItem.lhs] = aModuleUnit.symtab.ids[anOnlyItem.rhs]
                self.ids[anOnlyItem.lhs].origin = 'module:'+aModuleUnit.name()
            else:
                self.ids[anOnlyItem] = aModuleUnit.symtab.ids[anOnlyItem]
                self.ids[anOnlyItem].origin = 'module:'+aModuleUnit.name()

    def debug(self):
        outString = 'symbol table '+str(self)+':\n'
        for aKey in self.ids.keys():
            outString += '\t'+self.ids[aKey].debug(aKey)+'\n'
        if self.parent:
            outString += self.parent.debug()
        return outString

class SymtabEntry(object):
    def __init__(self,entryKind,type=None,dimensions=None,length=None,origin=None):
        self.entryKind = entryKind
        self.type = type
        self.dimensions = dimensions
        self.length = length
        self.origin = origin

    def enterEntryKind(self,newEntryKind,lineNumber=0):
        # the replacement entry kind must be an 'instance' of the existing one.
        # for example, we can replace a procedureKind with a functionKind,
        # but we cannot replace a variableKind with a functionKind
        if not isinstance(newEntryKind(),self.entryKind):
            raise SymtabError('SymtabEntry.enterEntryKind: replace kind '+str(self.entryKind)+' with '+str(newEntryKind)+' !!!!',self,lineNumber)
        self.entryKind = newEntryKind

    def enterType(self,newType,lineNumber=0):
        DebugManager.debug('\t\tSymtabEntry.enterType: entering type '+str(newType)+' for '+str(self))
        if not newType:
            raise SymtabError('SymtabEntry.enterType: newType is None!',self,lineNumber)
        if self.type and (self.type != newType):
            raise SymtabError('SymtabEntry.enterType: Error -- current type "'+str(self.type)+'" and new type "'+str(newType)+'" conflict!',self,lineNumber)
        # procedures: entering a type means we know it's a function
        if self.entryKind == self.ProcedureEntryKind:
            DebugManager.debug('\t\t\t(SymtabEntry.enterType: entering type information tells us that this procedure is a function)')
            self.entryKind = self.FunctionEntryKind
        self.type = newType

    def enterDimensions(self,newDimensions,lineNumber=0):
        DebugManager.debug('\t\tSymtab.enterDimensions: called on '+str(self)+' and setting dimensions to '+str(newDimensions))
        if self.dimensions and (self.dimensions != newDimensions):
            raise SymtabError('SymtabEntry.enterDimensions: Error -- current dimensions "'+str(self.dimensions)+'" and new dimensions "'+str(newDimensions)+'" conflict!',self,lineNumber)
        self.dimensions = newDimensions

    def enterLength(self,newLength,lineNumber=0):
        if self.length and (self.length != newLength):
            raise SymtabError('SymtabEntry.enterLength: Error -- current length "'+str(self.length)+'" and new length "'+str(newLength)+'" conflict!',self,lineNumber)
        self.length = newLength

    def lookupDimensions(self):
        DebugManager.debug('SymtabEntry.lookupDimensions: returning '+str(self.dimensions))
        return self.dimensions

    def debug(self,name):
        return '[SymtabEntry "'+name+'" -> entryKind='+str(self.entryKind)+\
                                         ', type='+str(self.type)+\
                                         ', dimensions='+str(self.dimensions)+\
                                         ', length='+str(self.length)+\
                                         ', origin='+str(self.origin)+\
                                         ', renameSource='+str(self.renameSource)+\
                                         ']'

    class GenericEntryKind(object):
        keyword = 'unknown'

        def __init__(self):
            pass

    class InterfaceEntryKind(GenericEntryKind):
        keyword = 'interface'

    class StatementFunctionEntryKind(GenericEntryKind):
        keyword = 'statement function'

    class ProcedureEntryKind(GenericEntryKind):
        keyword = 'procedure'

    class FunctionEntryKind(ProcedureEntryKind):
        keyword = 'function'

    class SubroutineEntryKind(ProcedureEntryKind):
        keyword = 'subroutine'

    class ProgramEntryKind(ProcedureEntryKind):
        keyword = 'program'

    class VariableEntryKind(GenericEntryKind):
        keyword = 'variable'

    class CharacterEntryKind(VariableEntryKind):
        keyword = 'character'

'''
if __name__ == '__main__':
    def check_it(self):
        return 'I am symtab' + str(self)

    Symtab.check_it = check_it

    Symtab.setTypeDefaults((fs.RealStmt,[]),(fs.IntegerStmt,[])) 
    s = Symtab()
    s.enter_name('foo','top level foo')
    s1 = Symtab(s)
    s1.enter_name('bar','level 2 bar')

    res1 = s1.lookup_name('bar')
    res2 = s1.lookup_name('foo')
    res3 = s.lookup_name('bar')
    res4 = s.lookup_name('foo')

    res5 = s.check_it()
    res6 = s1.check_it()
'''
