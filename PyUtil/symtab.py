'''Hierachical symbol table objects =
   Linked list of caselessDicts
'''

from _Setup import *

from PyUtil.caselessDict import caselessDict as cDict
from PyUtil.debugManager import DebugManager

from PyFort.fortExp import App
from PyFort.fortStmts import _PointerInit,_Kind

class SymtabError(Exception):
    def __init__(self,msg,symbolName=None,entry=None,lineNumber=None):
        self.msg = msg
        self.symbolName = symbolName
        self.entry = entry
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

    def _contextReplace(self,anExpression):
        '''
        replace an instances in anExpression of things that may be local to this context (such as renames)
        with things that are applicable in a general context
        '''
        if isinstance(anExpression,str):
            if (anExpression in self.ids) and (self.ids[anExpression].renameSource):
                return self.ids[anExpression].renameSource
        elif isinstance(anExpression,App):
            for anArg in anExpression.args:
                anArg = self._contextReplace(anArg)
        else:
            raise SymtabError('Symtab._contextReplace: Error -- expression'+str(anExpression)+'is not a string and not an App!')
        return anExpression

    def replicateEntry(self,aKey,localOrigin):
        '''
        create and return a new symbol table entry that is for use in another context
        In particular, anything that is the result of a local rename must be reverted to the original name for use in other contexts
        '''
        theLocalEntry = self.ids[aKey]
        theNewEntry = SymtabEntry(theLocalEntry.entryKind)
        # set the type
        theNewEntry.type = theLocalEntry.type
        if (theNewEntry.type):
            DebugManager.debug('symtab.replicateEntry('+theLocalEntry.debug(aKey)+')')
            # look for attribute mod names in this symbol table.  If they are in there and have a rename, use the rename in the new entry
            (typename,modifiers) = theNewEntry.type
            for aModifier in modifiers:
                if isinstance(aModifier,_Kind):
                    aModifier.mod = self._contextReplace(aModifier.mod)
        # set the dimensions
        theNewEntry.dimensions = theLocalEntry.dimensions
        # set the length
        theNewEntry.length = theLocalEntry.length
        # set the origin
        theNewEntry.updateOrigin(localOrigin)
        return theNewEntry

    def update_w_module_all(self,aModuleUnit,renameList):
        'update self with all ids from module "unit" symtab, making sure to enter local names whenever there are renames. module name = "name"'
        # go through everything in the module and add it to the local symbol table, being sure to check for it in the rename list
        for aKey in aModuleUnit.symtab.ids.keys():
            if aModuleUnit.symtab.ids[aKey].isPrivate : continue
            noRename = True
            if renameList:
                for aRenameItem in renameList:
                    if aRenameItem.rhs == aKey:
                        # add the local name to the symbol table
                        self.ids[aRenameItem.lhs] = aModuleUnit.symtab.replicateEntry(aKey,'module:'+aModuleUnit.name())
                        self.ids[aRenameItem.lhs].renameSource = aKey
                        noRename = False
            if noRename:
                self.ids[aKey] = aModuleUnit.symtab.replicateEntry(aKey,'module:'+aModuleUnit.name())

    def update_w_module_only(self,aModuleUnit,onlyList):
        'update self with the subset of ids from module "unit" symtab specified in onlyList. module name = "name"'
        for anOnlyItem in onlyList:
            # rename items: add only the lhs of the pointer init
            if isinstance(anOnlyItem,_PointerInit):
                self.ids[anOnlyItem.lhs] = aModuleUnit.symtab.replicateEntry(anOnlyItem.rhs,'module:'+aModuleUnit.name())
                self.ids[anOnlyItem.lhs].renameSource = anOnlyItem.rhs
            else:
                self.ids[anOnlyItem] = aModuleUnit.symtab.replicateEntry(anOnlyItem,'module:'+aModuleUnit.name())

    def debug(self):
        outString = 'symbol table '+str(self)+':\n'
        for aKey in self.ids.keys():
            outString += '\t'+self.ids[aKey].debug(aKey)+'\n'
        if self.parent:
            outString += self.parent.debug()
        return outString

class SymtabEntry(object):
    def __init__(self,entryKind,type=None,dimensions=None,length=None,origin=None,renameSource=None,isPrivate=False):
        self.entryKind = entryKind
        self.type = type
        self.dimensions = dimensions
        self.length = length
        self.origin = origin
        self.renameSource = renameSource
        self.isPrivate = isPrivate

    def enterEntryKind(self,newEntryKind):
        # the replacement entry kind must be an 'instance' of the existing one.
        # for example, we can replace a procedureKind with a functionKind,
        # but we cannot replace a variableKind with a functionKind
        if not isinstance(newEntryKind(),self.entryKind):
            raise SymtabError('name clash between symbols with kind '+str(self.entryKind)+' and kind '+str(newEntryKind)+' ',entry=self)
        self.entryKind = newEntryKind

    def enterType(self,newType):
        DebugManager.debug('\t\tSymtabEntry.enterType: entering type '+str(newType)+' for '+str(self))
        if not newType:
            raise SymtabError('SymtabEntry.enterType: newType is None!',entry=self)
        if self.type : # assume a name clash
            raise SymtabError('SymtabEntry.enterType: Name clash -- the declaration for this symbol conflicts with an earlier declaration using the same name"',entry=self)
           # The following code makes some (incomplete) effort to determine the equality of the current type and the new type.
           # It is commented out because (1) it's incomplete and (2): it's not clear that fortran allows us to re-declare any type information for a particular symbol.
           #(currentTypeClass,currentTypeModifier) = self.type
           #(newTypeClass,newTypeModifier) = newType
           #if (currentTypeClass != newTypeClass):
           #    raise SymtabError('SymtabEntry.enterType: Error -- current type class "'+str(currentTypeClass)+
           #                                                    '" and new type class "'+str(newTypeClass)+'" conflict!',entry=self)
           #if (currentTypeModifier[0].mod != newTypeModifier[0].mod):
           #    raise SymtabError('SymtabEntry.enterType: Error -- current type modifier "'+str(currentTypeModifier[0].mod)+
           #                                                    '" and new type modifier "'+str(newTypeModifier[0].mod)+'" conflict!',entry=self)
        # procedures: entering a type means we know it's a function
        if self.entryKind == self.ProcedureEntryKind:
            DebugManager.debug('\t\t\t(SymtabEntry.enterType: entering type information tells us that this procedure is a function)')
            self.entryKind = self.FunctionEntryKind
        self.type = newType

    def enterDimensions(self,newDimensions):
        DebugManager.debug('\t\tSymtab.enterDimensions: called on '+str(self)+' and setting dimensions to '+str(newDimensions))
        if self.dimensions and (self.dimensions != newDimensions):
            raise SymtabError('SymtabEntry.enterDimensions: Error -- current dimensions "'+str(self.dimensions)+'" and new dimensions "'+str(newDimensions)+'" conflict!',entry=self)
       # The following code makes some (incomplete) effort to determine the equality of the current dimensions and the new dimensions.
       # See the similar comment in enterType above for more details.
       #if self.dimensions:
       #    for currentDimItem,newDimItem in zip(self.dimensions,newDimensions):
       #        if (currentDimItem != newDimItem):
       #            raise SymtabError('SymtabEntry.enterDimensions: Error -- current dimensions "'+str(self.dimensions)+
       #                                                                  '" and new dimensions "'+str(newDimensions)+'" conflict!',entry=self)
        self.dimensions = newDimensions

    def enterLength(self,newLength):
        if self.length and (self.length != newLength):
            raise SymtabError('SymtabEntry.enterLength: Error -- current length "'+str(self.length)+'" and new length "'+str(newLength)+'" conflict!',entry=self)
        self.length = newLength

    def lookupDimensions(self):
        DebugManager.debug('SymtabEntry.lookupDimensions: returning '+str(self.dimensions))
        return self.dimensions

    def updateOrigin(self,anOriginStr):
        if self.origin:
            self.origin = self.origin+'|'+anOriginStr
        else:
            self.origin = anOriginStr

    def debug(self,name='<symbol name unknown>'):
        return '[SymtabEntry "'+name+'" -> entryKind='+str(self.entryKind)+\
                                         ', type='+str(self.type)+\
                                         ', dimensions='+str(self.dimensions)+\
                                         ', length='+str(self.length)+\
                                         ', origin='+str(self.origin)+\
                                         ', renameSource='+str(self.renameSource)+\
                                         ', isPrivate='+str(self.isPrivate)+\
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
