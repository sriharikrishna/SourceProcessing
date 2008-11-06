'''Hierachical symbol table objects =
   Linked list of caselessDicts
'''

from _Setup import *

from PyUtil.caselessDict import caselessDict as cDict

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
        self.dbg = False
        self.default_implicit()

    def _set_dbg(self,on=False):
        self.dbg = on

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
        if self.dbg:
            print "entering %s = %s in %s" % (name,val,self)
        self.ids[name] = val

    def lookupDimensions(self,name):
        entry = self.lookup_name(name)
        if entry: return entry.lookupDimensions()
        return None

    def lookupType(self,name):
        (entry,level) = self.lookup_name_level(name)
        if entry: return entry.lookupType(level.implicit[name[0]])
        return self.implicit[name[0]]

    def update_w_module(self,unit):
        'update self with ids from module "unit" symtab. module name = "name"'
        module_ids = unit.symtab.ids

        origin_label = 'module:'+unit.name()

        self.ids.update(module_ids)

        for n in module_ids:
            self.ids[n].origin = origin_label

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

    def enterEntryKind(self,newEntryKind):
        # the replacement entry kind must be an 'instance' of the existing one.
        # for example, we can replace a procedureKind with a functionKind,
        # but we cannot replace a variableKind with a functionKind
        if not isinstance(newEntryKind(),self.entryKind):
            raise SymtabError('SymtabEntry.enterEntryKind: replace kind '+str(self.entryKind)+' with '+str(newEntryKind)+' !!!!',self)
        self.entryKind = newEntryKind

    def enterType(self,newType):
        if not newType:
            raise SymtabError('SymtabEntry.enterType: newType is None!')
        if self.type and (self.type != newType):
            raise SymtabError('SymtabEntry.enterType: Error -- current type',self.type,'and new type',newType,'conflict!',self)
        # procedures: entering a type means we know it's a function
        if self.entryKind == self.ProcedureEntryKind:
#           print '++++++++SymtabEntry.enterType: entering type',newType,'for procedure (we now know it is a function)'
            self.entryKind = self.FunctionEntryKind
        self.type = newType

    def enterDimensions(self,newDimensions):
        if self.dimensions and (self.dimensions != newDimensions):
            raise SymtabError('SymtabEntry.enterDimensions: Error -- current dimensions',self.dimensions,'and new dimensions',newDimensions,'conflict!',self)
        self.dimensions = newDimensions

    def lookupDimensions(self):
        return self.dimensions

    def debug(self,name):
        return '[SymtabEntry "'+name+'" -> entryKind = '+str(self.entryKind)+\
                                         ', type = '+str(self.type)+\
                                         ', dimensions = '+str(self.dimensions)+\
                                         ', length = '+str(self.length)+\
                                         ', origin = '+str(self.origin)+\
                                         ']'

    class GenericEntryKind(object):
        keyword = 'unknown'

        def __init__(self):
            pass

    class ProcedureEntryKind(GenericEntryKind):
        keyword = 'procedure'

    class FunctionEntryKind(ProcedureEntryKind):
        keyword = 'function'

    class SubroutineEntryKind(ProcedureEntryKind):
        keyword = 'function'

    class VariableEntryKind(GenericEntryKind):
        keyword = 'variable'

    class CharacterEntryKind(VariableEntryKind):
        keyword = 'variable'

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
