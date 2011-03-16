'''Hierachical symbol table objects =
   Linked list of caselessDicts
'''

import copy
import itertools

from _Setup import *

from PyUtil.caselessDict import caselessDict as cDict
from PyUtil.debugManager import DebugManager

from PyFort.fortExp import App
from PyFort.fortStmts import _PointerInit, _Kind, PrivateStmt, PublicStmt

class GenericInfo(object):
    def __init__(self):
        self.resolvableTo = {}
        self.genericName=None

    def debug(self):
        outString = 'generic info '+str(self)+':'+str(self.resolvableTo)+':'+str(self.genericName)
        return outString

class SymtabError(Exception):
    def __init__(self,msg,symbolName=None,entry=None,lineNumber=None):
        self.msg = msg
        self.symbolName = symbolName
        self.entry = entry
        self.lineNumber = lineNumber

    def __str__(self):
        errString='\nERROR: SymtabError at line '+str(self.lineNumber)+':'+str(self.msg)
        if self.entry:
            symbolNameStr = self.symbolName or '<symbol name unknown>'
            errString+='\nFor entry'+str(self.entry.debug(symbolNameStr))
        return (errString)

class FormalArgs(object):
    def __init__(self):
        self.args = {} # <name>:(<position>.<symtabEntry>) 

    def nameByPosition(self,position):
        argsIter=itertools.ifilter(lambda l: position==l[1][0],self.args.items())
        try : 
            return argsIter.next()[0]
        except StopIteration, e:
            raise SymtabError("no formal argument known for position "+str(position))

    def debug(self):
        outString = 'formal args: '+str(self.args)
        return outString

class Symtab(object):
    @staticmethod
    def setTypeDefaults(defaultReal=None,defaultInt=None):
        Symtab._default_real = defaultReal
        Symtab._default_int = defaultInt

    @staticmethod
    def getRealTypeDefault():
        if (Symtab._default_real is None): 
            raise SymtabError('Symtab.getRealTypeDefault: no type set!')
        return Symtab._default_real

    ourAccessPrefix='default'
    ourSpecificAccessKWs=[PublicStmt.kw, PrivateStmt.kw]
    def __init__(self,parent=None):
        self.ids    = cDict() # string for the key and SymtabEntry for the value
        self.parent = parent
        self.labelRefs = {} # list of statements referring to a given label
        # the following settings refer to the kw of the PublicStmt and PrivateStmt 
        self.defaultAccess=None # None | 'default'{'private'|'public'}
        self.default_implicit()

    def default_implicit(self):
        if self.parent:
            self.implicit = copy.deepcopy(self.parent.implicit)
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
        ''' val should be a SymtabEntry '''
        self.ids[name] = val
        if (not val.access):
            val.access=self.defaultAccess 

    def lookupDimensions(self,name):
        DebugManager.debug('Symtab.lookupDimensions: called on '+str(name))
        entry = self.lookup_name(name)
        if entry: return entry.lookupDimensions()
        return None

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

    def replicateEntry(self,aKey,theOrigin):
        '''
        create and return a new symbol table entry that is for use in another context
        we do not set the access attribute from theOrigin here because the access is
        determined by the new context 
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
        theNewEntry.updateOrigin(theOrigin)
        # keep a ref to the same generic Info, arg list
        theNewEntry.genericInfo=theLocalEntry.genericInfo
        theNewEntry.funcFormalArgs=theLocalEntry.funcFormalArgs
        return theNewEntry

    def update_w_module_all(self,aModuleUnit,renameList):
        'update self with all ids from module "unit" symtab, making sure to enter local names whenever there are renames. module name = "name"'
        # go through everything in the module and add it to the local symbol table, being sure to check for it in the rename list
        for aKey in aModuleUnit.symtab.ids.keys():
            if aModuleUnit.symtab.ids[aKey].isPrivate() : continue
            noRename = True
            if renameList:
                for aRenameItem in renameList:
                    if aRenameItem.rhs == aKey:
                        # add the local name to the symbol table
                        self.enter_name(aRenameItem.lhs,aModuleUnit.symtab.replicateEntry(aKey,'module:'+aModuleUnit.name()))
                        # track the original name
                        self.ids[aRenameItem.lhs].renameSource = aKey
                        noRename = False
            if noRename:
                self.enter_name(aKey,aModuleUnit.symtab.replicateEntry(aKey,'module:'+aModuleUnit.name()))

    def update_w_module_only(self,aModuleUnit,onlyList):
        'update self with the subset of ids from module "unit" symtab specified in onlyList. module name = "name"'
        for anOnlyItem in onlyList:
            # rename items: add only the lhs of the pointer init
            if isinstance(anOnlyItem,_PointerInit):
                self.enter_name(anOnlyItem.lhs,aModuleUnit.symtab.replicateEntry(anOnlyItem.rhs,'module:'+aModuleUnit.name()))
                self.ids[anOnlyItem.lhs].renameSource = anOnlyItem.rhs
            else:
                self.enter_name(anOnlyItem,aModuleUnit.symtab.replicateEntry(anOnlyItem,'module:'+aModuleUnit.name()))

    def enterLabelRef(self,label,labelRef):
        if self.labelRefs.has_key(label) :
            if (not labelRef in self.labelRefs[label]):
                self.labelRefs[label].append(labelRef)
        else:
            self.labelRefs[label]=[labelRef]
            
    def setDefaultAccess(self,anAccessKW):
        if ( not anAccessKW or not (anAccessKW in Symtab.ourSpecificAccessKWs )):
            raise SymtabError(sys._getframe().f_code.co_name+": invalid  argument")    
        if (self.defaultAccess):
            raise SymtabError(sys._getframe().f_code.co_name+": already set")
        self.defaultAccess=Symtab.ourAccessPrefix+anAccessKW
        for     entry in self.ids.values(): 
            entry.setDefaultAccess(anAccessKW)

    def debug(self):
        outString = 'symbol table '+str(self)+' (defaultAccess:'+((self.defaultAccess and self.defaultAccess) or 'None')+'):\n'
        for aKey in self.ids.keys():
            outString += '\t'+self.ids[aKey].debug(aKey)+'\n'
        outString+="\timplicit:"+str(self.implicit)+'\n'
        if self.parent:
            outString += ' parent:'+self.parent.debug()
        return outString

class SymtabEntry(object):
    def __init__(self,entryKind,type=None,dimensions=None,length=None,origin=None,renameSource=None,access=None):
        self.entryKind = entryKind # some instanve of self.GenericEntryKind
        self.type = type # pair  (type class,type modifier) 
        self.dimensions = dimensions # None or list of expressions
        self.length = length
        self.origin = origin # None | [<parent origin>'|'](| 'local' | 'external' | 'temp' | 'common:'[<common block name])
        self.renameSource = renameSource
        self.access = access# None | 'private' | 'public' | 'privatedefault' | 'publicdefault']
        # takes a GenericInfo instance when used for generic functions/subroutines (interfaces)
        self.genericInfo = None 
        # for functions takes a FormalArgs instance when used for the specific (non-generic) parameter list 
        self.funcFormalArgs = None 
        self.memberOfDrvdType = None

    @staticmethod
    def ourTypePrint(type):
        rstr=type[0].kw_str
        rstr+=len(type[1]) and str(type[1][0]) or ''
        return rstr
    
    def typePrint(self):
        return SymtabEntry.ourTypePrint(self.type)
        
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

    def addResolveName(self,resolvesTo):
        if (self.genericInfo is None):
            self.genericInfo=GenericInfo()
        if not resolvesTo.lower in self.genericInfo.resolvableTo :
            self.genericInfo.resolvableTo[resolvesTo.lower()]=None

    def enterDrvdTypeName(self,aDrvdTypeName):
        self.memberOfDrvdType=aDrvdTypeName
        
    def isPrivate(self):
        return (self.access and self.access in [PrivateStmt.kw,Symtab.ourAccessPrefix+PrivateStmt.kw])

    def setDefaultAccess(self,anAccessKW):
        if (self.access):
            if (self.access in Symtab.ourSpecificAccessKWs):
                pass  # default does not override specific
            elif (anAccessKW!=self.access):
                raise SymtabError(sys._getframe().f_code.co_name+": already set")
        else:
            if (not (anAccessKW  in Symtab.ourSpecificAccessKWs)):
                raise SymtabError(sys._getframe().f_code.co_name+": invalid argument")
            self.access=Symtab.ourAccessPrefix+anAccessKW
            
    def setSpecificAccess(self,anAccessKW):
        if (self.access):
            if (self.access in Symtab.ourSpecificAccessKWs):
                raise SymtabError(sys._getframe().f_code.co_name+": already set")
        if (not (anAccessKW  in Symtab.ourSpecificAccessKWs)):
                raise SymtabError(sys._getframe().f_code.co_name+": invalid argument")
        self.access=anAccessKW
            

    def debug(self,name='<symbol name unknown>'):
        return '[SymtabEntry('+str(self)+') "'+name+'" -> entryKind='+str(self.entryKind)+\
                                         ', type='+str(self.type)+\
                                         ', dimensions='+str(self.dimensions)+\
                                         ', length='+str(self.length)+\
                                         ', origin='+str(self.origin)+\
                                         ', renameSource='+str(self.renameSource)+\
                                         ', access='+((self.access and self.access) or 'None')+\
                                         ', genericInfo='+((self.genericInfo and str(self.genericInfo.debug())) or 'None')+\
                                         ', funcFormalArgs='+((self.funcFormalArgs and str(self.funcFormalArgs.debug())) or 'None')+\
                                         ', memberOfDrvdType='+((self.memberOfDrvdType and self.memberOfDrvdType) or 'None')+\
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

    class DerivedTypeEntryKind(GenericEntryKind):
        keyword = 'type'

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
