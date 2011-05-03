'''Hierachical symbol table objects =
   Linked list of caselessDicts
'''

import copy
import itertools

from _Setup import *

from PyUtil.caselessDict import caselessDict as cDict
from PyUtil.debugManager import DebugManager

from PyFort.fortExp import App, Unary,Ops,is_const,_id_re
from PyFort.fortStmts import _PointerInit, _Kind, PrivateStmt, PublicStmt
from PyFort.intrinsic import is_intrinsic

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
        self.renames = cDict() # renameSource for the key and new name for the value
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

    def __renamedToSource(self,anExpression):
        '''
        replace an instances in anExpression of things that may be local to this context (such as renames)
        with things that are applicable in a general context
        '''
        if isinstance(anExpression,str):
            if (anExpression in self.ids) and (self.ids[anExpression].renameSource):
                return self.ids[anExpression].renameSource
        elif isinstance(anExpression,App):
            for anArg in anExpression.args:
                anArg = self.__renamedToSource(anArg)
        else:
            raise SymtabError(sys._getframe().f_code.co_name+': expression'+str(anExpression)+'is not a string and not an App!')
        return anExpression

    def __sourceToRenamed(self,anExpression):
        '''
        replace an instances in anExpression of things that may be local to this context (such as renames)
        with things that are applicable in a general context
        '''
        if isinstance(anExpression,App):
            for anArg in anExpression.args:
                anArg = self.__sourceToRenamed(anArg)
        elif isinstance(anExpression,str):
            if anExpression in self.renames:
                name=self.renames[anExpression]
                return name
        else:
            raise SymtabError(sys._getframe().f_code.co_name+': expression'+str(anExpression)+'is not a string and not an App!')
        return anExpression

    def __typeHandler(self,targetEntry,targetEntrySymtab):
        if (targetEntry.type):
            # look for attribute mod names in this symbol table.  If they are in there and have a rename, use the rename in the new entry
            (typename,modifiers) = targetEntry.type
            for aModifier in modifiers:
                if isinstance(aModifier,_Kind):
                    aModifier.mod = targetEntrySymtab.__sourceToRenamed(self.__renamedToSource(aModifier.mod))
        
        
    def replicateEntry(self,aKey,theOrigin,newName,otherSymtab):
        '''
        create and return a new symbol table entry that is for use in another context
        we do not set the access attribute from theOrigin here because the access is
        determined by the new context
        the entry is added under otherName to newSymtab
        '''
        theLocalEntry = self.ids[aKey]
        theNewEntry = SymtabEntry(theLocalEntry.entryKind)
        # set the type
        theNewEntry.type = SymtabEntry.copyType(theLocalEntry.type)
        self.__typeHandler(theNewEntry,otherSymtab)
        # set the dimensions
        theNewEntry.dimensions = theLocalEntry.dimensions
        # set the length
        theNewEntry.length = theLocalEntry.length
        # set the constInit
        theNewEntry.constInit = theLocalEntry.constInit
        # set the origin
        theNewEntry.updateOrigin(theOrigin)
        # keep a ref to the same generic Info, arg list
        theNewEntry.genericInfo=theLocalEntry.genericInfo
        theNewEntry.funcFormalArgs=theLocalEntry.funcFormalArgs
        otherSymtab.enter_name(newName,theNewEntry)
        DebugManager.debug(sys._getframe().f_code.co_name+': original < '+theLocalEntry.debug(aKey)+' > replicated as < '+theNewEntry.debug(newName)+' >')
        return theNewEntry

    def augmentParentEntry(self,origEntry,parentEntry,parentEntryName):
        if (self.parent.ids[parentEntryName]!=parentEntry):
            raise SymtabError(sys._getframe().f_code.co_name+": entry "+parentEntry.debug(parentEntryName)+" not in parent symbol table "+self.parend.debug())    
        parentEntry._augmentParentEntryFrom(origEntry)
        self.__typeHandler(parentEntry,self.parent)
        
    def update_w_module_all(self,aModuleUnit,renameList):
        'update self with all ids from module "unit" symtab, making sure to enter local names whenever there are renames. module name = "name"'
        # go through everything in the module and add it to the local symbol table, being sure to check for it in the rename list
        for aKey in aModuleUnit.symtab.ids.keys():
            if aModuleUnit.symtab.ids[aKey].isPrivate() : continue
            noRename = True
            if renameList:
                for aRenameItem in renameList:
                    if aRenameItem.rhs == aKey:
                        self.renames[aKey]=aRenameItem.lhs
                        # add the local name to the symbol table and track the original name
                        aModuleUnit.symtab.replicateEntry(aKey,'module:'+aModuleUnit.name(),aRenameItem.lhs,self).renameSource = aKey
                        noRename = False
            if noRename:
                aKey,aModuleUnit.symtab.replicateEntry(aKey,'module:'+aModuleUnit.name(),aKey,self)

    def update_w_module_only(self,aModuleUnit,onlyList):
        'update self with the subset of ids from module "unit" symtab specified in onlyList. module name = "name"'
        for anOnlyItem in onlyList:
            # rename items: add only the lhs of the pointer init
            if isinstance(anOnlyItem,_PointerInit):
                self.renames[anOnlyItem.rhs]=anOnlyItem.lhs
                aModuleUnit.symtab.replicateEntry(anOnlyItem.rhs,'module:'+aModuleUnit.name(),anOnlyItem.lhs,self).renameSource=anOnlyItem.rhs
            else:
                aModuleUnit.symtab.replicateEntry(anOnlyItem,'module:'+aModuleUnit.name(),anOnlyItem,self)

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

    def isConstInit(self,anExpression):
        if isinstance(anExpression,str) and is_const(anExpression):
            return True
        elif isinstance(anExpression,str) and _id_re.match(anExpression):
            stE=self.lookup_name(anExpression)
            if (stE and stE.constInit):
                return True
        elif isinstance(anExpression,Unary):
            return self.isConstInit(anExpression.exp)
        elif isinstance(anExpression,Ops):
            return self.isConstInit(anExpression.a1) and self.isConstInit(anExpression.a2)
        elif isinstance(anExpression,App):
            if is_intrinsic(anExpression.head):
                if anExpression.args:
                    return all(map(lambda l:self.isConstInit(l),anExpression.args))
                else: # e.g. for NULL()
                    return True
        return False

    def getConstInit(self,anExpression):
        if isinstance(anExpression,str) and is_const(anExpression):
            return anExpression
        elif isinstance(anExpression,str) and _id_re.match(anExpression):
            stE=self.lookup_name(anExpression)
            if (stE and stE.constInit):
                return stE.constInit
        elif isinstance(anExpression,Unary):
            if (self.isConstInit(anExpression.exp)):
                cpExpression=anExpression
                cpExpression.exp=self.getConstInit(anExpression.exp)
                return cpExpression
        elif isinstance(anExpression,Ops):
            if ( self.isConstInit(anExpression.a1) and self.isConstInit(anExpression.a2)):
                cpExpression=anExpression
                cpExpression.a1=self.getConstInit(anExpression.a1)
                cpExpression.a2=self.getConstInit(anExpression.a2)
                return cpExpression
        elif isinstance(anExpression,App):
            if is_intrinsic(anExpression.head):
                if ( all(map(lambda l:self.isConstInit(l),anExpression.args))):
                    cpExpression=anExpression
                    cpExpression.args=map(lambda l:self.getConstInit(l),anExpression.args)
                    return cpExpression
        raise SymtabError(sys._getframe().f_code.co_name+": cannot get it for "+str(anExpression)+" parsed as "+repr(anExpression))

                
    def debug(self):
        outString = 'symbol table '+str(self)+' (defaultAccess:'+((self.defaultAccess and self.defaultAccess) or 'None')+'):\n'
        for aKey in self.ids.keys():
            outString += '\t'+self.ids[aKey].debug(aKey)+'\n'
        outString+="\timplicit:"+str(self.implicit)+'\n'
        if self.parent:
            outString += ' parent:'+self.parent.debug()
        return outString

class SymtabEntry(object):

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

    def __init__(self,entryKind,type=None,dimensions=None,length=None,origin=None,renameSource=None,access=None):
        self.entryKind = entryKind # some instanve of self.GenericEntryKind
        self.type = type # pair  (type class,type modifier) 
        self.dimensions = dimensions # None or list of expressions
        self.length = length # specific for character statements, see stmt2unit
        self.constInit=None #  expression if initialized with a constant expression 
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

    @staticmethod
    def copyType(type):
        if not type:
            return None
        mods=[]
        for m in type[1]:
            mods.append(copy.deepcopy(m))
        return (type[0],mods)
    
    def typePrint(self):
        return SymtabEntry.ourTypePrint(self.type)
        
    def enterEntryKind(self,newEntryKind):
        # the replacement entry kind must be an 'instance' of the existing one.
        # for example, we can replace a procedureKind with a functionKind,
        # but we cannot replace a variableKind with a functionKind
        if not isinstance(newEntryKind(),self.entryKind):
            raise SymtabError(sys._getframe().f_code.co_name+': name clash between symbols with kind '+str(self.entryKind)+' and kind '+str(newEntryKind)+' ',entry=self)
        self.entryKind = newEntryKind

    def enterType(self,newType):
        DebugManager.debug('\t\tSymtabEntry.enterType: entering type '+str(newType)+' for '+str(self))
        if not newType:
            raise SymtabError('SymtabEntry.enterType: newType is None!',entry=self)
        if self.type : # assume a name clash
            raise SymtabError('SymtabEntry.enterType: Name clash -- the declaration for this symbol conflicts with an earlier declaration using the same name"',entry=self)
        if self.entryKind == self.ProcedureEntryKind:
            DebugManager.debug('\t\t\t(SymtabEntry.enterType: entering type information tells us that this procedure is a function)')
            self.entryKind = self.FunctionEntryKind
        self.type = newType

    def copyAndEnterType(self,newType):
        self.enterType(SymtabEntry.copyType(newType))

    def enterDimensions(self,newDimensions):
        DebugManager.debug('\t\tSymtab.enterDimensions: called on '+str(self)+' and setting dimensions to '+str(newDimensions))
        if self.dimensions and (self.dimensions != newDimensions):
            raise SymtabError('SymtabEntry.enterDimensions: Error -- current dimensions "'+str(self.dimensions)+'" and new dimensions "'+str(newDimensions)+'" conflict!',entry=self)
        self.dimensions = newDimensions

    def enterLength(self,newLength):
        if self.length and (self.length != newLength):
            raise SymtabError('SymtabEntry.enterLength: Error -- current length "'+str(self.length)+'" and new length "'+str(newLength)+'" conflict!',entry=self)
        self.length = newLength

    def enterConstInit(self,constInit):
        if self.constInit and (self.constInit != constInit):
            raise SymtabError('SymtabEntry.enterConstInit: Error -- already set',entry=self)
        self.constInit = constInit

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
    
    def _augmentParentEntryFrom(self,other):
        if (not self.type and other.type):
            self.copyAndEnterType(other.type)
        if (not self.dimensions):
            self.dimensions=other.dimensions
        if (self.entryKind==SymtabEntry.GenericEntryKind):
            self.enterEntryKind(other.entryKind)
        if (not self.length):
            self.length=other.length
        if (not self.constInit):
            self.constInit=other.constInit
        if (not self.genericInfo):
            self.genericInfo=other.genericInfo
        if (not self.funcFormalArgs):
            self.funcFormalArgs=other.funcFormalArgs    

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
                                         ', constInit='+((self.constInit and str(self.constInit)) or 'None')+\
                                         ', origin='+str(self.origin)+\
                                         ', renameSource='+str(self.renameSource)+\
                                         ', access='+((self.access and self.access) or 'None')+\
                                         ', genericInfo='+((self.genericInfo and str(self.genericInfo.debug())) or 'None')+\
                                         ', funcFormalArgs='+((self.funcFormalArgs and str(self.funcFormalArgs.debug())) or 'None')+\
                                         ', memberOfDrvdType='+((self.memberOfDrvdType and self.memberOfDrvdType) or 'None')+\
                                         ']'
    

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
