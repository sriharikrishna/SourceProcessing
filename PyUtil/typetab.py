from _Setup import *

from PyUtil.caselessDict import caselessDict as cDict
from PyUtil.arrayboundstab import ArrayBoundsTab

from PyFort.intrinsic import getBuiltInTypes
from PyFort.fortStmts import DrvdTypeDecl, DrvdTypeDefn

class TypetabError(Exception):
    def __init__(self,msg,theType=None,entry=None,lineNumber=None):
        self.msg = msg
        self.theType = theType
        self.entry = entry
        self.lineNumber = lineNumber

    def __str__(self):
        errString='\nERROR: TypetabError at line '+str(self.lineNumber)+':'+str(self.msg)
        if self.theType:
            typeStr = str(self.theType) or '<type class unknown>'
            errString+='\nFor type '+typeStr
        return (errString)

class Typetab(object):
    def __init__(self):
        self.ids = dict() # string for the key and TypetabEntry for the value
        self.type_counter=1
        self.__enterBuiltInTypes()
        self.arrayBoundsTab=ArrayBoundsTab()

    def lookupTypeId(self,typeid):
        '''check for typeid in the type table'''
        if typeid in self.ids:
            return self.ids[typeid]
        else:
            return None

    def __getBuiltInName(self,theType,localSymtab):
        from PyFort.inference import guessBytes
        if (theType.kw=='doubleprecision'):
            numBytes=guessBytes((theType.__class__,theType.get_mod()),localSymtab,0)
            typeName='real_'+str(numBytes)
        elif (theType.kw=='doublecomplex'):
            numBytes=guessBytes((theType.__class__,theType.get_mod()),localSymtab,0)
            typeName='complex_'+str(numBytes)
        elif (theType.kw=='logical' or theType.kw=='character'):
            return theType.kw
        else:
            numBytes=guessBytes((theType.__class__,theType.get_mod()),localSymtab,0)
            typeName=theType.kw+'_'+str(numBytes)
        return typeName

    def __getTypeKind(self,theType,localSymtab):
        if theType.pointer:
            if theType.dimension:
                return TypetabEntry.ArrayPointerEntryKind
            elif isinstance(theType,DrvdTypeDecl) or isinstance(theType,DrvdTypeDefn):
                return TypetabEntry.NamedTypePointerEntryKind
            else:
                # assume BuiltInPointerEntryKind; type name will be checked later
                return TypetabEntry.BuiltInPointerEntryKind
        else:
            if theType.dimension:
                return TypetabEntry.ArrayEntryKind
            elif isinstance(theType,DrvdTypeDecl) or isinstance(theType,DrvdTypeDefn):
                return TypetabEntry.NamedTypeEntryKind
            else:
                # assume BuiltInEntryKind; type name will be checked later
                return TypetabEntry.BuiltInEntryKind

    def __enterBuiltInTypes(self):
        for aType in getBuiltInTypes():
            kind = TypetabEntry.BuiltInEntryKind(aType)
            newEntry = TypetabEntry(kind,self.type_counter)
            self.ids[self.type_counter]=newEntry
            self.type_counter += 1

    # newType: pair  (type class,type modifier) => pass in only type class (don't need type mod)?
    # enter the type in the type table and return the typetab_id
    def __enterNewType(self,theType,localSymtab):
        typeKind=self.__getTypeKind(theType,localSymtab)
        if typeKind==TypetabEntry.NamedTypeEntryKind:
            kind=TypetabEntry.NamedTypeEntryKind(localSymtab)
            newEntry=TypetabEntry(kind,self.type_counter)
        elif typeKind==TypetabEntry.ArrayEntryKind:
            # check that type is defined in table & get typeid
            baseType = theType.__class__(theType.get_mod(),[],[])
            typeID = self.getType(baseType,localSymtab)
            # get arrayid
            arrayid=self.arrayBoundsTab.enterNewArrayBounds(theType)
            kind=TypetabEntry.ArrayEntryKind(typetab_id=typeID,arrayid=arrayid)
            newEntry=TypetabEntry(kind,self.type_counter)
        elif typeKind==TypetabEntry.ArrayPointerEntryKind or typeKind==TypetabEntry.BuiltInPointerEntryKind:
            # need to look up pointed-to type and check for typeid match
            baseType = theType.__class__(theType.get_mod(),[],[])
            baseType.dimension = theType.dimension
            typeID = self.getType(baseType,localSymtab)
            kind=typeKind(typetab_id=typeID) #TODO: is this legal python?
            newEntry=TypetabEntry(kind,self.type_counter)
        elif typeKind==TypetabEntry.NamedTypePointerEntryKind:
            if isinstance(theType,DrvdTypeDecl):
                symbolName=theType.get_mod()[0]
            elif isinstance(theType,DrvdTypeDefn):
                symbolName=theType.name
            kind=TypetabEntry.NamedTypePointerEntryKind(symbolName,localSymtab)
            newEntry=TypetabEntry(kind,self.type_counter)
        elif typeKind==TypetabEntry.BuiltInEntryKind:
            # these should already be added
            raise TypetabError('attempting to incorrectly add a Built In type to the type table.',str(theType))
        self.ids[self.type_counter]=newEntry
        self.type_counter += 1
        return newEntry.typetab_id

    # newType: pair  (type class,type modifier) => pass in only type class (don't need type mod)?
    # deal with pointer issue
    # check if theType is BuiltIn, NamedType or Array
    # if type is not present, add it
    # filter for same # derefs and entrykind
    # then filter results again for match
    # if not: create & add; return typeid
    # else: return typeid
    def lookupType(self,theType,localSymtab):
        typeKind=self.__getTypeKind(theType,localSymtab)
        kindEntryMatches = [e for e in self.ids.values() if isinstance(e.entryKind,typeKind)]
        if len(kindEntryMatches)==0:
            return None
        else:
            if typeKind==TypetabEntry.BuiltInEntryKind:
                builtInName=self.__getBuiltInName(theType,localSymtab)
                match = [e for e in kindEntryMatches if (e.entryKind.type_name==builtInName)]
                if match:
                    # return associated key
                    return match[0].typetab_id
            elif typeKind==TypetabEntry.ArrayPointerEntryKind or typeKind==TypetabEntry.BuiltInPointerEntryKind:
                # need to look up pointed-to type and check for typeid match
                baseType = theType.__class__(theType.get_mod(),[],[])
                baseType.dimension = theType.dimension
                typeID = self.lookupType(baseType,localSymtab)
                if typeID is not None:
                    match = [e for e in kindEntryMatches if (e.entryKind.typetab_id==typeID)]
                    if match:
                        return match[0].typetab_id                
            elif typeKind==TypetabEntry.NamedTypeEntryKind or typeKind==TypetabEntry.NamedTypePointerEntryKind:
                if typeKind==TypetabEntry.NamedTypePointerEntryKind:
                    # check symbol name
                    # newType is DrvdTypeDecl or DrvdTypeDefn
                    if isinstance(theType,DrvdTypeDecl):
                        kindEntryMatches = [e for e in kindEntryMatches if (e.entryKind.symbolName==theType.get_mod()[0])]
                    elif isinstance(theType,DrvdTypeDefn):
                        kindEntryMatches = [e for e in kindEntryMatches if (e.entryKind.symbolName==theType.name)]
                # check scope
                match = [e for e in kindEntryMatches if (e.entryKind.localSymtab==localSymtab)]
                if match:
                    return match[0].typetab_id
            elif typeKind==TypetabEntry.ArrayEntryKind:
                # check that type is defined in table
                baseType = theType.__class__(theType.get_mod(),[],[])
                typeID = self.lookupType(baseType,localSymtab)
                if typeID:
                    typeMatches = [e for e in kindEntryMatches if (e.entryKind.typetab_id==typeID)]
                    if typeMatches:
                        # check arrayid
                        arrayid=self.arrayBoundsTab.lookupArrayBounds(theType)
                        if arrayid:
                            match = [e for e in typeMatches if (e.entryKind.arrayid==arrayid)]
                            if match:
                                return match[0].typetab_id
            else:
                raise TypetabError("type of argument not recognized",theType=theType)
        return None

    # get the type id; if it is not already in the table, add it
    def getType(self,theType,localSymtab):
        typeid = self.lookupType(theType,localSymtab)
        if not typeid:
            return self.__enterNewType(theType,localSymtab)
        return typeid

    # type equivalence between two entries; Do not match array bounds exactly, only rank.
    # used for comparing formal arguments to dummy arguments
    def equivalence(self,entry1,entry2):
        if not (entry1.entryKind.keyword==entry2.entryKind.keyword):
            return False
        for aSon in entry1.entryKind._sons:
            theEntry1Son = getattr(entry1.entryKind,aSon)
            theEntry2Son = getattr(entry2.entryKind,aSon)
            if aSon is 'arrayid':
                # match rank only
                array1Entry = self.arrayBoundsTab.lookupArrayId(theEntry1Son)
                array2Entry = self.arrayBoundsTab.lookupArrayId(theEntry2Son)
                if array1Entry.rank != array2Entry.rank:
                    return False
            elif theEntry1Son != theEntry2Son:
                return False
        return True

class TypetabEntry(object):

    class GenericEntryKind(object):
        keyword = 'unknown'
        def __init__(self):
            pass

    class BuiltInEntryKind(GenericEntryKind):
        keyword = 'builtin'
        _sons = ['type_name']

        def __init__(self,type_name):
            self.type_name=type_name  # name of built-in type (e.g. integer) (use type class here?) only if not a pointer

        def debug(self):
            return 'BuiltInPointerEntryKind; name of built-in type: '+str(self.type_name)

    class BuiltInPointerEntryKind(GenericEntryKind):
        keyword = 'BIpointer'
        _sons = ['typetab_id']

        def __init__(self,typetab_id):
            self.typetab_id=typetab_id        # type id of built-in type pointed to

        def debug(self):
            return 'BuiltInPointerEntryKind; typeid of built-in type pointed to: '+str(self.typetab_id)

    class NamedTypeEntryKind(GenericEntryKind):
        keyword = 'namedtype'
        _sons = ['localSymtab']

        def __init__(self,localSymtab):
            self.localSymtab=localSymtab      # scope named type is defined in

        def debug(self):
            return 'NamedTypeEntryKind; localSymtab where the type is defined: '+self.localSymtab.debug()
            

    class NamedTypePointerEntryKind(GenericEntryKind):
        keyword = 'NTpointer'
        _sons = ['symbolName','localSymtab']

        def __init__(self,symbolName,localSymtab):
            self.symbolName=symbolName
            self.localSymtab=localSymtab # scope the symbolName is defined in

        def debug(self):
            return 'NamedTypePointerEntryKind; symbolName:'+str(self.symbolName)+\
                   ', localSymtab where symbolName is defined: '+self.localSymtab.debug()

    class ArrayEntryKind(GenericEntryKind):
        keyword = 'array'
        _sons = ['arrayid','typetab_id']

        def __init__(self,arrayid,typetab_id):
            self.arrayid=arrayid      # array id for array table where dimension information is stored
            self.typetab_id=typetab_id        # typeid of BI or NT array type

        def debug(self):
            return 'ArrayEntryKind; Array id for array table where dimension information is stored: '+str(self.arrayid)+\
                                    ', Type id of built-in or named array type: '+str(self.typetab_id)

    class ArrayPointerEntryKind(GenericEntryKind):
        keyword = 'ARpointer'
        _sons = ['typetab_id']

        def __init__(self,typetab_id):
            self.typetab_id=typetab_id        # type id of array type of target

        def debug(self):
            return 'ArrayPointerEntryKind; Type id of array type of target: '+str(self.typetab_id)

    def __init__(self,entryKind,typetab_id):
        self.entryKind = entryKind # some instance of self.GenericEntryKind
        self.typetab_id=typetab_id # typeid in type table for this TypeTabEntry

    
    def debug(self,name='<symbol name unknown>'):
        return '[TypetabEntry('+str(self)+') "'+name+'" -> entryKind='+self.entryKind.debug()+\
                                         ', typetab_id='+str(self.typetab_id)+\
                                         ']'

