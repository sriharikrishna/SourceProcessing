from _Setup import *

from PyUtil.debugManager import DebugManager
from PyUtil.caselessDict import caselessDict as cDict
from PyUtil.arrayboundstab import ArrayBoundsTab
from PyUtil.characterlentab import CharacterLenTab

from PyFort.intrinsic import getBuiltInTypes
from PyFort.fortStmts import DrvdTypeDecl, DrvdTypeDefn, CharacterStmt,RealStmt,_NoInit,UPClassStmt
from PyFort.fortExp import Ops,App

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
        self.intrinsicIdToTypeMap=dict() # typetab id for the key and (fortStmt Type,Kind) pair for value
        self.intrinsicTypeToIdMap=dict() # type name for key and typetab id for value
        self.type_counter=1
        self.__enterBuiltInTypes()
        self.arrayBoundsTab=ArrayBoundsTab()
        self.charLenTab=CharacterLenTab()

    def lookupTypeId(self,typeid):
        '''check for typeid in the type table'''
        DebugManager.debug('Typetab.lookupTypeId called on "'+str(typeid)+'"')
        if typeid in self.ids:
            return self.ids[typeid]
        else:
            return None

    def intrinsicTypeNameToEntry(self,type_name):
        return self.lookupTypeId(self.intrinsicTypeToIdMap[type_name])

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

    def __getDimensions(self,theType,localSymtab):
        if theType.dimension:
            return theType.dimension
        elif len(theType.get_decls())>0:
            theDecl=theType.get_decls()[0]
            if isinstance(theDecl,_NoInit):
                if isinstance(theDecl.lhs,App):
                    return theDecl.lhs.args
            elif isinstance(theDecl,App):
                return theDecl.args
        return None

    def __getTypeKind(self,theType,localSymtab):
        if theType.allocatable:
            return TypetabEntry.AllocatableEntryKind
        elif theType.pointer:
            if self.__getDimensions(theType,localSymtab):
                return TypetabEntry.ArrayPointerEntryKind
            elif isinstance(theType,DrvdTypeDecl) or isinstance(theType,DrvdTypeDefn):
                return TypetabEntry.NamedTypePointerEntryKind
            elif isinstance(theType,CharacterStmt) and len(theType.get_mod())>0:
                return TypetabEntry.CharacterPointerEntryKind
            else:
                # assume BuiltInPointerEntryKind; type name will be checked later
                return TypetabEntry.BuiltInPointerEntryKind
        else:
            if self.__getDimensions(theType,localSymtab):
                return TypetabEntry.ArrayEntryKind
            elif isinstance(theType,DrvdTypeDecl) or isinstance(theType,DrvdTypeDefn):
                return TypetabEntry.NamedTypeEntryKind
            elif isinstance(theType,CharacterStmt) and len(theType.get_mod())>0:
                return TypetabEntry.CharacterEntryKind
            else:
                # assume BuiltInEntryKind; type name will be checked later
                return TypetabEntry.BuiltInEntryKind

    def __enterBuiltInTypes(self):
        for aTypePair in getBuiltInTypes():
            if aTypePair[0].kw=='doubleprecision':
                entryKind=TypetabEntry.BuiltInEntryKind('real_8')
            elif aTypePair[0].kw=='doublecomplex':
                entryKind=TypetabEntry.BuiltInEntryKind('complex_16')
            elif aTypePair[0].kw=='character' or aTypePair[0].kw=='logical':
                entryKind=TypetabEntry.BuiltInEntryKind(aTypePair[0].kw)
            else:
                entryKind = TypetabEntry.BuiltInEntryKind(aTypePair[0].kw+'_'+str(aTypePair[1]))
            newEntry = TypetabEntry(entryKind,self.type_counter)
            self.intrinsicIdToTypeMap[self.type_counter]=aTypePair
            self.intrinsicTypeToIdMap[entryKind.type_name]=self.type_counter
            self.ids[self.type_counter]=newEntry
            self.type_counter += 1

    def enterArrayType(self,baseTypeID,dimensions,entryKind):
        if entryKind==TypetabEntry.AllocatableEntryKind:
            # AllocatableEntryKind
            newEntry=TypetabEntry(TypetabEntry.AllocatableEntryKind(baseTypeID,len(dimensions)),self.type_counter)
        elif entryKind==TypetabEntry.ArrayEntryKind:
            # ArrayEntryKind
            arrayid=self.arrayBoundsTab.getArrayBoundsId(dimensions)
            newEntry=TypetabEntry(TypetabEntry.ArrayEntryKind(arrayid,baseTypeID),self.type_counter)
        else:
            # ArrayPointerEntryKind
            newEntry=TypetabEntry(TypetabEntry.ArrayPointerEntryKind(baseTypeID),self.type_counter)
        self.ids[self.type_counter]=newEntry
        self.type_counter += 1
        return newEntry

    # get a NamedType entry (or NamedTypePointer) by symbolName & symtab
    # if it's not present already, add it
    def __getNamedType(self,symbolName,localSymtab):
        kindEntryMatches = [e for e in self.ids.values() if isinstance(e.entryKind,TypetabEntry.NamedTypeEntryKind) or isinstance(e.entryKind,TypetabEntry.NamedTypePointerEntryKind)]
        if len(kindEntryMatches)!=0:
            # check symbol name
            kindEntryMatches = [e for e in kindEntryMatches if (e.entryKind.symbolName==symbolName)]
            match = [e for e in kindEntryMatches if ((e.entryKind.localSymtab==None) or (e.entryKind.localSymtab==localSymtab))]
            if match:
                return match[0].typetab_id
        # if no match, add a new type entry
        # if every base type is added as it is searched, then the type entry here does not have a base type.
        newTypetabKind=TypetabEntry.NamedTypeEntryKind(symbolName,localSymtab,None)
        newEntry=TypetabEntry(newTypetabKind,self.type_counter)
        self.type_counter+=1
        return newEntry.typetab_id
        

    # newType: pair  (type class,type modifier) => pass in only type class (don't need type mod)?
    # enter the type in the type table and return the typetab_id
    def __enterNewType(self,theType,localSymtab):
        DebugManager.debug('Typetab.__enterNewType called on "'+str(theType)+'"')
        typeKind=self.__getTypeKind(theType,localSymtab)
        newEntry=None
        if typeKind==TypetabEntry.NamedTypeEntryKind:
            symbolName=theType.get_mod()[0]
            baseTypeId=None
            if localSymtab is not None:
                (symtabEntry,baseSymtab)=localSymtab.lookup_name_level(symbolName)
                if symtabEntry:
                    baseName=symtabEntry.entryKind.base_type
                    baseTypeId=self.__getNamedType(baseName,baseSymtab)
            kind=TypetabEntry.NamedTypeEntryKind(symbolName,localSymtab,baseTypeId)
            newEntry=TypetabEntry(kind,self.type_counter)
        elif typeKind==TypetabEntry.ArrayEntryKind:
            # check that type is defined in table & get typeid
            baseType = theType.__class__(theType.get_mod(),[],[])
            typeID = self.getType(baseType,localSymtab)
            newEntry=self.enterArrayType(typeID,self.__getDimensions(theType,localSymtab),typeKind)
            return newEntry.typetab_id
        elif typeKind==TypetabEntry.ArrayPointerEntryKind:
            # need to look up pointed-to type and check for typeid match
            baseType = theType.__class__(theType.get_mod(),[],[])
            baseType.dimension = self.__getDimensions(theType,localSymtab)
            typeID = self.getType(baseType,localSymtab)
            newEntry=self.enterArrayType(typeID,self.__getDimensions(theType,localSymtab),typeKind)
            return newEntry.typetab_id
        elif typeKind==TypetabEntry.BuiltInPointerEntryKind:
            # need to look up pointed-to type and check for typeid match
            baseType = theType.__class__(theType.get_mod(),[],[])
            baseType.dimension = self.__getDimensions(theType,localSymtab)
            typeID = self.getType(baseType,localSymtab)
            kind=typeKind(typeID)
            newEntry=TypetabEntry(kind,self.type_counter)
        elif typeKind==TypetabEntry.NamedTypePointerEntryKind:
            baseTypeId=None
            if isinstance(theType,DrvdTypeDecl):
                symbolName=theType.get_mod()[0]
                (symtabEntry,baseSymtab)=localSymtab.lookup_name_level(symbolName)
                if symtabEntry:
                    base_type=symtabEntry.entryKind.base_type
                    baseTypeId=self.__getNamedType(base_type,baseSymtab)
            elif isinstance(theType,DrvdTypeDefn):
                symbolName=theType.name
                (symtabEntry,baseSymtab)=localSymtab.lookup_name_level(theType.base_type)
                baseTypeId=self.__getNamedType(theType.base_type,baseSymtab)
            kind=TypetabEntry.NamedTypePointerEntryKind(symbolName,localSymtab,baseTypeId)
            newEntry=TypetabEntry(kind,self.type_counter)
        elif typeKind==TypetabEntry.CharacterEntryKind:
            charLenId = self.charLenTab.getCharLen(theType.get_mod()[0])
            kind=TypetabEntry.CharacterEntryKind(charLenId)
            newEntry=TypetabEntry(kind,self.type_counter)
        elif typeKind==TypetabEntry.AllocatableEntryKind:
            baseType = theType.__class__(theType.get_mod(),[],[])
            typeID = self.getType(baseType,localSymtab)
            newEntry=self.enterArrayType(typeID,self.__getDimensions(theType,localSymtab),typeKind)
            return newEntry.typetab_id
        elif typeKind==TypetabEntry.BuiltInEntryKind:
            # these should already be added, unless it's a character with a length specifier
            raise TypetabError('attempting to incorrectly add a Built In type to the type table.',str(theType))
        else:
            raise TypetabError('unrecognized typeKind: ',str(theType))
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
        DebugManager.debug('Typetab.lookupType called on "'+str(theType)+'"')
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
                baseType.dimension = self.__getDimensions(theType,localSymtab)
                typeID = self.lookupType(baseType,localSymtab)
                if typeID is not None:
                    match = [e for e in kindEntryMatches if (e.entryKind.typetab_id==typeID)]
                    if match:
                        return match[0].typetab_id                
            elif typeKind==TypetabEntry.NamedTypeEntryKind or typeKind==TypetabEntry.NamedTypePointerEntryKind:
                # check symbol name
                # newType is DrvdTypeDecl or DrvdTypeDefn
                if isinstance(theType,DrvdTypeDecl):
                    kindEntryMatches = [e for e in kindEntryMatches if (e.entryKind.symbolName==theType.get_mod()[0])]
                elif isinstance(theType,DrvdTypeDefn):
                    kindEntryMatches = [e for e in kindEntryMatches if (e.entryKind.symbolName==theType.name)]
                # check scope
                match = [e for e in kindEntryMatches if ((e.entryKind.localSymtab==None) or (e.entryKind.localSymtab==localSymtab))]
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
                        arrayid=self.arrayBoundsTab.lookupArrayBounds(self.__getDimensions(theType,localSymtab))
                        if arrayid:
                            match = [e for e in typeMatches if (e.entryKind.arrayid==arrayid)]
                            if match:
                                return match[0].typetab_id
            elif typeKind==TypetabEntry.CharacterEntryKind:
                charId = self.charLenTab.lookupCharLen(theType.get_mod()[0])
                if charId:
                    charMatch = [e for e in kindEntryMatches if (e.entryKind.charlen_id==charId)]
                    if charMatch:
                        return charMatch[0].typetab_id
            elif typeKind==TypetabEntry.AllocatableEntryKind:
                # check that type is defined in table
                baseType = theType.__class__(theType.get_mod(),[],[])
                typeID = self.lookupType(baseType,localSymtab)
                if typeID:
                    typeMatches = [e for e in kindEntryMatches if (e.entryKind.typetab_id==typeID)]
                    if typeMatches:
                        # check that rank matches
                        match = [e for e in typeMatches if (e.entryKind.rank==len(self.__getDimensions(theType,localSymtab)))]
                        if match:
                            return match[0].typetab_id
            else:
                raise TypetabError("type of argument not recognized",theType=theType)
        return None

    # get the type id; if it is not already in the table, add it
    def getType(self,theType,localSymtab):
        DebugManager.debug('Typetab.getType called on "'+str(theType)+'"')
        if isinstance(theType,UPClassStmt):
            # unlimited polymorphic entity=> not declared to have a type
            return None
        typeid = self.lookupType(theType,localSymtab)
        if not typeid:
            newType=self.__enterNewType(theType,localSymtab)
            return newType
        return typeid

    # get the type entry; if it is not already in the table, add it
    def getTypeEntry(self,theType,localSymtab):
        DebugManager.debug('Typetab.getTypeEntry called on "'+str(theType)+'"')
        typeid=self.getType(theType,localSymtab)
        return globalTypeTable.lookupTypeId(typeid)

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
            return 'BuiltInEntryKind; name of built-in type: '+str(self.type_name)

    class CharacterEntryKind(BuiltInEntryKind):
        _sons = ['type_name','charlen_id']

        def __init__(self,charLenId):
            self.charlen_id=charLenId
            TypetabEntry.BuiltInEntryKind.__init__(self,'character')

        def debug(self):
            return 'CharacterEntryKind; name of built-in type: '+str(self.type_name)

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

        def __init__(self,symbolName,localSymtab,baseTypeId=None,polymorphic=False):
            self.symbolName=symbolName        # symbolName of named type
            self.localSymtab=localSymtab      # scope named type is defined in
            self.baseTypeId=baseTypeId        # baseType, if this is a type which extends another named type

        def getBaseTypeEntry(self):
            return self.baseType

        def debug(self):
            returnString='NamedTypeEntryKind; symbolName:'+str(self.symbolName)
            if self.localSymtab:
                returnString+=', localSymtab where the type is defined: '+self.localSymtab.debug()
            else:
                returnString+=', localSymtab is None'
            return returnString

    class NamedTypePointerEntryKind(GenericEntryKind):
        keyword = 'NTpointer'
        _sons = ['symbolName','localSymtab']

        def __init__(self,symbolName,localSymtab,baseTypeId):
            self.symbolName=symbolName        # symbolName of named type
            self.localSymtab=localSymtab      # scope the symbolName is defined in
            self.baseTypeId=baseTypeId        # baseType, if this is a type which extends another named type

        def debug(self):
            returnString='NamedTypeEntryKind; symbolName:'+str(self.symbolName)
            if self.localSymtab:
                returnString+=', localSymtab where the type is defined: '+self.localSymtab.debug()
            else:
                returnString+=', localSymtab is None'
            return returnString

    class ArrayEntryKind(GenericEntryKind):
        keyword = 'array'
        _sons = ['arrayid','typetab_id']

        def __init__(self,arrayid,typetab_id):
            self.arrayid=arrayid      # array id for array table where dimension information is stored
            self.typetab_id=typetab_id        # typeid of BI or NT array type

        def getArrayBounds(self):
            arrayTabEntry=globalTypeTable.arrayBoundsTab.lookupArrayId(self.arrayid)
            arrayBounds=[]
            for dimEntry in arrayTabEntry.dimArray:
                arrayBounds.append(Ops(':',dimEntry.lower,dimEntry.upper))
            return arrayBounds
            
        def getArrayRank(self):
            arrayTabEntry=globalTypeTable.arrayBoundsTab.lookupArrayId(self.arrayid)
            return arrayTabEntry.rank

        def debug(self):
            return 'ArrayEntryKind; Array id for array table where dimension information is stored: '+str(self.arrayid)+\
                                    ', Type id of built-in or named array type: '+str(self.typetab_id)

    class ArrayPointerEntryKind(GenericEntryKind):
        keyword = 'ARpointer'
        _sons = ['typetab_id']

        def __init__(self,typetab_id):
            self.typetab_id=typetab_id             # type id of array type of target

        def debug(self):
            return 'ArrayPointerEntryKind; Type id of array type of target: '+str(self.typetab_id)

        def getArrayTypeEntry(self):
            return globalTypeTable.lookupTypeId(self.typetab_id)

    class AllocatableEntryKind(GenericEntryKind):
        keyword = 'allocatable'
        _sons = ['typetab_id','rank']

        def __init__(self,typetab_id,rank):
            self.typetab_id=typetab_id # type id of base type
            self.rank=rank             # rank

        def debug(self):
            return 'AllocatableEntryKind; Type id of allocatable base type: '+str(self.typetab_id)+';rank: '+str(self.rank)

    def __init__(self,entryKind,typetab_id):
        self.entryKind = entryKind # some instance of self.GenericEntryKind
        self.typetab_id=typetab_id # typeid in type table for this TypeTabEntry

    def getBaseTypeId(self):
        DebugManager.debug('TypetabEntry.getBaseTypeId called on type: '+self.debug())
        if isinstance(self.entryKind,TypetabEntry.ArrayEntryKind):
            return self.entryKind.typetab_id
        elif isinstance(self.entryKind,TypetabEntry.ArrayPointerEntryKind):
            return self.entryKind.getArrayTypeEntry().getBaseTypeId()
        elif isinstance(self.entryKind,TypetabEntry.BuiltInPointerEntryKind):
            return self.entryKind.typetab_id
        elif isinstance(self.entryKind,TypetabEntry.AllocatableEntryKind):
            return self.entryKind.typetab_id
        else:
            return self.typetab_id

    def getBaseTypeEntry(self):
        DebugManager.debug('TypetabEntry.getBaseTypeEntry called on type: '+self.debug())
        return globalTypeTable.lookupTypeId(self.getBaseTypeId())

    def debug(self,name='<symbol name unknown>'):
        return '[TypetabEntry('+str(self)+') "'+name+'" -> entryKind='+self.entryKind.debug()+\
                                         ', typetab_id='+str(self.typetab_id)+\
                                         ']'

global globalTypeTable
globalTypeTable=Typetab()
