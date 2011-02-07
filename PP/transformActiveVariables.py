from PyUtil.debugManager import DebugManager

from PP.unitPostProcess import PostProcessError
from PyFort.fortUnit import fortUnitIterator, Unit
from PyFort.inference import expressionType
from PyFort.intrinsic import is_intrinsic,is_inquiry,getGenericName
import PyFort.fortStmts as fs
import PyFort.fortExp as fe
import copy
from PyUtil.symtab import Symtab

class TransformError(Exception):
    '''Exception for errors that occur during active variable transformation'''
    def __init__(self,msg,lineNumber):
        self.msg = msg
        self.lineNumber = lineNumber

    def __str__(self):
        errString='\nERROR: TransformError at line '+str(self.lineNumber)+':'+str(self.msg)
        return (errString)

class TransformActiveVariables(object):
    'class to perform rudimentary tranformations of active variables on non-transformed files'

    _replacement_type = 'active' 

    @staticmethod
    def setReplacementType(replacementType):
        TransformActiveVariables._replacement_type = replacementType

    # common blocks which contain active variables
    _commonBlocks = set([])
    _activeVars = set([])
    # list of (subroutine name, indices of input variables which are active...)
    _subroutinesToModify=[]
    _subroutineActiveInputVarIndices=dict()
    # populates the commonBlocks array with the names of all common blocks which contain active variables
    @staticmethod
    def getCommonBlocksWithActiveVars(file,inputFormat=None):
        for aUnit in fortUnitIterator(file,inputFormat):
            for aDeclStmt in aUnit.decls:
                if isinstance(aDeclStmt,fs.CommonStmt):
                    (stmtClass,[expType])=expressionType(aDeclStmt.declList[0],aUnit.symtab,aDeclStmt.lineNumber)
                    if expType=='active':
                        TransformActiveVariables._commonBlocks.add(aDeclStmt.name.lower())
        DebugManager.debug('TransformActiveVariables: finished populating list of names of common blocks with active variables: '+str(TransformActiveVariables._commonBlocks))

    def __init__(self, aUnit):
        self.__myUnit = aUnit
        self.__newDecls=[]

    def __isActive(self,Exp,parentStmt):
        varName=fs.getVarName(Exp,parentStmt.lineNumber)
        (stmtClass,expType)=expressionType(varName,
                                           self.__myUnit.symtab,parentStmt.lineNumber)
        if (len(expType)>0 and expType[0]=='active') or \
                (varName.lower() in self._activeVars):
            return True
        return False

    # recursively transforms an expression if it contains a variable in
    # the activeVars array by adding %v to it
    def __transformActiveTypes(self,Exp,parentStmt):
        DebugManager.debug('TransformActiveVariables.__transformActiveTypes called on "'+str(Exp)+'"')
        if isinstance(Exp,list) :
            Exp = [self.__transformActiveTypes(s,parentStmt) for s in Exp]
        elif isinstance(Exp,fe.App) and is_intrinsic(Exp.head) and not is_inquiry(Exp.head):
            Exp.head=getGenericName(Exp.head)
            Exp.args=[self.__transformActiveTypes(arg,parentStmt) for arg in Exp.args]
        elif isinstance(Exp,str) or isinstance(Exp,fe.Sel) or isinstance(Exp,fe.App):
            try:
                if self.__isActive(Exp,parentStmt):
                    Exp = fe.Sel(Exp,"v")
                    parentStmt.modified = True
                if hasattr(Exp,'args'):
                    Exp.args=[self.__transformActiveTypes(arg,parentStmt) for arg in Exp.args]
                    parentStmt.modified=True
            except:
                pass
        elif hasattr(Exp, "_sons"):
            for aSon in Exp.get_sons() :
                theSon = getattr(Exp,aSon)
                newSon = self.__transformActiveTypes(theSon,parentStmt)
                Exp.set_son(aSon,newSon)
        DebugManager.debug('TransformActiveVariables.__transformActiveTypes returning '+str(Exp))
        return Exp
        
    # get common block active vars by comparing common blocks from this file with 
    # common blocks in activeVariables file then adding NEW common block var
    # names (from current file) to self._activeVars.
    # Add new method to be called before getEquivalencedVars
    def __getCommonBlockActiveVars(self):
        for aDecl in self.__myUnit.decls:
            if isinstance(aDecl,fs.CommonStmt) and \
                    aDecl.name.lower() in self._commonBlocks:
                map(lambda l: TransformActiveVariables._activeVars.add(fs.getVarName(l,aDecl.lineNumber)),
                    aDecl.declList)

    def __getVarsEquivalencedToActive(self):
        # get list of vars which are equivalenced to active variables (and therefore need to be active)
        varsToActivate=[]
        for aDecl in self.__myUnit.decls:
            if isinstance(aDecl,fs.EquivalenceStmt):
                for nlist in aDecl.nlists:
                    newList = []
                    active = False
                    append=newList.append
                    for item in nlist:
                        if isinstance(item,list):
                            for var in item:
                                # if one variable is active, they all need to be active;
                                # add non-active vars to a list of vars to be activated
                                (stmtClass,expType)=expressionType(var,self.__myUnit.symtab,aDecl.lineNumber)
                                # if equivalenced to an active variable not found in a common block, the type
                                # inference should find that it's active
                                if isinstance(expType,list) and len(expType)>0 and expType[0]=='active':
                                    active=True
                                # if equivalenced to an active variable from a common block, the variable will
                                # be in activeVars
                                elif fs.getVarName(var,aDecl.lineNumber) in self._activeVars:
                                    active=True
                                else:
                                    append(fs.getVarName(var,aDecl.lineNumber))
                    if len(newList)>0 and active:
                        varsToActivate.extend(newList)
        return varsToActivate

    # helper function for createActiveTypeDecls
    # gets a variable name from the input variable 'var' and adds it to the activeDeclList 
    # if it is in the list of varsToActivate
    # This activeDeclList is used to create a new active type declaration with all active variables
    # from the variables declared in theDecl
    def __getVarToActivateFromTypeDecl(self,varsToActivate,var,theDecl,activeDeclList,alreadyActivated,removeDeclList):
        strippedVar=fs.getVarName(var,theDecl.lineNumber)
        if strippedVar in varsToActivate:
            # accumulate var to make new active decl later
            if strippedVar not in alreadyActivated:
                activeDeclList.append(strippedVar)
                alreadyActivated.append(strippedVar)
            if isinstance(theDecl,fs.TypeDecl):
                removeDeclList.append(var)
            self._activeVars.add(strippedVar)
        return (activeDeclList,alreadyActivated,removeDeclList)

    # helper function for createActiveTypeDecls
    # adds a new active declaration to the list of declarations
    def __insertNewActiveTypeDecl(self,activeDecls,oldDecl,oldDeclList):
        if len(activeDecls)>0:
            newDecl=fs.DrvdTypeDecl([self._replacement_type],[],activeDecls,lead=oldDecl.lead)
            self.__newDecls.append(newDecl)
        if len(oldDeclList)>0:
            oldDecl.modified=True
            self.__newDecls.append(oldDecl)

    # for each type or dimension declaration in the unit, determine if any of the
    # variables declared are active variables; If so, create a new active type
    # declaration and add it to the list of declarations
    def __createActiveTypeDecls(self,varsToActivate):
        # activate variables in list of varsToActivate
        alreadyActivated=[]
        append=self.__newDecls.append
        for aDecl in self.__myUnit.decls:
            if isinstance(aDecl,fs.TypeDecl) or isinstance(aDecl,fs.DimensionStmt):
                decls = []; removeDeclList=[]
                for var in aDecl.get_decls():
                    (decls,alreadyActivated,removeDeclList) = self.__getVarToActivateFromTypeDecl(varsToActivate,var,
                                                                                                  aDecl,decls,
                                                                                                  alreadyActivated,
                                                                                                  removeDeclList)
                map(aDecl.get_decls().remove,removeDeclList)
                if len(removeDeclList)>0: aDecl.modified=True
                self.__insertNewActiveTypeDecl(decls,aDecl,aDecl.get_decls())
            else:
                append(aDecl)

    # transforms all exec statements in the file if they contain a variable
    # in the activeVars array and returns the unit
    def transformUnit(self):
        DebugManager.debug(('+'*55)+' transforming active varioables in unit <'+str(self.__myUnit.uinfo)+'> '+(55*'+'))
        DebugManager.debug('local '+self.__myUnit.symtab.debug())
        DebugManager.debug('subunits (len = '+str(len(self.__myUnit.ulist))+'):')

        for subUnit in self.__myUnit.ulist :
            DebugManager.debug(str(subUnit))
            TransformActiveVariables(subUnit).transformUnit()

        # add active module
        Unit.addActiveModule(self.__myUnit,self.__newDecls)

        # activate variables which are equivalenced to active variables
        self.__getCommonBlockActiveVars()
        varsToActivate = self.__getVarsEquivalencedToActive()
        if (self.__myUnit.uinfo is not None) and (self.__myUnit.uinfo.name.lower() in self._subroutinesToModify):
            #need to add appropriate input var to activeVars list
            valueList=self._subroutineActiveInputVarIndices.get(self.__myUnit.uinfo.name.lower())
            varsToActivate.extend([self.__myUnit.uinfo.args[l].lower() for l in valueList])
        if len(varsToActivate)>0:
            self.__createActiveTypeDecls(varsToActivate)
        else:
            self.__newDecls.extend(self.__myUnit.decls)
        self.__myUnit.decls=self.__newDecls

        for anExec in self.__myUnit.execs:
            DebugManager.debug('TransformActiveVariables.transformUnit: '\
                              +'processing exec statement "'+str(anExec)+'"',
                               lineNumber=anExec.lineNumber)
            if isinstance(anExec,fs.AllocateStmt) or \
               isinstance(anExec,fs.DeallocateStmt) : continue
            if isinstance(anExec,fs.CallStmt):
                i=0
                listEntry=[]
                while i<len(anExec.args):
                    if self.__isActive(anExec.args[i],anExec):
                        # append input index of active variable to list entry
                        listEntry.append(i)
                    i+=1
                if len(listEntry)>0:
                    self._subroutinesToModify.append(anExec.head.lower())
                    self._subroutineActiveInputVarIndices[anExec.head.lower()]=listEntry
            elif hasattr(anExec, "_sons"):
                for aSon in anExec.get_sons() :
                    theSon = getattr(anExec,aSon)
                    if isinstance(theSon,fs.AllocateStmt) or isinstance(theSon,fs.DeallocateStmt) :
                        continue
                    newSon = self.__transformActiveTypes(theSon,anExec)
                    anExec.set_son(aSon,newSon)
            DebugManager.debug('TransformActiveVariables.transformUnit: resulting exec statement: "'+str(anExec)+'"')
        DebugManager.debug('TransformActiveVariables.transformUnit: finished transforming exec statements for this unit.  execs = '+str(self.__myUnit.execs))
        return self.__myUnit
