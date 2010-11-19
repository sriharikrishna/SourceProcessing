from PyUtil.debugManager import DebugManager

from PP.unitPostProcess import PostProcessError
from PyFort.fortUnit import fortUnitIterator, Unit
from PyFort.inference import expressionType
import PyFort.fortStmts as fs
import PyFort.fortExp as fe
import re,copy
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

    _activeVars = []
    # common blocks which contain active variables
    _commonBlocks = set([])
    # populates the activeVars array with the variable names of all active
    # variables
    @staticmethod
    def getActiveDecls(file,inputFormat=None):
        for aUnit in fortUnitIterator(file,inputFormat):
            for aDeclStmt in aUnit.decls:
                if isinstance(aDeclStmt,fs.CommonStmt):
                    (stmtClass,[expType])=expressionType(aDeclStmt.declList[0],aUnit.symtab,aDeclStmt.lineNumber)
                    if expType=='active':
                        TransformActiveVariables._commonBlocks.add(aDeclStmt.name)
        DebugManager.debug('TransformActiveVariables: finished populating list of active variables: '+str(TransformActiveVariables._activeVars))

    def __init__(self, aUnit):
        self.__myUnit = aUnit
        self.__newDecls=[]

    # recursively transforms an expression if it contains a variable in
    # the activeVars array by adding %v to it
    def __transformActiveTypes(self,Exp,parentStmt):
        DebugManager.debug('TransformActiveVariables.__transformActiveTypes called on "'+str(Exp)+'"')
        if isinstance(Exp,list) :
            Exp = [self.__transformActiveTypes(s,parentStmt) for s in Exp]
        elif isinstance(Exp,str) or isinstance(Exp,fe.Sel) or isinstance(Exp,fe.App):
            try:
                (stmtClass,[expType])=expressionType(Exp,self.__myUnit.symtab,parentStmt.lineNumber)
                if fs.getVarName(Exp,parentStmt.lineNumber) in self._activeVars or expType=='active':
                    Exp = fe.Sel(Exp,"v")
                    parentStmt.modified = True
                if hasattr(Exp,'args'):
                    for arg in Exp.args:
                        arg=self.__transformActiveTypes(arg,parentStmt)
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
        
    # get common block active vars by comparing common blocks from this file with common blocks in activeVariables file
    # & then adding NEW common block var names (from current file) to self._activeVars.
    # Add new method to be called before getEquivalencedVars
    def __getCommonBlockActiveVars(self):
        for aDecl in self.__myUnit.decls:
            if isinstance(aDecl,fs.CommonStmt) and \
                    aDecl.name.lower() in self._commonBlocks:
                for decl in aDecl.declList:
                    TransformActiveVariables.\
                        _activeVars.append(fs.getVarName(decl,aDecl.lineNumber))

    def __getVarsEquivalencedToActive(self):
        # get list of vars which are equivalenced to active variables (and therefore need to be active)
        varsToActivate=[]
        for aDecl in self.__myUnit.decls:
            if isinstance(aDecl,fs.EquivalenceStmt):
                for nlist in aDecl.nlists:
                    newList = []
                    active = False
                    for item in nlist:
                        if isinstance(item,list):
                            for var in item:
                                # if one variable is active, they all need to be active;
                                # add non-active vars to a list of vars to be activated
                                if fs.getVarName(var,aDecl.lineNumber) in self._activeVars:
                                    active=True
                                else:
                                    newList.append(fs.getVarName(var,aDecl.lineNumber))
                    if len(newList)>0 and active:
                        varsToActivate.extend(newList)
        return varsToActivate

    def __getVarToActivateFromTypeDecl(self,varsToActivate,var,theDecl,oldDeclList,activeDeclList):
        strippedVar=fs.getVarName(var,theDecl.lineNumber)
        if strippedVar in varsToActivate:
            # accumulate var to make new active decl later
            activeDeclList.append(var)
            # add to active variables file
            self._activeVars.append(strippedVar)
            oldDeclList.remove(var)
            theDecl.modified=True
        return activeDeclList

    def __insertNewActiveTypeDecl(self,activeDecls,oldDecl,oldDeclList):
        if len(activeDecls)>0:
            newDecl=fs.DrvdTypeDecl([self._replacement_type],[],activeDecls,lead=oldDecl.lead)
            self.__newDecls.append(newDecl)
            if len(oldDeclList)!=0:
                self.__newDecls.append(oldDecl)

    def __createActiveTypeDecls(self,varsToActivate):
        # activate variables in list of varsToActivate
        for aDecl in self.__myUnit.decls:
            if isinstance(aDecl,fs.TypeDecl):
                decls = []
                for var in aDecl.decls:
                    decls = self.__getVarToActivateFromTypeDecl(varsToActivate,var,aDecl,aDecl.decls,decls)
                self.__insertNewActiveTypeDecl(decls,aDecl,aDecl.decls)
            elif isinstance(aDecl,fs.DimensionStmt):
                decls = []
                for var in aDecl.lst:
                    decls = self.__getVarToActivateFromTypeDecl(varsToActivate,var,aDecl,aDecl.lst,decls)
                self.__insertNewActiveTypeDecl(decls,aDecl,aDecl.lst)
            else:
                self.__newDecls.append(aDecl)

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
            if hasattr(anExec, "_sons"):
                for aSon in anExec.get_sons() :
                    theSon = getattr(anExec,aSon)
                    if isinstance(theSon,fs.AllocateStmt) or isinstance(theSon,fs.DeallocateStmt) :
                        continue
                    newSon = self.__transformActiveTypes(theSon,anExec)
                    anExec.set_son(aSon,newSon)
            DebugManager.debug('TransformActiveVariables.transformUnit: resulting exec statement: "'+str(anExec)+'"')
        DebugManager.debug('TransformActiveVariables.transformUnit: finished transforming exec statements for this unit.  execs = '+str(self.__myUnit.execs))
        return self.__myUnit
