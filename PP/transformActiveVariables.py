from PyUtil.debugManager import DebugManager

from PyFort.fortUnit import fortUnitIterator
import PyFort.fortStmts as fs
import PyFort.fortExp as fe
import re

class TransformError(Exception):
    '''Exception for errors that occur during active variable transformation'''
    def __init__(self,msg,lineNumber):
        self.msg = msg
        self.lineNumber = lineNumber


class TransformActiveVariables(object):
    'class to perform rudimentary tranformations of active variables on non-transformed files'

    _activeVars = []
    # populates the activeVars array with the variable names of all active
    # variables
    @staticmethod
    def getActiveDecls(file,inputFormat=None):
        for aUnit in fortUnitIterator(file,inputFormat):
            for aDeclStmt in aUnit.decls:
                if isinstance(aDeclStmt,fs.DrvdTypeDecl) and \
                   hasattr(aDeclStmt,'mod') and \
                   aDeclStmt.mod[0] == 'active' :
                    DebugManager.debug('TransformActiveVariables.getActiveDecls:'\
                                      +'processing active variable declaration "'+str(aDeclStmt)+'"')
                    for aDecl in aDeclStmt.decls:
                        if isinstance(aDecl, fs._NoInit):
                            if hasattr(aDecl.lhs, 'head'):
                                TransformActiveVariables.\
                                    _activeVars.append(aDecl.lhs.head.lower())
                            else:
                                TransformActiveVariables.\
                                    _activeVars.append(aDecl.lhs.lower())
        DebugManager.debug('TransformActiveVariables: finished populating list of active variables: '+str(TransformActiveVariables._activeVars))

    def __init__(self, aUnit):
        self.__myUnit = aUnit
        self.__transformDecls = []
        self.__newExecs = []

    # recursively transforms an expression if it contains a variable in
    # the activeVars array by adding %v to it
    def __transformActiveTypes(self,Exp,parentStmt):
        DebugManager.debug('TransformActiveVariables.__transformActiveTypes called on "'+str(Exp)+'"')
        if isinstance(Exp,list) :
            Exp = [self.__transformActiveTypes(s,parentStmt) for s in Exp]
        elif isinstance(Exp,str) :
            if Exp.lower() in self._activeVars :
                Exp = fe.Sel(Exp,"v")
                parentStmt.modified = True
        elif Exp in self._activeVars:
            Exp = fe.Sel(Exp,"v")
            parentStmt.modified = True
        elif isinstance(Exp,fe.App):
            if Exp.head.lower() in self._activeVars:
                Exp = fe.Sel(Exp,"v")
                parentStmt.modified = True
            else:
                Exp.args[0] = self.__transformActiveTypes(Exp.args[0],parentStmt)
                parentStmt.modified = True
        else:
            if hasattr(Exp, "_sons"):
                for aSon in Exp.get_sons() :
                    theSon = getattr(Exp,aSon)
                    newSon = self.__transformActiveTypes(theSon,parentStmt)
                    Exp.set_son(aSon,newSon)
        DebugManager.debug('TransformActiveVariables.__transformActiveTypes returning '+str(Exp))
        return Exp
        
    # transforms all exec statements in the file if they contain a variable
    # in the activeVars array and returns the unit
    def transformUnit(self):
        DebugManager.debug(('+'*55)+' transforming active varioables in unit <'+str(self.__myUnit.uinfo)+'> '+(55*'+'))
        DebugManager.debug('local '+self.__myUnit.symtab.debug())
        DebugManager.debug('subunits (len = '+str(len(self.__myUnit.ulist))+'):')

        for subUnit in self.__myUnit.ulist :
            DebugManager.debug(str(subUnit))
            TransformActiveVariables(subUnit).transformUnit()
            
        implicit_none = True
        for aDecl in self.__myUnit.decls:
            if isinstance(aDecl,fs.ImplicitStmt):
                if aDecl.type == 'double precision':
                    aDecl.type = 'type(active)'
                    aDecl.rawline = str(aDecl)
                    new_stmt = fs.UseAllStmt(moduleName='OAD_active',renameList=None)
                    self.__myUnit.decls.insert(0,new_stmt)
                implicit_none = False
            if not implicit_none and isinstance(aDecl,fs.DimensionStmt):
                for var in aDecl.lst:
                    if isinstance(var,fe.App):
                        if var.head[0].lower() in 'abcdefghopqrstuvwxyz':
                            newDecl = fs.DrvdTypeDecl(['active'],[],[var])
                            self.__myUnit.decls.insert(self.__myUnit.decls.index(aDecl),newDecl)
                            aDecl.lst.remove(var)
                            aDecl.modified = True
                    else:
                        if var[0].lower() in 'abcdefghopqrstuvwxyz':
                            newDecl = fs.DrvdTypeDecl(['active'],[],[var])
                            self.__myUnit.decls.insert(self.__myUnit.decls.index(aDecl),newDecl)
                            aDecl.lst.remove(var)
                            aDecl.modified = True
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
