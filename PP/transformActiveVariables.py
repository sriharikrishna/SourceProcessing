from PyUtil.debugManager import DebugManager

from PyFort.fortUnit import fortUnitIterator
import PyFort.fortStmts as fs
import PyFort.fortExp as fe

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
    def getActiveDecls(file,isFreeFormat=False):
        for aUnit in fortUnitIterator(file,isFreeFormat):
            for aDeclStmt in aUnit.decls:
                if isinstance(aDeclStmt,fs.DrvdTypeDecl) and \
                   hasattr(aDeclStmt,'mod') and \
                   aDeclStmt.mod[0] == '(active)' :
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

    def __init__(self, aUnit):
        self.__myUnit = aUnit
        self.__transformDecls = []
        self.__newExecs = []

    # recursively transforms an expression if it contains a variable in
    # the activeVars array by adding %v to it
    def __transformActiveTypes(self,Exp):
        DebugManager.debug('TransformActiveVariables.__transformActiveTypes called on "'+str(Exp)+'"' \
                         +' with self._activeVars = '+str(self._activeVars))
        if isinstance(Exp,list) :
            Exp = [self.__transformActiveTypes(s) for s in Exp]
        elif isinstance(Exp,str) :
            if Exp.lower() in self._activeVars :
                Exp = fe.Sel(Exp,"v")
        elif Exp in self._activeVars:
            Exp = fe.Sel(Exp,"v")
        elif isinstance(Exp,fe.App):
            if Exp.head.lower() in self._activeVars:
                Exp = fe.Sel(Exp,"v")
            else:
                Exp.args[0] = self.__transformActiveTypes(Exp.args[0])
        else:
            if hasattr(Exp, "_sons"):
                for aSon in Exp.get_sons() :
                    theSon = getattr(Exp,aSon)
                    newSon = self.__transformActiveTypes(theSon)
                    setattr(Exp,aSon,newSon)
        DebugManager.debug('TransformActiveVariables.__transformActiveTypes returning '+str(Exp))
        return Exp
        
    # transforms all exec statements in the file if they contain a variable
    # in the activeVars array and returns the unit
    def transformFile(self):
        for anExec in self.__myUnit.execs:
            DebugManager.debug('TransformActiveVariables.transformFile: '\
                              +'processing exec statement "'+str(anExec)+'"',
                               lineNumber=anExec.lineNumber)
            if isinstance(anExec,fs.AllocateStmt) or \
               isinstance(anExec,fs.DeallocateStmt) : continue
            if hasattr(anExec, "_sons"):
                for aSon in anExec.get_sons() :
                    theSon = getattr(anExec,aSon)
                    newSon = self.__transformActiveTypes(theSon)
                    setattr(anExec,aSon,newSon)
            DebugManager.debug('TransformActiveVariables.transformFile: resulting exec statement: "'+str(anExec)+'"')
        DebugManager.debug('TransformActiveVariables.transformFile: finished transforming exec statements for this unit.  execs = '+str(self.__myUnit.execs))
        return self.__myUnit
