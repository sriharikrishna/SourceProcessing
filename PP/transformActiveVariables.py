from PyFort.fortUnit import fortUnitIterator
import PyFort.fortStmts as fs
import PyFort.fortExp as fe

class TransformActiveVariables(object):
    'class to perform rudimentary tranformations of active variables on non-transformed files'

    _activeVars = []
    # populates the activeVars array with the variable names of all active
    # variables
    @staticmethod
    def getActiveDecls(file):
        for aUnit in fortUnitIterator(file,False):
            for aDecl in aUnit.decls:
                if aDecl.mod[0] == '(active)':
                    if hasattr(aDecl, 'decls'):
                        for adecl in aDecl.decls:
                            if isinstance(adecl, fs._NoInit):
                                if hasattr(adecl.lhs, 'head'):
                                    TransformActiveVariables.\
                                        _activeVars.append(adecl.lhs.head.lower())
                                else:
                                    TransformActiveVariables.\
                                        _activeVars.append(adecl.lhs.lower())

    def __init__(self, aUnit):
        self.__myUnit = aUnit
        self.__transformDecls = []
        self.__newExecs = []

    # recursively transforms an expression if it contains a variable in
    # the activeVars array by adding %v to it
    def __transformActiveTypes(self,Exp):
        if Exp in self._activeVars:
            Exp = fe.Sel(Exp,"v")
        elif isinstance(Exp,fe.App):
            if Exp.head.lower() in self._activeVars:
                Exp = fe.Sel(Exp,"v")
            else:
                Exp.args[0] = self.__transformActiveTypes(Exp.args[0])
        else:
            if hasattr(Exp, "_sons"):
                for aSon in Exp._sons:
                    theSon = getattr(Exp,aSon)
                    newSon = self.__transformActiveTypes(theSon)
                    setattr(Exp,aSon,newSon)
        return Exp
        
    # transforms all exec statements in the file if they contain a variable
    # in the activeVars array and returns the unit
    def transformFile(self):
        for anExec in self.__myUnit.execs:
            if hasattr(anExec, "_sons"):
                for aSon in anExec._sons:
                    theSon = getattr(anExec,aSon)
                    newSon = self.__transformActiveTypes(theSon)
                    setattr(anExec,aSon,newSon)
                    anExec.flow()
            self.__newExecs.append(anExec)
        self.__myUnit.execs = self.__newExecs
        return self.__myUnit
