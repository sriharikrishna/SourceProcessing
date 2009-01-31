from _Setup import *

from PyUtil.symtab import Symtab,SymtabEntry,SymtabError
from PyFort.typeInference import TypeInferenceError,expressionType,functionType,isArrayReference
import PyFort.fortExp as fe
import PyFort.fortStmts as fs
import PyFort.intrinsic as intrinsic
import re
import copy

class PostProcessError(Exception):
    '''Exception for errors that occur during postprocessing'''
    def __init__(self,msg,linenumber):
        self.msg = msg
        self.linenumber = linenumber

class UnitPostProcessor(object):
    'class to facilitate post-processing on a per-unit basis'

    _verbose = False

    @staticmethod
    def setVerbose(isVerbose):
        UnitPostProcessor._verbose = isVerbose    

    _inline_deriv = False

    @staticmethod
    def setDerivType(inlineDerivType):
        UnitPostProcessor._inline_deriv = inlineDerivType

    _replacement_type = 'active'

    @staticmethod
    def setReplacementType(replacementType):
        UnitPostProcessor._replacement_type = replacementType

    def __init__(self, aUnit):
        self.__myUnit = aUnit
        self.__myNewDecls = []
        self.__myNewExecs = []
        self.__recursionDepth = 0
        self.__expChanged = False
        # if we're in an inquiry expression
        self.__inquiryExpression = False
        # the recursion level at which the inquiry expression occurred
        self.__inquiryRecursionLevel = 0

    # adds the active module OAD_active
    def __addActiveModule(self,arg):
        if self._verbose: print 'unitPostProcessor.__addActiveModule called on: "'+str(arg)+"'"
        new_stmt = fs.UseStmt('OAD_active')
        return new_stmt

    # Rewrites the active type in derived type declarations
    # returns the declaration
    def __rewriteActiveType(self, DrvdTypeDecl):
        ''' convert type (OpenAD_type) to type(active)
        only applied to type declaration statements '''
        if self._verbose: print 'unitPostProcessor.__rewriteActiveType called on: "'+str(DrvdTypeDecl)+"'"
        newDecls = []
        for decl in DrvdTypeDecl.decls:
            newDecls.append(self.__inlinableExpression(decl))
        DrvdTypeDecl.decls = newDecls
        if DrvdTypeDecl.mod[0].lower() in set(['(openadty_active)',
                                               '(openad_type)']):
            DrvdTypeDecl.mod = ['('+self._replacement_type+')']
            DrvdTypeDecl.dblc = True
        return DrvdTypeDecl

    # Executes inlining for an expression recursively
    # (replaces instances of __value__ and __deriv__)
    def __inlinableExpression(self,theExpression):
        'mutate __value__ and __deriv__ calls'
        # deepcopy allows for comparison of return value and input value in calling function
        if self.__recursionDepth is 0:
            replacementExpression = copy.deepcopy(theExpression)
        else:
            replacementExpression = theExpression

        if self._verbose: 
            print self.__recursionDepth*'|\t'+'unitPostProcessor.__inlinableExpression called on"'+str(theExpression)+'"'
        self.__recursionDepth += 1
        if isinstance(replacementExpression, fe.App):
            if intrinsic.is_inquiry(replacementExpression.head):
                self.__inquiryExpression = True
                self.__inquiryRecursionLevel = self.__recursionDepth
            replacementExpression.args = map(self.__inlinableExpression,replacementExpression.args)
            if replacementExpression.head == '__value__':
                if self.__inquiryExpression:
                    replacementExpression = replacementExpression.args[0]
                else:
                    nv = replacementExpression.args[0]
                    replacementExpression = fe.Sel(nv,"v")

                self.__expChanged=True
            elif replacementExpression.head == '__deriv__':
                if self._inline_deriv:
                    nv = replacementExpression.args[0]
                    replacementExpression = fe.Sel(nv,"d")
                else:
                    replacementExpression = replacementExpression.args[0]
                self.__expChanged=True
        else:
            if hasattr(replacementExpression, "_sons"):
                for aSon in replacementExpression._sons:
                    theSon = getattr(replacementExpression,aSon)
                    newSon = self.__inlinableExpression(theSon)
                    setattr(replacementExpression,aSon,newSon)
            elif isinstance(replacementExpression,fs._NoInit):
                replacementExpression = fs._NoInit(self.__inlinableExpression(replacementExpression.lhs))


        self.__recursionDepth -= 1
        if self.__recursionDepth == self.__inquiryRecursionLevel:
            self.__inquiryExpression = False
            self.__inquiryRecursionLevel = 0
        if self.__expChanged is True:
            return replacementExpression
        else:
            return theExpression


    # does inlining on a SubCallStmt
    def __processSubCallStmt(self,aSubCallStmt):
        if self._verbose: print 'unitPostProcessor.__processSubCallStmt called on: "'+str(aSubCallStmt)+"'"
        replacementArgs = []
        for anArg in aSubCallStmt.args:
            if isinstance(anArg,fe.App):
                newArg = self.__inlinableExpression(anArg)
                replacementArgs.append(newArg)
            else:
                replacementArgs.append(anArg)
        replacementStatement = fs.CallStmt(aSubCallStmt.head,
                                           replacementArgs,
                                           aSubCallStmt.stmt_name,
                                           lineNumber=aSubCallStmt.lineNumber,
                                           label=aSubCallStmt.label,
                                           lead=aSubCallStmt.lead).flow()
        return replacementStatement

    # Does inlining on a StmtFnStmt; reconstructs it as an AssignStmt if StmtFnStmt.name is "__value__"
    def __processStmtFnStmt(self, StmtFnStmt):
        if self._verbose: print 'unitPostProcessor.__processStmtFnStmt called on: "'+str(StmtFnStmt)+"'"

        new_args = map(self.__inlinableExpression,StmtFnStmt.args)
        newStatement = fs.StmtFnStmt(name=self.__inlinableExpression(StmtFnStmt.name),
                                             args=new_args,
                                             body=self.__inlinableExpression(StmtFnStmt.body),
                                             lineNumber=StmtFnStmt.lineNumber,
                                             label=StmtFnStmt.label,
                                             lead=StmtFnStmt.lead).flow()
        if newStatement.name == '__value__':
            newApp = self.__inlinableExpression(fe.App(newStatement.name,newStatement.args))
            replacementStatement = fs.AssignStmt(newApp,
                                                 newStatement.body,
                                                 newStatement.lineNumber,
                                                 label=newStatement.label,
                                                 lead=newStatement.lead).flow()

        else:
            replacementStatement = newStatement
        return replacementStatement


    def __inlineStmt(self,aStmt):
        '''Does inlining on general executable statements'''
        if self._verbose: print 'unitPostProcessor.__inlineStmt called on: "'+str(aStmt)+"'"

        if not hasattr(aStmt,"_sons") or (aStmt._sons == []):
            return aStmt
        
        self.__expChanged=False
        for aSon in aStmt._sons:
            theSon = getattr(aStmt,aSon)
            newSon = self.__inlinableExpression(theSon)    
            if newSon is not theSon:
                diff = True
                setattr(aStmt,aSon,newSon)
        # if statement is unchanged, leave it alone
        if self.__expChanged is True:
            aStmt.flow()
        return aStmt

    # Processes all statements in the unit
    def processUnit(self):
        ''' post-process a unit '''
        if self._verbose: print ('+'*55)+' Begin post-processing unit <',self.__myUnit.uinfo,'> '+(55*'+'),'\nlocal',self.__myUnit.symtab.debug()
        if (self._verbose and self.__myUnit.ulist): print 'subunits (len =',len(self.__myUnit.ulist),'):'

        for subUnit in self.__myUnit.ulist:
            if self._verbose: print subUnit
            UnitPostProcessor(subUnit).processUnit()

        if (self._verbose and self.__myUnit.execs): print 'post-processing executable statements:'

        for anExecStmt in self.__myUnit.execs:
            try:
                if self._verbose: 
                    print '[Line '+str(anExecStmt.lineNumber)+']:'
                if isinstance(anExecStmt,fs.CallStmt):
                    newStmt = self.__processSubCallStmt(anExecStmt)
                else:
                    newStmt = self.__inlineStmt(anExecStmt)
                self.__myNewExecs.append(newStmt)
                    
            except TypeInferenceError,e:
                raise PostProcessError('Caught TypeInferenceError: '+e.msg,anExecStmt.lineNumber)
            except SymtabError,e:
                raise PostProcessError('Caught SymtabError: '+e.msg,anExecStmt.lineNumber)

        # Used for adding the active module 
        # (active module added after first use statement)
        # 1 if  the last statement seen was a use statement
        UseStmtSeen = 0

        for aDecl in self.__myUnit.decls:
            try:
                if self._verbose: 
                    print '[Line '+str(aDecl.lineNumber)+']:'
                if isinstance(aDecl, fs.UseStmt):
                    # add old use stmt
                    self.__myNewDecls.append(aDecl) 
                    if UseStmtSeen: # don't add the active module
                        continue
                    else: # add the active module
                        newDecl = self.__addActiveModule(aDecl)
                    # build rawlines for the new declarations
                        newDecl.lead = self.__myUnit.uinfo.lead+''
                        newDecl.flow()
                        UseStmtSeen = 1
                elif isinstance(aDecl,fs.DrvdTypeDecl):
                    newDecl = self.__rewriteActiveType(aDecl)
                    newDecl.flow()
                    UseStmtSeen = 0
                elif isinstance(aDecl,fs.StmtFnStmt):
                    newDecl = self.__processStmtFnStmt(aDecl)
                    newDecl.flow()
                    UseStmtSeen = 0
                else:
                    if self._verbose: print 'Statement "'+str(aDecl)+'" is assumed to require no post-processing'
                    newDecl = aDecl
                    UseStmtSeen = 0
                self.__myNewDecls.append(newDecl)                    

            except TypeInferenceError,e:
                raise PostProcessError('Caught TypeInferenceError: '+e.msg,aDecl.lineNumber)
            except SymtabError,e:
                raise PostProcessError('Caught SymtabError: '+e.msg,aDecl.lineNumber)

        self.__myUnit.decls = self.__myNewDecls
        self.__myUnit.execs = self.__myNewExecs
        
        if (self.__recursionDepth is not 0):
            raise PostProcessError('Recursion errorin unitPostProcess: final recursion depth is not zero')

        if self._verbose: print ('+'*54)+' End post-process unit <',self.__myUnit.uinfo,'> '+(54*'+')+'\n\n'
        return self.__myUnit




