'''
   Transformations:
      1. add the active_module
      2. convert type(openadty_active)  ... TO
         type(active) :: ...
      3. convert __value__($p) --> $p%v
      4. convert __deriv__($p) --> $p
'''


from _Setup import *

from PyUtil.symtab import Symtab,SymtabEntry,SymtabError
from PyFort.typeInference import TypeInferenceError,expressionType,functionType,isArrayReference
import PyFort.fortExp as fe
import PyFort.fortStmts as fs
import re

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

    def __init__(self, aUnit):
        self.__myUnit = aUnit
        self.__myNewDecls = []
        self.__myNewExecs = []
        self.__recursionDepth = 0

    # Rewrites the active type in derived type declarations
    # returns the declaration
    def __rewriteActiveType(self, DrvdTypeDecl):
        ''' convert type (OpenAD_type) to type(active)
        only applied to type declaration statements '''
        if self._verbose: print 'unitPostProcessor.__rewriteActiveType called on: "'+str(DrvdTypeDecl)+"'"
        newDecls = []
        for decl in DrvdTypeDecl.decls:
            newDecls.append(self.__inlinableDeclaration(decl))
        DrvdTypeDecl.decls = newDecls
        if DrvdTypeDecl.mod[0].lower() in set(['(openadty_active)',
                                               '(openad_type)']):
            DrvdTypeDecl.mod = ['(active)']
            DrvdTypeDecl.dblc = True
        return DrvdTypeDecl

    # Manages inlining on an iterator over a declaration
    # "add_str" is the string to replace the matches in the iterator
    # __value__(x) -> (x)%v, and __deriv__(x) -> (x)%d 
    def __inline(self,iterator,DeclString,add_str):
        decl_str = ''
        last = 0
        for match in iterator:
            (start,end) = match.span()
            count = 0 # count number of parens to keep them balanced
            newstr = ''
            index = end
            for char in DeclString[end:len(DeclString)]:
                index +=1
                if char == '(':
                    newstr = newstr + char
                    count += 1
                elif char == ')':
                    newstr = newstr + char
                    count -= 1
                else:
                    newstr = newstr + char
                if count == 0:
                    break
            if last == 0:
                decl_str = DeclString[0:start]
            last = index
            decl_str += newstr+add_str
        decl_str += DeclString[last:]
        return decl_str

    # Executes inlining for a declaration by iterating over it
    # (replaces instances of __value__ and __deriv__)
    def __inlinableDeclaration(self,aDecl):
        if self._verbose: print 'unitPostProcessor.__inlinableDeclaration called on: "'+str(aDecl)+'"'
        value = re.compile('__value__')
        iterator = value.finditer(str(aDecl))
        value_str = self.__inline(iterator,str(aDecl),'%v')

        deriv = re.compile('__deriv__')
        iterator = deriv.finditer(value_str)
        decl_str = self.__inline(iterator,value_str,'%d')

        newDecl = fs._NoInit(decl_str)
        return newDecl

    # Executes inlining for an expression recursively
    # (replaces instances of __value__ and __deriv__)
    def __inlinableExpression(self,theExpression):
        'mutate __value__ and __deriv__ calls'
        if self._verbose: 
            print self.__recursionDepth*'|\t'+'unitPostProcessor.__inlinableExpression called on"'+str(theExpression)+'"'
        self.__recursionDepth += 1
        if isinstance(theExpression, fe.App):
            if isinstance(theExpression.args[0],fe.App):
                theExpression.args[0] = self.__inlinableExpression(theExpression.args[0])
            if len(theExpression.args) > 1:
                theExpression.args[1:] = self.__inlinableExpression(theExpression.args[1:])
            if theExpression.head == '__value__':
                nv = theExpression.args[0]
                replacementExpression = fe.Sel(nv,"v")
            elif theExpression.head == '__deriv__':
                nv = theExpression.args[0]
                replacementExpression = fe.Sel(nv,"d")
            else:
                replacementExpression = theExpression

#        elif isinstance(theExpression, fe.Sel):
#            print "the expression is fe.sel:\n"
#            replacementExpression = self.__inlinableExpression(theExpression.head)

        elif isinstance(theExpression, fe.Unary):
            if isinstance(theExpression, fe.Umi):
                replacementExpression = fe.Umi(self.__inlinableExpression(theExpression.exp))
            elif isinstance(theExpression, fe.Upl):
                replacementExpression = fe.Upl(self.__inlinableExpression(theExpression.exp))
            elif isinstance(theExpression, fe.Not):
                replacementExpression = fe.Not(self.__inlinableExpression(theExpression.exp))
            elif isinstance(theExpression, fe.ParenExp):
                replacementExpression = fe.ParenExp(self.__inlinableExpression(theExpression.exp))
            else:
                replacementExpression = theExpression

        elif isinstance(theExpression, fe.Ops):
            replacementExpression = fe.Ops(theExpression.op,
                   self.__inlinableExpression(theExpression.a1),
                   self.__inlinableExpression(theExpression.a2))
        else:
            replacementExpression = theExpression

        self.__recursionDepth -= 1
        return replacementExpression

    # adds the active module OAD_active
    def __addActiveModule(self,arg):
        if self._verbose: print 'unitPostProcessor.__addActiveModule called on: "'+str(arg)+"'"
        new_stmt = fs.UseStmt('OAD_active')
        new_comment = fs.comment_bl('use active module (OAD_active)')
        self.__myNewDecls.append(new_comment)
        return new_stmt

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
                                           lineNumber=aSubCallStmt.lineNumber,
                                           label=aSubCallStmt.label,
                                           lead=aSubCallStmt.lead).flow()
        self.__myNewExecs.append(replacementStatement)
        return

    # Does inlining on an AssignStmt
    def __processAssignStmt(self, anAssignStmt):
        if self._verbose: print 'unitPostProcessor.__processAssignStmt called on: "'+str(anAssignStmt)+"'"
        replacementStatement = fs.AssignStmt(self.__inlinableExpression(anAssignStmt.lhs),
                                             self.__inlinableExpression(anAssignStmt.rhs),
                                             lineNumber=anAssignStmt.lineNumber,
                                             label=anAssignStmt.label,
                                             lead=anAssignStmt.lead).flow()
        self.__myNewExecs.append(replacementStatement)
        return

    # Does inlining on a StmtFnStmt; reconstructs it as an AssignStmt
    def __processStmtFnStmt(self, StmtFnStmt):
        if self._verbose: print 'unitPostProcessor.__processStmtFnStmt called on: "'+str(StmtFnStmt)+"'"
        newStatement = fs.StmtFnStmt(name=self.__inlinableExpression(StmtFnStmt.name),
                                             args=StmtFnStmt.args,
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
                    self.__processSubCallStmt(anExecStmt)
                elif isinstance(anExecStmt,fs.AssignStmt):
                    self.__processAssignStmt(anExecStmt)
                else:
                    if self._verbose: print 'Statement "'+str(anExecStmt)+'" is assumed to require no post-processing'
                    self.__myNewExecs.append(anExecStmt)
                    
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
                elif isinstance(aDecl, fs.DrvdTypeDecl):
                    newDecl = self.__rewriteActiveType(aDecl)
                    newDecl.flow()
                    UseStmtSeen = 0
                elif isinstance(aDecl, fs.StmtFnStmt):
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

        for anExec in self.__myNewExecs:
            anExec.lead = self.__myUnit.uinfo.lead+''
            try:
                if isinstance(anExec,fs.Comments):
                    pass
                else:
                    anExec.flow()
            except TypeInferenceError,e:
                raise PostProcessError('Caught TypeInferenceError: '+e.msg,anExec.lineNumber)
            except SymtabError,e:
                raise PostProcessError('Caught SymtabError: '+e.msg,aDecl.lineNumber)
        
        self.__myUnit.execs = self.__myNewExecs

        if self._verbose: print ('+'*54)+' End post-process unit <',self.__myUnit.uinfo,'> '+(54*'+')+'\n\n'
        return self.__myUnit




