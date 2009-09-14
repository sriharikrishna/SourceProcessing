from _Setup import *

from PyUtil.debugManager import DebugManager
from PyUtil.symtab import Symtab,SymtabEntry,SymtabError

from PyFort.typeInference import TypeInferenceError,expressionType,functionType,isArrayReference
import PyFort.fortExp as fe
import PyFort.fortStmts as fs
import PyFort.intrinsic as intrinsic
from PyFort.fortUnit import fortUnitIterator
from PP.templateExpansion import * 
import PyFort.flow as flow
import re
import copy

# Handles errors that occur during the postprocessing stage
class PostProcessError(Exception):
    '''Exception for errors that occur during postprocessing'''
    def __init__(self,msg,lineNumber):
        self.msg = msg
        self.lineNumber = lineNumber

# Handles postprocessing
class UnitPostProcessor(object):
    'class to facilitate post-processing on a per-unit basis'

    _transform_deriv = False

    @staticmethod
    def setDerivType(transformDerivType):
        UnitPostProcessor._transform_deriv = transformDerivType

    # set the default here
    _inlineFile = 'ad_inline.f'
    _inlineFileUnits = []

    @staticmethod
    def setInlineFile(inlineFile):
        UnitPostProcessor._inlineFile = inlineFile

    # set something here for the unit tests
    _replacement_type = 'active' 

    @staticmethod
    def setReplacementType(replacementType):
        UnitPostProcessor._replacement_type = replacementType

    # set something here for the unit tests
    _abstract_type = 'oadactive'

    @staticmethod
    def setAbstractType(abstractType):
        UnitPostProcessor._abstract_type = abstractType.lower()

    @staticmethod
    def setOutputFormat(freeOutput):
        flow.free_flow(freeOutput)

    _mode = 'forward'

    @staticmethod
    def setMode(mode):
        UnitPostProcessor._mode = mode

    _free = False
    @staticmethod
    def setFreeFlow(free):
        UnitPostProcessor._free = free

    _activeVariablesFileName=None

    @staticmethod
    def setActiveVariablesFile(fileName):
        UnitPostProcessor._activeVariablesFileName = fileName

    def __init__(self, aUnit):
        self.__myUnit = aUnit
        self.__myNewDecls = []
        self.__myNewExecs = []
        self.__recursionDepth = 0
        self.__expChanged = False
        # if we're processing an inquiry expression
        self.__inquiryExpression = False
        # the recursion level at which the inquiry expression occurred
        self.__inquiryRecursionLevel = 0
        # the current unit being inserted from the inline file (reverse mode)
        self.__inlineUnit = None
        # the file which contains all declarations of active variables
        self.__active_file = None
        # determines whether or not the current subroutine 
        # should be written to the active file
        self.__write_subroutine=True

    # adds the active module OAD_active
    # called when a module declaration is encountered in the unit's declarations
    # PARAMS:
    # arg -- the first use statement declaration in this subunit
    # RETURNS: a new statement to append to the unit's declaration statements
    def __addActiveModule(self,arg):
        DebugManager.debug('unitPostProcessor.__addActiveModule called on: "'\
                           +str(arg)+"'")
        new_stmt = fs.UseAllStmt(moduleName='OAD_active',renameList=None)
        return new_stmt

    # Rewrites the active type in derived type declarations
    # returns the declaration
    # PARAMS:
    # DrvdTypeDecl: A derived type declaration to be transformed
    # RETURNS: a transformed declaration statement to apppend to the unit's 
    # declaration statements
    def __rewriteActiveType(self, DrvdTypeDecl):
        ''' convert abstract to concrete active type 
        only applied to type declaration statements '''
        DebugManager.debug('unitPostProcessor.__rewriteActiveType called on: "'+str(DrvdTypeDecl)+"'")
        newDecls = []
        for decl in DrvdTypeDecl.decls:
            newDecls.append(self.__transformActiveTypesExpression(decl))
        DrvdTypeDecl.decls = newDecls
        if DrvdTypeDecl.mod[0].lower() == '('+self._abstract_type+')':
            DrvdTypeDecl.mod = ['('+self._replacement_type+')']
            DrvdTypeDecl.dblc = True
        else:
            DrvdTypeDecl.dblc = False
        DrvdTypeDecl.flow()
        return DrvdTypeDecl

    # Transforms active types for an expression recursively
    # (replaces instances of __value__ and __deriv__)
    # PARAMS:
    # theExpression -- an fe.Exp object in which to transform active types
    # RETURNS: a transformed expression with all __value__ and __deriv__ calls
    # replaced
    def __transformActiveTypesExpression(self,theExpression):
        'mutate __value__ and __deriv__ calls'
        # deepcopy allows for comparison of return value and input value in calling function
        if self.__recursionDepth is 0:
            replacementExpression = copy.deepcopy(theExpression)
        else:
            replacementExpression = theExpression

        DebugManager.debug(self.__recursionDepth*'|\t'+'unitPostProcessor.__transformActiveTypesExpression called on"'+str(theExpression)+'"')
        self.__recursionDepth += 1
        if isinstance(replacementExpression, fe.App):
            if intrinsic.is_inquiry(replacementExpression.head):
                self.__inquiryExpression = True
                self.__inquiryRecursionLevel = self.__recursionDepth
            replacementExpression.args = \
                map(self.__transformActiveTypesExpression,replacementExpression.args)
            if replacementExpression.head == '__value__':
                if self.__inquiryExpression and \
                        self.__inquiryRecursionLevel == self.__recursionDepth - 1:
                    replacementExpression = replacementExpression.args[0]
                else:
                    nv = replacementExpression.args[0]
                    replacementExpression = fe.Sel(nv,"v")
                self.__expChanged=True
            elif replacementExpression.head == '__deriv__':
                if self._transform_deriv:
                    nv = replacementExpression.args[0]
                    replacementExpression = fe.Sel(nv,"d")
                else:
                    replacementExpression = replacementExpression.args[0]
                self.__expChanged=True
        else:
            if hasattr(replacementExpression, "_sons"):
                for aSon in replacementExpression._sons:
                    theSon = getattr(replacementExpression,aSon)
                    newSon = self.__transformActiveTypesExpression(theSon)
                    setattr(replacementExpression,aSon,newSon)
            elif isinstance(replacementExpression,fs._NoInit):
                replacementExpression = fs._NoInit(self.__transformActiveTypesExpression(replacementExpression.lhs))


        self.__recursionDepth -= 1
        if self.__recursionDepth == self.__inquiryRecursionLevel:
            self.__inquiryExpression = False
            self.__inquiryRecursionLevel = 0
        if self.__expChanged is True:
            return replacementExpression
        else:
            return theExpression


    # transforms active types in a subroutine Call statement
    # PARAMS:
    # aSubCallStmt -- an instance of fs.CallStmt to be processed
    # RETURNS: a new fs.CallStmt with __value__ and __deriv__ calls replaced
    def __processSubCallStmt(self,aSubCallStmt):
        DebugManager.debug('unitPostProcessor.__processSubCallStmt called on: "'+str(aSubCallStmt)+"'")
        replacementArgs = []
        for anArg in aSubCallStmt.args:
           replacementArgs.append(self.__transformActiveTypesExpression(anArg))
        replacementStatement = \
            fs.CallStmt(aSubCallStmt.head,
                        replacementArgs,
                        aSubCallStmt.stmt_name,
                        lineNumber=aSubCallStmt.lineNumber,
                        label=aSubCallStmt.label,
                        lead=aSubCallStmt.lead).flow()
        return replacementStatement    

    # transforms __value__/__deriv__ in active type variables in any IOStmt instance
    # PARAMS:
    # anIOStmt -- the instance of fs.IOStmt to be processed
    def __processIOStmt(self,anIOStmt):
        DebugManager.debug('unitPostProcessor.__processIOStmt called on: "'\
                               +str(anIOStmt)+" "+str(anIOStmt.__class__)+"'")
        newItemList=[]
        for item in anIOStmt.itemList:
            newItemList.append((self.__transformActiveTypesExpression(item)))
        anIOStmt.itemList=newItemList
        return anIOStmt.flow()
        
    # Does active type transformations on a StmtFnStmt; 
    # reconstructs it as an AssignStmt if StmtFnStmt.name is "__value__"
    # PARAMS:
    # StmtFnStmt -- an instance of fs.StmtFnStmt to be processed. If the
    # processed statement has __value__ or __deriv__ as the statement name, it
    # must be reconstructed as an fs.AssignStmt. The parser processes these
    # statements as declarations, but after __value__ and __deriv__ calls are
    # transformed (active types replaced), the statements are in the form of
    # Assign statements, and must become executive statements
    # RETURNS: a processed AssignStmt with all __value__ and __deriv__ calls 
    # replaced
    def __processStmtFnStmt(self, StmtFnStmt):
        DebugManager.debug('unitPostProcessor.__processStmtFnStmt called on: "'+str(StmtFnStmt)+"'")

        new_args = map(self.__transformActiveTypesExpression,StmtFnStmt.args)
        newStatement = \
            fs.StmtFnStmt(name=self.__transformActiveTypesExpression(StmtFnStmt.name),
                          args=new_args,
                          body=self.__transformActiveTypesExpression(StmtFnStmt.body),
                          lineNumber=StmtFnStmt.lineNumber,
                          label=StmtFnStmt.label,
                          lead=StmtFnStmt.lead).flow()
        if newStatement.name == '__value__':
            newApp = self.__transformActiveTypesExpression(fe.App(newStatement.name,newStatement.args))
            replacementStatement = \
                fs.AssignStmt(newApp,
                              newStatement.body,
                              newStatement.lineNumber,
                              label=newStatement.label,
                              lead=newStatement.lead).flow()
        elif newStatement.name == '__deriv__':
            newApp = self.__transformActiveTypesExpression(fe.App(newStatement.name,newStatement.args))
            replacementStatement = \
                fs.AssignStmt(newApp,
                              newStatement.body,
                              newStatement.lineNumber,
                              label=newStatement.label,
                              lead=newStatement.lead).flow()
        else:
            replacementStatement = newStatement
        return replacementStatement

    # Transforms active types on executable statements
    # PARAMS:
    # aStmt -- a generic fs.Exec statement to be processed
    # RETURNS: a transformed statement with all __value__ and __deriv__ calls
    # replaced
    def __transformActiveTypes(self,aStmt):
        '''Transforms active types on general executable statements'''
        DebugManager.debug('unitPostProcessor.__transformActiveTypes called on: "'+str(aStmt)+"'")

        if not hasattr(aStmt,"_sons") or (aStmt._sons == []):
            return aStmt
        
        for aSon in aStmt._sons:
            theSon = getattr(aStmt,aSon)
            newSon = self.__transformActiveTypesExpression(theSon)    
            if newSon is not theSon:
                setattr(aStmt,aSon,newSon)
        return aStmt.flow()

    # Determines the function to be inlined (if there is one)
    # from the comment, and sets inlineUnit (the current unit being inlined)
    # to be the corresponding unit from the inline file's units
    # PARAMS:
    # aComment -- a comment from the input file being processed
    # RETURNS: a tuple containing a comment and a boolean. If an inline command
    # was contained within the input comment, the return comment is the remainder
    # of the comment after the inline request is removed. The boolean determines
    # whether or not inlining should happen in the next applicable statement. 
    # inline is True if an inline was requested, and false otherwise
    def __getInlinedFunction(self,aComment):
        '''Retrieves the unit to be inlined'''
        function = None
        inline = False
        match = re.search('C[ ]+[$]openad[$][ ]+inline',\
                              aComment.rawline,re.IGNORECASE)
        if match:
            p = re.compile(r'\(')
            # get name of inlined function
            inlineFunction = p.split(aComment.rawline[match.end():])[0].lstrip()
            aComment =\
                fs.Comments("C!! requested inline of '"+inlineFunction+\
                                "' has no defn\n")
            for aUnit in UnitPostProcessor._inlineFileUnits:
                if (aUnit.uinfo.name).lower() == (inlineFunction).lower():
                    self.__inlineUnit = aUnit
                    aComment = None
                    inline = True
                    break
        return (aComment,inline)

    # gets the replacement number from a begin replacement comment
    # PARAMS:
    # aComment -- a comment from the input file being processed
    # RETURNS: a pragma number for replacement, if the comment contained a begin
    # replacement command. Otherwise 0
    def __getReplacementNum(self,aComment):
        '''Determines the pragma number for replacement'''
        begin_match = \
            re.search('C[ ]+[$]openad[$][ ]+begin[ ]+replacement[ ]+',
                      aComment.rawline,re.IGNORECASE)
        if begin_match:
            num_match=re.search('[0-9]+',aComment.rawline[begin_match.end(0):])
            if num_match:
                replacementNum = num_match.group(0)
                return int(replacementNum)
        else:
            return 0

    # finds the end of a replacement
    # PARAMS:
    # aComment -- a comment from the input file being processed
    # RETURNS: True if an end replacement command is contained in the comment.
    # False otherwise
    def __endReplacement(self,aComment):
        '''Finds the end of a replacement'''
        end_match = \
            re.search('C[ ]+[$]openad[$][ ]+end[ ]+replacement',
                      aComment.rawline,re.IGNORECASE)
        if end_match:
            return True
        else:
            return False

    # removes all statements which should not be inserted
    # from a unit (function) in the inline file
    # PARAMS:
    # function -- a unit from the inline file to be used in processing
    # RETURNS: a modified unit with all extraneous statements removed
    @staticmethod
    def __getInlineSubroutine(function):
        pattern = 'C([ ]+)[$]openad[$]([ ]+)end([ ]+)decls'
        newDecls = []
        newExecs = []

        for aDecl in function.decls:
            if isinstance(aDecl,fs.StmtFnStmt):
                newDecls.append(aDecl)

        for anExec in function.execs:
            if anExec.is_comment():
                match=re.search(pattern,anExec.rawline,re.IGNORECASE)
                if match:
                    cmnt = anExec.rawline[:match.start()]+anExec.rawline[match.end():]
                    newExecs.append(fs.Comments(cmnt.strip()))
                else:
                    newExecs.append(anExec)
            else:
                newExecs.append(anExec)            

        function.decls = newDecls
        function.execs = newExecs
        return function        

    # Replaces inline args with the given args (as determined from a comment)
    # During inlining of a subroutine function in reverse mode
    # used on a string (AssignStmts, etc)
    # PARAMS:
    # argReps -- the number of times to loop through looking at arguments (the
    # number of arguments to look at); equal to the minimum of the number of
    # inlineArgs and number of replacementArgs
    # string -- the string in which arguments must be replaced
    # inlineArgs -- arguments from the inline file (args to be replaced)
    # replacementArgs -- arguments from the input file being processed
    # RETURNS: a modified strings with all inlineArgs replaced by the 
    # appropriate argument from replacementArgs
    def __replaceArgs(self,argReps,string,inlineArgs,replacementArgs):
        while argReps >= 0:
            string = self.__replaceArg(string,\
                                       str(inlineArgs[argReps]),\
                                       str(replacementArgs[argReps]))
            argReps -= 1
        return string
    
    # Replace every instance of one particular argument in a string
    def __replaceArg(self,string,inlineArg,replacementArg):
        strList = string.split(inlineArg)
        i = 1
        while i < len(strList):
            if (strList[i-1])[-1:].isalnum() or (strList[i])[:1].isalnum():
                strList[i-1] = strList[i-1]+inlineArg+strList[i]
                strList.pop(i)
            else:
                i += 1
        newStr = replacementArg.join(strList)
        return newStr

    # Replaces inline args with the given args (as determined from a comment)
    # During inlining of a subroutine function in reverse mode
    # called on _son attributes
    # PARAMS:
    # arg -- the expression to be modified (is one of the sons of a statement)
    # inlineArgs -- arguments from the inline file (args to be replaced)
    # replacementArgs -- arguments from the input file being processed
    # RETURNS: a modified expression to replace the old son in the statement
    # being processed
    def __replaceSon(self,arg,inlineArgs,replacementArgs):
        newSon = arg
        if isinstance(arg,fe.Sel):
            try:
                index = inlineArgs.index(arg.head)
                head = replacementArgs[index]
                newSon = fe.Sel(head,arg.proj)
            except:
                pass
        elif isinstance(arg,fe.App):
            head = arg.head
            args = arg.args
            newArgs = []
            i = 0
            while i < len(arg.args):
                anArg = arg.args[i]
                if isinstance(anArg,fe.App) or isinstance(anArg,fe.Sel):
                    newArgs.append(self.__replaceSon(anArg,inlineArgs,replacementArgs))
                else:
                    try:
                        index = inlineArgs.index(anArg)
                        newArg = replacementArgs[index]
                        newArgs.append(newArg)
                    except:
                        newArgs.append(anArg)
                i += 1
            if len(newArgs) != 0:
                args = newArgs
            try:
                index = inlineArgs.index(arg.head)
                head = replacementArgs[index]
            except:
                pass
            newSon = fe.App(head,newArgs)
        else:
            try:
                index = inlineArgs.index(arg)
                newSon = replacementArgs[index]
            except:
                pass
        return newSon

    # Given new exec statement args (as determined from inline comment)
    # replace inline args in given inline file subroutine with new args
    # transform all active types, and return all new exec statements
    # PARAMS:
    # execStmtArgs -- arguments with which to replace the inline arguments in
    # the subroutine from the inline file
    # stmt_lead -- the lead at the beginning of the statement being processed
    # RETURNS: new exec statements created from statements from the inline file
    # and the execStmtArgs
    def __createNewExecs(self,execStmtArgs,stmt_lead):
        replacementArgs = []
        Execs = []; Stmts = []
        for anArg in execStmtArgs:
            if isinstance(anArg,fe.App):
                newArg = self.__transformActiveTypesExpression(anArg)
                replacementArgs.append(newArg)
            else:
                replacementArgs.append(anArg)
        inlineArgs = self.__inlineUnit.uinfo.args
        for decl in self.__inlineUnit.decls:
            Stmts.append(copy.deepcopy(decl))
        for Exec in self.__inlineUnit.execs:
            Stmts.append(copy.deepcopy(Exec))
        self.__inlineUnit = None

        for Stmt in Stmts:
            argReps = min(len(inlineArgs),len(replacementArgs))
            argReps -= 1
            if isinstance(Stmt,fs.Comments):
                Stmt.rawline = \
                    self.__replaceArgs(argReps,Stmt.rawline,inlineArgs,replacementArgs)
                Execs.append(Stmt.flow())
            elif isinstance(Stmt,fs.AssignStmt):
                lhs = self.__replaceArgs(argReps,str(Stmt.lhs),inlineArgs,replacementArgs)
                rhs = self.__replaceArgs(argReps,str(Stmt.rhs),inlineArgs,replacementArgs)
                newStmt = fs.AssignStmt(lhs,rhs,lead=stmt_lead)
                Execs.append(newStmt.flow())
            elif isinstance(Stmt,fs.IOStmt):
                newItemList = []
                for item in Stmt.itemList:
                    newItem=self.__replaceArgs(argReps,str(item),inlineArgs,replacementArgs)
                    newItemList.append(newItem)
                Stmt.itemList = newItemList
                Execs.append(Stmt.flow())
            elif isinstance(Stmt,fs.AllocateStmt):
                Stmt.rawline= \
                            self.__replaceArgs(argReps,Stmt.rawline,inlineArgs,replacementArgs)
                Execs.append(Stmt.flow())
            elif isinstance(Stmt,fs.DeallocateStmt):
                Stmt.rawline= \
                            self.__replaceArgs(argReps,Stmt.rawline,inlineArgs,replacementArgs)
                Execs.append(Stmt.flow())
            elif isinstance(Stmt,fs.WhileStmt) or \
                     isinstance(Stmt,fs.DoStmt):
                for aSon in Stmt._sons:
                    theSon = getattr(Stmt,aSon)
                    newSon = self.__replaceArgs(argReps,str(theSon),inlineArgs,replacementArgs)
                    setattr(Stmt,aSon,newSon)
                Execs.append(Stmt.flow())
            elif hasattr(Stmt, "_sons"):
                for aSon in Stmt._sons:
                    theSon = getattr(Stmt,aSon) 
                    if theSon is None:
                        continue
                    elif isinstance(theSon,list):
                        index = 0
                        while index < len(theSon):
                            arg = theSon[index]
                            newSon = self.__replaceSon(arg,inlineArgs,replacementArgs)
                            theSon[index] = newSon
                            index += 1
                    else:
                        newSon = self.__replaceSon(theSon,inlineArgs,replacementArgs)
                        setattr(Stmt,aSon,newSon)
                    Stmt.flow()
                Stmt.rawline.strip()
                Stmt.lead = stmt_lead
                Execs.append(Stmt.flow())
        return Execs



    # processes the comments (used for reverse mode)
    # determines if a comment declares inlining or pragma replacement
    # PARAMS:
    # Comments -- an instance of fs.Comments from the file being processed
    # replacementNum -- the replacement number for the pragma after which exec
    # statements are currently being processed
    # commentList -- a list where each index contains a list of comments. 
    #   Comments are grouped in specific indices by replacement number
    # currentComments -- comments currently being processed
    # inline -- whether or not upcoming statements require inlined 
    # functions to be inserted into file
    # RETURNS: the updated comment list, the current set of statements that have
    # been processed for the current replacement number, whether or not
    # statements should be inlined, and the current pragma (replacement) number
    def __processComments(self,Comments,replacementNum,commentList,
                          currentComments,inline=False):
        for commentString in Comments:
            if commentString == '' or commentString.strip() == '':
                continue
            newComment = fs.Comments(commentString+"\n")
            newRepNum = self.__getReplacementNum(newComment)
            if newRepNum != 0:
                replacementNum = newRepNum
                if replacementNum == 1:
                    commentList.append(currentComments)
                    currentComments = []
            elif (self.__endReplacement(newComment)):
                commentList.append(currentComments)
                currentComments = []
            else:
                (Comment,inline) = self.__getInlinedFunction(newComment)
                if Comment is not None:
                    currentComments.append(Comment.flow())
        return (commentList,currentComments,inline,replacementNum)
                    
    # transforms all active types in an exec statement
    # determines if inlining should occur (based on comments)
    # creates new exec statements for inlining based on the inline file
    # PARAMS:
    # (in forward mode, only anExecStmt and Execs are used)
    # anExecStmt -- the exec statement to be processed
    # Execs -- The execs currently being accumulated for this pragma number
    # execList -- a list of lists of accumulated processed exec statements
    # indexed by pragma number
    # inline -- a boolean determining whether or not inlining should occur
    # (if a comment to begin replacement just occurred)
    # replacementNum -- current pragma number being processed
    # RETURNS:
    # forward mode: returns a list of processed exec statements
    # reverse mode: a list of accumulated exec statements indexed by pragma
    # number, a list of execs for the current pragma number, a boolean
    # determining whether or not inlining should occur in the next statements
    # (if there was a comment to begin replacement), and a replacement pragma
    # number
    def __processExec(self,anExecStmt,Execs,execList=[],
                      inline=False,replacementNum=0): 
        try:
            DebugManager.debug('[Line '+str(anExecStmt.lineNumber)+']:')
            newStmt = None
            if anExecStmt.is_comment():
                if self._mode == 'reverse':
                    comments = anExecStmt.rawline.splitlines()
                    (execList,Execs,inline,replacementNum) = \
                        self.__processComments(comments,replacementNum,
                                               execList,Execs,inline)
                else:
                    Execs.append(anExecStmt.flow())
            elif isinstance(anExecStmt,fs.CallStmt):
                if inline is True:
                    newExecs = self.__createNewExecs(anExecStmt.args,anExecStmt.lead)
                    inline = False
                    if newExecs is not None:
                        Execs.extend(newExecs)
                else:
                    newStmt = self.__processSubCallStmt(anExecStmt)
                    Execs.append(newStmt.flow())
            elif isinstance(anExecStmt,fs.IOStmt):
                newStmt = self.__processIOStmt(anExecStmt)
                Execs.append(newStmt)
            else:
                newStmt = self.__transformActiveTypes(anExecStmt)
                Execs.append(newStmt)
            if self._mode == 'reverse':
                return (execList,Execs,inline,replacementNum)
            else:
                return Execs

        except TypeInferenceError,e:
            raise PostProcessError('Caught TypeInferenceError: '+e.msg,anExecStmt.lineNumber)
        except SymtabError,e: # add a lineNumber to SymtabErrors that don't have one
            e.lineNumber = e.lineNumber or anExecStmt.lineNumber
            raise e
       
    # transforms all active types in an declaration statement; determines
    # if inlining or pragma replacement should occur (based on comments)
    # creates new exec statements for inlining based on the inline file
    # PARAMS:
    # (in forward mode, only aDecl,Decls,Execs and UseStmtSeen are used)
    # aDecl -- the decl statement to be processed
    # Decls -- The declarations currently being accumulated for this pragma number
    # Execs -- The exec statements currently being accumulated for this pragma
    # number (since StmtFnStmt instances may be transformed into Assign statements)
    # UseStmtSeen -- boolean to determine if active module needs to be added
    # declList -- a list of lists of accumulated processed declaration statements
    # indexed by pragma number
    # execList -- a list of lists of accumulated processed exec statements
    # indexed by pragma number
    # replacementNum -- current pragma number being processed
    # RETURNS:
    # forward mode: returns a list of processed exec statements
    # reverse mode: a list of accumulated exec statements indexed by pragma
    # number, a list of execs for the current pragma number, a boolean
    # determining whether or not inlining should occur in the next statements
    # (if there was a comment to begin replacement), and a replacement pragma
    # number
    def __processDecl(self,aDecl,Decls,Execs,UseStmtSeen,declList=[],
                      execList=[],replacementNum=0):
        try:
            DebugManager.debug('[Line '+str(aDecl.lineNumber)+']:')
            if isinstance(aDecl, fs.UseStmt):
                # add old use stmt
                Decls.append(aDecl.flow())
                if not UseStmtSeen: # add the active module
                    newDecl = self.__addActiveModule(aDecl)
                    # build rawlines for the new declarations
                    newDecl.lead = self.__myUnit.uinfo.lead+''
                    UseStmtSeen = True
                    Decls.append(newDecl.flow())
            elif aDecl.is_comment():
                if self._mode == 'reverse':
                    comments = aDecl.rawline.splitlines()
                    (declList,Decls,inline,newRepNum) = \
                        self.__processComments(comments,replacementNum,
                                               declList,Decls)
                    while replacementNum != newRepNum:
                        if len(Execs) == 0:
                            execList.append([None])
                        else:
                            execList.append(Execs)
                            Execs = []
                        replacementNum += 1
                else:
                    Decls.append(aDecl.flow())
            elif isinstance(aDecl,fs.DrvdTypeDecl):
                newDecl = self.__rewriteActiveType(aDecl)
                if (self.__active_file and newDecl.dblc):
                    if self.__write_subroutine is True:
                        self.__active_file.write(self.__myUnit.uinfo.rawline)
                        self.__write_subroutine = False
                    self.__active_file.write(newDecl.rawline)
                UseStmtSeen = False
                Decls.append(newDecl)
            elif isinstance(aDecl,fs.StmtFnStmt):
                newDecl = self.__processStmtFnStmt(aDecl)
                newDecl.flow()
                UseStmtSeen = False
                if not isinstance(newDecl,fs.AssignStmt):
                    Decls.append(newDecl)
                else:
                    Execs.append(newDecl)
            else:
                DebugManager.debug('Statement "'+str(aDecl)+'" is assumed to require no post-processing')

                Decls.append(aDecl.flow())
                UseStmtSeen = False
            if self._mode == 'reverse':
                return (declList,Decls,execList,Execs,UseStmtSeen,replacementNum)
            else:
                return (Decls,Execs,UseStmtSeen)

        except TypeInferenceError,e:
            raise PostProcessError('Caught TypeInferenceError: '+e.msg,aDecl.lineNumber)
        except SymtabError,e: # add a lineNumber to SymtabErrors that don't have one
            e.lineNumber = e.lineNumber or aDecl.lineNumber
            raise e

    # Determines if a template should be expanded 
    # (if it's not a Function or Module Stmt)
    # processes the input file
    # calls expandTemplate with the processed statements
    # updates self.__myUnit with all processed and added statements
    def __templateExpansion(self):
        (Decls,Execs) = self.__reverseProcessDeclsAndExecs()
        if isinstance(self.__myUnit.uinfo,fs.ModuleStmt) \
                or isinstance(self.__myUnit.uinfo,fs.FunctionStmt):
            i = 0
            while i < len(Decls):
                for aDecl in Decls[0]:
                    if aDecl is not None:
                        self.__myNewDecls.append(aDecl)
                i += 1
            i = 0
            while i < len(Execs):
                for anExec in Execs[0]:
                    if anExec is not None:
                        self.__myNewExecs.append(anExec)
                i += 1
            return

        template = TemplateExpansion(self.__myUnit)
        newUnit = template.expandTemplate(Decls,Execs)
        self.__myUnit.uinfo.name = newUnit.uinfo.name
        self.__myUnit.cmnt = newUnit.cmnt
        self.__myNewDecls = newUnit.decls
        self.__myNewExecs = newUnit.execs
        self.__myUnit.end = newUnit.end


    # processes all declaration and execution statements in forward mode
    # RETURNS: a tuple containing a list of processed decls and a list of
    # processed execs.
    def __forwardProcessDeclsAndExecs(self):
        UseStmtSeen = False
        execNum = 0;
        Execs = []; Decls = []
        for aDecl in self.__myUnit.decls:
            (Decls,Execs,UseStmtSeen) =\
                self.__processDecl(aDecl,Decls,Execs,UseStmtSeen)
        for anExec in self.__myUnit.execs:
            Execs = self.__processExec(anExec,Execs)
        return (Decls,Execs)


    # process all decls and execs, adding all decls which are transformed 
    # to assign stmts by post processing to new execs
    # RETURNS: a tuple containing a list of processed decls and a list of
    # processed execs. Each list is indexed by pragma numbers, such that 
    # Execs[pragma] is all the execs that should be inserted in the template
    # for the given pragma.
    def __reverseProcessDeclsAndExecs(self):
        UseStmtSeen = False; inline=False
        replacementNum = 0 
        currentExecs = []; currentDecls = []
        Execs = []; Decls = []
        for aDecl in self.__myUnit.decls:
            (Decls,currentDecls,Execs,currentExecs,UseStmtSeen,replacementNum) = \
                self.__processDecl(aDecl,currentDecls,currentExecs,
                                   UseStmtSeen,Decls,Execs,replacementNum)
        if len(currentDecls) != 0:
            Decls.append(currentDecls)
        for anExec in self.__myUnit.execs:
            (Execs,currentExecs,inline,replacementNum) = \
                self.__processExec(anExec,currentExecs,Execs,
                                   inline,replacementNum)
        if len(currentExecs) != 0:
            Execs.append(currentExecs)
        return (Decls,Execs)

    # Parses the inline file into units, processes the units, and appends them
    # to inlineFileUnits for use in inlining
    @staticmethod
    def processInlineFile():
        # may be None if so set in postProcess.py
        if not UnitPostProcessor._inlineFile:
            return
        for aUnit in fortUnitIterator(UnitPostProcessor._inlineFile,False):
            newUnit = UnitPostProcessor.__getInlineSubroutine(aUnit)
            UnitPostProcessor._inlineFileUnits.append(newUnit)

    # format non-declaration or exec statements by calling flow
    def __formatUnit(self):
        # flow comments
        if self.__myUnit.cmnt is None or \
               self.__myUnit.cmnt.rawline.strip() == '':
            self.__myUnit.cmnt = None
        else:
            self.__myUnit.cmnt.flow()
        # flow unit info
        if self.__myUnit.uinfo is not None:
            self.__myUnit.uinfo.flow()
        # flow unit contains entries
        for aContainsEntry in self.__myUnit.contains:
            aContainsEntry.flow()
        # flow unit end statements
        if isinstance(self.__myUnit.end,list):
            for aStmt in self.__myUnit.end:
                aStmt.flow()
        else:
            self.__myUnit.end.flow()

    # Processes all statements in the unit
    def processUnit(self):
        ''' post-process a unit '''
        DebugManager.debug(('+'*55)+' Begin post-processing unit <'+str(self.__myUnit.uinfo)+'> '+(55*'+'))
        DebugManager.debug('local '+self.__myUnit.symtab.debug())
        DebugManager.debug('subunits (len = '+str(len(self.__myUnit.ulist))+'):')

        for subUnit in self.__myUnit.ulist:
            DebugManager.debug(str(subUnit))
            UnitPostProcessor(subUnit).processUnit()

        if (UnitPostProcessor._activeVariablesFileName):     
            self.__active_file = open(UnitPostProcessor._activeVariablesFileName,'a')

        self.__formatUnit()

        if self._mode == 'reverse':
            inline = False
            self.__templateExpansion()
            self.__myUnit.decls = self.__myNewDecls
            self.__myUnit.execs = self.__myNewExecs

        else:
            (Decls,Execs) = self.__forwardProcessDeclsAndExecs()
            self.__myUnit.decls = Decls
            self.__myUnit.execs = Execs

        if self.__active_file and self.__write_subroutine is False:
            if self.__myUnit.end:
                for anEndListEntry in self.__myUnit.end:
                    self.__active_file.write(anEndListEntry.rawline,)
            self.__write_subroutine = True
            self.__active_file.close()

        if (self.__recursionDepth is not 0):
            raise PostProcessError('Recursion error in unitPostProcess: final recursion depth is not zero')
        DebugManager.debug(('+'*54)+' End post-process unit <'+str(self.__myUnit.uinfo)+'> '+(54*'+')+'\n\n')

        return self.__myUnit
