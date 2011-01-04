from _Setup import *

from PyUtil.debugManager import DebugManager
from PyUtil.symtab import Symtab,SymtabEntry,SymtabError
from PyUtil.argreplacement import replaceArgs, replaceSon

from PyFort.inference import InferenceError,expressionType,isArrayReference
import PyFort.fortExp as fe
import PyFort.fortStmts as fs
import PyFort.intrinsic as intrinsic
from PyFort.fortUnit import fortUnitIterator, Unit
from PP.templateExpansion import *
import PyFort.flow as flow
from PyFort.fortFile import Ffile
import re
import copy

# Handles errors that occur during the postprocessing stage
class PostProcessError(Exception):
    '''Exception for errors that occur during postprocessing'''
    def __init__(self,msg,lineNumber):
        self.lineNumber = lineNumber
        self.msg = msg

    def __str__(self):
        errString='\nERROR: PostProcessError at line '+str(self.lineNumber)+': '+str(self.msg)+'\n'
        return (errString)
        
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

    _mode = 'forward'

    @staticmethod
    def setMode(mode):
        UnitPostProcessor._mode = mode

    _explicitInit = False
    @staticmethod
    def setExplicitInit(explicitInit=True):
        UnitPostProcessor._explicitInit = explicitInit

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
        # temporary setting to figure out if we are within an interface
        self.inInterface=False


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
        newDecls=[self.__transformActiveTypesExpression(decl) for decl in DrvdTypeDecl.get_decls()]
        DrvdTypeDecl.set_decls(newDecls)
        if DrvdTypeDecl.get_mod()[0].lower() == self._abstract_type:
            DrvdTypeDecl.set_mod([self._replacement_type])
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
        if (isinstance(replacementExpression, fe.App) and not isinstance(replacementExpression.head,fe.Sel)):
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
                for aSon in replacementExpression.get_sons():
                    newSon = self.__transformActiveTypesExpression(getattr(replacementExpression,aSon))
                    replacementExpression.set_son(aSon,newSon)
            elif isinstance(replacementExpression,fs._NoInit):
                replacementExpression = fs._NoInit(self.__transformActiveTypesExpression(replacementExpression.lhs))
            elif isinstance(replacementExpression,list):
                replacementExpression=[self.__transformActiveTypesExpression(item) for item in replacementExpression]

        self.__recursionDepth -= 1
        if self.__recursionDepth == self.__inquiryRecursionLevel:
            self.__inquiryExpression = False
            self.__inquiryRecursionLevel = 0
        if self.__expChanged is True:
            return replacementExpression
        else:
            return theExpression


    # PARAMS:
    # aSubCallStmt -- an instance of fs.CallStmt to be processed
    # RETURNS: a new fs.CallStmt with __value__ and __deriv__ calls replaced
    def __processSubCallStmt(self,aSubCallStmt):
        '''transforms active types in a subroutine Call statement'''
        DebugManager.debug('unitPostProcessor.__processSubCallStmt called on: "'+str(aSubCallStmt)+"'")
        replacementArgs=[self.__transformActiveTypesExpression(arg) for arg in aSubCallStmt.get_args()]
        replacementStatement = \
            fs.CallStmt(aSubCallStmt.get_head(),
                        replacementArgs,
                        aSubCallStmt.stmt_name,
                        lineNumber=aSubCallStmt.lineNumber,
                        label=aSubCallStmt.label,
                        lead=aSubCallStmt.lead)
        return replacementStatement    

    # PARAMS:
    # anIOStmt -- the instance of fs.IOStmt to be processed
    def __processIOStmt(self,anIOStmt):
        '''transforms __value__/__deriv__ in active type variables in any IOStmt instance'''
        DebugManager.debug('unitPostProcessor.__processIOStmt called on: "'\
                               +str(anIOStmt)+" "+str(anIOStmt.__class__)+"'")
        newItemList=[self.__transformActiveTypesExpression(item) for item in anIOStmt.get_itemList()]
        anIOStmt.set_itemList(newItemList)
        return anIOStmt

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
        '''Performs active type transformations on a StmtFnStmt; reconstructs it as an AssignStmt if StmtFnStmt.name is "__value__"'''
        DebugManager.debug('unitPostProcessor.__processStmtFnStmt called on: "'+str(StmtFnStmt)+"'")

        new_args = map(self.__transformActiveTypesExpression,StmtFnStmt.args)
        newStatement = \
            fs.StmtFnStmt(name=self.__transformActiveTypesExpression(StmtFnStmt.name),
                          args=new_args,
                          body=self.__transformActiveTypesExpression(StmtFnStmt.body),
                          lineNumber=StmtFnStmt.lineNumber,
                          label=StmtFnStmt.label,
                          lead=StmtFnStmt.lead)
        if newStatement.name == '__value__' or newStatement.name=='__deriv__':
            newApp = self.__transformActiveTypesExpression(fe.App(newStatement.name,newStatement.args))
            replacementStatement = \
                fs.AssignStmt(newApp,
                              newStatement.body,
                              lineNumber=newStatement.lineNumber,
                              label=newStatement.label,
                              lead=newStatement.lead)
        else:
            replacementStatement = newStatement
        return replacementStatement

    # PARAMS:
    # aStmt -- a generic fs.Exec statement to be processed
    # RETURNS: a transformed statement with all __value__ and __deriv__ calls
    # replaced
    def __transformActiveTypes(self,aStmt):
        '''Transforms active types on general executable statements'''
        DebugManager.debug('unitPostProcessor.__transformActiveTypes called on: "'+str(aStmt)+"'")

        if not hasattr(aStmt,"_sons") or (aStmt._sons == []):
            return aStmt
        
        for aSon in aStmt.get_sons():
            theSon = getattr(aStmt,aSon)
            newSon = self.__transformActiveTypesExpression(theSon)
            if newSon is not theSon:
                aStmt.set_son(aSon,newSon)
        return aStmt

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
        match=re.search('C[ ]+[$]openad[$][ ]+inline',aComment.rawline,re.IGNORECASE)
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
        return 0

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
        return False

    # PARAMS:
    # function -- a unit from the inline file to be used in processing
    # RETURNS: a modified unit with all extraneous statements removed
    @staticmethod
    def __getInlineSubroutine(function):
        '''removes all statements which should not be inserted from a unit (function) in the inline file'''
        pattern = re.compile('C([ ]+)[$]openad[$]([ ]+)end([ ]+)decls',re.IGNORECASE)
        function.decls = (filter(lambda l:isinstance(l,fs.StmtFnStmt),function.decls))
        newExecs = []
        newExecsAppend=newExecs.append
        for anExec in function.execs:
            if anExec.is_comment():
                match=pattern.search(anExec.get_rawline())
                if match:
                    cmnt = anExec.get_rawline()[:match.start()]+anExec.get_rawline()[match.end():]
                    newExecsAppend(fs.Comments(cmnt.strip()))
                else:
                    newExecsAppend(anExec)
            else:
                newExecsAppend(anExec)            

        function.execs = newExecs
        return function        


    # PARAMS:
    # execStmtArgs -- arguments with which to replace the inline arguments in
    # the subroutine from the inline file
    # stmt_lead -- the lead at the beginning of the statement being processed
    # RETURNS: new exec statements created from statements from the inline file
    # and the execStmtArgs
    def __createNewExecs(self,execStmtArgs,stmt_lead):
        '''Given new exec statement args (as determined from inline comment), replace inline args in given inline file subroutine with new args; transform all active types and return all new exec statements'''
        replacementArgs = []
        Execs = []; Stmts = []
        ExecsAppend=Execs.append
        for anArg in execStmtArgs:
            if isinstance(anArg,fe.App):
                anArg = self.__transformActiveTypesExpression(anArg)
            replacementArgs.append(anArg)
        inlineArgs = self.__inlineUnit.uinfo.args
        map(lambda l:map(lambda e:Stmts.append(copy.deepcopy(e)),l),
            [self.__inlineUnit.decls,self.__inlineUnit.execs])
        self.__inlineUnit = None

        for Stmt in Stmts:
            argReps = min(len(inlineArgs),len(replacementArgs))
            argReps -= 1
            if isinstance(Stmt,fs.Comments):
                Stmt.set_rawline(replaceArgs(argReps,Stmt.get_rawline(),inlineArgs,replacementArgs))
                ExecsAppend(Stmt)
            elif isinstance(Stmt,fs.AssignStmt):
                lhs = replaceArgs(argReps,str(Stmt.get_lhs()),inlineArgs,replacementArgs)
                rhs = replaceArgs(argReps,str(Stmt.get_rhs()),inlineArgs,replacementArgs)
                newStmt = fs.AssignStmt(lhs,rhs,lead=stmt_lead)
                ExecsAppend(newStmt)
            elif isinstance(Stmt,fs.StmtFnStmt):
                name=replaceArgs\
                      (argReps,str(Stmt.get_name()),inlineArgs,replacementArgs)
                newArgs=[replaceArgs(argReps,str(arg),inlineArgs,replacementArgs) for arg in Stmt.get_args()]
                body=replaceArgs\
                      (argReps,str(Stmt.get_body()),inlineArgs,replacementArgs)
                newStmt = fs.StmtFnStmt(name,newArgs,body,lead=stmt_lead)
                ExecsAppend(newStmt)
            elif isinstance(Stmt,fs.IOStmt):
                newItemList=[replaceArgs(argReps,str(item),inlineArgs,replacementArgs) for item in Stmt.get_itemList()]
                Stmt.set_itemList(newItemList)
                Stmt.lead = stmt_lead
                ExecsAppend(Stmt)
            elif isinstance(Stmt,fs.AllocateStmt) \
              or isinstance(Stmt,fs.DeallocateStmt) :
                Stmt.set_rawline(replaceArgs(argReps,Stmt.get_rawline(),inlineArgs,replacementArgs)+''.join(Stmt.internal))
                Stmt.lead = stmt_lead
                ExecsAppend(Stmt)
            elif isinstance(Stmt,fs.WhileStmt) or \
                     isinstance(Stmt,fs.DoStmt):
                for aSon in Stmt.get_sons():
                    theSon = getattr(Stmt,aSon)
                    if theSon :
                    	newSon = replaceArgs(argReps,str(theSon),inlineArgs,replacementArgs)
                        Stmt.set_son(aSon,newSon)
                Stmt.lead = stmt_lead
                ExecsAppend(Stmt)
            elif hasattr(Stmt, "_sons"):
                for aSon in Stmt.get_sons():
                    theSon = getattr(Stmt,aSon) 
                    if isinstance(theSon,list):
                        index = 0
                        while index < len(theSon):
                            arg = theSon[index]
                            newSon = replaceSon(argReps,arg,inlineArgs,replacementArgs)
                            if newSon is not arg:
                                theSon[index] = newSon
                                Stmt.modified = True
                            index += 1
                    elif theSon is not None:
                        newSon = replaceSon(argReps,theSon,inlineArgs,replacementArgs)
                        Stmt.set_son(aSon,newSon)
                Stmt.lead = stmt_lead
                ExecsAppend(Stmt)
            else:
                raise PostProcessError('unitPostProcess.py.__createNewExecs: don\'t know how to handle exec statement "'+Stmt+'"')
        return Execs



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
        '''processes the comments (used for reverse mode); determines if a comment declares inlining or pragma replacement'''
        commentListAppend=commentList.append
        for commentString in Comments:
            if commentString == '' or commentString.strip() == '':
                continue
            newComment = fs.Comments(commentString+"\n")
            newRepNum = self.__getReplacementNum(newComment)
            if newRepNum != 0:
                replacementNum = newRepNum
                if replacementNum == 1:
                    commentListAppend(currentComments)
                    currentComments = []
            elif (self.__endReplacement(newComment)):
                commentListAppend(currentComments)
                currentComments = []
            else:
                (Comment,inline) = self.__getInlinedFunction(newComment)
                if Comment is not None:
                    currentComments.append(Comment)
        return (commentList,currentComments,inline,replacementNum)
                    
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
        '''transforms all active types in an exec statement; determines if inlining should occur (based on comments); creates new exec statements for inlining based on the inline file'''
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
                    Execs.append(anExecStmt)
            elif isinstance(anExecStmt,fs.CallStmt):
                if inline is True:
                    newExecs = self.__createNewExecs(anExecStmt.args,anExecStmt.lead)
                    inline = False
                    if newExecs is not None:
                        Execs.extend(newExecs)
                else:
                    newStmt = self.__processSubCallStmt(anExecStmt)
                    Execs.append(newStmt)
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

        except InferenceError,e:
            raise PostProcessError('Caught InferenceError: '+e.msg,anExecStmt.lineNumber)
        except SymtabError,e: # add a lineNumber to SymtabErrors that don't have one
            e.lineNumber = e.lineNumber or anExecStmt.lineNumber
            raise e
    
    class UseActiveInInterface:
        def __init__(self):
            self.inInterface=False
            self.beginStmt=None
            self.lead=None
    
    # transforms all active types in an declaration statement; determines
    # if inlining or pragma replacement should occur (based on comments)
    # creates new exec statements for inlining based on the inline file
    # PARAMS:
    # (in forward mode, only aDecl,Decls,Execs  are used)
    # aDecl -- the decl statement to be processed
    # Decls -- The declarations currently being accumulated for this pragma number
    # Execs -- The exec statements currently being accumulated for this pragma
    # number (since StmtFnStmt instances may be transformed into Assign statements)
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
    def __processDecl(self,aDecl,Decls,Execs,pendingUse,declList=[],
                      execList=[],replacementNum=0):
        '''transforms all active types in a declaration statement; determines if inlining or pragma replacement should occur (based on comments); creates new exec statements for inlining based on the inline file'''
        try:
            DebugManager.debug('[Line '+str(aDecl.lineNumber)+']:')
            if pendingUse.beginStmt and not pendingUse.lead and not aDecl.is_comment():
                pendingUse.lead=aDecl.lead
            if aDecl.is_comment():
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
                    Decls.append(aDecl)
            elif isinstance(aDecl,fs.DrvdTypeDecl):
                newDecl = self.__rewriteActiveType(aDecl)
                Decls.append(newDecl)
            elif isinstance(aDecl,fs.InterfaceStmt):
                if (pendingUse.inInterface):
                    raise PostProcessError('logic error assume to be in interface already',aDecl.lineNumber)
                pendingUse.inInterface=True
                Decls.append(aDecl)
            elif isinstance(aDecl,fs.EndInterfaceStmt):
                if (not pendingUse.inInterface):
                    raise PostProcessError('logic error assume to be in interface ',aDecl.lineNumber)                
                pendingUse.inInterface=False
                Decls.append(aDecl)
            elif pendingUse.inInterface and any(map(lambda l: isinstance(aDecl,l),[fs.SubroutineStmt,fs.FunctionStmt])):
                Decls.append(aDecl)
                pendingUse.beginStmt=aDecl
            elif pendingUse.inInterface and any(map(lambda l: isinstance(aDecl,l),[fs.EndSubroutineStmt,fs.EndFunctionStmt])):
                Decls.append(aDecl)
                if  any(map(lambda l: isinstance(self.__myUnit.uinfo,l),[fs.SubroutineStmt,fs.FunctionStmt,fs.ProgramStmt,fs.ModuleStmt ])):
                    Decls.insert(Decls.index(pendingUse.beginStmt)+1,
                                 fs.UseAllStmt(moduleName='OAD_active',lead=pendingUse.lead,renameList=None))
                pendingUse.beginStmt=None
                pendingUse.lead=None
            elif isinstance(aDecl,fs.StmtFnStmt):
                newDecl = self.__processStmtFnStmt(aDecl)
                if not isinstance(newDecl,fs.AssignStmt):
                    Decls.append(newDecl)
                else:
                    Execs.append(newDecl)
            else:
                DebugManager.debug('Statement "'+str(aDecl)+'" is assumed to require no post-processing')
                Decls.append(aDecl)
            if self._mode == 'reverse':
                return (declList,Decls,execList,Execs,replacementNum)
            else:
                return (Decls,Execs)

        except InferenceError,e:
            raise PostProcessError('Caught InferenceError: '+e.msg,aDecl.lineNumber)
        except SymtabError,e: # add a lineNumber to SymtabErrors that don't have one
            e.lineNumber = e.lineNumber or aDecl.lineNumber
            raise e

    # Determines if a template should be expanded 
    # (if it's not a Function or Module Stmt)
    # processes the input file
    # calls expandTemplate with the processed statements
    # updates self.__myUnit with all processed and added statements
    def __templateExpansion(self):
        '''Determines if a template should be expanded (if it's not a Function or Module statement); processes the input file, and calls expandTemplate with the processed statements; updates self.__myUnit with all processed and added statements'''
        (Decls,Execs) = self.__reverseProcessDeclsAndExecs()
        if isinstance(self.__myUnit.uinfo,fs.ModuleStmt) \
                or isinstance(self.__myUnit.uinfo,fs.FunctionStmt):
            if len(Decls)>0:
                map(self.__myNewDecls.append,filter(lambda l: l is not None,Decls[0]))
            if len(Execs)>0:
                map(self.__myNewExecs.append,filter(lambda l: l is not None,Execs[0]))
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
        '''processes all declaration and execution statements in forward mode'''
        execNum = 0;
        Execs = []; Decls = []
        Unit.addActiveModule(self.__myUnit,Decls)
        pendingUse=self.UseActiveInInterface()
        for aDecl in self.__myUnit.decls:
            (Decls,Execs) =\
                self.__processDecl(aDecl,Decls,Execs,pendingUse)
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
        '''processes all decls and execs, moving all decls which are transformed to assign statements by post processing to Execs list'''
        inline=False
        replacementNum = 0 
        currentExecs = []; currentDecls = []
        Execs = []; Decls = []
        Unit.addActiveModule(self.__myUnit,currentDecls)
        pendingUse=self.UseActiveInInterface()
        for aDecl in self.__myUnit.decls:
            (Decls,currentDecls,Execs,currentExecs,replacementNum) = \
                self.__processDecl(aDecl,currentDecls,currentExecs,pendingUse,
                                   Decls,Execs,replacementNum)
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
        '''Parses the inline file into units, processes the units, and appends them to inlineFileUnits for use in inlining'''
        # may be None if so set in postProcess.py
        if not UnitPostProcessor._inlineFile:
            return
        inlineUnitAppend=UnitPostProcessor._inlineFileUnits.append
        for aUnit in fortUnitIterator(UnitPostProcessor._inlineFile):
            newUnit = UnitPostProcessor.__getInlineSubroutine(aUnit)
            inlineUnitAppend(newUnit)

    @staticmethod
    # fortStmt: a CommonStmt from a common block in the file being processed with 
    # only active variables in the declList
    # typeDecls: list of all type declarations for variables in the file
    # being processed
    # RETURNS: a new procedure initializing variables for the common block specified by fortStmt or None
    def createInitProcedure(fortStmt,typeDecls):
        '''creates an initialization subroutine for the active variables in a specified common block'''
        if isinstance(fortStmt,fs.CommonStmt):
            # create a new unit for the initializations
            newUnit = Unit()
            newUnit.uinfo = fs.SubroutineStmt('common_'+fortStmt.name+'_init',[])
            # insert oad_active module
            newDecl = fs.UseAllStmt(moduleName='OAD_active',renameList=None,lead='\t')
            newUnit.decls.append(newDecl)
            newStmt = fs.CommonStmt(fortStmt.name,copy.deepcopy(fortStmt.declList),lead='\t')
            newUnit.decls.append(newStmt)
            # insert type declarations for variables which occur in the common statement declList
            for decl in typeDecls:
                if isinstance(decl,fs.DrvdTypeDecl):
                    newDecls = []
                    for var in decl.get_decls():
                        if str(var) in fortStmt.declList:
                            newDecls.append(var)
                            fortStmt.declList.remove(str(var))
                    decl.set_decls(newDecls)
                    if decl.get_mod()[0].lower() == UnitPostProcessor._abstract_type:
                        decl.set_mod([UnitPostProcessor._replacement_type])
                    decl.lead = '\t'
                    if len(decl.get_decls()) > 0:
                        newUnit.decls.append(decl)
                else:
                    for var in decl.get_decls():
                        if str(var) in fortStmt.declList:
                            newUnit.decls.append(decl)
            
            # create a new AssignStmt to initialize the derivative component to 0 for all 
            # arguments in the common block's declList
            for arg in newStmt.declList:
                lhs = fe.Sel(arg,'d')
                rhs = '0'
                newExec = fs.AssignStmt(lhs,rhs,lead='\t')
                newUnit.execs.append(newExec)
            newUnit.end = [fs.EndSubroutineStmt()]
            return newUnit
        return None
    
    # initNames: a list of the names of subroutines which initialize active variables
    # RETURNS: a procedure which calls all other initialization subroutines
    @staticmethod
    def createGlobalInitProcedure(initNames):
        '''a procedure which initializes all active global variables by calling all initialization subroutines which have been created for modules or common blocks'''
        newUnit = Unit()
        newUnit.uinfo = fs.SubroutineStmt('OAD_globalVar_init',[])
        for name in initNames:
            if name[0:3] == 'mod':
                mod_name = name[4:]
                newStmt = fs.UseAllStmt(mod_name,[],lead='\t')
                newUnit.decls.append(newStmt)
                newStmt = fs.CallStmt(name+'_init',[],lead='\t')
            else:
                newStmt = fs.CallStmt('common_'+name+'_init',[],lead='\t')                
            newUnit.execs.append(newStmt)
        newUnit.end = [fs.EndSubroutineStmt()]
        return newUnit

    # RETURNS: a new unit which is a contains block with a subroutine initializing active variables in the module
    def __createModuleInitProcedure(self):
        '''creates a contains block in the module with a new subroutine initializing all active variables within the module'''
        activeTypeDecls = []
        activeTypeDeclsAppend=activeTypeDecls.append
        for decl in self.__myUnit.decls:
            if isinstance(decl,fs.DrvdTypeDecl) and \
                    (decl.get_mod()[0].lower() == self._abstract_type):
                activeTypeDeclsAppend(decl)
        if len(activeTypeDecls) == 0:
            return None

        subUnit = Unit()
        subUnit.uinfo = fs.SubroutineStmt('mod_'+self.__myUnit.uinfo.name+'_init',[])
        # insert oad_active module
        newDecl = fs.UseAllStmt(moduleName='OAD_active',renameList=None,lead='\t')
        subUnit.decls.append(newDecl)

        subUnitExecsAppend=subUnit.execs.append
        for decl in activeTypeDecls:
            for arg in decl.get_decls():
                lhs = fe.Sel(arg,'d')
                rhs = '0'
                newExec = fs.AssignStmt(lhs,rhs,lead='\t')
                subUnitExecsAppend(newExec)
        subUnit.end = [fs.EndSubroutineStmt()]
        if len(self.__myUnit.contains) == 0:
            self.__myUnit.contains.append(fs.ContainsStmt())
        return subUnit

    # find all common block variables to be initialized
    # initNames: contains the names of subroutines which will be called in the global init procedure
    # typeDecls: type declarations for all variables
    def getInitCommonStmts(self,initSet,initNames,typeDecls):
        '''find all common block variables to be initialized and the names of all the initialization procedures which will be created so they can be called later in the global initialization procedure'''
        if isinstance(self.__myUnit.uinfo,fs.ModuleStmt):
            if not self.__myUnit.uinfo.name in initNames:
                for decl in self.__myUnit.decls:
                    # if there is an active variable in the module, add the the name of 
                    # the initialization subroutine which will be created (in processUnit)
                    # to initNames
                    if isinstance(decl,fs.DrvdTypeDecl) and \
                            (decl.get_mod()[0].lower() == self._abstract_type):
                        initNames.append('mod_'+self.__myUnit.uinfo.name)
                        return
        initCommonStmt = None
        for decl in self.__myUnit.decls:
            if isinstance(decl,fs.CommonStmt):
                # create a new common statement with a declList that contains all 
                # active variables from the original common statement 
                initCommonStmt = fs.CommonStmt(decl.name,[])
                for var in decl.declList:
                    # lookup in symtab
                    var_type = self.__myUnit.symtab.lookup_name(var).type
                    if not isinstance(var_type[1][0],fs._Kind) and (var_type[1][0].lower() == self._abstract_type):
                        initCommonStmt.declList.append(var)
                # avoid initializing variables twice
                # don't create subroutines for common blocks with no active variables
                if not (len(initCommonStmt.declList)==0) and not initCommonStmt.name in initNames:
                    initSet.add(initCommonStmt)
                    initNames.append(decl.name)
            if isinstance(decl,fs.TypeDecl):
                typeDecls.add(decl)

    # Processes all statements in the unit
    def processUnit(self):
        ''' post-process a unit '''
        DebugManager.debug(('+'*55)+' Begin post-processing unit <'+str(self.__myUnit.uinfo)+'> '+(55*'+'))
        DebugManager.debug('local '+self.__myUnit.symtab.debug())
        DebugManager.debug('subunits (len = '+str(len(self.__myUnit.ulist))+'):')

        for subUnit in self.__myUnit.ulist:
            DebugManager.debug(str(subUnit))
            UnitPostProcessor(subUnit).processUnit()

        if self._explicitInit and isinstance(self.__myUnit.uinfo,fs.ModuleStmt):
            # create init subroutine & add it inside module
            subUnit = self.__createModuleInitProcedure()
            if subUnit is not None:
                subUnit.parent = self.__myUnit
                self.__myUnit.ulist.append(subUnit)

        if self._mode == 'reverse':
            inline = False
            self.__templateExpansion()
            self.__myUnit.decls = self.__myNewDecls
            self.__myUnit.execs = self.__myNewExecs
        else:
            (Decls,Execs) = self.__forwardProcessDeclsAndExecs()
            self.__myUnit.decls = Decls
            self.__myUnit.execs = Execs

        # write all declarations to _activeVariablesFileName
        if UnitPostProcessor._activeVariablesFileName :
            if not (isinstance(self.__myUnit.uinfo,fs.FunctionStmt) or
                    isinstance(self.__myUnit.uinfo,fs.SubroutineStmt)) :
                currentOutputFormat = flow.outputFormat
                (base,activeOutputFormat) = os.path.splitext(UnitPostProcessor._activeVariablesFileName)
                # set active variables file output format
                flow.setOutputFormat(Ffile.get_format(activeOutputFormat))
                self.__active_file = open(UnitPostProcessor._activeVariablesFileName,'a')
                self.__myUnit.printDecls(self.__active_file)
                self.__active_file.close()
                # restore original output format
                flow.setOutputFormat(currentOutputFormat)

        if (self.__recursionDepth is not 0):
            raise PostProcessError('Recursion error in unitPostProcess: final recursion depth is not zero')
        DebugManager.debug(('+'*54)+' End post-process unit <'+str(self.__myUnit.uinfo)+'> '+(54*'+')+'\n\n')

        return self.__myUnit
