from _Setup import *

from PyUtil.symtab import Symtab,SymtabEntry,SymtabError
from PyFort.typeInference import TypeInferenceError,expressionType,functionType,isArrayReference
import PyFort.fortExp as fe
import PyFort.fortStmts as fs
import PyFort.intrinsic as intrinsic
from PyFort.fortUnit import fortUnitIterator
import re, string
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

    _transform_deriv = False

    @staticmethod
    def setDerivType(transformDerivType):
        UnitPostProcessor._transform_deriv = transformDerivType

    _input_file = 'ad_inline.f'

    @staticmethod
    def setInputFile(inputFile):
        UnitPostProcessor._input_file = inputFile

    _replacement_type = 'active'

    @staticmethod
    def setReplacementType(replacementType):
        UnitPostProcessor._replacement_type = replacementType

    _mode = 'forward'

    @staticmethod
    def setMode(mode):
        UnitPostProcessor._mode = mode

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
        # list of tuples of (ordered function args, fs.AssignStmt)
        self.__inlineFunctionTuples = []

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
            newDecls.append(self.__transformActiveTypesExpression(decl))
        DrvdTypeDecl.decls = newDecls
        if DrvdTypeDecl.mod[0].lower() in set(['(openadty_active)',
                                               '(openad_type)']):
            DrvdTypeDecl.mod = ['('+self._replacement_type+')']
            DrvdTypeDecl.dblc = True
        return DrvdTypeDecl

    # Transforms active types for an expression recursively
    # (replaces instances of __value__ and __deriv__)
    def __transformActiveTypesExpression(self,theExpression):
        'mutate __value__ and __deriv__ calls'
        # deepcopy allows for comparison of return value and input value in calling function
        if self.__recursionDepth is 0:
            replacementExpression = copy.deepcopy(theExpression)
        else:
            replacementExpression = theExpression

        if self._verbose: 
            print self.__recursionDepth*'|\t'+'unitPostProcessor.__transformActiveTypesExpression called on"'+str(theExpression)+'"'
        self.__recursionDepth += 1
        if isinstance(replacementExpression, fe.App):
            if intrinsic.is_inquiry(replacementExpression.head):
                self.__inquiryExpression = True
                self.__inquiryRecursionLevel = self.__recursionDepth
            replacementExpression.args = map(self.__transformActiveTypesExpression,replacementExpression.args)
            if replacementExpression.head == '__value__':
                if self.__inquiryExpression:
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


    # transforms active types in a SubCallStmt
    def __processSubCallStmt(self,aSubCallStmt):
        if self._verbose: print 'unitPostProcessor.__processSubCallStmt called on: "'+str(aSubCallStmt)+"'"
        replacementArgs = []
        for anArg in aSubCallStmt.args:
            if isinstance(anArg,fe.App):
                newArg = self.__transformActiveTypesExpression(anArg)
                replacementArgs.append(newArg)
            else:
                replacementArgs.append(anArg)
        replacementStatement = \
            fs.CallStmt(aSubCallStmt.head,
                        replacementArgs,
                        aSubCallStmt.stmt_name,
                        lineNumber=aSubCallStmt.lineNumber,
                        label=aSubCallStmt.label,
                        lead=aSubCallStmt.lead).flow()
        return replacementStatement

    # Does active type transformations on a StmtFnStmt; reconstructs it as an AssignStmt if StmtFnStmt.name is "__value__"
    def __processStmtFnStmt(self, StmtFnStmt):
        if self._verbose: print 'unitPostProcessor.__processStmtFnStmt called on: "'+str(StmtFnStmt)+"'"

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
        else:
            replacementStatement = newStatement
        return replacementStatement


    def __transformActiveTypes(self,aStmt):
        '''Transforms active types on general executable statements'''
        if self._verbose: print 'unitPostProcessor.__transformActiveTypes called on: "'+str(aStmt)+"'"

        if not hasattr(aStmt,"_sons") or (aStmt._sons == []):
            return aStmt
        
        self.__expChanged=False
        for aSon in aStmt._sons:
            theSon = getattr(aStmt,aSon)
            newSon = self.__transformActiveTypesExpression(theSon)    
            if newSon is not theSon:
                diff = True
                setattr(aStmt,aSon,newSon)
        # if statement is unchanged, leave it alone
        if self.__expChanged is True:
            aStmt.flow()
        return aStmt

    def __getInlinedFunction(self,aComment,definitions):
        funMatch = None
        Comment = aComment
        match = re.search('C[ ]+[$]openad[$][ ]+inline',aComment.rawline,re.IGNORECASE)
        if match:
            p = re.compile(r'\(')
            # get name of inlined function
            inlineFunction = p.split(aComment.rawline[match.end():])[0].lstrip()
            pattern = 'subroutine([ ]+)'+inlineFunction
            pattern2 = 'end([ ]+)subroutine'
            pat1 = re.compile(pattern,re.IGNORECASE)
            pmatch = pat1.search(definitions)
            if pmatch:
                pat2 = re.compile(pattern2,re.IGNORECASE)
                p2match = pat2.search(definitions,pmatch.end())
                if pmatch and p2match:
                    funMatch = definitions[pmatch.start():p2match.end()]
            Comment = fs.Comments(aComment.rawline[:match.start(0)])
        return (Comment,funMatch)

    # returns a list of tuples of (ordered_args,stmt) for processing 
    # when appropriate arguments are known
    def __getNewExecInfo(self,function):
        newStmtInfo = []
        argpattern = '\(.*\)'
        argpat = re.compile(argpattern,re.IGNORECASE)
        args = argpat.search(function)
        if not args:
            return newStmtInfo

        pattern = 'C([ ]+)[$]openad[$]([ ]+)end([ ]+)decls'
        pat = re.compile(pattern,re.IGNORECASE)
        match = pat.search(function)
        if match:
            pat2 = re.compile(r'[ ]+end[ ]+subroutine',re.IGNORECASE)
            endmatch = pat2.search(function)
            newFuncCode = function[match.end():endmatch.start()]
            p = re.compile(r'\n')
            execStmts = p.split(newFuncCode)
            for aStmt in execStmts:
                aStmt.lstrip().rstrip()
                if not aStmt == '':
                    p = re.compile(r'=')
                    splitStmt = p.split(aStmt.lstrip())
                    if len(splitStmt) == 2:
                        newStmt = fs.AssignStmt(splitStmt[0],splitStmt[1])
                        p2 = re.compile(r'[\(|\)|,]')
                        funArgs = filter(lambda i: i != '',
                                         (p2.split(args.group(0))))
                        stmtInfo = (funArgs,newStmt)
                        newStmtInfo.append(stmtInfo)
        return newStmtInfo

    # for each tuple (args,stmt) in self.__inlineFunctionTuples,
    # create a new exec stmt by replacing args in the stmt with execStmtArgs
    def __createNewExecs(self,execStmtArgs,lead):
        replacementArgs = []
        Execs = []
        for anArg in execStmtArgs:
            if isinstance(anArg,fe.App):
                newArg = self.__transformActiveTypesExpression(anArg)
                replacementArgs.append(newArg)
            else:
                replacementArgs.append(anArg)

        for (inlineArgs,AssignStmt) in self.__inlineFunctionTuples:
            argReps = len(inlineArgs)
            if argReps != len(replacementArgs):
                return
            argReps -= 1
            lhs = AssignStmt.lhs
            rhs = AssignStmt.rhs
            while argReps >= 0:
                p = re.compile(inlineArgs[argReps])
                lhs = p.sub(str(replacementArgs[argReps]),lhs)
                rhs = p.sub(str(replacementArgs[argReps]),rhs)
                argReps -= 1
            newStmt = fs.AssignStmt(lhs,rhs,lead=lead).flow()
            Execs.append(newStmt)
        return Execs


    def __templateExpansion(self,definitions,inline):
        if isinstance(self.__myUnit.uinfo,fs.ModuleStmt) \
                or isinstance(self.__myUnit.uinfo,fs.FunctionStmt):
            self.__processDeclsAndExecs(self.__myNewDecls,self.__myNewExecs,definitions,inline)
            return
        (Decls,Execs) = self.__processDeclsAndExecs([],[],definitions,inline)
#        for aDecl in Decls:
#            print "processed decl:",aDecl.rawline
#        for anExec in Execs:
#            print "processed exec:",anExec.rawline
        inputDeclNum = 0
        inputExecNum = 0
        for aUnit in fortUnitIterator("ad_template.f",False):
            self.__myUnit.cmnt.rawline = aUnit.cmnt.rawline+self.__myUnit.cmnt.rawline
            if isinstance(aUnit.uinfo,fs.SubroutineStmt):
                aUnit.uinfo.name = self.__myUnit.uinfo.name
            replacementNum = 0
            for aDecl in aUnit.decls:
                if aDecl.is_comment():
                    pat = re.compile(
                        "[ ]*[!][ ]*[$]template[_]pragma[_]declarations",
                        re.IGNORECASE)
                    match = pat.search(aDecl.rawline)
                    if match:
                        newStmt = fs.Comments(aDecl.rawline[:match.start()])
                        self.__myNewDecls.append(newStmt)
                        # return to input
                        print "original input decls"
                        (inputDeclNum,replacementNum) = self.__inputFileDecls(Decls,inputDeclNum)
                        print "replacementNum:",replacementNum
                        print "return to template"
                        # continue template
                        newStmt = fs.Comments(aDecl.rawline[match.end():])
                        self.__myNewDecls.append(newStmt)
                        continue
                self.__myNewDecls.append(aDecl)
            self.__inputFileDecls(Decls,inputDeclNum,(replacementNum != 0),True)
            execRepNum = 0
            if replacementNum == 0:
                (inputExecNum,execRepNum) = \
                    self.__inputFileExecs(Execs,inputExecNum,definitions,
                                          replacementNum,(execRepNum != 0))
                replacementNum = execRepNum
            for anExecStmt in aUnit.execs:
                if anExecStmt.is_comment():
                    pat = re.compile(
                        "[!][ ]*[$]placeholder[_]pragma[$][ ]+id[=]",
                        re.IGNORECASE)
                    match = pat.search(anExecStmt.rawline)
                    if match:
                        newStmt = fs.Comments(anExecStmt.rawline[:match.start()])
                        self.__myNewExecs.append(newStmt)
                        if anExecStmt.rawline[match.end()].rstrip() == replacementNum:
                            # return to input
                            print "original input execs"
                            (inputExecNum,execRepNum) = \
                                self.__inputFileExecs(Execs,inputExecNum,definitions,
                                                      replacementNum,(execRepNum != 0))
                            replacementNum = execRepNum
                            print "replacement number:",replacementNum
                            #continue template
                            print "return to template"
                        pat = re.compile("[0-9]+")
                        newmatch = pat.search(anExecStmt.rawline[match.end():])
                        newStmt = fs.Comments(anExecStmt.rawline[match.end()+newmatch.end():])
                        self.__myNewExecs.append(newStmt)
                        continue
                self.__myNewExecs.append(anExecStmt)
            for endStmt in aUnit.end:
                newEndStmts = []
                if isinstance(endStmt,fs.EndStmt):
                    print endStmt.rawline
                    match = re.search("template",endStmt.rawline,re.IGNORECASE)
                    if match:
                        endStmt.rawline = \
                            endStmt.rawline[:match.start(0)] + \
                            self.__myUnit.uinfo.name + \
                            endStmt.rawline[match.end(0):]
                    newEndStmts.append(endStmt)
            self.__myUnit.end = newEndStmts


    def __inputFileExecs(self,Execs,inputExecNum,definitions,replacementNum,writeComment):
        execNum = inputExecNum
        replacement_num = 0
        inline = False
        for anExec in Execs[inputExecNum:]:
            if anExec.is_comment():
                (replacement_num,inline,writeComment) = \
                    self.__processInputComment(anExec,"exec",writeComment,
                                               definitions,inline,replacementNum)
                if replacement_num != 0:
                    return (execNum,replacement_num)
            else:
                self.__myNewExecs.append(anExec)
            execNum += 1
        return (execNum,replacement_num)
                
    def __processInputComment(self,Comment,Type,writeComment,definitions=None,inline=False,UseStmtSeen=False,commentToExecs=False,replacementNum=0):
        Stmt = Comment
        replacement_num = 0
        while Stmt:
            end_pat = \
                re.compile("[ ]*C[ ]*[$]openad[$][ ]+end[ ]+replacement[ ]*",
                                   re.IGNORECASE)
            end_match = end_pat.search(Stmt.rawline)
            begin_pat = \
                re.compile("C[ ]*[$]openad[$][ ]+begin[ ]+replacement",
                           re.IGNORECASE)
            begin_match = begin_pat.search(Stmt.rawline)
            if end_match:
                if begin_match and begin_match.start() < end_match.start():
                    pass
                else:
                    if end_match.start() != 0:
                        inputStmt =  fs.Comments(Stmt.rawline[:end_match.start()])
                        if Type == "exec":
                            self.__myNewExecs.append(inputStmt)
                        elif Type == "decl":
                            self.__myNewDecls.append(inputStmt)
                    if end_match.end() == len(Stmt.rawline): break
                    if not begin_match:
                        Stmt = fs.Comments(Stmt.rawline[end_match.end():])
            if begin_match:
                match = re.search("[0-9]+",Stmt.rawline[begin_match.end(0):])
                if not writeComment:
                    if begin_match.start(0) != 0:
                        if end_match:
                            if end_match.start(0) == 0 \
                                    and end_match.end(0)+1 == begin_match.start(0):
                                pass
                        else:
                            inputStmt = \
                                fs.Comments(Stmt.rawline[:begin_match.start()])
                            if Type == "exec":
                                self.__myNewExecs.append(inputStmt)
                            elif Type == "decl":
                                self.__myNewDecls.append(inputStmt)
                    if match:
                        replacement_num = match.group(0).rstrip()

                        return (replacement_num,inline,writeComment)
                else:
                    if begin_match.end(0)+match.end(0) == len(Stmt.rawline): break
                    Stmt = fs.Comments(Stmt.rawline[begin_match.end()+match.end():])
                    writeComment = False
            else:
                if Type == "exec":
                    self.__myNewExecs.append(Stmt)
                elif Type == "decl":
                    self.__myNewDecls.append(Stmt)
                break
        if Type == "exec":
            return (replacement_num,inline,writeComment)
        elif Type == "decl":
            return (replacement_num,UseStmtSeen,writeComment)


    def __inputFileDecls(self,Decls,inputDeclNum,writeComment=False,commentToExecs=False):
        UseStmtSeen = False
        declNum = inputDeclNum
        replacement_num = 0
        for aDecl in Decls[inputDeclNum:]:
            if aDecl.is_comment():
                (replacement_num,UseStmtSeen,writeComment) = \
                    self.__processInputComment(aDecl,"decl",writeComment,UseStmtSeen=UseStmtSeen,commentToExecs=commentToExecs)
                if replacement_num != 0:
                    return (declNum,replacement_num)
                declNum += 1
                continue
            self.__myNewDecls.append(aDecl)
            declNum += 1
        return (declNum,replacement_num)

    def __processExec(self,anExecStmt,definitions=None,inline=False): 
        try:
            if self._verbose: 
                print '[Line '+str(anExecStmt.lineNumber)+']:'
            newStmt = None
            Execs = []
            if anExecStmt.is_comment() and definitions is not None:
                (Comment,function) = self.__getInlinedFunction(anExecStmt,definitions)
                Execs.append(Comment)
                if function is not None:
                    self.__inlineFunctionTuples = self.__getNewExecInfo(function)
                    inline = True
            elif isinstance(anExecStmt,fs.CallStmt):
                if inline is True:
                    Execs = self.__createNewExecs(anExecStmt.args,anExecStmt.lead)
                    inline = False
                else:
                    newStmt = self.__processSubCallStmt(anExecStmt)
                    Execs.append(newStmt)
            else:
                newStmt = self.__transformActiveTypes(anExecStmt)
                Execs.append(newStmt)
            return (Execs,inline)

        except TypeInferenceError,e:
            raise PostProcessError('Caught TypeInferenceError: '+e.msg,anExecStmt.lineNumber)
        except SymtabError,e:
            raise PostProcessError('Caught SymtabError: '+e.msg,anExecStmt.lineNumber)
       

    def __processDecl(self,aDecl,Decls,UseStmtSeen):
        try:
            if self._verbose: 
                print '[Line '+str(aDecl.lineNumber)+']:'
            if isinstance(aDecl, fs.UseStmt):
                # add old use stmt
                if UseStmtSeen: # don't add the active module
                    return (aDecl,UseStmtSeen)
                else: # add the active module
                    Decls.append(aDecl) 
                    newDecl = self.__addActiveModule(aDecl)
                    # build rawlines for the new declarations
                    newDecl.lead = self.__myUnit.uinfo.lead+''
                    newDecl.flow()
                    UseStmtSeen = True
            elif isinstance(aDecl,fs.DrvdTypeDecl):
                newDecl = self.__rewriteActiveType(aDecl)
                newDecl.flow()
                UseStmtSeen = False
            elif isinstance(aDecl,fs.StmtFnStmt):
                newDecl = self.__processStmtFnStmt(aDecl)
                newDecl.flow()
                UseStmtSeen = False
            else:
                if self._verbose: print 'Statement "'+str(aDecl)+'" is assumed to require no post-processing'
                newDecl = aDecl
                UseStmtSeen = False
            return (newDecl,UseStmtSeen)

        except TypeInferenceError,e:
            raise PostProcessError('Caught TypeInferenceError: '+e.msg,aDecl.lineNumber)
        except SymtabError,e:
            raise PostProcessError('Caught SymtabError: '+e.msg,aDecl.lineNumber)

    # process all decls and execs, adding all decls which are transformed 
    # to assign stmts by post processing to new execs
    def __processDeclsAndExecs(self,Decls,Execs,definitions=None,inline=False):
        UseStmtSeen = False
        execNum = 0
        for aDecl in self.__myUnit.decls:
            (newDecl,UseStmtSeen) = self.__processDecl(aDecl,Decls,UseStmtSeen)
            if isinstance(newDecl,fs.AssignStmt):
                for anExec in self.__myUnit.execs[execNum:]:
                    if anExec.lineNumber < newDecl.lineNumber:
                        (newExecs,inline) = self.__processExec(anExec,definitions,inline)
                        for newExec in newExecs:
                            if newExec is not None:
                                Execs.append(newExec)
                        execNum += 1
                    else:
                        Execs.append(newDecl)
                        break
            else:
                Decls.append(newDecl)
        for anExec in self.__myUnit.execs[execNum:]:
            (newExecs,inline) = self.__processExec(anExec,definitions,inline)
            for newExec in newExecs:
                if newExec is not None:
                    Execs.append(newExec)
        return (Decls,Execs)

    # Processes all statements in the unit
    def processUnit(self):
        ''' post-process a unit '''
        if self._verbose: print ('+'*55)+' Begin post-processing unit <',self.__myUnit.uinfo,'> '+(55*'+'),'\nlocal',self.__myUnit.symtab.debug()
        if (self._verbose and self.__myUnit.ulist): print 'subunits (len =',len(self.__myUnit.ulist),'):'

        for subUnit in self.__myUnit.ulist:
            if self._verbose: print subUnit
            UnitPostProcessor(subUnit).processUnit()

        if self._mode == 'reverse':
            try:
                fd = open(self._input_file)
                definitions = fd.read()
            except IOError,e: 
                raise PostProcessError('Caught IOError: '+e.msg,self._input_file)
            inline = False
            self.__templateExpansion(definitions,inline)
            try:
                fd.close()
            except IOError,e:
                raise PostProcessError('Caught IOError '+e.msg)

        else:
            self.__processDeclsAndExecs(self.__myNewDecls,self.__myNewExecs)

        self.__myUnit.decls = self.__myNewDecls
        self.__myUnit.execs = self.__myNewExecs

        if (self.__recursionDepth is not 0):
            raise PostProcessError('Recursion errorin unitPostProcess: final recursion depth is not zero')

        if self._verbose: print ('+'*54)+' End post-process unit <',self.__myUnit.uinfo,'> '+(54*'+')+'\n\n'

        return self.__myUnit




