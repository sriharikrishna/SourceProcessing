from _Setup import *

from PyUtil.debugManager import DebugManager
from PyUtil.symtab import Symtab,SymtabEntry,SymtabError

from PyFort.typeInference import TypeInferenceError,expressionType,functionType,isArrayReference
import PyFort.fortExp as fe
import PyFort.fortStmts as fs
import PyFort.intrinsic as intrinsic
from PyFort.fortUnit import fortUnitIterator
from PyFort.fortParse import parse_stmt,parse_cmnt
import re, string
import copy

class PostProcessError(Exception):
    '''Exception for errors that occur during postprocessing'''
    def __init__(self,msg,lineNumber):
        self.msg = msg
        self.lineNumber = lineNumber

class UnitPostProcessor(object):
    'class to facilitate post-processing on a per-unit basis'

    _transform_deriv = False

    @staticmethod
    def setDerivType(transformDerivType):
        UnitPostProcessor._transform_deriv = transformDerivType

    _inlineFile = 'ad_inline.f'

    @staticmethod
    def setInlineFile(inlineFile):
        UnitPostProcessor._inlineFile = inlineFile

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
        self.__active_file = None
        self.__write_subroutine=True

    # adds the active module OAD_active
    def __addActiveModule(self,arg):
        DebugManager.debug('unitPostProcessor.__addActiveModule called on: "'+str(arg)+"'")
        new_stmt = fs.UseStmt('OAD_active')
        return new_stmt

    # Rewrites the active type in derived type declarations
    # returns the declaration
    def __rewriteActiveType(self, DrvdTypeDecl):
        ''' convert type (OpenAD_type) to type(active)
        only applied to type declaration statements '''
        DebugManager.debug('unitPostProcessor.__rewriteActiveType called on: "'+str(DrvdTypeDecl)+"'")
        newDecls = []
        for decl in DrvdTypeDecl.decls:
            newDecls.append(self.__transformActiveTypesExpression(decl))
        DrvdTypeDecl.decls = newDecls
        if DrvdTypeDecl.mod[0].lower() in set(['(openadty_active)',
                                               '(openad_type)']):
            DrvdTypeDecl.mod = ['('+self._replacement_type+')']
            DrvdTypeDecl.dblc = True
        else:
            DrvdTypeDecl.dblc = False
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


    # transforms active types in a SubCallStmt
    def __processSubCallStmt(self,aSubCallStmt):
        DebugManager.debug('unitPostProcessor.__processSubCallStmt called on: "'+str(aSubCallStmt)+"'")
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
        else:
            replacementStatement = newStatement
        return replacementStatement


    def __transformActiveTypes(self,aStmt):
        '''Transforms active types on general executable statements'''
        DebugManager.debug('unitPostProcessor.__transformActiveTypes called on: "'+str(aStmt)+"'")

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
        units = None
        match = re.search('C[ ]+[$]openad[$][ ]+inline',aComment.rawline,re.IGNORECASE)
        if match:
            p = re.compile(r'\(')
            # get name of inlined function
            inlineFunction = p.split(aComment.rawline[match.end():])[0].lstrip()
            pattern = 'subroutine([ ]+)'+inlineFunction+"[\(]"
            pattern2 = 'end[ ]+subroutine'
            pat1 = re.compile(pattern,re.IGNORECASE)
            pmatch = pat1.search(definitions)
            if pmatch:
                pat2 = re.compile(pattern2,re.IGNORECASE)
                p2match = pat2.search(definitions,pmatch.end())
                if pmatch and p2match:
                    funMatch = definitions[pmatch.start():p2match.end()]
                aComment = None
            else:
                aComment = \
                    fs.Comments("C!! requested inline of '"+inlineFunction+"' has no defn\n")
        return (aComment,funMatch)

    def __getReplacementNum(self,aComment):
        begin_match = \
            re.search('C[ ]+[$]openad[$][ ]+begin[ ]+replacement[ ]+',
                      aComment.rawline,re.IGNORECASE)
        if begin_match:
            num_match = re.search('[0-9]+',aComment.rawline[begin_match.end(0):])
            if num_match:
                replacementNum = num_match.group(0)
                return int(replacementNum)
        else:
            return 0

    def __endReplacement(self,aComment):
        end_match = \
            re.search('C[ ]+[$]openad[$][ ]+end[ ]+replacement',
                      aComment.rawline,re.IGNORECASE)
        if end_match:
            return True
        else:
            return False

    def __parseNewStmts(self,newFuncCode,funArgs):
        newStmtInfo = []
        if newFuncCode.strip() != '':
            p = re.compile(r'[\n]|[;]')
            execStmts = p.split(newFuncCode)            
            i = 0
            while i < len(execStmts):
                stmt = execStmts[i]
                n = 1
                while i+n < len(execStmts):
                    match = re.search("[ ]+[+][\w]",execStmts[i+n])
                    if match:
                        stmt = stmt+(execStmts[i+n])[match.end()-1:]
                        n += 1
                    else:
                        break
                if stmt.strip() is '':
                    i += n
                    continue
                parsed_stmt = (parse_stmt(stmt,0)).flow()
                ws = re.search("[ ]*",stmt)
                parsed_stmt.lead = ws.group(0)
                stmtInfo = (funArgs,parsed_stmt)
                newStmtInfo.append(stmtInfo)
                i += n
        return newStmtInfo

    # returns a list of tuples of (ordered_args,stmt) for processing 
    # when appropriate arguments are known
    def __getNewExecInfo(self,function):
        newStmtInfo = []
        argpattern = '\((.*([\n][ ]*[+])*)+\)[ ]*[\n]'
        argpat = re.compile(argpattern,re.IGNORECASE)
        args = argpat.search(function)
        if not args:
            return newStmtInfo
        pattern = 'C([ ]+)[$]openad[$]([ ]+)end([ ]+)decls'
        match = re.search(pattern,function,re.IGNORECASE)
        if match:
            pat2 = re.compile('[ ]+end[ ]+subroutine',re.IGNORECASE)
            endmatch = pat2.search(function)
            newFuncCode = function[match.end():endmatch.start()]
            p2 = re.compile(r'[\(|\)|,|[\n]|[ ]*[+]]')
            funArgs = filter(lambda i: i != '',
                             (p2.split(args.group(0))))
            i = 0
            while i < len(funArgs):
                arg = funArgs[i]
                linebreak = re.search("[ ]*[+]",arg)
                if linebreak:
                    funArgs[i] = arg[linebreak.end():]
                i += 1
            comments = re.finditer("[ ]*C[ ]+.*[\n]*",newFuncCode,re.IGNORECASE)
            index = 0
            for aComment in comments:
                end = aComment.start()
                if aComment.start() == index:
                    index = aComment.end()
                else:
                    newStmtInfo.extend(self.__parseNewStmts(newFuncCode[index:end],funArgs))
                comment = fs.Comments(aComment.group())
                stmtInfo = (funArgs,comment)
                newStmtInfo.append(stmtInfo)
                index = aComment.end()
            newStmtInfo.extend(self.__parseNewStmts(newFuncCode[index:],funArgs))
        return newStmtInfo

    def __replaceArgs(self,argReps,string,inlineArgs,replacementArgs):
        while argReps >= 0:
            lno_replace = "[\w]"+inlineArgs[argReps]
            rno_replace = inlineArgs[argReps]+"[\w]"
            p = re.compile("("+rno_replace+"|"+lno_replace+")")
            p2 = re.compile(inlineArgs[argReps])
            pats = p.finditer(string,re.IGNORECASE)
            prevEnd = 0; stopRep=len(string)
            for match in pats:
                (stopRep,end) = match.span()
                m2 = p.search(string[prevEnd:stopRep])
                if m2:
                    prevEnd = m2.end()
                newstr = p2.sub(str(replacementArgs[argReps]),string[prevEnd:stopRep]) 
                string = string[:prevEnd]+\
                    newstr+\
                    string[(stopRep):]
                prevEnd = end
                stopRep = len(string)
            match = p.search(string[prevEnd:])
            if match:
                string = string[:match.end()]+\
                    p2.sub(str(replacementArgs[argReps]),string[match.end():])
            else:
                string = string[:prevEnd]+\
                    p2.sub(str(replacementArgs[argReps]),string[prevEnd:])
            argReps -= 1
        return string

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
        for (inlineArgs,Stmt) in self.__inlineFunctionTuples:
            argReps = min(len(inlineArgs),len(replacementArgs))
            argReps -= 1
            if isinstance(Stmt,fs.Comments):
                Stmt.rawline = \
                    self.__replaceArgs(argReps,Stmt.rawline,inlineArgs,replacementArgs)
                Execs.append(Stmt)
            elif isinstance(Stmt,fs.AssignStmt):
                if len(Stmt.lead) > len(lead):
                    lead = Stmt.lead
                lhs = self.__replaceArgs(argReps,str(Stmt.lhs),inlineArgs,replacementArgs)
                rhs = self.__replaceArgs(argReps,str(Stmt.rhs),inlineArgs,replacementArgs)
                newStmt = fs.AssignStmt(lhs,rhs,lead=lead).flow()
                ws = re.search("[ ]*",Stmt.rawline)
                Execs.append(newStmt)
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
                Stmt.flow()
                Execs.append(Stmt)
        return Execs

    def __insertSubroutineName(self,Unit,anExecStmt):
        match = re.search('__SRNAME__',anExecStmt.rawline,re.IGNORECASE)
        if match:
            anExecStmt.rawline = \
                anExecStmt.rawline[:match.start()]+ \
                Unit.uinfo.name +\
                anExecStmt.rawline[match.end():]
        return anExecStmt

    def __expandTemplate(self,template,Decls,Execs,template_comment=True):
        inputDeclNum = 0
        inputExecNum = 0
        pragma = 0
        for aUnit in fortUnitIterator(template,False):
            if template_comment:
                if aUnit.cmnt is not None:
                    if self.__myUnit.cmnt is not None:
                        self.__myUnit.cmnt.rawline = \
                            aUnit.cmnt.rawline+self.__myUnit.cmnt.rawline
                    else:
                        self.__myUnit.cmnt = aUnit.cmnt
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
                        if len(Decls) > 0:
                            for decl in Decls[0]:
                                if decl is not None:
                                    self.__myNewDecls.append(decl)
                        # continue template
                        newStmt = fs.Comments(aDecl.rawline[match.end():])
                        self.__myNewDecls.append(newStmt)
                        continue
                self.__myNewDecls.append(aDecl)
            i = 1; j = 0
            while i < len(Decls):
                for aDecl in Decls[i]:
                    if aDecl is not None:
                        self.__myNewDecls.append(aDecl)
                    j += 1
                j = 0
                i += 1
            if len(Execs) > 0:
                for anInputExec in Execs[0]:
                    if anInputExec is not None:
                        self.__myNewExecs.append(anInputExec)
                    j += 1
            execRepNum = 0
            firstIter = True
            for anExecStmt in aUnit.execs:
                if anExecStmt.is_comment():
                    pat = re.compile(
                        "[!][ ]*[$]placeholder[_]pragma[$][ ]+id[=]",
                        re.IGNORECASE)
                    match = pat.search(anExecStmt.rawline)
                    if match:
                        newStmt = fs.Comments(anExecStmt.rawline[:match.start()])
                        self.__myNewExecs.append(newStmt)
                        endline = re.search('[\n]',anExecStmt.rawline[match.end():])
                        if endline:
                            end = match.end()+endline.start()
                            pragma = int(anExecStmt.rawline[match.end():end].strip())
                        else:
                            pragma = int(anExecStmt.rawline[match.end()].strip())
                        if pragma < len(Execs):
                            # return to input
                            for anInputExec in Execs[pragma]:
                                if anInputExec is not None:
                                    self.__myNewExecs.append(anInputExec)
                        # continue template
                        pat = re.compile("[0-9]+")
                        newmatch = pat.search(anExecStmt.rawline[match.end():])
                        newStmt = fs.Comments(anExecStmt.rawline[match.end()+newmatch.end():])
                        self.__myNewExecs.append(newStmt)
                        continue
                anExecStmt = self.__insertSubroutineName(aUnit,anExecStmt)
                self.__myNewExecs.append(anExecStmt)
            for endStmt in aUnit.end:
                newEndStmts = []
                if isinstance(endStmt,fs.EndStmt):
                    match = re.search("template",endStmt.rawline,re.IGNORECASE)
                    if match:
                        endStmt.rawline = \
                            endStmt.rawline[:match.start(0)] + \
                            self.__myUnit.uinfo.name + \
                            endStmt.rawline[match.end(0):]
                    newEndStmts.append(endStmt)
            self.__myUnit.end = newEndStmts    
        return (None,[],[])

    def __templateExpansion(self,definitions,inline):
        (Decls,Execs) = self.__reverseProcessDeclsAndExecs(definitions,inline)
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
        template = self.__getTemplateName()
        insert_template_comment = True
        while template is not None:
            (template,Decls,Execs) = \
                self.__expandTemplate(template,Decls,Execs,insert_template_comment)
            insert_template_comment = False

    # gets the name of the first template used
    def __getTemplateName(self):
        if self.__myUnit.cmnt is not None:
            template = self.__getTemplate(self.__myUnit.cmnt)
            if template is not None:
                return template
        for aDecl in self.__myUnit.decls:
                if aDecl.is_comment():
                    template = self.__getTemplate(aDecl)
                if template is not None:
                    return template
        for anExec in self.__myUnit.execs:
            if anExec.is_comment():
                template = self.__getTemplate(anExec)
                if template is not None:
                    return template
        return template

    # extracts the template name from a comment
    def __getTemplate(self,comment):
        name = None
        p = re.compile('c[ ]*[$]openad[ ]+xxx[ ]+template[ ]+',re.IGNORECASE)
        match = p.search(comment.rawline)
        if match:
            end_name = re.search('[\n]|[ ]',comment.rawline[match.end():])
            if end_name:
                name = (comment.rawline[match.end():(match.end()+end_name.start())]).strip()
            else:
                name = (comment.rawline[match.end():]).strip()
        return name

    def __processComments(self,Comments,replacementNum,commentList,
                          currentComments,definitions=None,inline=False):
        for aComment in Comments:
            if aComment == '' or aComment.strip() == '':
                continue
            newComment = fs.Comments(aComment+"\n")
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
                (Comment,function) = self.__getInlinedFunction(newComment,definitions)
                if Comment is not None:
                    currentComments.append(Comment)
                if function is not None:
                    self.__inlineFunctionTuples = self.__getNewExecInfo(function)
                    inline = True
        return (commentList,currentComments,inline,replacementNum)
                    
    def __processExec(self,anExecStmt,Execs,execList=[],
                      definitions=None,inline=False,replacementNum=0): 
        try:
            DebugManager.debug('[Line '+str(anExecStmt.lineNumber)+']:')
            newStmt = None
            if anExecStmt.is_comment() and definitions is not None:
                if self._mode == 'reverse':
                    p = re.compile(r'[\n]')
                    comments = p.split(anExecStmt.rawline)
                    (execList,Execs,inline,replacementNum) = \
                        self.__processComments(comments,replacementNum,
                                               execList,Execs,definitions,inline)
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
            else:
                newStmt = self.__transformActiveTypes(anExecStmt)
                Execs.append(newStmt)
            if self._mode == 'reverse':
                return (execList,Execs,inline,replacementNum)
            else:
                return Execs

        except TypeInferenceError,e:
            raise PostProcessError('Caught TypeInferenceError: '+e.msg,anExecStmt.lineNumber)
        except SymtabError,e:
            raise PostProcessError('Caught SymtabError: '+e.msg,anExecStmt.lineNumber)
       

    def __processDecl(self,aDecl,Decls,Execs,UseStmtSeen,declList=[],
                      execList=[],replacementNum=0):
        try:
            DebugManager.debug('[Line '+str(aDecl.lineNumber)+']:')
            if isinstance(aDecl, fs.UseStmt):
                # add old use stmt
                Decls.append(aDecl)
                if not UseStmtSeen: # add the active module
                    newDecl = self.__addActiveModule(aDecl)
                    # build rawlines for the new declarations
                    newDecl.lead = self.__myUnit.uinfo.lead+''
                    newDecl.flow()
                    UseStmtSeen = True
                    Decls.append(newDecl)
            elif aDecl.is_comment():
                if self._mode == 'reverse':
                    p = re.compile(r'[\n]')
                    comments = p.split(aDecl.rawline)
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
                newDecl.flow()
                if newDecl.dblc is True:
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

                Decls.append(aDecl)
                UseStmtSeen = False
            if self._mode == 'reverse':
                return (declList,Decls,execList,Execs,UseStmtSeen,replacementNum)
            else:
                return (Decls,Execs,UseStmtSeen)

        except TypeInferenceError,e:
            raise PostProcessError('Caught TypeInferenceError: '+e.msg,aDecl.lineNumber)
        except SymtabError,e:
            raise PostProcessError('Caught SymtabError: '+e.msg,aDecl.lineNumber)


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
    def __reverseProcessDeclsAndExecs(self,definitions=None,inline=False):
        UseStmtSeen = False
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
                                   definitions,inline,replacementNum)
        if len(currentExecs) != 0:
            Execs.append(currentExecs)
        return (Decls,Execs)

    # Processes all statements in the unit
    def processUnit(self):
        ''' post-process a unit '''
        DebugManager.debug(('+'*55)+' Begin post-processing unit <'+str(self.__myUnit.uinfo)+'> '+(55*'+'))
        DebugManager.debug('local '+self.__myUnit.symtab.debug())
        DebugManager.debug('subunits (len =',len(self.__myUnit.ulist),'):')

        for subUnit in self.__myUnit.ulist:
            DebugManager.debug(str(subUnit))
            UnitPostProcessor(subUnit).processUnit()

        self.__active_file = open('active_variables.f','a')

        if self._mode == 'reverse':
            try:
                fd = open(self._inlineFile)
            except IOError: 
                errstr = "IOError: Error cannot open file named "+self._inlineFile
                raise PostProcessError(errstr,0)
            try:
                definitions = fd.read()
            except IOError:
                errstr = "IOError: Error cannot read file named "+self._inlineFile
                raise PostProcessError(errstr,0)
            inline = False
            self.__templateExpansion(definitions,inline)
            self.__myUnit.decls = self.__myNewDecls
            self.__myUnit.execs = self.__myNewExecs

            try:
                fd.close()
            except IOError:
                errstr = "IOError: Error cannot close file named "+self._inlineFile
                raise PostProcessError(errstr,0)

        else:
            (Decls,Execs) = self.__forwardProcessDeclsAndExecs()
            self.__myUnit.decls = Decls
            self.__myUnit.execs = Execs

        if self.__write_subroutine is False:
            if self.__myUnit.end:
                for anEndListEntry in self.__myUnit.end:
                    self.__active_file.write(anEndListEntry.rawline,)
            self.__write_subroutine = True
        self.__active_file.close()

        if (self.__recursionDepth is not 0):
            raise PostProcessError('Recursion error in unitPostProcess: final recursion depth is not zero')
        DebugManager.debug(('+'*54)+' End post-process unit <'+str(self.__myUnit.uinfo)+'> '+(54*'+')+'\n\n')

        return self.__myUnit




