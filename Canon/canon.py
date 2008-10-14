'''
canonicalization routines
'''

from _Setup import *
from PyFort.sym_entries import var
import PyFort.fortExp as fe
import PyFort.fortStmts as fs

class CanonError(Exception):
   '''exception for errors that occur during canonicalization'''
   def __init__(self,msg,lineNumber):
       self.msg  = msg
       self.lineNumber = lineNumber

class UnitCanonicalizer(object):
    'class to facilitate canonicalization on a per-unit basis'

    _verbose = False

    @staticmethod
    def setVerbose(isVerbose):
        UnitCanonicalizer._verbose = isVerbose

    def __init__(self,aUnit):
        self.__myUnit = aUnit
        self.__myNewDecls = []
        self.__myNewExecs = []
        self.__tmp_prefix   = 'oad_ctmp'
        self.__call_prefix  = 'oad_s_'
        self.__tempCounter = 0
        self.__recursionDepth = 0

    def __newTemp(self,anExpression):
        '''The new temporary variable assumes the value of anExpression'''
        theNewTemp = self.__tmp_prefix + str(self.__tempCounter)
        self.__tempCounter += 1
        (varTypeClass,varModifierList) = fe.exptype(anExpression,
                                                    self.__myUnit.symtab.lookup_type,
                                                    fs.kw2type,
                                                    fs.lenfn,
                                                    fs._Kind,
                                                    fe.isPolymorphic,
                                                    fs.typemerge)
        if self._verbose: print >> sys.stderr,str(theNewTemp)+': varTypeClass = "'+str(varTypeClass)+'" varModifierList = "'+str(varModifierList)+'"'
        theNewDecl = varTypeClass(varModifierList,[],[theNewTemp])
        self.__myNewDecls.append(theNewDecl)
        self.__myUnit.symtab.enter_name(theNewTemp,var((varTypeClass,varModifierList),()))
        return (theNewTemp,varTypeClass)

    def __canonicalizeFuncCall(self,theFuncCall,lead):
        '''turn a function call into a sunroutine call
        returns the new temp created as return value for the new subroutine call'''
        if self._verbose: print >> sys.stderr,self.__recursionDepth*'|\t'+'canonicalizing nonintrinsic function call "'+str(theFuncCall)+'"'
        self.__recursionDepth += 1
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|  creating new temp for the result of the subroutine that replaces "'+str(theFuncCall)+'":',
        (theNewTemp,newTempType) = self.__newTemp(theFuncCall)
        polymorphismSuffix = ''
        if fs.isPolymorphic(theFuncCall):
            polymorphismSuffix = '_'+newTempType.kw.lower()[0]
        self.__myNewExecs.append(self.__canonicalizeSubCallStmt(fs.CallStmt(self.__call_prefix + theFuncCall.head + polymorphismSuffix,
                                                                            theFuncCall.args + [theNewTemp],
                                                                            lead=lead
                                                                           ).flow()))
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|_'
        self.__recursionDepth -= 1
        return theNewTemp

    def __canonicalizeExpression(self,theExpression,lead):
        '''Canonicalize an expression tree by recursively replacing function calls with subroutine calls
           returns an expression that replaces theExpression'''
        if self._verbose: print >> sys.stderr,self.__recursionDepth*'|\t'+'canonicalizing expression "'+str(theExpression)+'"',
        self.__recursionDepth += 1
        # Variable or constant: do nothing
        if isinstance(theExpression, str):
            if self._verbose: print >> sys.stderr,', which is a string (should be a constant or a variable => no canonicalization necessary)'
            theNewExpression = theExpression
        # Nonintrinsic function call -> replace with subroutine call and hoist its arguments
        elif fe.isNonintrinsicFuncApp(theExpression,self.__myUnit.symtab):
            if self._verbose: print >> sys.stderr,', which is a non-intrinsic function app with argument(s) "'+str(theExpression.args)+'"'
            theNewExpression = self.__canonicalizeFuncCall(theExpression,lead)
        # Intrinsic function call or array ref (any App that's not a nonintrinsic) -> recursively canonicalize args
        elif isinstance(theExpression, fe.App):
            if self._verbose: print >> sys.stderr,', which is either an intrinsic or array ref with argument(s) "'+str(theExpression.args)+'"'
            replacementArgs = []
            for arg in theExpression.args:
                replacementArgs.append(self.__canonicalizeExpression(arg,lead))
            theNewExpression = fe.App(theExpression.head,replacementArgs)
        # Unary operation
        elif isinstance(theExpression,fe.Unary):
            if self._verbose: print >> sys.stderr,', which is a unary op. with exp: "'+str(theExpression.exp)+'"'
            if isinstance(theExpression,fe.Umi): # Unary Minus
                theNewExpression = fe.Umi(self.__canonicalizeExpression(theExpression.exp,lead))
            elif isinstance(theExpression,fe.Upl): # Unary plus
                theNewExpression = fe.Upl(self.__canonicalizeExpression(theExpression.exp,lead))
            elif isinstance(theExpression,fe.Not): # Not expression
                theNewExpression = fe.Not(self.__canonicalizeExpression(theExpression.exp,lead))
            elif isinstance(theExpression,fe.ParenExp): # Parenthesized expression
                theNewExpression = fe.ParenExp(self.__canonicalizeExpression(theExpression.exp,lead))
        # Binary operation
        elif isinstance(theExpression,fe.Ops):
            if self._verbose: print >> sys.stderr,', which is a binary op. with a1="'+str(theExpression.a1)+'", a2="'+str(theExpression.a2)+'"'
            a1Result = self.__canonicalizeExpression(theExpression.a1,lead)
            a2result = self.__canonicalizeExpression(theExpression.a2,lead)
            theNewExpression = fe.Ops(theExpression.op,a1Result,a2result)
        # Everything else...
        else:
            if self._verbose: print >> sys.stderr,', which is some other nontrivial type that is assumed to require no canonicalization'
            theNewExpression = theExpression
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|_'
        self.__recursionDepth -= 1
        return theNewExpression

    def __canonicalizeSubCallStmt(self,aSubCallStmt):
        '''Canonicalize a subroutine call by hoisting all arguments (except simple variables) to temporaries.'''
        if self._verbose: print >> sys.stderr,self.__recursionDepth*'|\t'+'canonicalizing subroutine call statement "'+str(aSubCallStmt)+'"'
        self.__recursionDepth += 1
        # canonicalize each of the the expressions that serve as arguments
        replacementArgs = []
        for anArg in aSubCallStmt.args:
            if self._verbose: print >> sys.stderr,(self.__recursionDepth - 1)*'|\t'+'|- argument "'+str(anArg)+'" ',
            # string that resides in symbol table: a variable
            if isinstance(anArg,str) and self.__myUnit.symtab.lookup_name(anArg):
                if self._verbose: print >> sys.stderr,'is a variable'+ \
                                                 ' with symbol table entry "'+str(self.__myUnit.symtab.lookup_name(anArg))+ \
                                                 '" and dims "'+str(self.__myUnit.symtab.lookup_dims(anArg))+'"'
                replacementArgs.append(anArg)
            # Array accesses
            elif fe.isArrayAccess(anArg,self.__myUnit.symtab):
                if self._verbose: print >> sys.stderr,'is an array access expression'
                replacementArgs.append(self.__canonicalizeExpression(anArg,aSubCallStmt.lead))
            # function calls
            elif fe.isNonintrinsicFuncApp(anArg,self.__myUnit.symtab):
                if self._verbose: print >> sys.stderr,'is a nonintrinsic function call'
                replacementArgs.append(self.__canonicalizeFuncCall(anArg,aSubCallStmt.lead))
            # everything else: nontrivial expressions (including constants)
            else:
                if self._verbose: print >> sys.stderr,'is a nontrivial expression that is to be hoisted.  Creating new temp:',
                # create a new temp and add it to decls
                (theNewTemp,newTempType) = self.__newTemp(anArg)
                replacementArgs.append(theNewTemp)
                self.__myNewExecs.append(self.__canonicalizeAssignStmt(fs.AssignStmt(theNewTemp,
                                                                                     anArg,
                                                                                     lead=aSubCallStmt.lead)))
        # replace aCallStmt with the canonicalized version
        replacementStatement = fs.CallStmt(aSubCallStmt.head,
                                           replacementArgs,
                                           lineNumber=aSubCallStmt.lineNumber,
                                           lead=aSubCallStmt.lead
                                          ).flow()
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|_'
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeAssignStmt(self,anAssignStmt):
        '''Canonicalize an assigment statement by removing function calls from the rhs'''
        if self._verbose: print >> sys.stderr,self.__recursionDepth*'|\t'+'canonicalizing assignment statement "'+str(anAssignStmt)+'"'
        self.__recursionDepth += 1
        replacementStatement = fs.AssignStmt(anAssignStmt.lhs,
                                             self.__canonicalizeExpression(anAssignStmt.rhs,anAssignStmt.lead),
                                             lineNumber=anAssignStmt,
                                             label=anAssignStmt.label,
                                             lead=anAssignStmt.lead
                                            ).flow()
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|_'
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeIfNonThenStmt(self,anIfNonThenStmt):
        '''Canonicalize if stmt (without "then" by canonicalizing the test component and the conditionally executed statement
        returns a list of statements that replace anIfNonThenStmt'''
        if self._verbose: print >> sys.stderr,self.__recursionDepth*'|\t'+'canonicalizing if statement (without "then") "'+str(anIfNonThenStmt)+'"'
        self.__recursionDepth += 1
        replacementStatement = fs.IfNonThenStmt(self.__canonicalizeExpression(anIfNonThenStmt.test,lead=anIfNonThenStmt.lead),
                                                self.__canonicalizeExpression(anIfNonThenStmt.stmt,lead=anIfNonThenStmt.lead),
                                                lineNumber=anIfNonThenStmt.lineNumber,
                                                label=anIfNonThenStmt.label,
                                                lead=anIfNonThenStmt.lead
                                               ).flow()
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|_'
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeIfThenStmt(self,anIfThenStmt):
        '''Canonicalize if-then stmt by canonicalizing the test component
        returns a list of statements that replace anIfThenStmt'''
        if self._verbose: print >> sys.stderr,self.__recursionDepth*'|\t'+'canonicalizing if-then statement "'+str(anIfThenStmt)+'"'
        self.__recursionDepth += 1
        replacementStatement = fs.IfThenStmt(self.__canonicalizeExpression(anIfThenStmt.test,anIfThenStmt.lead),
                                             lineNumber=anIfThenStmt.lineNumber,
                                             label=anIfThenStmt.label,
                                             lead=anIfThenStmt.lead
                                            ).flow()
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|_'
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeElseifStmt(self,anElseifStmt):
        '''Canonicalize anElseifStmt by canonicalizing the test component.  Returns a canonicalized ElseifStmt that replaces anElseifStmt'''
        if self._verbose: print >> sys.stderr,self.__recursionDepth*'|\t'+'canonicalizing elseif-then statement "'+str(anElseifStmt)+'"'
        self.__recursionDepth += 1
        newExecsLength = len(self.__myNewExecs)
        replacementStatement = fs.ElseifStmt(self.__canonicalizeExpression(anElseifStmt.test,anElseifStmt.lead),
                                             lineNumber=anElseifStmt.lineNumber,
                                             label=anElseifStmt.label,
                                             lead=anElseifStmt.lead
                                            ).flow()
        if len(self.__myNewExecs) > newExecsLength: # this is the case iff some new statements were inserted
            raise CanonError('elseif test-component "'+str(anElseifStmt.test)+'" requires hoisting, but the placement of the extra assignment(s) is problematic.',anElseifStmt.lineNumber)
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|_'
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeDoStmt(self,aDoStmt):
        '''
        Canonicalize aDoStmt statement by canonicalizing the loop start, end, and stride expressions.  Returns a canonicalized DoStmt that replaces aDoStmt.
        '''
        if self._verbose: print >> sys.stderr,self.__recursionDepth*'|\t'+'canonicalizing do statement "'+str(aDoStmt)+'"'
        self.__recursionDepth += 1
        replacementStart = self.__canonicalizeExpression(aDoStmt.loopStart,aDoStmt.lead)
        newExecsLength = len(self.__myNewExecs)
        replacementStatement = fs.DoStmt(aDoStmt.loopVar,
                                         replacementStart,
                                         self.__canonicalizeExpression(aDoStmt.loopEnd,aDoStmt.lead),
                                         self.__canonicalizeExpression(aDoStmt.loopStride,aDoStmt.lead),
                                         lineNumber=aDoStmt.lineNumber,
                                         label=aDoStmt.label,
                                         lead=aDoStmt.lead
                                        ).flow()
        if len(self.__myNewExecs) > newExecsLength: # this is the case iff loopEnd or loopStride required hopisting
            raise CanonError('Either loopEnd "'+str(aDoStmt.loopEnd)+'" or loopStride "'+str(aDoStmt.loopStride)+'" for DoStmt "'+str(aDoStmt)+'" requires hoisting, but the placement of the extra assignment(s) is problematic.',aDoStmt.lineNumber)
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|_'
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeWhileStmt(self,aWhileStmt):
        '''
        Canonicalize aWhileStmt statement by canonicalizing the test expression.  Returns a canonicalized while statement that replaces aWhileStmt.
        '''
        if self._verbose: print >> sys.stderr,self.__recursionDepth*'|\t'+'canonicalizing while statement "'+str(aWhileStmt)+'"'
        self.__recursionDepth += 1
        newExecsLength = len(self.__myNewExecs)
        replacementStatement = fs.WhileStmt(self.__canonicalizeExpression(aWhileStmt.testExpression,aWhileStmt.lead),
                                            lineNumber=aWhileStmt.lineNumber,
                                            label=aWhileStmt.label,
                                            lead=aWhileStmt.lead
                                           ).flow()
        if len(self.__myNewExecs) > newExecsLength: # this is the case iff some new statements were inserted
            raise CanonError('while statement test expression "'+str(aWhileStmt.testExpression)+'" requires hoisting, but the placement of the extra assignment(s) is problematic.',aWhileStmt.lineNumber)
        if self._verbose: print >> sys.stderr,(self.__recursionDepth-1)*'|\t'+'|_'
        self.__recursionDepth -= 1
        return replacementStatement

    def canonicalizeUnit(self):
        '''Recursively canonicalize \p aUnit'''
        if self._verbose: print >>sys.stderr,'+++++++++++++++++++++++++++++++++++++++++++++++++++++', \
                                             'Begin canonicalize unit <',self.__myUnit.uinfo,'>', \
                                             '+++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n'
        if (self._verbose and self.__myUnit.ulist): print >> sys.stderr,'subunits (len =',len(self.__myUnit.ulist),'):'
        for subUnit in self.__myUnit.ulist:
            if self._verbose: print >> sys.stderr,subUnit
            UnitCanonicalizer(subUnit).canonicalizeUnit()

        if (self._verbose and self.__myUnit.execs): print >> sys.stderr,'canonicalizing executable statements:'
        for anExecStmt in self.__myUnit.execs:
            if self._verbose: print >> sys.stderr,'Line '+str(anExecStmt.lineNumber)+':'
            newExecsLength = len(self.__myNewExecs) # store the current number of execs (to determine afterwards whether we've added some)
            replacementStatement = anExecStmt
            if isinstance(anExecStmt,fs.CallStmt):
                replacementStatement = self.__canonicalizeSubCallStmt(anExecStmt)
            elif isinstance(anExecStmt,fs.AssignStmt):
                replacementStatement = self.__canonicalizeAssignStmt(anExecStmt)
            elif isinstance(anExecStmt,fs.IfNonThenStmt):
                replacementStatement = self.__canonicalizeIfNonThenStmt(anExecStmt)
            elif isinstance(anExecStmt,fs.IfThenStmt):
                replacementStatement = self.__canonicalizeIfThenStmt(anExecStmt)
            elif isinstance(anExecStmt,fs.ElseifStmt):
                replacementStatement = self.__canonicalizeElseifStmt(anExecStmt)
            elif isinstance(anExecStmt,fs.DoStmt):
                replacementStatement = self.__canonicalizeDoStmt(anExecStmt)
            elif isinstance(anExecStmt,fs.WhileStmt):
                replacementStatement = self.__canonicalizeWhileStmt(anExecStmt)
            else:
                if self._verbose: print >> sys.stderr,'Statement "'+str(anExecStmt)+'" is assumed to require no canonicalization'
            if self._verbose: print >>sys.stderr,''
            if self.__recursionDepth != 0:
                raise CanonError('Recursion depth did not resolve to zero when canonicalizing',anExecStmt,anExecStmt.lineNumber)
            # determine whether a change was made
            if len(self.__myNewExecs) > newExecsLength: # some new statements were inserted
                self.__myNewExecs.append(replacementStatement) # => replace anExecStmt with the canonicalized version
            else: # no new statements were inserted
                self.__myNewExecs.append(anExecStmt) # => leave anExecStmt alone

        # build rawlines for the new declarations and add them to the unit
        for aDecl in self.__myNewDecls:
            aDecl.lead = self.__myUnit.uinfo.lead+'  '
            aDecl.flow()
        self.__myUnit.decls.extend(self.__myNewDecls)

        # replace the executable statements for the unit
        self.__myUnit.execs = self.__myNewExecs

        if self._verbose: print >>sys.stderr,'++++++++++++++++++++++++++++++++++++++++++++++++++++++', \
                                             'End canonicalize unit <',self.__myUnit.uinfo,'>', \
                                             '++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n'
        return self.__myUnit

