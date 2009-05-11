'''
canonicalization routines
'''

from _Setup import *

from PyUtil.debugManager import DebugManager
from PyUtil.symtab import Symtab,SymtabEntry,SymtabError

import PyFort.intrinsic as intrinsic
from PyFort.typeInference import TypeInferenceError,expressionType,functionType,isArrayReference
import PyFort.fortExp as fe
import PyFort.fortStmts as fs
import PyFort.fortUnit as fu
import PyFort.flow as fl

import function2subroutine

class CanonError(Exception):
    '''exception for errors that occur during canonicalization'''
    def __init__(self,msg,lineNumber):
        self.msg  = msg
        self.lineNumber = lineNumber

def _subroutinizeIntrinsic(aFunction):
    return intrinsic.getGenericName(aFunction.head) in ('max','min')

_requiredSubroutinizedIntrinsics=[]

def requireSubroutinezedIntrinsic(key,typeClass):
    if ((key,typeClass) not in _requiredSubroutinizedIntrinsics):
        _requiredSubroutinizedIntrinsics.append((key,typeClass))

_call_prefix  = 'oad_s_'
_tmp_prefix   = 'oad_ctmp'

def makeSubroutinezedIntrinsicName(intrName,typeClass):
    return _call_prefix + intrName + (intrinsic.isPolymorphic(intrName) and '_'+typeClass.kw.lower()[0] or '')    

def makeSubroutinizedIntrinsics():
    ''' this is just a starter and currently works only for max/min '''
    subroutinizedIntrsincis=[]
    for (key,typeClass) in _requiredSubroutinizedIntrinsics:
        newUnit=fu.Unit()
        subroutinizedIntrsincis.append(newUnit)
        newUnit.uinfo=fs.SubroutineStmt(makeSubroutinezedIntrinsicName(key,typeClass),
                                        ["a","b","r"],lead=fl.formatStart).flow()
        newUnit.decls.append(typeClass(None,None,'a',lead=fl.formatStart+'  ').flow())
        newUnit.decls.append(typeClass(None,None,'b',lead=fl.formatStart+'  ').flow())
        newUnit.decls.append(typeClass(None,None,'r',lead=fl.formatStart+'  ').flow())
        testExpr=None
        if key=='max':
            testExpr=fe.Ops('>','a','b')
        else:
            testExpr=fe.Ops('<','a','b')
        newUnit.execs.append(fs.IfThenStmt(testExpr,lead=fl.formatStart+'  ').flow())
        newUnit.execs.append(fs.AssignStmt('r','a',lead=fl.formatStart+'    ').flow())
        newUnit.execs.append(fs.ElseStmt(lead=fl.formatStart+'  ').flow())
        newUnit.execs.append(fs.AssignStmt('r','b',lead=fl.formatStart+'    ').flow())
        newUnit.execs.append(fs.EndifStmt(lead=fl.formatStart+'  ').flow())
        newUnit.end.append(fs.EndStmt(lead=fl.formatStart).flow())
    return subroutinizedIntrsincis
    
class UnitCanonicalizer(object):
    'class to facilitate canonicalization on a per-unit basis'

    _hoistConstantsFlag = False
    _hoistStringsFlag = False

    @staticmethod
    def setHoistConstantsFlag(hoistConstantsFlag):
        UnitCanonicalizer._hoistConstantsFlag = hoistConstantsFlag

    @staticmethod
    def setHoistStringsFlag(hoistStringsFlag):
        UnitCanonicalizer._hoistStringsFlag = hoistStringsFlag

    def __init__(self,aUnit):
        self.__myUnit = aUnit
        self.__myNewDecls = []
        self.__myNewExecs = []
        self.__tempCounter = 0
        self.__recursionDepth = 0

    def shouldSubroutinizeFunction(self,theApp,parentStmt):
        '''
        A function should be subroutinized if it is an intrinsic and _subroutinizeIntrinsic
        returns true or if is not an intrinsic
        '''
        DebugManager.debug('UnitCanonicalizer.shouldSubroutinizeFunction called on "'+str(theApp)+'"')
        if not isinstance(theApp,fe.App):
            raise CanonError('UnitCanonicalizer.shouldSubroutinizeFunction called on non-App object '+str(theApp),parentStmt.lineNumber)
        theSymtabEntry = self.__myUnit.symtab.lookup_name(theApp.head)
        if theSymtabEntry and isinstance(theSymtabEntry.entryKind,SymtabEntry.VariableEntryKind):
            raise CanonError('UnitCanonicalizer.shouldSubroutinizeFunction called on array reference '+str(theApp),parentStmt.lineNumber)
        try:
            (funcType,modifier) = functionType(theApp,self.__myUnit.symtab,parentStmt.lineNumber)
        except TypeInferenceError,errorObj:
            sys.stdout.flush()
            raise CanonError('UnitCanonicalizer.shouldSubroutinizeFunction: TypeInferenceError: '+errorObj.msg,parentStmt.lineNumber)
        if intrinsic.is_intrinsic(theApp.head):
            DebugManager.debug('UnitCanonicalizer.shouldSubroutinizeFunction: It\'s an intrinsic of type '+str(funcType))
            return _subroutinizeIntrinsic(theApp) and not funcType == fs.IntegerStmt
        else:
            return True

    def __newTemp(self,anExpression,parentStmt):
        '''The new temporary variable assumes the value of anExpression'''
        theNewTemp = _tmp_prefix + str(self.__tempCounter)
        self.__tempCounter += 1
        (varTypeClass,varModifierList) = expressionType(anExpression,self.__myUnit.symtab,parentStmt.lineNumber)
        if (varModifierList!=[] and isinstance(varModifierList[0],fs._FLenMod) and varModifierList[0].len=='*'):
            raise CanonError('unable to determine length of temporary variable for '+str(anExpression),parentStmt.lineNumber)
        if varTypeClass == fs.RealStmt and varModifierList == []:
            varTypeClass = Symtab._default_real[0]
            if varTypeClass == fs.DoubleStmt:
                print >>sys.stderr,'WARNING: Temp variable forced to 8-byte float (real -> double)'
        DebugManager.debug('replaced with '+str(theNewTemp)+' of type '+str(varTypeClass)+'('+str(varModifierList)+')')
        theNewDecl = varTypeClass(varModifierList,[],[theNewTemp])
        self.__myNewDecls.append(theNewDecl)
        self.__myUnit.symtab.enter_name(theNewTemp,
                                        SymtabEntry(SymtabEntry.VariableEntryKind,
                                                    type=(varTypeClass,varModifierList),
                                                    origin='temp'))
        return (theNewTemp,varTypeClass)

    def __canonicalizeFuncCall(self,theFuncCall,parentStmt):
        '''turn a function call into a subroutine call
        returns the new temp created as return value for the new subroutine call'''
        DebugManager.debug(self.__recursionDepth*'|\t'+'converting function call "'+str(theFuncCall)+'" to a subroutine call')
        self.__recursionDepth += 1
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|  creating new temp for the result of the subroutine that replaces "'+str(theFuncCall)+'":',newLine=False)
        (theNewTemp,newTempType) = self.__newTemp(theFuncCall,parentStmt)
        newSubName=''
        if intrinsic.is_intrinsic(theFuncCall.head):
            funcName=intrinsic.getGenericName(theFuncCall.head)
            newSubName=makeSubroutinezedIntrinsicName(funcName,
                                                      newTempType)
            requireSubroutinezedIntrinsic(funcName,newTempType)
        else:
            newSubName=_call_prefix + theFuncCall.head
        self.__myNewExecs.append(self.__canonicalizeSubCallStmt(fs.CallStmt(newSubName,
                                                                            theFuncCall.args + [theNewTemp],
                                                                            lineNumber=parentStmt.lineNumber,
                                                                            label=False,
                                                                            lead=parentStmt.lead
                                                                            ).flow()))
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return theNewTemp

    def __hoistExpression(self,theExpression,parentStmt):
        # function calls that are to be turned into subroutine calls
        # -> return the temp that carries the value for the new subcall
        if isinstance(theExpression,fe.App) and \
           not isArrayReference(theExpression,self.__myUnit.symtab,parentStmt.lineNumber) and \
           self.shouldSubroutinizeFunction(theExpression,parentStmt):
            DebugManager.debug('it is a function call to be subroutinized')
            return self.__canonicalizeFuncCall(theExpression,parentStmt)
        # Anything else: create an assignment to a temporary and return that temporary
        (theNewTemp,newTempType) = self.__newTemp(theExpression,parentStmt)
        self.__myNewExecs.append(self.__canonicalizeAssignStmt(fs.AssignStmt(theNewTemp,
                                                                             theExpression,
                                                                             lineNumber=parentStmt.lineNumber,
                                                                             label=False,
                                                                             lead=parentStmt.lead)))
        return theNewTemp

    def __canonicalizeExpression(self,theExpression,parentStmt):
        '''Canonicalize an expression tree by recursively replacing function calls with subroutine calls
           returns an expression that replaces theExpression'''
        DebugManager.debug(self.__recursionDepth*'|\t'+'canonicalizing expression "'+str(theExpression)+'"',newLine=False)
        self.__recursionDepth += 1
        replacementExpression = theExpression
        # variable or constant -> do nothing
        if isinstance(theExpression, str):
            DebugManager.debug(', which is a string (should be a constant or a variable => no canonicalization necessary)')
        # application expressions
        elif isinstance(theExpression,fe.App):
            # array reference -. do nothing
            if isArrayReference(theExpression,self.__myUnit.symtab,parentStmt.lineNumber):
                DebugManager.debug(', which is an array reference (no canonicalization necessary)')
            # function calls to subroutinize -> subroutinize and recursively canonicalize args
            elif self.shouldSubroutinizeFunction(theExpression,parentStmt):
                DebugManager.debug(', it\'s a function call (subroutinized)')
                replacementExpression = self.__canonicalizeFuncCall(theExpression,parentStmt)
            # function calls that won't be subroutinized -> recursively canonicalize args
            else: 
                DebugManager.debug(', it\'s a function call (non-subroutinized)')
                replacementArgs = []
                for arg in theExpression.args:
                    replacementArgs.append(self.__canonicalizeExpression(arg,parentStmt))
                replacementExpression = fe.App(theExpression.head,replacementArgs)
        # Unary operation -> recursively canonicalize the sole subexpression
        elif isinstance(theExpression,fe.Unary):
            DebugManager.debug(', which is a unary op. with exp: "'+str(theExpression.exp)+'"')
            replacementExpression = theExpression.__class__(self.__canonicalizeExpression(theExpression.exp,parentStmt))
        # Binary operation -> recursively canonicalize both subexpressions
        elif isinstance(theExpression,fe.Ops):
            DebugManager.debug(', which is a binary op. with a1="'+str(theExpression.a1)+'", a2="'+str(theExpression.a2)+'"')
            a1Result = self.__canonicalizeExpression(theExpression.a1,parentStmt)
            a2result = self.__canonicalizeExpression(theExpression.a2,parentStmt)
            replacementExpression = fe.Ops(theExpression.op,a1Result,a2result)
        # Everything else...
        else:
            DebugManager.debug(', which is some other nontrivial type that is assumed to require no canonicalization')
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return replacementExpression

    def __canonicalizeSubCallStmt(self,aSubCallStmt):
        '''
        Canonicalize a subroutine call by hoisting all arguments
        (except simple variables) to temporaries.
        '''
        DebugManager.debug(self.__recursionDepth*'|\t'+'canonicalizing subroutine call statement "'+str(aSubCallStmt)+'"')
        self.__recursionDepth += 1
        # canonicalize each of the the expressions that serve as arguments
        replacementArgs = []
        for anArg in aSubCallStmt.args:
            #TODO: remove perens when the whole argument is in them??
            DebugManager.debug((self.__recursionDepth - 1)*'|\t'+'|- argument "'+str(anArg)+'" ',newLine=False)
            (argType,argTypeMod) = expressionType(anArg,self.__myUnit.symtab,aSubCallStmt.lineNumber)
            # constant character expressions
            if argType == fs.CharacterStmt:
                if not self._hoistStringsFlag:
                    DebugManager.debug('is a string expression (which we aren\'t hoisting)')
                    replacementArgs.append(anArg)
                elif isinstance(anArg,str) and self.__myUnit.symtab.lookup_name(anArg):
                    DebugManager.debug('is a string variable (which we aren\'t hoisting)')
                    replacementArgs.append(anArg)
                elif isinstance(anArg,fe.App) and isArrayReference(anArg,self.__myUnit.symtab,aSubCallStmt.lineNumber):
                    DebugManager.debug('is a character array reference (which we aren\'t hoisting)')
                    replacementArgs.append(anArg)
                else:
                    DebugManager.debug('is a string expression to be hoisted:',newLine=False)
                    replacementArgs.append(self.__hoistExpression(anArg,aSubCallStmt))
            # other constant expressions
            elif fe.isConstantExpression(anArg):
                if not self._hoistConstantsFlag:
                    DebugManager.debug('is a constant expression (which we aren\'t hoisting)')
                    replacementArgs.append(anArg)
                else:
                    DebugManager.debug('is a constant expression to be hoisted:',newLine=False)
                    replacementArgs.append(self.__hoistExpression(anArg,aSubCallStmt))
            # variables (with VariableEntry in symbol table) -> do nothing
            elif isinstance(anArg,str) and self.__myUnit.symtab.lookup_name(anArg):
                symtabEntry = self.__myUnit.symtab.lookup_name(anArg)
                DebugManager.debug('is an identifier (variable,function,etc.)')
                replacementArgs.append(anArg)
            # Array References -> do nothing
            elif isinstance(anArg,fe.App) and isArrayReference(anArg,self.__myUnit.symtab,aSubCallStmt.lineNumber):
                DebugManager.debug('is an array reference')
                replacementArgs.append(anArg)
            # everything else -> hoist and create an assignment to a temp variable
            else:
                DebugManager.debug('is a nontrivial expression to be hoisted:',newLine=False)
                replacementArgs.append(self.__hoistExpression(anArg,aSubCallStmt))
        # replace aCallStmt with the canonicalized version
        replacementStatement = fs.CallStmt(aSubCallStmt.head,
                                           replacementArgs,
                                           lineNumber=aSubCallStmt.lineNumber,
                                           label=aSubCallStmt.label,
                                           lead=aSubCallStmt.lead
                                          ).flow()
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeAssignStmt(self,anAssignStmt):
        '''Canonicalize an assigment statement by removing function calls from the rhs'''
        DebugManager.debug(self.__recursionDepth*'|\t'+'canonicalizing assignment statement "'+str(anAssignStmt)+'"')
        self.__recursionDepth += 1
        replacementStatement = fs.AssignStmt(anAssignStmt.lhs,
                                             self.__canonicalizeExpression(anAssignStmt.rhs,anAssignStmt),
                                             lineNumber=anAssignStmt.lineNumber,
                                             label=anAssignStmt.label,
                                             lead=anAssignStmt.lead
                                            ).flow()
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeIfNonThenStmt(self,anIfNonThenStmt):
        '''Canonicalize if stmt (without "then" by canonicalizing the test component and the conditionally executed statement
        returns a list of statements that replace anIfNonThenStmt'''
        DebugManager.debug(self.__recursionDepth*'|\t'+'canonicalizing if statement (without "then") "'+str(anIfNonThenStmt)+'"')
        self.__recursionDepth += 1
        replacementStatement = fs.IfNonThenStmt(self.__canonicalizeExpression(anIfNonThenStmt.test,anIfNonThenStmt),
                                                self.__canonicalizeExpression(anIfNonThenStmt.stmt,anIfNonThenStmt),
                                                lineNumber=anIfNonThenStmt.lineNumber,
                                                label=anIfNonThenStmt.label,
                                                lead=anIfNonThenStmt.lead
                                               ).flow()
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeIfThenStmt(self,anIfThenStmt):
        '''Canonicalize if-then stmt by canonicalizing the test component
        returns a list of statements that replace anIfThenStmt'''
        DebugManager.debug(self.__recursionDepth*'|\t'+'canonicalizing if-then statement "'+str(anIfThenStmt)+'"')
        self.__recursionDepth += 1
        replacementStatement = fs.IfThenStmt(self.__canonicalizeExpression(anIfThenStmt.test,anIfThenStmt),
                                             lineNumber=anIfThenStmt.lineNumber,
                                             label=anIfThenStmt.label,
                                             lead=anIfThenStmt.lead
                                            ).flow()
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeElseifStmt(self,anElseifStmt):
        '''Canonicalize anElseifStmt by canonicalizing the test component.  Returns a canonicalized ElseifStmt that replaces anElseifStmt'''
        DebugManager.debug(self.__recursionDepth*'|\t'+'canonicalizing elseif-then statement "'+str(anElseifStmt)+'"')
        self.__recursionDepth += 1
        newExecsLength = len(self.__myNewExecs)
        replacementStatement = fs.ElseifStmt(self.__canonicalizeExpression(anElseifStmt.test,anElseifStmt),
                                             lineNumber=anElseifStmt.lineNumber,
                                             label=anElseifStmt.label,
                                             lead=anElseifStmt.lead
                                            ).flow()
        if len(self.__myNewExecs) > newExecsLength: # this is the case iff some new statements were inserted
            raise CanonError('elseif test-component "'+str(anElseifStmt.test)+'" requires hoisting, but the placement of the extra assignment(s) is problematic.',anElseifStmt.lineNumber)
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeDoStmt(self,aDoStmt):
        '''
        Canonicalize aDoStmt statement by canonicalizing the loop start, end, and stride expressions.  Returns a canonicalized DoStmt that replaces aDoStmt.
        '''
        DebugManager.debug(self.__recursionDepth*'|\t'+'canonicalizing do statement "'+str(aDoStmt)+'"')
        self.__recursionDepth += 1
        replacementStart = self.__canonicalizeExpression(aDoStmt.loopStart,aDoStmt)
        newExecsLength = len(self.__myNewExecs)
        replacementStatement = fs.DoStmt(aDoStmt.doLabel,
                                         aDoStmt.loopVar,
                                         replacementStart,
                                         self.__canonicalizeExpression(aDoStmt.loopEnd,aDoStmt),
                                         self.__canonicalizeExpression(aDoStmt.loopStride,aDoStmt),
                                         lineNumber=aDoStmt.lineNumber,
                                         label=aDoStmt.label,
                                         lead=aDoStmt.lead
                                        ).flow()
        if len(self.__myNewExecs) > newExecsLength: # this is the case iff loopEnd or loopStride required hopisting
            raise CanonError('Either loopEnd "'+str(aDoStmt.loopEnd)+'" or loopStride "'+str(aDoStmt.loopStride)+'" for DoStmt "'+str(aDoStmt)+'" requires hoisting, but the placement of the extra assignment(s) is problematic.',aDoStmt.lineNumber)
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeWhileStmt(self,aWhileStmt):
        '''
        Canonicalize aWhileStmt statement by canonicalizing the test expression.  Returns a canonicalized while statement that replaces aWhileStmt.
        '''
        DebugManager.debug(self.__recursionDepth*'|\t'+'canonicalizing while statement "'+str(aWhileStmt)+'"')
        self.__recursionDepth += 1
        newExecsLength = len(self.__myNewExecs)
        replacementStatement = fs.WhileStmt(self.__canonicalizeExpression(aWhileStmt.testExpression,aWhileStmt),
                                            lineNumber=aWhileStmt.lineNumber,
                                            label=aWhileStmt.label,
                                            lead=aWhileStmt.lead
                                           ).flow()
        if len(self.__myNewExecs) > newExecsLength: # this is the case iff some new statements were inserted
            raise CanonError('while statement test expression "'+str(aWhileStmt.testExpression)+'" requires hoisting, but the placement of the extra assignment(s) is problematic.',aWhileStmt.lineNumber)
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return replacementStatement

    def __canonicalizeSelectCaseStmt(self,aSelectCaseStmt):
        '''
        Canonicalize aSelectCaseStmt by canonicalizing the case expression.  Returns a canonicalized select case statement that replaces aSelectCaseStmt.
        '''
        DebugManager.debug(self.__recursionDepth*'|\t'+'canonicalizing select case statement "'+str(aSelectCaseStmt)+'"')
        self.__recursionDepth += 1
        replacementStatement = fs.SelectCaseStmt(self.__canonicalizeExpression(aSelectCaseStmt.caseExpression,aSelectCaseStmt),
                                                 lineNumber=aSelectCaseStmt.lineNumber,
                                                 label=aSelectCaseStmt.label,
                                                 lead=aSelectCaseStmt.lead
                                                ).flow()
        DebugManager.debug((self.__recursionDepth-1)*'|\t'+'|_')
        self.__recursionDepth -= 1
        return replacementStatement

    def canonicalizeUnit(self):
        '''Recursively canonicalize \p aUnit'''
        DebugManager.debug(('+'*55)+' Begin canonicalize unit <'+str(self.__myUnit.uinfo)+'> '+(55*'+'))
        DebugManager.debug('local '+self.__myUnit.symtab.debug())
        DebugManager.debug('subunits (len ='+str(len(self.__myUnit.ulist))+'):')
        for subUnit in self.__myUnit.ulist:
            DebugManager.debug(str(subUnit))
            UnitCanonicalizer(subUnit).canonicalizeUnit()

        DebugManager.debug('canonicalizing executable statements:')
        for anExecStmt in self.__myUnit.execs:
            try:
                DebugManager.debug('[Line '+str(anExecStmt.lineNumber)+']:')
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
                elif isinstance(anExecStmt,fs.SelectCaseStmt):
                    replacementStatement = self.__canonicalizeSelectCaseStmt(anExecStmt)
                else:
                    DebugManager.debug('Statement "'+str(anExecStmt)+'" is assumed to require no canonicalization')
                if self.__recursionDepth != 0:
                    raise CanonError('Recursion depth did not resolve to zero when canonicalizing',anExecStmt,anExecStmt.lineNumber)
                # determine whether a change was made
                if len(self.__myNewExecs) > newExecsLength: # some new statements were inserted
                    self.__myNewExecs.append(replacementStatement) # => replace anExecStmt with the canonicalized version
                else: # no new statements were inserted
                    self.__myNewExecs.append(anExecStmt) # => leave anExecStmt alone
            except TypeInferenceError,e:
                raise CanonError('Caught TypeInferenceError: '+e.msg,anExecStmt.lineNumber)
            except SymtabError,e:
                raise CanonError('Caught SymtabError: '+e.msg,anExecStmt.lineNumber)

        # build rawlines for the new declarations and add them to the unit
        for aDecl in self.__myNewDecls:
            aDecl.lead = self.__myUnit.uinfo.lead+'  '
            aDecl.flow()
        self.__myUnit.decls.extend(self.__myNewDecls)

        # replace the executable statements for the unit
        self.__myUnit.execs = self.__myNewExecs

#       # for function units, also create a corresponding subroutine
#       if isinstance(self.__myUnit.uinfo,fs.FunctionStmt):
#           print 'I know I\'m in a function unit!'
#           function2subroutine.convertFunction(self.__myUnit)

        DebugManager.debug(('+'*54)+' End canonicalize unit <'+str(self.__myUnit.uinfo)+'> '+(54*'+')+'\n\n')
        return self.__myUnit

