'''
utility for managing debugging verbosity on a global level 
'''

from _Setup import *
import enum

class DebugManager(object):

    _verbose = False

    _quiet = False

    @staticmethod
    def setVerbose(isVerbose):
        DebugManager._verbose = isVerbose

    @staticmethod
    def setQuiet(aBool):
        DebugManager._quiet = aBool

    _progress = False
    @staticmethod
    def dumpProgress():
        DebugManager._progress=True
        
    _processedFile = ""

    @staticmethod
    def setProcessedFile(aFileName):
        DebugManager._processedFile = aFileName
        if (DebugManager._progress):
            sys.stderr.write('SourceProcessing: PROGRESS: '+ DebugManager._processedFile+'\n')

    @staticmethod
    def processedFile():
        return DebugManager._processedFile

    WarnType = enum.Enum(['undefined', 'implicit','hoisting','ifStmtToIfConstr','controlFlow','nesting'])
        
    _warnOnlyTypeList=[]
    
    @staticmethod
    def warnOnlyOn(theWarnTypeList):
        for w in theWarnTypeList:
            DebugManager._warnOnlyTypeList.append(eval('DebugManager.WarnType.'+w))
        
    @staticmethod
    def warning(warningMessage,lineNumber=0,warnType=WarnType.undefined):
        if ((not DebugManager._quiet and not DebugManager._warnOnlyTypeList) 
            or 
            (DebugManager._warnOnlyTypeList and warnType in DebugManager._warnOnlyTypeList)): 
            sys.stderr.write('SourceProcessing: WARNING: '+warningMessage+' ('+DebugManager.processedFile())
            if (lineNumber>0):
                sys.stderr.write(':'+str(lineNumber))
            sys.stderr.write(')\n')
            sys.stderr.flush()

    @staticmethod
    def debug(debugMessage,outStream=sys.stdout,newLine=True,lineNumber=None):
        if (DebugManager._verbose):
            newLineStr = newLine and '\n' \
                                  or ''
            lineNumberStr = ''
            if lineNumber :
                lineNumberStr += '[Line '+str(lineNumber)+']: '
            outStream.write(lineNumberStr+debugMessage+newLineStr)
            outStream.flush()

