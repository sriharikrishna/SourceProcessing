'''
utility for managing debugging verbosity on a global level 
'''

from _Setup import *

class DebugManager(object):

    _verbose = False

    _quiet = False

    @staticmethod
    def setVerbose(isVerbose):
        DebugManager._verbose = isVerbose

    @staticmethod
    def setQuiet(aBool):
        DebugManager._quiet = aBool

    _processedFile = ""

    @staticmethod
    def setProcessedFile(aFileName):
        DebugManager._processedFile = aFileName

    @staticmethod
    def processedFile():
        return DebugManager._processedFile

    @staticmethod
    def warning(warningMessage,lineNumber=0):
        if (not DebugManager._quiet): 
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









