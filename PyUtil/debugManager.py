'''
utility for managing debugging verbosity on a global level 
'''

from _Setup import *

class DebugManager(object):

    _verbose = False

    @staticmethod
    def setVerbose(isVerbose):
        DebugManager._verbose = isVerbose

    _processedFile = ""

    @staticmethod
    def setProcessedFile(aFileName):
        DebugManager._processedFile = aFileName

    @staticmethod
    def processedFile():
        return DebugManager._processedFile

    @staticmethod
    def warning(warningMessage,lineNumber=0):
        sys.stderr.write('WARNING: '+warningMessage+' ('+DebugManager.processedFile())
        if (lineNumber>0):
            sys.stderr.write(':'+str(lineNumber))
        sys.stderr.write(')\n')
        sys.stderr.flush()

    @staticmethod
    def debug(debugMessage,outStream=sys.stdout,newLine=True):
        if (DebugManager._verbose):
            newLineStr = newLine and '\n' \
                                  or ''
            outStream.write(debugMessage+newLineStr)
            outStream.flush()









