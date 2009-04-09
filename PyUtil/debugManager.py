'''
utility for managing debugging verbosity on a global level 
'''

from _Setup import *

class DebugManager(object):

    _verbose = False

    @staticmethod
    def setVerbose(isVerbose):
        DebugManager._verbose = isVerbose

    @staticmethod
    def debug(debugMessage,outStream=sys.stdout,newLine=True):
        if (DebugManager._verbose):
            if (newLine):
                outStream.write(debugMessage+'\n')
            else:
                outStream.write(debugMessage)
            outStream.flush()
