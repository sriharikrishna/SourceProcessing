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
    def debug(debugMessage,outStream=sys.stdout):
        if (DebugManager._verbose):
            print >>outStream,debugMessage
