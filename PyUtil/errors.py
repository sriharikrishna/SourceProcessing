'''
specific exceptions that we may want to catch 
at the top level rather than the system exceptions
which should continue to produce a stack trace.
'''

class UserError(Exception):
    '''exception for errors caused by the user'''
    def __init__(self,msg):
        self.msg  = msg


class ScanError(Exception):
    '''
	exception for errors detected in the scanner caused either by 
	faulty code or by incomplete logic in the scanner implementation
    '''
    def __init__(self,lineNumber,aFortLine,scanned,rest):
        self.lineNumber=lineNumber
        self.aFortLine=aFortLine
	self.scanned=scanned
	self.rest=rest

class ParseError(Exception):
    '''
	exception for errors detected in the parser caused either by 
	faulty code or by incomplete logic in the parser implementation
    '''
    def __init__(self,scannedLine,target):
        '''
        the parser failed to parse scannedLine as a target
        where target is some string to indicated verbally what it is
        '''
        self.scannedLine=scannedLine
        self.target=target

