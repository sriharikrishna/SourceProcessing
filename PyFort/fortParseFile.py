'''
Turn a scanned line into a parsed line
'''
from _Setup import *
from fortFile import Ffile

import fortStmts    as fs
#from   PyIR.mapper       import _Map

from fortParse import parse_stmt,parse_cmnt

class fortParseFile:
    def __init__(self,fn,free=False):
        self.ff    = Ffile.file(fn,free,parse_cmnt,parse_stmt)
        self.lines = self.ff.lines

#    def map(self,lexi,*args,**kwargs):
#        return self.ff.map(lexi,*args,**kwargs)

fortParseFileIter = fortParseFile
