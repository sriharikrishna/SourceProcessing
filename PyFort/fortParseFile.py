'''
Turn a scanned line into a parsed line
'''
from _Setup import *
from fortFile import Ffile
from fortScan import scan1
from PyUtil.errors import ScanError,ParseError

import fortStmts    as fs
from   PyIR.mapper       import _Map

def parse_stmt(jl):
    'parse a statment from a line'
    (scan,rm) = scan1.scan(jl)
    if rm:
        raise ScanError(jl,scan,rm)
    obj = fs.parse(scan)
    return obj

def parse_cmnt(dta):
    return fs.Comments('')

class fortParseFile:
    def __init__(self,fn,free=False):
        self.ff    = Ffile.file(fn,free,parse_cmnt,parse_stmt)
        self.lines = self.ff.lines

    def map(self,lexi,*args,**kwargs):
        return self.ff.map(lexi,*args,**kwargs)

fortParseFileIter = fortParseFile
