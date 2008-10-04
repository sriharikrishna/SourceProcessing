'''Parsing functions, suitable for passing to Ffile
'''
from _Setup import *

import fortStmts    as fs
from fortScan import scan1
from PyUtil.errors import ScanError

def parse_stmt(jl,rawLineNumber):
    'parse a statment from a line'
    (scan,rm) = scan1.scan(jl)
    if rm:
        raise ScanError(rawLineNumber,jl,scan,rm)
    obj = fs.parse(scan)
    return obj

def parse_cmnt(dta,rawLineNumber):
    return fs.Comments('')
