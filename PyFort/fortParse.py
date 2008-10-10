'''Parsing functions, suitable for passing to Ffile
'''
from _Setup import *

import fortStmts    as fs
from fortScan import scan1
from PyUtil.errors import ScanError

def parse_stmt(jl,lineNumber):
    'parse a statment from a line'
    (scan,rm) = scan1.scan(jl)
    if rm:
        raise ScanError(lineNumber,jl,scan,rm)
    obj = fs.parse(scan,lineNumber)
    return obj

def parse_cmnt(dta,lineNumber):
    return fs.Comments('',lineNumber)
