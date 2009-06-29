'''Parsing functions, suitable for passing to Ffile
'''
from _Setup import *

from PyUtil.l_assembler import *

import fortStmts    as fs
from fortScan import scan1
from PyUtil.errors import ScanError

# returns a list of parsed statements
def parse_stmts(jl,lineNumber):
    'parse statements from a line'
    (scan,rm) = scan1.scan(jl)
    if rm:
        raise ScanError(lineNumber,jl,scan,rm)
    objs = []

    count = scan.count('\n')
    while count > 0:
        index = scan.index('\n')
        stmt = scan[:index]
        if len(stmt) == 0:
            count -= 1
            continue
        objs.append(fs.parse(stmt,lineNumber))
        scan = scan[index+1:]
        count -= 1
        
    if len(scan) != 0:
        objs.append(fs.parse(scan,lineNumber))

    return objs
    
def parse_cmnt(dta,lineNumber):
    return fs.Comments('',lineNumber)
