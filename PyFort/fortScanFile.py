'''
Module for creating partially processed Fortran files that have
a token list, line number representation as well as a rawline
'''
from _Setup import *
from fortFile import Ffile
from fortScan import scan1
from PyUtil.errors import ScanError

class fortScanLine(object): pass
class fortScanComment(object): pass

def _scan_cmt(dta):
    return fortScanComment()

def _scan_stmt(jl):
    sl = fortScanLine()
    (sl.scan,sl.rm) = scan1.scan(jl)
    if sl.rm:
        raise ScanError(jl,sl.scan,sl.rm)
    return sl

#scan_cmt = _scan_cmt
#scan_stmt = _scan_stmt
    
class fortScanFile:
    'scan File object should have a generator for scanned lines'

    def __init__(self,fname,free=False):
        self.ff        = Ffile.file(fname,free,_scan_cmt,_scan_stmt)
        self.scanlines = self.ff.lines

    def map(self,lexi):
        return self.ff.map(lexi)
