'''
Module for creating partially processed Fortran files that have
a token list, line number representation as well as a rawline
'''
from _Setup import *
from PyUtil.chomp import chomp
from PyUtil.errors import ScanError

import re
_lineno_re = re.compile(r'(\s*)(\d+)')
_lead_re   = re.compile(r'(\s*)')

def _ident(self):
    return [self]

class _fortScanLine(object):
    'base class for scanned lines'

    map = _ident

    def __str__(self):
        return chomp(self.rawline)
    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,repr(str(self)))

class fortScanComment(_fortScanLine):
    'scanned comments == comments'

    def __init__(self,cline):

        self.rawline = cline.rawline
        self.rm      = ''

class fortScanLine(_fortScanLine):
    'non comment fortran lines'

    def __init__(self,fline):
        '''initialize line format characteristics and
        supply a token scan

        Also, convert !@#$% leading tabs into spaces
        '''
        from fortScan import scan1

        self.rawline         = fline.rawline
        line                 = fline.line
        if line[0] == '\t':
            line = ' ' * 8 + line[1:]
        m                    = _lineno_re.match(line)
        self.lineno          = m and int(m.group(2))
        linelead             = m and m.end(0) or 0
        self.lead            = linelead * ' '+_lead_re.match(line[linelead:]).group(1)
        (self.scan,self.rm)  = scan1.scan(line[len(self.lead):])
	if(self.rm) : 
	  raise ScanError(m,fline,self.scan, self.rm)

    def __repr__(self):
        return '%s(%s)' % ('fortScanLine',self.scan)

class fortScanFile(object):
    'scan File object should have a generator for scanned lines'

    def __init__(self,fname,free=False):
        from fortFile import Ffile
        import fortLine as fl

        lexi = ((fl.cline,lambda l: [fortScanComment(l)]),
                (fl.fline,lambda l: [fortScanLine(l)]))

        self.scanlines = Ffile.file(fname,free).map(lexi)

    def map(self,lexi):
        for (cls,meth) in lexi:
            cls.map = meth
        for l in self.scanlines:
            for v in l.map():
                yield v
        for (cls,meth) in lexi:
            cls.map = _ident
