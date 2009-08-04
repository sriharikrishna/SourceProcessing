import sys

from PyUtil.errors import ScanError,ParseError

from fortScan import scan1
from fortExp import Exp
from fortStmts import parse

def scan(s):
    (v,rst) = scan1.scan(s)
    if rst:
        raise ScanError(lineNumber=0,aFortLine=s,scanned=v,rest=rst)
    return v

def ep(s):
    (v,rst) = Exp(scan(s))
    if rst:
        raise "incomplete Exp parse: '%s', rst = %s" % (s,rst)
    return v

def pps(s):
    try:
        return parse(scan(s),lineNumber=0)
    except ScanError,e:
        print >>sys.stderr,'\nERROR: ScanError: scanner fails on the following line:'
        print >>sys.stderr,e.aFortLine
        print >>sys.stderr,'\tscanned =',e.scanned
        print >>sys.stderr,'\trest =',e.rest
        return '!!! ScanError thrown (see above) !!!'
    except ParseError,e:
        print >>sys.stderr,'\nERROR: ParseError: parser fails to assemble tokens in the following line:'
        print >>sys.stderr,e.scannedLine
        if e.details:
            print >>sys.stderr,"Details:",e.details
        if e.target:
            print >>sys.stderr,"tried to parse as",e.target
        return '!!! ParseError thrown (see above) !!!'

