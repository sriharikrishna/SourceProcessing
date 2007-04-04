from fortScan import scan1
from fortExp import Exp
from fortStmts import parse
from fortParseFile import fortParseFile as fpf

def scan(s):
    (v,rst) = scan1.scan(s)
    if rst:
        raise "incomplete scan '%s', rst = '%s'" % (s,rst)
    return v

def ep(s):
    (v,rst) = Exp(scan(s))
    if rst:
        raise "incomplete Exp parse: '%s', rst = %s" % (s,rst)
    return v

def pps(s):

    return parse(scan(s))
#
#    (t,rst) = parse(scan(s))
#    if not rst:
#        raise("parse failure: '%s', leaves rst = %s" % (s,rst))
#    return t
