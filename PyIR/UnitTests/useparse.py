from _Setup import *
from PyFort.fortScan import scan1
from PyFort.fortExp import Exp
from PyFort.fortStmts import parse
from PyFort.fortParseFile import fortParseFile as fpf

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
