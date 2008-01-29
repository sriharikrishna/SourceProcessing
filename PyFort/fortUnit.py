'''Use assembly patterns, and vgen to produce a unit
generator from a parsed line generator

patterns are as follows:

unit = seq(zo1(cblk),baseunit)

baseunit = seq(startunit,star(ustmt),zo1(cblk),endunit)

startunit, endunit are predicate patterns based on specific statement types

ustmt = seq(zo1(cblk),base_ustmt)

base_ustmt = disj(pred(! (startunit|endunit|contains)),seq(contains,unitlist))

unitlist = plus(unit) # note recursion here, also empty contains not allowed

So, toplevel unit generator = vgen(unit,stmt_generator)
'''

from _Setup import *
import fortStmts as fs
from fortFile import Ffile
from fortParseFile import fortParseFile
from PyUtil.assembler import *
from PyUtil.buf_iter import buf_iter

def instance_pred(*class_list):
    '''define a predicate that checks if argument is an instance of 1 of the
    classes listed in class_list
    '''
    def the_pred(x):
        for c in class_list:
            if isinstance(x,c):
                return True
        return False
    return the_pred

def contains0(s):
    'eta expansion to simulate letrec'
    return seq(contains,unitlist)(s)

startunit  = pred(instance_pred(fs.PUstart))
endunit    = pred(instance_pred(fs.PUend))
cblk       = pred(instance_pred(fs.Comments))
contains   = pred(instance_pred(fs.ContainsStmt))

_base_helper = instance_pred(fs.PUstart,fs.PUend,fs.ContainsStmt)

base_ustmt0 = pred(lambda x:not _base_helper(x))
base_ustmt  = disj(base_ustmt0,contains0)
ustmt       = seq(zo1(cblk),base_ustmt)
baseunit    = seq(startunit,star(ustmt),zo1(cblk),endunit)
unit        = seq(zo1(cblk),baseunit)
unitlist    = plus(unit)

def fortUnitIterator(fn,free):
    return vgen(unit,buf_iter(fortParseFile(fn,free).lines))

'''
fn0 = 'UnitTests/Tfiles/units1.f90'
i0 = fortUnitIterator(fn0,True)

#pp0 = fortParseFile(fn,True)
#bb0 = buf_iter(pp.lines)

fn = 'UnitTests/Tfiles/units2.f90'
#pp = fortParseFile(fn,True)
#bb = buf_iter(pp.lines)
i1 = fortUnitIterator(fn,True)
'''
