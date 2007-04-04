'''
Routine to kill bang(= !) comments fromt the end of the line
'''
from fortScan import q_re,qq_re
import re

_no_bang_re = r'''^((?: [^'"!] | (?: %s ) | (?: %s) )*) (! .*)$'''
_no_bang_re = _no_bang_re % (q_re,qq_re)
_no_bang_re = re.compile(_no_bang_re,re.X)

def kill_bang_comment(l):
    '''kill bang comments from end of line'''
    mm = _no_bang_re.match(l)
    if not mm:
        return(l,'')
    else:
        return(mm.group(1),mm.group(2))

