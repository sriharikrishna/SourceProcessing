from _Setup import *

from PyUtil.flatten import flatten
from PyUtil.chomp import chomp
from kill_bang import *
from PyUtil.assembler import *

_cont_re = r'& \s* $'
_cont_re = re.compile(_cont_re,re.X)

_comment_re = r'\s* !'
_blank_re   = r' \s* $'

_comm_re    = r'(%s | %s)' % (_comment_re,_blank_re)
_comm_re    = re.compile(_comm_re,re.X)

def is_cont(line):
    '''check to see if line is continued (assuming no bang comments)'''

    (l,r) = kill_bang_comment(line)
    return _cont_re.search(l)

def is_comm(line):
    '''check to see if line is a comment line'''
    return _comm_re.match(line)

_comment_p = is_comm

token_cont_re = r'\s* &'
token_cont_re = re.compile(token_cont_re,re.X)

def kill_cont(l):
    return l[:l.find('&')]

def kill_token_cont(l):
    if token_cont_re.match(l):
        return l[l.find('&')+1:]
    return l
        
def _fjoin(asm):
    '''assemble a logical line from the assembled
    lines come in from the assembler as

    [[continuation]*,primary line]

    continuations are also structured as
    [continued_line, [comments]*]
    '''

    rawline = ''.join(flatten(asm))

    internal_comments = []
    (conts,prim) = asm
    current_line = []
    for (cl,comments) in conts:
        (l,eol_comm) = kill_bang_comment(cl)
        if eol_comm:
            internal_comments.append(eol_comm)
        internal_comments.extend(comments)
        cl = kill_cont(kill_token_cont(cl))
        current_line.append(cl)
    (prim,eol_comm) = kill_bang_comment(kill_token_cont(chomp(prim)))
    if eol_comm:
        internal_comments.append(eol_comm)
    current_line.append(prim)

    joined    = ''.join(current_line)
    return (rawline,joined,internal_comments)

cont = pred(is_cont)
_comm = pred(is_comm)
prim = pred(lambda l: not (is_comm(l) or is_cont(l)))

_a_stmt = seq(star(seq(cont,star(_comm))),prim)

class freefmt(object):
    'hold helper functions for free fmt'

    a_stmt    = staticmethod(_a_stmt)
    comm      = staticmethod(_comm)
    comment_p = staticmethod(_comment_p)
    fjoin     = staticmethod(_fjoin)
