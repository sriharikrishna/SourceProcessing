'''
Logical line iterator for free format lines
'''
from _Setup import *
import fortStmts as fs
from kill_bang import kill_bang_comment
import re
import string
import PyUtil.assembler as as
import PyUtil.buf_iter as b
from PyUtil.chomp import chomp
from PyUtil.flatten import flatten
from fortScan import scan1
import PyIR.mapper as mapper
import cStringIO as cs

_cont_re = r'& \s* $'
_cont_re = re.compile(_cont_re,re.X)

_comment_re = r'\s* !'
_blank_re   = r' \s* $'

_comm_re    = r'(%s | %s)' % (_comment_re,_blank_re)
_comm_re    = re.compile(_comm_re,re.X)

_lineno_re = re.compile(r'\s* (\d+) \s*',re.X)
_lead_re   = re.compile(r'([\d\s]*) (.+) $',re.X)

def is_cont(line):
    '''check to see if line is continued (assuming no bang comments)'''

    (l,r) = kill_bang_comment(line)
    return _cont_re.search(l)

def is_comm(line):
    '''check to see if line is a comment line'''
    return _comm_re.match(line)

token_cont_re = r'\s* &'
token_cont_re = re.compile(token_cont_re,re.X)

def kill_cont(l):
    return l[:l.find('&')]

def kill_token_cont(l):
    if token_cont_re.match(l):
        return l[l.find('&')+1:]
    return l
        
__leadtbl = string.maketrans(string.digits,' ' * 10)
def _makelead(s):
    return s.translate(__leadtbl)

def fjoin(asm):
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
    prim = kill_token_cont(chomp(prim))
    current_line.append(prim)

    joined    = ''.join(current_line)
    if joined[0] == '\t':
        joined = ' ' * 8 + joined[1:]

    m                = _lineno_re.match(joined)
    lineno           = m and int(m.group(1))
    leadstr          = _lead_re.match(joined)
    lead,rest        = leadstr.groups()
    
    scan,scan_rm     = scan1.scan(rest)

    rv               = fs.parse(scan)
    rv.rm            = scan_rm
    rv.rawline       = rawline
    rv.lineno        = lineno
    rv.lead          = lead
    rv.internal_com  = internal_comments
    
    return rv

cont = as.pred(is_cont)
comm = as.pred(is_comm)
prim = as.pred(lambda l: not (is_comm(l) or is_cont(l)))

cblk = as.plus(comm)
cblk = as.treat(cblk,lambda l: fs.Comments(''.join(l)))

stmt = as.seq(as.star(as.seq(cont,as.star(comm))),prim)
stmt = as.treat(stmt,fjoin)

a_line = as.disj(cblk,stmt)

class _LineIter(mapper._Map):
    '''yield logical lines where a logical line is has all
    of the continuation lines joined together.
    Also, comments are blocked together.

    src should be a file-like object (yielding unprocessed lines)
    '''
    def __init__(self,src):
        self.lines = as.vgen(a_line,b.buf_iter(src))

class LineIterFile(_LineIter):
    def __init__(self,fname):
        _LineIter.__init__(self,open(fname))

class LineIterStr(_LineIter):
    def __init__(self,here):
        _LineIter.__init__(self,cs.StringIO(here))

def stmt_iter_file(fname):
    'given a file name, return the stmt_iter derived from the file'
    return as.vgen(a_line,b.buf_iter(open(fname)))

def stmt_iter_str(here):
    'given a string, return the stmt_iter derived from the string'
    return as.vgen(a_line,b.buf_iter(cs.StringIO(here)))
