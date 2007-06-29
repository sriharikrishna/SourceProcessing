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
import cStringIO as cs

comre   = re.compile(r'''[^\d\s]''')
blankre = re.compile(r'''\s* $''',re.X)
shoutre = re.compile(r''' \s* !''',re.X)

def comment_p(l):
    '''given a line l, return true if l is a comment (or blank) line'''
    return comre.match(l) or blankre.match(l) or shoutre.match(l)

def cont_p(l):
    '''given a line l, return true if l is a continuation line'''
    contre = re.compile(r'''\s{5} \S''',re.X)
    return contre.match(l)

def strt_p(l):
    '''given a line l, return true if l is a starting statement'''
    return not (comment_p(l) or cont_p(l))

_lineno_re = re.compile(r'\s* (\d+) \s*',re.X)
_lead_re   = re.compile(r'([\d\s]*) (.+) $',re.X)

__leadtbl = string.maketrans(string.digits,' ' * 10)

def _makelead(s):
    return s.translate(__leadtbl)

def fjoin(asm):
    '''a true fortran line dta structure comes in as:
    [initial stmt [continuation_lines]]

    continuation_lines are structures as well:
        [[comments] continuation_line]
    '''
    rawline = ''.join(flatten(asm))
    internal_comments = []

    (strt,conts) = asm
    (s,c)        = kill_bang_comment(chomp(strt))
    current_line = [s]

    if c:
        internal_comments.append(chomp(c))

    for (comments,cl) in conts:
        (l,eol_comm) = kill_bang_comment(chomp(cl))
        if eol_comm:
            internal_comments.append(eol_comm)
        internal_comments.extend(comments)
        l = l[6:]
        current_line.append(l)

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
    rv.lead          = _makelead(lead)
    rv.internal_com  = internal_comments
    
    return rv

strt   = as.pred(strt_p)
comm   = as.pred(comment_p)
cont   = as.pred(cont_p)

cblk   = as.plus(comm)
cblk   = as.treat(cblk,lambda l: fs.Comments(''.join(l)))

stmt   = as.seq(strt,as.star(as.seq(as.star(comm),cont)))
stmt   = as.treat(stmt,fjoin)

a_line = as.disj(cblk,stmt)

def stmt_iter_file(fname):
    'given a file name, return the stmt_iter derived from the file'
    return as.vgen(a_line,b.buf_iter(open(fname)))

def stmt_iter_str(here):
    'given a string, return the stmt_iter derived from the string'
    return as.vgen(a_line,b.buf_iter(cs.StringIO(here)))
