'''read Fortran Lines from sources into a Line object'''

from _Setup import *
from PyUtil.flatten import flatten
from PyUtil.assembler import *
from PyUtil.chomp import chomp
from fortScan import q_re,qq_re
import re


def ident(self):
    '''generic identity function for default mappers'''
    return self

class anyFortLine(object):
    '''generic line class for any fortran line'''
    pass

class fline(anyFortLine):
    '''a non-comment (semantic) class for a fortran line'''

    def __init__(self,rawline,line,internal):

        self.rawline  = rawline
        self.line     = line
        self.internal = internal

class cline(anyFortLine):
    '''a comment (or blank) line'''
    def __init__(self,dta):
        self.rawline = ''.join(flatten(dta))

    def comment_list(self):
        return self.rawline.splitlines()

def fline_from_asm(dta):
    '''a true fortran line dta structure comes in as:
    [initial stmt [continuation_lines]]

    continuation_lines are structures as well:
        [[comments] continuation_line]
    '''

    rawline           = ''.join(flatten(dta))
    internal_comments = []
    (line,c)          = kill_bang_comment(chomp(dta[0]))
    if not c == '':
        internal_comments.append(chomp(c))

    for cont in dta[1]:
        internal_comments.extend([ chomp(l) for l in cont[0]])
        (l,c) = kill_bang_comment(chomp(cont[1])[6:])
        if not c == '':
            internal_comments.append(chomp(c))
        line += l

    return fline(rawline,line,internal_comments)

def fline_from_line(line):

    if comment_p(line):
        return cline([line])

    return fline(flow_line(line),line,[])

def comment_p(l):
    '''given a line l, return true if l is a comment (or blank) line'''
    comre   = re.compile(r'''[^\d\s]''')
    blankre = re.compile(r'''\s* $''',re.X)
    shoutre = re.compile(r''' \s* !''',re.X)
    return comre.match(l) or blankre.match(l) or shoutre.match(l)

def cont_p(l):
    '''given a line l, return true if l is a continuation line'''
    contre = re.compile(r'''\ {5} \S''',re.X)
    return contre.match(l)

def stmt_p(l):
    '''given a line l, return true if l is a starting statement'''
    return not (comment_p(l) or cont_p(l))

def flow_line(l,cont='+'):
    '''given a long line, write it out as a series of continued lines'''
    l1 = chomp(l)
    if comment_p(l) or (len(l1) <= 72):
        return l

    rv = l1[0:72] + '\n'
    rem = l1[72:]
    while len(rem) > 66:
        tmp  = rem[0:66]
        rv  += ' ' * 5 + cont + tmp + '\n'
        rem  = rem[66:]
    if len(rem) > 0:
        rv  += ' ' * 5 + cont + rem + '\n'
    return rv

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

strt   = pred(stmt_p)
comm   = pred(comment_p)
cont   = pred(cont_p)

cblk   = treat(plus(comm),cline)
stmt   = treat(seq(strt,star(seq(star(comm),cont))),fline_from_asm)

a_line = disj(cblk,stmt)
