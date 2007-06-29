'''read Fortran Lines from sources into a Line object'''

from _Setup import *
from PyUtil.flatten import flatten
from PyUtil.assembler import *
from PyUtil.chomp import chomp
import re

fmt = 'fix'

if fmt == 'fix':
    from fixedfmt import fjoin,comm,a_stmt,comment_p
else:
    from freefmt import fjoin,comm,a_stmt,comment_p

_ad_directive_prefix_re    = r'\$OpenAD\s*\$?' # comments that have $OpenAD w opt trailing $

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

    sep = fjoin(dta)
    return fline(sep[0],sep[1],sep[2])

def fline_from_line(line):

    if comment_p(line):
        return cline([line])

    return fline(flow_line(line),line,[])

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

cblk   = treat(plus(comm),cline)
stmt   = treat(a_stmt,fline_from_asm)

a_line = disj(cblk,stmt)
