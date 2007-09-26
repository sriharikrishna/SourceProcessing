'''read Fortran Lines from sources into a Line object'''

from _Setup import *
from PyUtil.flatten import flatten
from PyUtil.assembler import *
from PyUtil.chomp import chomp
from fixedfmt import fixedfmt
import flow
import re

fmt = 'fix'

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

class fortLine(object):
    def __init__(self,fmtobj=fixedfmt):
        self.fmtobj    = fmtobj
        a_stmt         = fmtobj.a_stmt
        comm           = fmtobj.comm
        cblk           = treat(plus(comm),cline)
        stmt           = treat(a_stmt,self.fline_from_asm)
        self.a_line    = disj(cblk,stmt)

    def fline_from_asm(self,dta):
        '''a true fortran line dta structure comes in as:
        [initial stmt [continuation_lines]]

        continuation_lines are structures as well:
        [[comments] continuation_line]
        '''

        fj  = self.fmtobj.fjoin
        sep = fj(dta)
        return fline(sep[0],sep[1],sep[2])

    def fline_from_line(self,line):
        comment_p = self.fmtobj.comment_p
        flow_line = flow.flow_line

        if comment_p(line):
            return cline([line])

        return fline(flow_line(line),line,[])
