from _Setup import *

from PyUtil.flatten import flatten
from PyUtil.chomp import chomp
from kill_bang import *
from PyUtil.assembler import *

def cont_p(l):
    '''given a line l, return true if l is a continuation line'''
    contre = re.compile(r'''\ {5} \S''',re.X)
    return contre.match(l)

def comment_p(l):
    '''given a line l, return true if l is a comment (or blank) line'''
    blankre = re.compile(r'''\s* $''',re.X)
    return nbl_comment_p(l) or blankre.match(l)

def stmt_p(l):
    '''given a line l, return true if l is a starting statement'''
    return not (comment_p(l) or cont_p(l))

comm   = pred(comment_p)

strt   = pred(stmt_p)
cont   = pred(cont_p)
a_stmt   = seq(strt,star(seq(star(comm),cont)))

_comre   = re.compile(r'''[^\d\s]''')
_shoutre = re.compile(r''' \s* !''',re.X)

def nbl_comment_p(l):
    'return true if line is a (nonblank) comment'
    return _comre.match(l) or _shoutre.match(l)

def fjoin(dta):
    '''take in a pattern match nested seq data structure for a fortran stmt,
    return tuple (rawline,joined_line,internal_comments)

    For fixed format lines:
    a true fortran line dta structure comes in as:
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

    return (rawline,line,internal_comments)

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
