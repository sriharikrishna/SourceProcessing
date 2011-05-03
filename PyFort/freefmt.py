from _Setup import *

from PyUtil.flatten import flatten
from PyUtil.chomp import chomp
from kill_bang import *
from PyUtil.assembler import *
from fortScan import postluded_lo_q_patn, preluded_ro_q_patn

_cont_re = r'& (([\s]*[!].*) | \s*) $'
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

_delimROpenREs = {
    "'":re.compile(preluded_ro_q_patn("'"),re.X),
    '"':re.compile(preluded_ro_q_patn('"'),re.X)}
_delimLOpenREs = {
    "'":re.compile(postluded_lo_q_patn("'"),re.X),
    '"':re.compile(postluded_lo_q_patn('"'),re.X)}

def hasOpenQuotation(line,delimiter):
    '''check to see if line has an open quatation:
       if delimiter is not None then we expect line to be a continuation 
          of an open quotation and look for a right-open quotation 
       if delimiter is None then we look for left - open quotation'''
    if (delimiter and  _delimLOpenREs[delimiter].match(line)):
        return (True,delimiter)
    else:
        for d,theRe in _delimROpenREs.items():
            if (theRe.match(line)):
                return (True,d)
    return (False,None)
        
_comment_p = is_comm

token_cont_re = r'\s* &'
token_cont_re = re.compile(token_cont_re,re.X)

def kill_cont(l):
    cont_match = _cont_re.search(l)
    if cont_match:
        return l[:cont_match.start()]
    return l

def kill_token_cont(l):
    token_match = token_cont_re.match(l)
    if token_match:
        return l[token_match.end():]
    return ' '+l.lstrip()
        
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
    initial_line = True
    delimOQ = None
    currDelimOQ = None
    hasOQ = False
    for (cl,comments) in conts:
        eol_comm=None
        (hasOQ,currDelimOQ)=hasOpenQuotation(cl,delimOQ)
        if (hasOQ) :
            if (delimOQ):
                if (currDelimOQ == delimOQ):
                    # this is the end of the open quotation
                    (cl,eol_comm) = kill_bang_comment_lo_q(cl,delimOQ)
                    delimOQ = None
                else :
                    # it is conceivable that one has embedded the other quoation
                    # mark but it ends up on a separate line but then
                    # Note: no comments are permitted
                    (cl,eol_comm) = kill_bang_comment(cl)
            else:
                # this is the start of the open quoatation
                delimOQ = currDelimOQ
                (cl,eol_comm) = kill_bang_comment(cl)
        else: 
            (cl,eol_comm) = kill_bang_comment(cl)
        if eol_comm:
            internal_comments.append(eol_comm)
        internal_comments.extend(comments)
        if initial_line:
            cl = kill_cont(cl)
            initial_line = False
        else:
            cl = kill_cont(kill_token_cont(cl))
        current_line.append(cl)
    # dealing with prim
    if not initial_line:
        prim = kill_token_cont(prim)
    prim=chomp(prim)
    (hasOQ,currDelimOQ)=hasOpenQuotation(prim,delimOQ)
    if (hasOQ):
        if (delimOQ and currDelimOQ == delimOQ):
            # this is the end of the open quotation
            (prim,eol_comm) = kill_bang_comment_lo_q(prim,delimOQ)
            delimOQ = None
            hasOQ=False
        else :
            # this is parsed as a new open quoatation but there is no
            # continuation so it probably is seen in the comment part.
            if (prim.rfind(currDelimOQ)>prim.rfind('!')):
                (prim,eol_comm) = kill_bang_comment(prim)
                hasOQ=False
    else:
        (prim,eol_comm) = kill_bang_comment(prim)
    if (delimOQ or hasOQ):
        raise Exception("open quotation bug: hasOQ="+str(hasOQ)+" delimOQ="+str(delimOQ)+" currDelimOQ="+str(currDelimOQ)+" for "+str(asm))
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
