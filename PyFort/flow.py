'''
Module to control the flow_line method
'''
from fixedfmt     import fixedfmt
from freefmt      import freefmt
from PyUtil.chomp import chomp

def _fixed_flow_line(l,cont='+'):
    '''given a long line, write it out as a series of continued lines'''
    comment_p = fixedfmt.comment_p
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

_free_line_len = 80

def _free_flow_line(l):
    'given a long line l, write it out as a series of continued lines'
    comment_p = freefmt.comment_p
    fll = _free_line_len
    fl2 = fll - 6
    cont = '&'
    l1 = chomp(l)
    if comment_p(l) or (len(l1) <= fll):
        return l

    rv = l1[0:fll] + '&\n'
    rem = l1[fll:]
    while len(rem) > fl2:
        tmp  = rem[0:fl2]
        rv  += ' ' * 5 + cont + tmp + '\n'
        rem  = rem[fl2:]
    if len(rem) > 0:
        rv  += ' ' * 5 + cont + rem + '\n'
    return rv

flow_line = _fixed_flow_line

def free_flow(switch=True):
    global flow_line
    flow_line = (switch and _free_flow_line) or _fixed_flow_line

