'''
Module to control the flow_line method
'''
from fixedfmt import fixedfmt
from PyUtil.chomp import chomp

def _static_flow_line(l,cont='+'):
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

flow_line = _static_flow_line
