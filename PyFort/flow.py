'''
Module to control the flow_line method
'''
from fixedfmt     import fixedfmt
from freefmt      import freefmt
from PyUtil.chomp import chomp

_fixedFormatStart=' ' * 6
_freeFormatStart=''
_fixed_line_len = 72

def _fixed_flow_line(l,cont='+'):
    '''given a long line, write it out as a series of continued lines'''
    comment_p = fixedfmt.comment_p
    l1 = chomp(l)
    if comment_p(l) or (len(l1) <= line_len):
        return l

    rv = l1[0:line_len] + '\n'
    rem = l1[line_len:]
    while len(rem) > (line_len-6):
        tmp  = rem[0:(line_len-6)]
        rv  += ' ' * 5 + cont + tmp + '\n'
        rem  = rem[(line_len-6):]
    if len(rem) > 0:
        rv  += ' ' * 5 + cont + rem
    return rv

_free_line_len = 80

def _free_flow_line(l):
    'given a long line l, write it out as a series of continued lines'
    comment_p = freefmt.comment_p
    fll = line_len
    fl2 = fll - 6
    cont = '&'
    l1 = chomp(l)
    if comment_p(l) or (len(l1) <= fll):
        return l

    rv = l1[0:fll] + cont + '\n'
    rem = l1[fll:]
    while len(rem) > fl2:
        tmp  = rem[0:fl2]
        rv  += ' ' * 5  + cont + tmp + cont + '\n'
        rem  = rem[fl2:]
    if len(rem) > 0:
        rv  += ' ' * 5  + cont + rem + '\n'
    return rv

def flow_comment(l):
    '''given a long comment, write it out as a series of continued lines'''
    l1 = chomp(l)
    rv = l1[0:line_len] + '\n'
    rem = l1[line_len:]
    while len(rem) > (line_len-2):
        tmp = rem[0:(line_len-2)]
        rv += 'C '+tmp+'\n'
        rem = rem[(line_len-2):]
    if len(rem) > 0:
        rv += 'C ' + rem+'\n'
    return rv

# the default is fixed format:
flow_line = _fixed_flow_line
formatStart = _fixedFormatStart
line_len = _fixed_line_len

def setFixedOrFreeFormatting(switch=True):
    global flow_line
    flow_line = (switch and _free_flow_line) or _fixed_flow_line
    global formatStart
    formatStart = (switch and _freeFormatStart) or _fixedFormatStart
    _line_len = (switch and _free_line_len) or _fixed_line_len

freeInput = False
freeOutput = False

# default output format is the same as the input format
def setFixedOrFreeFormat(free_input=False,free_output=None):
    global freeInput
    global freeOutput
    freeInput = free_input
    freeOutput = free_output
    if freeOutput == None:
        freeOutput = free_input
    setFixedOrFreeFormatting(freeOutput)


def setLineLength(length):
    line_len = length
