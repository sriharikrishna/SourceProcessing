'''
Module to control the flow_line method
'''
from fixedfmt     import fixedfmt
from freefmt      import freefmt
from PyUtil.chomp import chomp

_fixedFormatStart=' ' * 6
_freeFormatStart=''
_fixed_line_len = 72
_fixed_comment = 'C '

def _fixed_flow_line(l,cont='+'):
    '''given a long line, write it out as a series of continued lines'''
    comment_p = fixedfmt.comment_p
    fl1 = line_len
    fl2 = fl1 - 6
    l1 = chomp(l)
    if comment_p(l) or (len(l1) <= fl1):
        return l
    
    rv = l1[0:fl1] + '\n'
    rem = l1[fl1:]
    while len(rem) > fl2:
        tmp  = rem[0:fl2]
        rv  += ' ' * 5 + cont + tmp + '\n'
        rem  = rem[fl2:]
    if len(rem) > 0:
        rv  += ' ' * 5 + cont + rem
    return rv

_free_line_len = 80
_free_comment = '! '

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
    fl1 = line_len
    fl2 = line_len-2
    rv = l1[0:fl1] + '\n'
    rem = l1[fl1:]
    while len(rem) > fl2:
        tmp = rem[0:fl2]
        rv += commentInit+tmp+'\n'
        rem = rem[fl2:]
    if len(rem) > 0:
        rv += commentInit+ rem+'\n'
    return rv

# the default is fixed format:
flow_line = _fixed_flow_line
formatStart = _fixedFormatStart
line_len = _fixed_line_len
commentInit = _fixed_comment

inputFormat = 'fixed'
outputFormat = 'fixed'

# default output format is the same as the input format
def setFixedOrFreeFormat(input_format='fixed',output_format=None):
    '''sets input and output formats. When the output format is not specified, it defaults to be the same as the input format. When the input format is not specified, it defaults to fixed format.'''
    global inputFormat
    global outputFormat
    inputFormat = input_format
    outputFormat = output_format
    if outputFormat == None:
        outputFormat = input_format
    _setOutputFormat(outputFormat=='free')

def _setOutputFormat(switch=True):
    '''sets the output format'''
    global flow_line
    flow_line = (switch and _free_flow_line) or _fixed_flow_line
    global formatStart
    formatStart = (switch and _freeFormatStart) or _fixedFormatStart
    global commentInit
    commentInit = (switch and _free_comment) or _fixed_comment
    global line_len
    if line_len == _fixed_line_len:
        line_len = (switch and _free_line_len) or _fixed_line_len
        
def setLineLength(length):
    global line_len
    line_len = int(length)
