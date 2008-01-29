from _Setup import *
from PyUtil.flatten import flatten

import re
_lineno_re = re.compile(r'(\s*)(\d+)')
_lead_re   = re.compile(r'(\s*)')

def process_fort_stmt(stmt_tuple,jlf):
    '''process a stmt tuple = (rawline,joined_line,internal_comments), using
    jlf routine on the jointed_line part
    convert !@#$% leading tabs into spaces
    extract line #
    compute leading spaces
    process the joined line
    obj = jlf(joined_line)
    obj.rawline = raw
    obj.internal = internal_comments
    obj.lead     = lead
    obj.lineno   = lineno
    '''
    (raw,jl,intl) = stmt_tuple
    if jl[0] == '\t':
        jl = ' ' * 8 + jl[1:]

    m        = _lineno_re.match(jl)
    lineno   = m and int(m.group(2))
    linelead = m and m.end(0) or 0
    lead     = linelead * ' '+_lead_re.match(jl[linelead:]).group(1)
    obj      = jlf(jl[len(lead):])

    obj.rawline  = raw
    obj.internal = intl
    obj.lineno   = lineno
    obj.lead     = lead

    return obj

def process_fort_cmnt(dta,cmnt_list_fn):
    obj         = cmnt_list_fn(dta)
    obj.rawline = ''.join(flatten(dta))
    return obj

