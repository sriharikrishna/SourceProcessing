from _Setup import *
import flow
import PyFort.fortStmts as fs
from PyUtil.flatten import flatten

import re
_label_re = re.compile(r'(\s*)(\d+)')
_lead_re   = re.compile(r'(\s*)')

def process_fort_stmt(stmt_tuple,lineNumber,jlf):
    '''process a stmt tuple = (rawline,joined_line,internal_comments), using
    jlf routine on the jointed_line part
    convert !@#$% leading tabs into spaces
    extract line #
    compute leading spaces
    process the joined line
    obj = jlf(joined_line)
    obj.rawline = raw
    obj.internal = internal_comments
    obj.label = label
    obj.lead = lead
    '''
    (raw,jl,intl) = stmt_tuple
    jl = format_linelead(jl)
    m = _label_re.match(jl)
    label = m and int(m.group(2))

    # lead shouldn't include fixed flow spacing (first 6 places)
    if flow.freeInput:
        linelead = m and m.end(0) or 0
        lead = linelead * ' '+_lead_re.match(jl[linelead:]).group(1)
        raw = jl[len(lead):].strip()
        init_len = len(lead)
    else:
        lead = _lead_re.match(jl[6:]).group(1)
        raw = jl[6:].lstrip()
        init_len = 6+len(lead)
    obj = jlf(jl[init_len:],lineNumber)
    if isinstance(obj,list):
        obj[0].internal = intl
        obj[0].label = label
        for anObj in obj:
            anObj.lead = lead
    else:
        obj.rawline = raw
        obj.internal = intl
        obj.label = label
        obj.lead = lead
    return obj

def process_fort_cmnt(dta,lineNumber,cmnt_list_fn):
    obj         = cmnt_list_fn(dta,lineNumber)
    obj.rawline = ''.join(flatten(dta))
    return obj

def format_linelead(jl):
    if flow.freeInput:
        if jl[0] == '\t':
            jl = ' ' * 8 + jl[1:]
    else:
        i = 0
        while i < 6:
            if jl[i] == '\t':
                jl = ' '*6 + jl[i+1:]
                if i != 0:
                    jl = jl[:i-1] + jl
                break
            i += 1
    return jl
