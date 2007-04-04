'''Various regular expressions for fortran tokens
to use these expressions, be sure to re.compile with re.X
'''

import re
import myscan

def __str_intern(s):
    template = r"""
         (?:            # body of quote is a collection of chars
            [^%s\\] |   #    non-quote or backslash OR
            %s%s    |   #    repeated quote         OR
            \\.         #    backslashed anything
         )*             # 0 or more
"""
    return template % (s,s,s)

def __closed_q(s):
    return '(?x)' + s + __str_intern(s) + s

def __ro_q(s):
    prelude = r'''
    (?x)
    ^(?: [^%s] | (?: %s) )*'''

    return prelude % (s,__closed_q(s)) + s + __str_intern(s) + '$'

q_re     = __closed_q("'")
qq_re    = __closed_q('"')

ro_q_re  = __ro_q("'")
ro_qq_re = __ro_q('"')

id_re    = r'(?i)[_a-zA-Z$][\w$]*'
int_re   = r'(?x)\d+ (?:_ \w+)?'
dcoln_re = r'::'
conc_re  = r'//'
exp_re   = r'\*\*'
cmpr_re  = r'(?:>|<|=|/)='
symb_re  = r'[-+*/()=,;%:<>]'
dot_re   = r'''(?x)(?i)\.    # fortran "dotted" keywords
            (?:
                eqv    |
                neqv   |
                eq     |
                ne     |
                gt     |
                ge     |
                lt     |
                le     |
                and    |
                or     |
                not    |
                true   |
                false
             )
             \.
'''

floexp_re = r'''(?ix)
              [dDeE]  # exponent marker
              [-+]? # optional sign
              \d+   # exponent
'''

flonum_re = r'''(?ix)
             (?:       # EITHER
               (?:
                  \d+        # some digits
                  (?!%s)     # NOT followed by a dot kw, but ARE
                  (?:        # followed by
                     (?:     #  EITHER
                        \.         # decimal point
                        \d*        # 0 or digits
                        (?:%s)?    # optional exp { subst floexp_re }
                     ) |     #   OR
                     %s            # definite exp { subst floexp_re }
                    )
                  )  |  # OR
                    (?:    # no leading digits
                        \.       # decimal point
                        \d+      # digits after decimal pt
                        (?:%s)?  # optional exp { subst floexp_re }
                    )
               )
'''
flonum_re = flonum_re % ( dot_re,floexp_re,floexp_re,floexp_re )
flonum_re = flonum_re + r'(?:_\w+)?'

white_re  = r'\s+'

def s_ident(self,s):
    'simplest possible scanner function: return matched string'
    return s

scan1 = myscan.Scanner([
    (id_re,      s_ident),
    (conc_re,    s_ident),
    (dcoln_re,   s_ident),
    (exp_re,     s_ident),
    (cmpr_re,    s_ident),
    (symb_re,    s_ident),
    (dot_re,     s_ident),
    (q_re,       s_ident),
    (qq_re,      s_ident),
    (flonum_re,  s_ident),
    (int_re,     s_ident),
    (white_re,   None),
    ],re.I | re.X)
