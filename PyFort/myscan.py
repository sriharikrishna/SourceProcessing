import sys
if sys.version_info[1] == 4:
    from sre import Scanner
else:
    from re import Scanner
