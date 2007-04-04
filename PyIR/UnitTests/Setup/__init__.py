import sys,os.path

# print 'from setup package: ',sys.path[0]

mypath = sys.path[0]

libpath = os.path.normpath(os.path.join(sys.path[0],'..'))

# print 'new path to add: ',libpath

sys.path.insert(0,libpath)

# print 'new sys.path = ', sys.path

def preclip(s):
    'clip leading "\n" on string s'
    if s[0] == '\n':
        return s[1:]
    else:
        return s

def open_t(fname,mode='r',bufsize=-1):
    '''utility to open a file in the Tfiles directory'''
    return open(os.path.join(mypath,'Tfiles',fname),mode,bufsize)

def fname_t(fname):
    '''add the path to Tfiles to fname'''
    return os.path.join(mypath,'Tfiles',fname)

def asuite(*cases):
    'make a suite from a list of cases'
    from unittest import makeSuite
    rv = makeSuite(cases[0])
    for case in cases[1:]:
        rv.addTest(makeSuite(case))

    return rv

def runit(s):
    'standard way to run the test runner'
    from unittest import TextTestRunner
    TextTestRunner(verbosity=2).run(s)
