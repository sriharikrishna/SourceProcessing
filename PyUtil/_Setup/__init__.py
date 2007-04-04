import sys,os.path
mypath = sys.path[0]

libpath = os.path.normpath(os.path.join(sys.path[0],'..'))

sys.path.insert(0,libpath)
