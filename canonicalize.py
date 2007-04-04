#!/usr/bin/python
'''
canonicalization
'''
import sys
import cPickle as cp

from PyFort.fortContextFile import fortContextFile

from Canon.canon import canon_lexi,decl_lexi,_verbose

from PyUtil.errors import UserError, ScanError, ParseError
from PyUtil.assembler import AssemblerException as AsE
from PyUtil.l_assembler import AssemblerException as LAsE
 
def hook1(self):
    if hasattr(self.toplev,'_scnt'):
        pass
    else:
        self.toplev._scnt = 0
        self.toplev.slice_undo = dict()

def main():
  _verbose   = True
  if (len(sys.argv)!=2) :
    print >>sys.stderr, "Error: missing argument!"
    print >>sys.stderr, "Usage: ", sys.argv[0], " <input file name>"
    return 1
  else :
    try: 
      f1 = fortContextFile(sys.argv[1],hook1)
      f1rw = f1.rewrite(canon_lexi).rewrite(decl_lexi)
      slcf = open('reslice.dat','w')
      pp = cp.Pickler(slcf)
      pp.dump(f1rw.lines[0].ctxt.toplev.slice_undo)
      slcf.close()
      f1rw.printit()
    except UserError,e : 
      print >>sys.stderr, "Error: ", e.msg
      return 1 
    except ScanError,e : 
      print >>sys.stderr, "Error: scanner fails at line:", e.aFortLine.line
      print >>sys.stderr, " tokens scanned ok: ", e.scanned
      print >>sys.stderr, "    unable to scan: ", e.rest
      print >>sys.stderr, " This failure is likely due to possibly legal but unconventional Fortran, such as unusual spacing.", e.rest
      print >>sys.stderr, " Please consider modifying your source code.", e.rest
      return 1 
    except ParseError,e : 
      print >>sys.stderr, "Error: parser fails to assemble tokens in scanned line:", e.scannedLine, " as ", e.target
      return 1 
    except AsE, e: 
      print >>sys.stderr, "Internal Error: parser failed:", e.msg
      return 1 
    except LAsE, e: 
      print >>sys.stderr, "Internal Error: parser failed:", e.msg
      return 1 
    return 0

if __name__ == "__main__":
  sys.exit(main())
