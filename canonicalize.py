#!/usr/bin/python
'''
canonicalization
'''
import sys
import cPickle as cp
from optparse import OptionParser

from PyFort.fortContextFile import fortContextFile,fortUnitContextFile
from PyFort.flow import free_flow

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
  usage = '%prog [options] <input_file>'
  opt = OptionParser(usage=usage)
  opt.add_option('--free',dest='free',
                 help="free format source",
                 action='store_true',default=False)
  opt.add_option('-o','--output',dest='ofile',
                 help='output file (defaults to <input_file.base>.pp.<input_file.extension>)',
                 metavar='<output_file>')
  config, args = opt.parse_args()
  if len(args) != 1:
     opt.error("expect input file argument")
  fn         = args[0]
  
  free_flow(config.free)
  try: 
      fu = fortUnitContextFile(fn,config.free,hook1)
      for f1 in fu:
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
      print >>sys.stderr, " This failure is likely due to possibly legal but unconventional Fortran,"
      print >>sys.stderr, " such as unusual spacing. Please consider modifying your source code."
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
