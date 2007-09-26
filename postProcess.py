#!/usr/bin/python
'''
post processing
'''
import sys
import os
from optparse import OptionParser

from PyFort.fortParseFile import fortParseFile as fpf
from PyIR.prog1 import Prog1
import PyUtil.lexi_visitor as lv
import PyFort.fortStmts as fs
import PyFort.fortExp   as fe
import PyIR.mutable_tree as mt
from PP.xaifpp import val_deriv_m as valm
from PP.xaifpp import type_active_m as actm
from PP.xaifpp import add_active_module as addm

from PyUtil.errors import UserError, ScanError, ParseError
from PyUtil.assembler import AssemblerException as AsE
from PyUtil.l_assembler import AssemblerException as LAsE

def normal(self,arg):
    'attach self to prog repr'
    arg.attach(self)

def exec_s(self,arg):
    'mutate an executable statement, and attach'
    self.mutate(self)
    arg.attach(self)

def noop(self,*a):
    'do nothing'
    pass

def main():
  usage = '%prog [options] <input_file>'
  opt = OptionParser(usage=usage)
  opt.add_option('-f','--forward',dest='fwd',
                 help="use forward mode postprocessing",
                 action='store_true',default=False)
  opt.add_option('--free',dest='free',
                 help='free format source',
                 action='store_true',default=False)
  opt.add_option('-t','--template',dest='tfile',
                 default='ad_template.f',
                 help='template file (defaults to ad_template.f)',
                 metavar='<template_file>')
  opt.add_option('-i','--inline',dest='ifile',
                 default='ad_inline.f',
                 help='inline definition file (defaults to ad_inline.f)',
                 metavar='<inline_file>')
  opt.add_option('-o','--output',dest='ofile',
                 help='output file (defaults to <input_file.base>.pp.<input_file.extension>)',
                 metavar='<output_file>')
  config, args = opt.parse_args()
  if len(args) != 1:
     opt.error("expect input file argument")
  fn         = args[0]
  try: 
    (base,ext) = os.path.splitext(fn)
    if config.ofile is None:
        newfn      = base + '.pp' + ext
    else:    
        newfn      = config.ofile
    exp1  = mt.LexiMutator([(fe._Exp,noop),
                            (fe.App,valm),
                            ])
    vstr  = lv.LexiVisitor([(fs.GenStmt,normal),
                            (fs.Exec,exec_s),
                            (fs.DrvdTypeDecl,actm),
                            (fs.UseStmt,addm),
                            ],'build')
    xaifv = lv.MultiLexiVisitor(vstr,exp1)
    Prog1(xaifv,fpf(fn,config.free).lines).writeit(newfn)
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
