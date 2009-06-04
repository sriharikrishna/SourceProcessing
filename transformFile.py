#!/usr/bin/env python
'''
Rudimentary transformations on non-transformed files
'''
import sys
from optparse import OptionParser

from PP.transformActiveVariables import TransformActiveVariables
from PyFort.fortUnit import Unit,fortUnitIterator
from PyUtil.symtab import Symtab,SymtabError
import PyFort.fortStmts as fs

def main():
    usage = '%prog <input_file>'
    opt = OptionParser(usage=usage)
    opt.add_option('-d',
                   '--vardefs',
                   dest='vardefs',
                   help='file with definitions for active variables',
                   default='activeVariableDefinitions.f')
    opt.add_option('-o',
                   '--output',
                   dest='output',
                   help='redirect output to file OUTPUT (default output is stdout)',
                   default=None)
    config,args = opt.parse_args()

    # Set input file
    if len(args) != 1:
        opt.error("expect exactly one argument <input_file>, given are "+str(len(args))+" which are:"+str(args)+" and the following options: "+str(config))
    inputFile = args[0]
    
    # set symtab type defaults
    Symtab.setTypeDefaults((fs.RealStmt,[]),(fs.IntegerStmt,[]))

    if config.output: 
        out = open(config.output,'w')
    else:
        out=sys.stdout

    TransformActiveVariables.getActiveDecls(config.vardefs)
    for aUnit in fortUnitIterator(inputFile,False):
        TransformActiveVariables(aUnit).transformFile().printit(out)

    if config.output: 
        out.close()

if __name__ == "__main__":
    sys.exit(main())
