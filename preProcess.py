#!/usr/bin/env python
'''
canonicalization
'''
import sys
import cPickle as cp
from optparse import OptionParser

from PyUtil.errors import UserError, ScanError, ParseError
from PyUtil.assembler import AssemblerException
from PyUtil.l_assembler import AssemblerException as l_AssemblerException

from PyFort.flow import free_flow
from PyFort.fortUnit import Unit,fortUnitIterator
import PyFort.fortStmts as fs

from Canon.canon import UnitCanonicalizer,CanonError
 
def main():
    usage = '%prog [options] <input_file>'
    opt = OptionParser(usage=usage)
    opt.add_option('--free',
                   dest='isFreeFormat',
                   help="input source is free format",
                   action='store_true',
                   default=False)
    opt.add_option('-o',
                   '--output',
                   dest='outputFile',
                   help='set output file (defaults to stdout)',
                   metavar='<output_file>',
                   default=None)
    opt.add_option('-v',
                   '--verbose',
                   dest='isVerbose',
                   help='turns on verbose debugging output',
                   action='store_true',
                   default=False)
    config, args = opt.parse_args()

    # Set input file
    if len(args) != 1:
        opt.error("expect input file argument")
    inputFile = args[0]

    # set free/fixed format
    free_flow(config.isFreeFormat) 

    # set verbosity
    UnitCanonicalizer.setVerbose(config.isVerbose)
    Unit.setVerbose(config.isVerbose)

    try: 
        if config.outputFile: out = open(config.outputFile,'w')
        else: out = sys.stdout
        for aUnit in fortUnitIterator(inputFile,config.isFreeFormat):
            UnitCanonicalizer(aUnit).canonicalizeUnit().printit(out)
        if config.outputFile: out.close()
    except CanonError,e:
        print >>sys.stderr,'Canoncalization Error on line '+str(e.lineNumber)+':\n',e.msg
        return 1
    except UserError,e:
        print >>sys.stderr,'UserError:',e.msg
        return 1 
    except ScanError,e: 
        print >>sys.stderr,'ScanError: scanner fails at line '+str(e.lineNumber)+':'
        print >>sys.stderr,e.aFortLine
        print >>sys.stderr,(len(e.aFortLine)-len(e.rest))*' '+'^'
        print >>sys.stderr,''
        print >>sys.stderr,"Tokens scanned ok: ", e.scanned,'\tUnable to scan: "'+e.rest+'"'
        print >>sys.stderr,''
        print >>sys.stderr,"This failure is likely due to possibly legal but unconventional Fortran,"
        print >>sys.stderr,"such as unusual spacing. Please consider modifying your source code."
        return 1 
    except ParseError,e: 
        print >>sys.stderr,'ParseError: parser fails to assemble tokens in scanned line '+str(e.lineNumber)+':'
        print >>sys.stderr,e.scannedLine
        print >>sys.stderr,"as",e.target
        return 1 
    except AssemblerException,e:
        print >>sys.stderr,"AssemblerError: parser failed:",e.msg
        return 1 
    except l_AssemblerException,e:
        print >>sys.stderr,"ListAssemblerError: parser failed:",e.msg
        return 1 
    return 0

if __name__ == "__main__":
    sys.exit(main())

