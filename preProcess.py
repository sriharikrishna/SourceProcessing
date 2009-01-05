#!/usr/bin/env python
'''
canonicalization
'''
import sys
from optparse import OptionParser

from PyUtil.errors import UserError, ScanError, ParseError
from PyUtil.assembler import AssemblerException
from PyUtil.l_assembler import AssemblerException as ListAssemblerException
from PyUtil.symtab import Symtab,SymtabError

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
    opt.add_option('-m',
                   '--mode',
                   dest='mode',
                   help='set default options for forward and reverse mode (this currently includes hoisting of constant expressions).  Additional specific settings will override those set here.',
                   metavar='<f|r>',
                   default=None)
    opt.add_option('',
                   '--r8',
                   dest='r8',
                   help="set default size of REAL to 8 bytes",
                   action='store_true',
                   default=False)
    opt.add_option('-H',
                   '--hoist-non-string-constants',
                   dest='hoistConstantsFlag',
                   help='enable the hoisting of non-string constant arguments to subroutine calls (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('-S',
                   '--hoist-string-constants',
                   dest='hoistStringsFlag',
                   help='enable the hoisting of string constant arguments to subroutine calls (defaults to False)',
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

    # configure forward/reverse mode
    if config.mode:
        if config.mode[0] == 'f':
            UnitCanonicalizer.setHoistConstantsFlag(False)
            UnitCanonicalizer.setHoistStringsFlag(False)
        elif config.mode[0] == 'r':
            UnitCanonicalizer.setHoistConstantsFlag(True)
            UnitCanonicalizer.setHoistStringsFlag(False)

    # set symtab type defaults
    if config.r8:
        Symtab.setTypeDefaults((fs.DoubleStmt,[]),(fs.IntegerStmt,[]))
    else:
        Symtab.setTypeDefaults((fs.RealStmt,[]),(fs.IntegerStmt,[]))

    # set free/fixed format
    free_flow(config.isFreeFormat) 

    # configure constant expression hoisting
    if config.hoistConstantsFlag:
        UnitCanonicalizer.setHoistConstantsFlag(config.hoistConstantsFlag)
    # configure string hoisting
    if config.hoistStringsFlag:
        UnitCanonicalizer.setHoistStringsFlag(config.hoistStringsFlag)

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
        print >>sys.stderr,'\nCanonicalization Error on line '+str(e.lineNumber)+': ',e.msg
        return 1
    except SymtabError,e:
        debugstr = e.entry and e.entry.debug('unknown') \
                            or ''
        print >>sys.stderr,'\nSymtabError on line '+str(e.lineNumber)+':',e.msg,' for entry',debugstr
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
    except ListAssemblerException,e:
        print >>sys.stderr,"ListAssemblerError: parser failed:",e.msg
        return 1 
    return 0

if __name__ == "__main__":
    sys.exit(main())

