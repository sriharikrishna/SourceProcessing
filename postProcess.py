#!/usr/bin/env python
'''
Postprocessing
'''
import sys
import os
import traceback
from optparse import OptionParser

from PyUtil.errors import UserError, ScanError, ParseError
from PyUtil.assembler import AssemblerException
from PyUtil.l_assembler import AssemblerException as ListAssemblerException
from PyUtil.symtab import Symtab,SymtabError
from PyUtil.debugManager import DebugManager

from PyIR.prog1 import Prog1

from PyFort.flow import free_flow
from PyFort.fortUnit import Unit,fortUnitIterator
import PyFort.fortExp as fe
import PyFort.fortStmts as fs

from PP.unitPostProcess import UnitPostProcessor,PostProcessError
 
def main():
    usage = '%prog [options] <input_file>'
    modes={'f':'forward','r':'reverse'}
    modeChoices=modes.keys()
    modeChoicesHelp=""
    for k,v in modes.items():
        modeChoicesHelp+=k+" = "+v+"; "
    opt = OptionParser(usage=usage)
    opt.add_option('-d', '--deriv', dest='transformDerivType',
                   help='appends %d to deriv types instead of removing __deriv__',
                   action='store_true',
                   default=False)
    opt.add_option('--free',
                   dest='isFreeFormat',
                   help="input source is free format",
                   action='store_true',
                   default=False)
    opt.add_option('-m','--mode',dest='mode',
                   type='choice', choices=modeChoices,
                   help='set default options for transformation mode with MODE being one of: '+ modeChoicesHelp+ '  reverse mode  implies -H but not -S; specific settings override the mode defaults.',
                   default=None)
    opt.add_option('-n',
                   dest='width',
                   help='-n [width] splits units into separate files numbered in compile order with [width] number of ints specifying output file naming scheme for each unit. (e.g. outfile_name.post0000.f for a width of four.)')
    opt.add_option('-o',
                   '--output',
                   dest='outputFile',
                   help='set output file (defaults to stdout)',
                   metavar='<output_file>',
                   default=None)
    opt.add_option('-i',
                   '--input',
                   dest='inputFile',
                   help='set input file for reverse mode post processing (defaults to ad_inline.f)',
                   metavar='<input>',
                   default='ad_inline.f')
    opt.add_option('-t',
                   '--type',
                   dest='replacementType',
                   help='selects replacement type for openadty_active types (defaults to active)',
                   metavar='<type>',
                   default=False)
    opt.add_option('-v',
                   '--verbose',
                   dest='isVerbose',
                   help='turns on verbose debugging output',
                   action='store_true',
                   default=False)
    opt.add_option('-w',
                   '--whitespace',
                   dest='useWhitespace',
                   help='puts whitespace between operations in output',
                   action='store_true',
                   default=False)


    config, args = opt.parse_args()

    # Set input file
    if len(args) != 1:
        opt.error("expect input file argument")
    inputFile = args[0]

    # set symtab type defaults
    Symtab.setTypeDefaults((fs.RealStmt,[]),(fs.IntegerStmt,[]))

    # set __deriv__ output format(__deriv__(v) -> "(v)%d" if -d option or "v" by default)
    UnitPostProcessor.setDerivType(config.transformDerivType)

    # set input file
    if config.inputFile:
        UnitPostProcessor.setInputFile(config.inputFile)

    # set free/fixed format
    free_flow(config.isFreeFormat) 

    # configure forward/reverse mode
    if config.mode:
        if config.mode[0] == 'f':
            UnitPostProcessor.setMode('forward')
        elif config.mode[0] == 'r':
            UnitPostProcessor.setMode('reverse')

    # set options for splitting compile units
    if config.width:
        splitUnits = True
        unitNameWidth = config.width
    else:
        splitUnits = False

    # set replacement type for openadty_active
    if config.replacementType:
        UnitPostProcessor.setReplacementType(config.replacementType)

    # set verbosity
    UnitPostProcessor.setVerbose(config.isVerbose)
    DebugManager.setVerbose(config.isVerbose)

    # set whitespace
    fe.Ops.setWhitespace(config.useWhitespace)

    try: 
        if config.outputFile: 
            outfile = config.outputFile
        else: 
            (base,ext) = os.path.splitext(inputFile)
            outfile = base + ".post" + ext
        if splitUnits:
            (base,ext) = os.path.splitext(outfile)
            unitNumExt = "%0"+str(unitNameWidth)+"d"
            unit_num = 0
            for aUnit in fortUnitIterator(inputFile,config.isFreeFormat):
                output = base + unitNumExt % unit_num + ext
                out = open(output,'w')
                UnitPostProcessor(aUnit).processUnit().printit(out)
                out.close()
                unit_num += 1
        else:
            out = open(outfile, 'w')            
            for aUnit in fortUnitIterator(inputFile,config.isFreeFormat):
                UnitPostProcessor(aUnit).processUnit().printit(out)
            out.close()
    except PostProcessError,e:
        print >>sys.stderr,'\nPostprocessing Error on line '+str(e.lineNumber)+':\n',e.msg
        return 1

    except SymtabError,e:
        debugstr = e.entry and e.entry.debug('unknown') \
                            or ''
        print >>sys.stderr,'\nSymtabError on line '+str(e.lineNumber)+':\n',e.msg,'\nfor entry',debugstr
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

