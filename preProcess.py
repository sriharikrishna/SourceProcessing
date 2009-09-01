#!/usr/bin/env python
'''
canonicalization
'''
import sys
import datetime
from optparse import OptionParser

from PyUtil.errors import UserError, ScanError, ParseError
from PyUtil.assembler import AssemblerException
from PyUtil.l_assembler import AssemblerException as ListAssemblerException
from PyUtil.debugManager import DebugManager
from PyUtil.symtab import Symtab,SymtabError

from PyFort.flow import setFixedOrFreeFormatting, setFixedOrFreeInput
from PyFort.fortUnit import Unit,fortUnitIterator
import PyFort.fortStmts as fs

from Canon.canon import UnitCanonicalizer,CanonError
from Canon.subroutinizedIntrinsics import makeSubroutinizedIntrinsics,SubroutinizeError

sys.setrecursionlimit(1500)

def cleanup(config):
    import os 
    if ((not config.noCleanup) and (not config.outputFile is None) and  os.path.exists(config.outputFile)):
        try: 
            os.remove(config.outputFile)
        except:
            print >>sys.stderr,'Cannot remove output file '+config.outputFile
 
def main():
    usage = '%prog [options] <input_file> [additional input files]'
    modes={'f':'forward','r':'reverse'}
    modeChoices=modes.keys()
    modeChoicesHelp=""
    for k,v in modes.items():
        modeChoicesHelp+=k+" = "+v+"; "
    opt = OptionParser(usage=usage)
    opt.add_option('--freeOutput',
                   dest='freeOutput',
                   help="<output_file> is in free format",
                   action = 'store_true',
                   default=False)
    opt.add_option('',
                   '--free',
                   dest='isFreeFormat',
                   help="input source is free format",
                   action='store_true',
                   default=False)
    opt.add_option('',
                   '--keepFunction',
                   dest='keepFunction',
                   help="keep original function definition when it is transformed to a subroutine definitions",
                   action='store_true',
                   default=False)
    opt.add_option('-m','--mode',dest='mode',
                   type='choice', choices=modeChoices,
                   help='set default options for transformation mode with MODE being one of: '+ modeChoicesHelp+ '  reverse mode  implies -H but not -S; specific settings override the mode defaults.',
                   default=None)
    opt.add_option('-n',
                   '--noCleanup',
                   dest='noCleanup',
                   help='do not remove the output file if an error was encountered (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('--noWarnings',
                   dest='noWarnings',
                   help='suppress warning messages (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('',
                   '--r8',
                   dest='r8',
                   help="set default size of REAL to 8 bytes",
                   action='store_true',
                   default=False)
    opt.add_option('-H',
                   '--hoistNonStringConstants',
                   dest='hoistConstantsFlag',
                   help='enable the hoisting of non-string constant arguments to subroutine calls (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('-S',
                   '--hoistStringConstants',
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
    opt.add_option('--recursionLimit',
                   dest='recursionLimit',
                   type='int',
                   help='recursion limit for the python interpreter (default: '+str(sys.getrecursionlimit())+'; setting it too high may permit a SEGV in the interpreter)')
    opt.add_option('--timing',
                   dest='timing',
                   help='simple timing of the execution',
                   action='store_true',
                   default=False)
    opt.add_option('-v',
                   '--verbose',
                   dest='isVerbose',
                   help='turns on verbose debugging output',
                   action='store_true',
                   default=False)
    config, args = opt.parse_args()

    if (config.recursionLimit):
        sys.setrecusionlimit(config.recursionLimit);
        
    startTime=None
    if (config.timing):
        startTime=datetime.datetime.utcnow()

    # Set input file(s)
    if len(args) == 0:
        opt.error("expected at least one input file argument")
    inputFileList = args

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
    setFixedOrFreeFormatting(config.isFreeFormat)
    setFixedOrFreeInput(config.isFreeFormat) 

    # set keep function definition
    UnitCanonicalizer.setKeepFunctionDef(config.keepFunction)
    # configure constant expression hoisting
    if config.hoistConstantsFlag:
        UnitCanonicalizer.setHoistConstantsFlag(config.hoistConstantsFlag)
    # configure string hoisting
    if config.hoistStringsFlag:
        UnitCanonicalizer.setHoistStringsFlag(config.hoistStringsFlag)
    # set output format
    UnitCanonicalizer.setOutputFormat(config.freeOutput)

    # set verbosity
    DebugManager.setVerbose(config.isVerbose)
    DebugManager.setQuiet(config.noWarnings)

    try: 
        if config.outputFile: out = open(config.outputFile,'w')
        else: out = sys.stdout
        currentInputFile = '<none>'
        for anInputFile in inputFileList:
            currentInputFile = anInputFile
            if (len(inputFileList) > 1): # output the file start pragma
                out.write('!$openad xxx file_start ['+anInputFile+']\n')
                out.flush()
            for aUnit in fortUnitIterator(anInputFile,config.isFreeFormat):
                UnitCanonicalizer(aUnit).canonicalizeUnit().printit(out)
        for aUnit in makeSubroutinizedIntrinsics():
            aUnit.printit(out)
        if config.outputFile: out.close()
        if (config.timing):
            print 'SourceProcessing: timing: '+str(datetime.datetime.utcnow()-startTime)
    except CanonError,e:
        print >>sys.stderr,'\nERROR: CanonError in '+currentInputFile+' at line '+str(e.lineNumber)+': ',e.msg
        cleanup(config)
        return 1
    except SymtabError,e:
        print >>sys.stderr,'\nERROR: SymtabError in '+currentInputFile+' at line '+str(e.lineNumber)+':',e.msg
        if e.entry:
            symbolNameStr = e.symbolName or '<symbol name unknown>'
            print >>sys.stderr,'For entry', e.entry.debug(symbolNameStr)
        cleanup(config)
        return 1
    except UserError,e:
        print >>sys.stderr,'\nERROR: UserError in '+currentInputFile+':',e.msg
        cleanup(config)
        return 1 
    except ScanError,e: 
        print >>sys.stderr,'\nERROR: ScanError: scanner fails in '+currentInputFile+' at line '+str(e.lineNumber)+':'
        print >>sys.stderr,e.aFortLine
        print >>sys.stderr,(len(e.aFortLine)-len(e.rest))*' '+'^'
        print >>sys.stderr,''
        print >>sys.stderr,"Tokens scanned ok: ", e.scanned,'\tUnable to scan: "'+e.rest+'"'
        print >>sys.stderr,''
        if (e.rest == '&' and not config.isFreeFormat):
            print >>sys.stderr,"This failure is likely due to running this script on free-formatted code without specifying the --free flag."
        else:
            print >>sys.stderr,"This failure is likely due to possibly legal but unconventional Fortran,"
            print >>sys.stderr,"such as unusual spacing. Please consider modifying your source code."
        cleanup(config)
        return 1 
    except ParseError,e: 
        print >>sys.stderr,'\nERROR: ParseError: parser fails to assemble tokens in '+currentInputFile+' at scanned line '+str(e.lineNumber)+':'
        print >>sys.stderr,e.scannedLine
        if e.details: print >>sys.stderr,e.details
        if e.target: print >>sys.stderr,"tried to parse as",e.target
        cleanup(config)
        return 1 
    except AssemblerException,e:
        print >>sys.stderr,'\nERROR: AssemblerError: parser failed in '+currentInputFile+':',e.msg
        cleanup(config)
        return 1 
    except ListAssemblerException,e:
        print >>sys.stderr,'\nERROR: ListAssemblerError: parser failed in '+currentInputFile+':',e.msg
        print >>sys.stderr,'rest =', e.rest
        cleanup(config)
        return 1 
    except RuntimeError,e:
        if (len(e.args)>=1 and "maximum recursion depth exceeded" <= e.args[0]):
            print >>sys.stderr,'\nERROR: RuntimeError: python interpreter failed with: ',e.args[0]
            print >>sys.stderr,'\twhich can happen for deeply nested bracketing. Try to set a value larger than the current '+str(sys.getrecursionlimit())+' with --recursionLimit .' 
            cleanup(config)
            return 1
        else:
            raise e
    return 0

if __name__ == "__main__":
    sys.exit(main())

