#!/usr/bin/env python
'''
canonicalization
'''
import sys
import os
import datetime
import tempfile
import shutil
from optparse import OptionParser

from PyUtil.errors import UserError, ScanError, ParseError
from PyUtil.assembler import AssemblerException
from PyUtil.l_assembler import AssemblerException as ListAssemblerException
from PyUtil.debugManager import DebugManager
from PyUtil.symtab import Symtab,SymtabError

from PyFort.flow import setOutputLineLength, setOutputFormat
from PyFort.fortUnit import Unit,fortUnitIterator
from PyFort.fortFile import Ffile
import PyFort.fortStmts as fs
from PyFort.inference import InferenceError
from PyFort.intrinsic import getNonStandard,useNonStandard

from Canon.canon import UnitCanonicalizer,CanonError
from Canon.subroutinizedIntrinsics import makeSubroutinizedIntrinsics,SubroutinizeError,getModuleName
from Canon.function2subroutine import FunToSubError

sys.setrecursionlimit(1500)

ourOutFileNameList=[]
ourOutFileHandle=None

def cleanup(config):
    import os 
    if  ourOutFileHandle and not ourOutFileHandle.closed : 
        ourOutFileHandle.close()
    if (not config.noCleanup):
        for fileName in ourOutFileNameList:
            if os.path.exists(fileName):
                try: 
                    os.remove(config.outputFile)
                except:
                    print >>sys.stderr,'Cannot remove output file '+config.outputFile

def mkOutputDir(config,head):
    outputDirectory = config.pathPrefix+head+config.pathSuffix
    if outputDirectory == '':
        outputDirectory = './'
    if not os.path.exists(outputDirectory): 
        os.makedirs(outputDirectory)
    return outputDirectory

def main():
    global ourOutFileNameList
    global ourOutFileHandle
    usage = '%prog [options] <input_file> [additional input files]'
    modes={'f':'forward','r':'reverse'}
    modeChoices=modes.keys()
    modeChoicesHelp=""
    for k,v in modes.items():
        modeChoicesHelp+=k+" = "+v+"; "
    opt = OptionParser(usage=usage)
    opt.add_option('--outputFormat',
                   metavar='{ fixed | free }',
                   dest='outputFormat',
                   help="<output_file> is in either 'fixed' or 'free' format",
                   default=None)
    opt.add_option('',
                   '--inputFormat',
                   metavar='{ fixed | free }',
                   dest='inputFormat',
                   help="<input_file> is in either 'fixed' or 'free' format",
                   default=None)
    opt.add_option('','--inputLineLength',
                   dest='inputLineLength',
                   metavar='INT',
                   type=int,
                   help='sets the max line length of the input file. The default line length is 72 for fixed format and 132 for free format.',
                   default=None)
    opt.add_option('','--outputLineLength',
                   dest='outputLineLength',
                   metavar='INT',
                   type=int,
                   help='sets the max line length of the output file. The default line length is 72 for fixed format and 132 for free format.',
                   default=None)
    opt.add_option('-I','',
                   metavar='PATH',
                   dest='includePaths',
                   type='string',
                   help='directory to be added to the search path for Fortran INCLUDE directives; (default is the current directory)',
                   action='append',
                   default=[])
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
    opt.add_option('--warn',
                   dest='warn',
                   type='choice',
                   choices=DebugManager.WarnType.getNames()[1:],
                   help='issue warning messages only for the specified type which is one of ( '+' | '.join(name for name in DebugManager.WarnType.getNames()[1:])+' ); conflicts with --noWarnings',
                   action='append',
                   default=[])
    opt.add_option('',
                   '--keepGoing',
                   dest='keepGoing',
                   help="try to continue despite error messages; this is intended only to find trouble spots for the canonicalization, if problems occur the output may contain invalid code (defaults to False)",
                   action='store_true',
                   default=False)
    opt.add_option('',
                   '--removeFunction',
                   dest='removeFunction',
                   help="remove original function definition when it is transformed to a subroutine definitions",
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
    opt.add_option('','--nonStandard',dest='nonStandard',
                   type='choice', choices=getNonStandard(),
                   help='allow non-standard intrinsics: ( '+' | '.join(getNonStandard())+' ) ; can be specified multiple times  (defaults to None).',
                   action='append',
                   default=[])
    opt.add_option('-o',
                   '--output',
                   dest='outputFile',
                   help='redirect output to  file OUTPUT (default output is stdout); If the "--outputFormat" option is not used, the output format is taken from the extension of this filename',
                   metavar='<output_file>',
                   default=None)
    opt.add_option('--separateOutput',
                   dest='separateOutput',
                   help='split output into files corresponding to input files (defaults to False, conflicts with --output)',
                   action='store_true',
                   default=False)
    opt.add_option('--pathPrefix',
                   dest='pathPrefix',
                   help='for use with --separateOutput: prepend this prefix to the directory name of the corresponding input file (defaults to an empty string)',
                   default='')
    opt.add_option('',
                   '--pathSuffix',
                   dest='pathSuffix',
                   help='for use with --separateOutput: append this suffix to the directory name of the corresponding input file (defaults to "OAD")',
                   default='OAD')
    opt.add_option('',
                   '--filenameSuffix',
                   dest='filenameSuffix',
                   help='for use with --separateOutput: append this suffix to the name of the corresponding input file (defaults to an empty string)',
                   default='')   
    opt.add_option('--recursionLimit',
                   dest='recursionLimit',
                   metavar='INT',
                   type='int',
                   help='recursion limit for the python interpreter (default: '+str(sys.getrecursionlimit())+'; setting it too high may permit a SEGV in the interpreter)')
    opt.add_option('--subroutinizeIntegerFunctions',
                   dest='subroutinizeIntegerFunctions',
                   help='should integer function calls be subroutinized (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('--timing',
                   dest='timing',
                   help='simple timing of the execution',
                   action='store_true',
                   default=False)
    opt.add_option('--progress',
                   dest='progress',
                   help='issue progress message to stderr per opened input file (default is False)',
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
    if (config.inputFormat<>'fixed') and \
           (config.inputFormat<>'free') and \
           (config.inputFormat is not None):
        opt.error("inputFormat option must be specified with either 'fixed' or 'free' as an argument")
    # set outputFormat explicitly if format or output file are supplied by user. 
    # otherwise, outputFormat is set to inputFormat during parsing
    if config.outputFormat == None:
        if config.outputFile:
            ext = os.path.splitext(config.outputFile)[1]
            config.outputFormat = Ffile.get_format(ext)
            setOutputFormat(config.outputFormat)
    elif (config.outputFormat<>'fixed') and (config.outputFormat<>'free'):
        opt.error("outputFormat option must be specified with either 'fixed' or 'free' as an argument")
    else:
        setOutputFormat(config.outputFormat)

    if config.inputLineLength:
        if config.inputLineLength < 72 or \
           config.inputLineLength > 132:
            opt.error("inputLineLength option must be specified with a value >=72 and <=132")
        else:
            # figure out what this does
            pass
    if config.outputLineLength:
        if config.outputLineLength < 72 or \
           config.outputLineLength > 132:
            opt.error("outputLineLength option must be specified with a value >=72 and <=132")
        else:
            setOutputLineLength(config.outputLineLength)
    if config.outputFile and config.separateOutput: 
        opt.error("options --outputFile and --separateOutput are mutually exclusive")

    if config.removeFunction:
        UnitCanonicalizer.setKeepFunctionDef(False)
    if config.hoistConstantsFlag:
        UnitCanonicalizer.setHoistConstantsFlag(config.hoistConstantsFlag)
    if config.hoistStringsFlag:
        UnitCanonicalizer.setHoistStringsFlag(config.hoistStringsFlag)
    if config.subroutinizeIntegerFunctions:
        UnitCanonicalizer.setSubroutinizeIntegerFunctions(True)
    if config.keepGoing:
        CanonError.keepGoing()
    DebugManager.setVerbose(config.isVerbose)
    if (config.noWarnings and config.warn):
        opt.error("Option --noWarnings conflicts with option --warn="+config.warn[0])
    DebugManager.setQuiet(config.noWarnings)
    if config.warn:
        DebugManager.warnOnlyOn(config.warn)
    if config.progress:
        DebugManager.dumpProgress()    
    if config.includePaths:
        Ffile.setIncludeSearchPath(config.includePaths)
    if config.nonStandard:
        useNonStandard(config.nonStandard)
    try: 
        if (not (config.outputFile or config.separateOutput)) :
            ourOutFileHandle = sys.stdout
            if (len(inputFileList) > 1): # output the file start pragma for the subroutinized intrinsics
                ourOutFileHandle.write('!$openad xxx file_start ['+getModuleName()+'.f90]\n')
                ourOutFileHandle.flush()
            for aUnit in makeSubroutinizedIntrinsics(False):
                aUnit.printit(ourOutFileHandle)
        currentInputFile = '<none>'
        for anInputFile in inputFileList:
            if (config.outputFile and not ourOutFileHandle): 
                ourOutFileNameList.append(config.outputFile)
                ourOutFileHandle = open(config.outputFile,'w')
            if (config.separateOutput):
                if ourOutFileHandle: 
                    ourOutFileHandle.close()
                (head,tail) = os.path.split(anInputFile)
                (base,extension) = os.path.splitext(tail)
                outputDirectory = mkOutputDir(config, head) 
                newOutputFile = os.path.join(outputDirectory,base+config.filenameSuffix+".f90")
                ourOutFileNameList.append(newOutputFile)
                ourOutFileHandle = open(newOutputFile,'w')                                    
            currentInputFile = anInputFile
            if (len(inputFileList) > 1): # output the file start pragma
                ourOutFileHandle.write('!$openad xxx file_start ['+anInputFile+']\n')
                ourOutFileHandle.flush()
            for aUnit in fortUnitIterator(anInputFile,config.inputFormat):
                UnitCanonicalizer(aUnit).canonicalizeUnit().printit(ourOutFileHandle)
        if config.outputFile or config.separateOutput:
            ourOutFileHandle.close()
            if config.separateOutput:
                outputDirectory = mkOutputDir(config, '') 
                newOutputFile = os.path.join(outputDirectory,getModuleName()+'.f90')
            if config.outputFile :
                newOutputFile=tempfile.mktemp()
            ourOutFileNameList.append(newOutputFile)
            ourOutFileHandle=open(newOutputFile,'w')
            if (len(inputFileList) > 1): # output the file start pragma for the subroutinized intrinsics
                ourOutFileHandle.write('!$openad xxx file_start ['+getModuleName()+'.f90]\n')
                ourOutFileHandle.flush()
            for aUnit in makeSubroutinizedIntrinsics(True):
                aUnit.printit(ourOutFileHandle)
            if (config.outputFile):
                oFile=open(config.outputFile)
                ourOutFileHandle.write(oFile.read())
                oFile.close
            ourOutFileHandle.close()
            if (config.outputFile):
                shutil.move(newOutputFile,config.outputFile)
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
        if (e.rest == '&' and (config.inputFormat=='fixed')):
            print >>sys.stderr,"This failure is likely due to running this script on free-formatted code without specifying the --inputFormat=free flag."
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
    except InferenceError,e: 
        print >>sys.stderr,'\nERROR: InferenceError:  in '+currentInputFile+' at line '+str(e.lineNumber)+':'
        if e.msg: print >>sys.stderr,e.msg
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
    except FunToSubError,e: 
        print >>sys.stderr,'\nERROR: FunToSubError:  in '+currentInputFile+':'
        if e.msg: print >>sys.stderr,e.msg
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

