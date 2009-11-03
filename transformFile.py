#!/usr/bin/env python
'''
Rudimentary transformations on non-transformed files
'''
import os
import sys
from optparse import OptionParser

from PyUtil.errors import UserError, ScanError, ParseError
from PP.transformActiveVariables import TransformActiveVariables,TransformError
from PyFort.fortUnit import Unit,fortUnitIterator
from PyUtil.debugManager import DebugManager
from PyUtil.symtab import Symtab,SymtabError
from PyFort.fortFile import Ffile
import PyFort.fortStmts as fs
from PyFort.flow import setInputLineLength, setOutputLineLength, setOutputFormat

def cleanup(config):
    import os 
    if ((not config.noCleanup) and (not config.output is None) and  os.path.exists(config.output)):
        try: 
            os.remove(config.output)
        except:
            print >>sys.stderr,'Cannot remove output file '+config.output
 
def main():
    usage = '%prog <input_file>'
    opt = OptionParser(usage=usage)
    opt.add_option('',
                   '--inputFormat',
                   dest='inputFormat',
                   help="input source is free format (input file and vardefs file are assumed to have the same formatting)",
                   default=None)
    opt.add_option('--outputFormat',
                   dest='outputFormat',
                   help="<output_file> is in 'free' or 'fixed' format",
                   default=None)
    opt.add_option('-d',
                   '--vardefs',
                   dest='vardefs',
                   help='file with definitions for active variables',
                   default='activeVariableDefinitions.f')
    opt.add_option('','--inputLineLength',
                   dest='inputLineLength',
                   type=int,
                   help='sets the max line length of the input file. The default line length is 72 for fixed format and 132 for free format.',
                   default=None)
    opt.add_option('','--outputLineLength',
                   dest='outputLineLength',
                   type=int,
                   help='sets the max line length of the output file. The default line length is 72 for fixed format and 132 for free format.',
                   default=None)
    opt.add_option('-o',
                   '--output',
                   dest='output',
                   help='redirect output to file OUTPUT (default output is stdout)',
                   default=None)
    opt.add_option('-n',
                   '--noCleanup',
                   dest='noCleanup',
                   help='do not remove the output file if an error was encountered (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('--outputDir',
                   dest='outputDir',
                   help='for use with >1 input file (and not with --output): output each file in this directory, keeping the same file name (defaults to the local directory)',
                   default='')
    opt.add_option('-v',
                   '--verbose',
                   dest='isVerbose',
                   help='turns on verbose debugging output',
                   action='store_true',
                   default=False)
    config,args = opt.parse_args()

    # set verbosity
    DebugManager.setVerbose(config.isVerbose)

    # set symtab type defaults
    Symtab.setTypeDefaults((fs.RealStmt,[]),(fs.IntegerStmt,[]))

    # set free/fixed format
    if (config.inputFormat<>'fixed') and \
           (config.inputFormat<>'free') and \
           (config.inputFormat is not None):
        opt.error("inputFormat option must be specified with either 'fixed' or 'free' as an argument")
    if config.outputFormat == None:
        if config.output:
            ext = os.path.splitext(config.output)[1]
            config.outputFormat = Ffile.get_format(ext)
        else:
            config.outputFormat = config.inputFormat
    elif (config.outputFormat<>'fixed') and (config.outputFormat<>'free'):
        opt.error("outputFormat option must be specified with either 'fixed' or 'free' as an argument")
    setOutputFormat(config.outputFormat)

    # set line length
    if config.inputLineLength:
        if config.inputLineLength < 72 or \
               config.inputLineLength > 132:
            opt.error("inputLineLength option must be specified with a value >=72 and <=132")
        else:
            # figure out what this does
            setInputLineLength(config.inputLineLength)
    if config.outputLineLength:
        if config.outputLineLength < 72 or \
               config.outputLineLength > 132:
            opt.error("outputLineLength option must be specified with a value >=72 and <=132")
        else:
            setOutputLineLength(config.outputLineLength)

    # check input/output options
    if len(args) == 0:
        opt.error('expected at least one argument <input_file> ;' \
                 +' the following options were given: '+str(config))
    inputFileList = args
    if config.output and len(inputFileList) > 1 :
            opt.error('No output file can be specified when more than one input file is given.' \
                     +' the following options were given: '+str(config))
    if config.outputDir :
        if not os.path.exists(config.outputDir): os.makedirs(config.outputDir)

    currentFile = config.vardefs
    try:
        # suppress missing module warnings???
        # AL: shouldnt be necessary now that we're putting everything in the active variables file
        TransformActiveVariables.getActiveDecls(config.vardefs,\
                                                config.inputFormat,config.outputFormat)
        # only one input file
        if len(inputFileList) == 1 :
            currentFile = inputFileList[0]
            out = config.output and open(config.output,'w') \
                                 or sys.stdout
            for aUnit in fortUnitIterator(inputFileList[0],config.inputFormat):
                TransformActiveVariables(aUnit).transformFile().printit(out)
            if config.output :
                out.close()
        # multiple input files
        else :
            for anInputFile in inputFileList :
                currentFile = anInputFile
                out = open(os.path.join(config.outputDir,anInputFile),'w')
                for aUnit in fortUnitIterator(anInputFile,config.inputFormat):
                    TransformActiveVariables(aUnit).transformFile().printit(out)
                out.close()

    except TransformError,e :
        print >>sys.stderr,'\nERROR: TransformError in '+currentFile+' at line '+str(e.lineNumber)+':',e.msg
        cleanup(config)
        return 1
    except SymtabError,e:
        print >>sys.stderr,'\nERROR: SymtabError in '+currentFile+' at line '+str(e.lineNumber)+':',e.msg
        if e.entry:
            symbolNameStr = e.symbolName or '<symbol name unknown>'
            print >>sys.stderr,'For entry', e.entry.debug(symbolNameStr)
        cleanup(config)
        return 1
    except UserError,e:
        print >>sys.stderr,'\nERROR: UserError:',e.msg
        cleanup(config)
        return 1 
    except ScanError,e: 
        print >>sys.stderr,'\nERROR: ScanError: scanner fails in '+currentFile+' at line '+str(e.lineNumber)+':'
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
        print >>sys.stderr,'\nERROR: ParseError: parser fails to assemble tokens in '+currentFile+' at scanned line '+str(e.lineNumber)+':'
        print >>sys.stderr,e.scannedLine
        if e.details: print >>sys.stderr,e.details
        if e.target: print >>sys.stderr,"tried to parse as",e.target
        cleanup(config)
        return 1 


if __name__ == "__main__":
    sys.exit(main())
