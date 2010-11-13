#!/usr/bin/env python
'''
Rudimentary transformations on non-transformed files
'''
import os
import sys
from optparse import OptionParser
from PyUtil.options import addTransformFileOptions,TransformFileOptErrors,setTransformFileFlags
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
    if ((not config.noCleanup) and (not config.outputFile is None) and  os.path.exists(config.outputFile)):
        try: 
            os.remove(config.outputFile)
        except:
            print >>sys.stderr,'Cannot remove output file '+config.outputFile
 
def main():
    usage = '%prog <input_file>'
    opt = OptionParser(usage=usage)
    addTransformFileOptions(opt)
    config,args = opt.parse_args()

    TransformFileOptErrors(config,args)
    setTransformFileFlags(config)
    inputFileList = args
    if config.outputDir :
        if not os.path.exists(config.outputDir): os.makedirs(config.outputDir)

    currentFile = config.vardefs
    try:
        # suppress missing module warnings???
        # AL: shouldnt be necessary now that we're putting everything in the active variables file
        TransformActiveVariables.getActiveDecls(config.vardefs,\
                                                config.inputFormat)
        # only one input file
        if len(inputFileList) == 1 :
            currentFile = inputFileList[0]
            out = config.outputFile and open(config.outputFile,'w') \
                                 or sys.stdout
            for aUnit in fortUnitIterator(inputFileList[0],config.inputFormat):
                TransformActiveVariables(aUnit).transformUnit().printit(out)
            if config.outputFile :
                out.close()
        # multiple input files
        else :
            for anInputFile in inputFileList :
                currentFile = anInputFile
                out = open(os.path.join(config.outputDir,anInputFile),'w')
                for aUnit in fortUnitIterator(anInputFile,config.inputFormat):
                    TransformActiveVariables(aUnit).transformUnit().printit(out)
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
