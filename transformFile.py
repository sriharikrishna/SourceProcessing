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
            for aUnit in fortUnitIterator(currentFile,config.inputFormat):
                TransformActiveVariables(aUnit).transformUnit().printit(out)
            if config.outputFile :
                out.close()
        # multiple input files
        else :
            for anInputFile in inputFileList :
                currentFile = anInputFile
                out = open(os.path.join(config.outputDir,currentFile),'w')
                for aUnit in fortUnitIterator(currentFile,config.inputFormat):
                    TransformActiveVariables(aUnit).transformUnit().printit(out)
                out.close()

    except (TransformError,SymtabError,UserError,ScanError,ParseError),e:
        sys.stderr.write(str(e))
        cleanup(config)
        return 1

if __name__ == "__main__":
    sys.exit(main())
