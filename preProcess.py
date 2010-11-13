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
from PyUtil.options import addCanonOptions,CanonOptErrors,setCanonFlags

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
                    os.remove(fileName)
                except:
                    print >>sys.stderr,'Cannot remove output file '+fileName

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
    opt = OptionParser(usage=usage)
    addCanonOptions(opt)
    config, args = opt.parse_args()

    startTime=None
    if (config.timing):
        startTime=datetime.datetime.utcnow()

    CanonOptErrors(config,args)
    inputFileList = args
    setCanonFlags(config)

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

