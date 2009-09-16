#!/usr/bin/env python
'''
Rudimentary transformations on non-transformed files
'''
import sys
from optparse import OptionParser

from PyUtil.errors import UserError, ScanError, ParseError
from PP.transformActiveVariables import TransformActiveVariables
from PyFort.fortUnit import Unit,fortUnitIterator
from PyUtil.debugManager import DebugManager
from PyUtil.symtab import Symtab,SymtabError
import PyFort.fortStmts as fs

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
                   '--free',
                   dest='isFreeFormat',
                   help="input source is free format (input file and vardefs file are assumed to have the same formatting)",
                   action='store_true',
                   default=False)
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
    opt.add_option('-n',
                   '--noCleanup',
                   dest='noCleanup',
                   help='do not remove the output file if an error was encountered (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('-v',
                   '--verbose',
                   dest='isVerbose',
                   help='turns on verbose debugging output',
                   action='store_true',
                   default=False)
    config,args = opt.parse_args()

    # set verbosity
    DebugManager.setVerbose(config.isVerbose)

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

    currentFile = config.vardefs
    try:
        # suppress missing module warnings???
        TransformActiveVariables.getActiveDecls(config.vardefs,config.isFreeFormat)
        currentFile = inputFile
        for aUnit in fortUnitIterator(inputFile,config.isFreeFormat):
            TransformActiveVariables(aUnit).transformFile().printit(out)
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
        if (e.rest == '&' and not config.isFreeFormat):
            print >>sys.stderr,"This failure is likely due to running this script on free-formatted code without specifying the --free flag."
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

    if config.output: 
        out.close()

if __name__ == "__main__":
    sys.exit(main())
