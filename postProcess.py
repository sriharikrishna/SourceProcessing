#!/usr/bin/env python
'''
Postprocessing
'''
import sys
import os
import re
import datetime
import traceback
from optparse import OptionParser

from PyUtil.errors import UserError, ScanError, ParseError
from PyUtil.assembler import AssemblerException
from PyUtil.l_assembler import AssemblerException as ListAssemblerException
from PyUtil.symtab import Symtab,SymtabError
from PyUtil.debugManager import DebugManager

from PyIR.prog1 import Prog1

from PyFort.flow import setFixedOrFreeFormat, setLineLength
from PyFort.fortUnit import Unit,fortUnitIterator
import PyFort.fortExp as fe
import PyFort.fortStmts as fs

from PP.unitPostProcess import UnitPostProcessor,PostProcessError
from PP.templateExpansion import TemplateExpansion

sys.setrecursionlimit(1500)

def cleanup(outFileNameList):
    for outFile in outFileNameList:
        if os.path.exists(outFile):
            try: 
                os.remove(outFile)
            except:
                print >>sys.stderr,'ERROR: cleanup - cannot remove output file '+outFile

def main():
    usage = '%prog [options] <input_file>'
    modes={'f':'forward','r':'reverse'}
    modeChoices=modes.keys()
    modeChoicesHelp=""
    for k,v in modes.items():
        modeChoicesHelp+=k+" = "+v+"; "
    opt = OptionParser(usage=usage)
    opt.add_option('-d', '--deriv', dest='deriv',
                   help='appends %d to deriv types instead of removing __deriv__',
                   action='store_true',
                   default=False)
    opt.add_option('-i',
                   '--inline',
                   dest='inline',
                   help='file with definitions for inlinable routines for reverse mode post processing (defaults to ad_inline.f); requires reverse mode ( -m r )',
                   default=None) # cannot set default here because of required reverse mode
    opt.add_option('-l','--line_len',
                   dest='line_len',
                   help='sets the max line length of the output file',
                   default=None)
    opt.add_option('-m','--mode',dest='mode',
                   type='choice', choices=modeChoices,
                   help='set default options for transformation mode with MODE being one of: '+ modeChoicesHelp+ ' (default is \'f\')',
                   default='f')
    opt.add_option('-o',
                   '--output',
                   dest='output',
                   help='redirect output to  file OUTPUT (default output is stdout); cannot be specified together with --width',
                   default=None)
    opt.add_option('-p',
                   '--progress',
                   dest='progress',
                   help='progress message to stdout per processed unit',
                   action='store_true',
                   default=False)
    opt.add_option('-t',
                   '--template',
                   dest='template',
                   help='file with subroutine template for reverse mode post processing (defaults to ad_template.f) for subroutines that do not have a template file specified via the template pragma; requires reverse mode ( -m r )',
                   default=None) # cannot set default here because of required reverse mode
    opt.add_option('-v',
                   '--verbose',
                   dest='verbose',
                   help='verbose output to stdout',
                   action='store_true',
                   default=False)
    opt.add_option('--abstractType',
                   dest='abstractType',
                   help='change the abstract active type name to be replaced  (see also --concreteType ) to ABSTRACTTYPE; defaults to \'oadactive\')',
                   default='oadactive')
    opt.add_option('--activeVariablesFile',
                   dest='activeVariablesFile',
                   help='write all active variable declarations into file ACTIVEVARIABLESFILEFILE.',
                   default=None) 
    opt.add_option('--concreteType',
                   dest='concreteType',
                   help='replace abstract active string (see also --abstractType ) with concrete active type CONCRETETYPE; defaults to \'active\'',
                   default='active')
    opt.add_option('--freeOutput',
                   dest='freeOutput',
                   help="<output_file> is in free format",
                   action = 'store_true',
                   default=None)
    opt.add_option('--free',
                   dest='isFreeFormat',
                   help="<input_file> is in free format",
                   action='store_true',
                   default=False)
    opt.add_option('--noCleanup',
                   dest='noCleanup',
                   help='do not remove the output file(s) if an error was encountered (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('--noInline',
                   dest='noInline',
                   help='no inlining; overrides the defaults of the reverse mode settings; (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('--noWarnings',
                   dest='noWarnings',
                   help='suppress warning messages (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('--separateOutput',
                   dest='separateOutput',
                   help='split output into files as specified by the input file pragmas placed by preProcess.py (defaults to False)',
                   action='store_true',
                   default=False)
    opt.add_option('--pathPrefix',
                   dest='pathPrefix',
                   help='for use with --separateOutput: prepend this prefix to the directory name of the corresponding input file (defaults to an empty string)',
                   default='')
    opt.add_option('-P',
                   '--pathSuffix',
                   dest='pathSuffix',
                   help='for use with --separateOutput: append this suffix to the directory name of the corresponding input file (defaults to "OAD")',
                   default='OAD')
    opt.add_option('-F',
                   '--filenameSuffix',
                   dest='filenameSuffix',
                   help='for use with --separateOutput: append this suffix to the name of the corresponding input file (defaults to an empty string)',
                   default='')
    opt.add_option('--recursionLimit',
                   dest='recursionLimit',
                   type='int',
                   help='recursion limit for the python interpreter (default: '+str(sys.getrecursionlimit())+'; setting it too high may permit a SEGV in the interpreter)')
    opt.add_option('--timing',
                   dest='timing',
                   help='simple timing of the execution',
                   action='store_true',
                   default=False)
    opt.add_option('--width',
                   dest='width',
                   help='write one compile unit per output file with WIDTH digits prepended to the extension of <input_file>, e.g. for -n 2 and three compile units in an input file named \'a.f\' we create \'a.00.f\', a.01.f\', \'a.02.f\'; also creates a file named \'postProcess.make\' for reference within a makefile; cannot be specified together with -o')
    opt.add_option('--whitespace',
                   dest='whitespace',
                   help='inserts whitespaces between tokens',
                   action='store_true',
                   default=False)

    outFileNameList=[]
    try:
        config, args = opt.parse_args()

        startTime=None
        if (config.timing):
            startTime=datetime.datetime.utcnow()

        if (config.recursionLimit):
            sys.setrecusionlimit(config.recursionLimit);

        # Set input file
        if len(args) != 1:
            opt.error("expect exactly one argument <input_file>, given are "+str(len(args))+" which are:"+str(args)+" and the following options: "+str(config))
        inputFile = args[0]

        if ((config.width and config.output) or
            (config.width and config.separateOutput) or
            (config.output and config.separateOutput)):
            opt.error("cannot specify more than one of --width, --separateOutput, and -o")

        # set symtab type defaults
        Symtab.setTypeDefaults((fs.RealStmt,[]),(fs.IntegerStmt,[]))

        # set __deriv__ output format(__deriv__(v) -> "(v)%d" if -d option or "v" by default)
        UnitPostProcessor.setDerivType(config.deriv)

        # set free/fixed format
        if config.freeOutput is None:
            setFixedOrFreeFormat(config.isFreeFormat)
        else:
            setFixedOrFreeFormat(config.isFreeFormat,config.freeOutput)

        if config.line_len:
            setLineLength(config.line_len)
        
        if (config.activeVariablesFile):
            UnitPostProcessor.setActiveVariablesFile(activeVariablesFile)
            if (os.path.exists(config.activeVariablesFile)):
                os.remove(config.activeVariablesFile)

        # configure forward/reverse mode (including inline file for reverse mode)
        if (config.mode != 'r'):
            if (config.inline):
                opt.error("option -i requires reverse mode ( -m r )")
            if (config.template):
                opt.error("option -t requires reverse mode ( -m r )")
        if config.mode == 'f':
            UnitPostProcessor.setMode('forward')
        if config.mode == 'r':
            UnitPostProcessor.setMode('reverse')
            if (config.inline):
                if (config.noInline):
                    opt.error("option --noInline conflicts with option -i")
                UnitPostProcessor.setInlineFile(config.inline)
            if (config.noInline):
                UnitPostProcessor.setInlineFile(None)
            UnitPostProcessor.processInlineFile()
            templateFile = config.template or 'ad_template.f'
            TemplateExpansion.setTemplateFile(templateFile)

        # set options for splitting compile units
        if config.width:
            splitUnits = True
            unitNameWidth = config.width
        else:
            splitUnits = False

        # set replacement type 
        UnitPostProcessor.setReplacementType(config.concreteType)
        # set abstract type 
        UnitPostProcessor.setAbstractType(config.abstractType)

        # set verbosity
        DebugManager.setVerbose(config.verbose)
        DebugManager.debug("running for <input_file>:"+args[0]+" and the following options: "+str(config))
        DebugManager.setQuiet(config.noWarnings)

        # set whitespace
        fe.setWhitespace(config.whitespace)

        if splitUnits:
            (base,ext) = os.path.splitext(inputFile)
            unitNumExt = "%0"+str(unitNameWidth)+"d"
            unit_num = 0
            unitStartTime=None
            if (config.timing):
                unitStartTime=datetime.datetime.utcnow()
            for aUnit in fortUnitIterator(inputFile,config.isFreeFormat):
                output = base + unitNumExt % unit_num + ext
                out = open(output,'w')
                outFileNameList.append(output)
                UnitPostProcessor(aUnit).processUnit().printit(out)
                out.close()
                if (config.progress):
                    msg='SourceProcessing: progress: done with unit '+aUnit.uinfo.name
                    if (config.timing):
                        nTime=datetime.datetime.utcnow()
                        msg+=' took: '+str(nTime-unitStartTime)
                        unitStartTime=nTime
                    print msg
                unit_num += 1
            makeOut = open('postProcess.make','w')
            makeOut.write("POSTPROCESSEDFILES=")
            for outFileName in outFileNameList:
                makeOut.write(" \\\n"+outFileName)
            makeOut.write("\n")
            makeOut.close()
        # SEPARATE OUTPUT INTO FILES AS SPECIFIED BY PRAGMAS
        elif config.separateOutput:
            out = None
            for aUnit in fortUnitIterator(inputFile,config.isFreeFormat):
                # We expect to find file pragmas in the cmnt section of units exclusively
                if aUnit.cmnt:
                    if (re.search('openad xxx file_start',aUnit.cmnt.rawline,re.IGNORECASE)):
                        if out: # close the previous output file (if any)
                            out.close()
                        # extract the new output file location (and add path and filename suffixes)
                        (head,tail) = os.path.split(aUnit.cmnt.rawline.split('start [')[1].split(']')[0])
                        (fileName,fileExtension) = os.path.splitext(tail)
                        outputDirectory = config.pathPrefix+head+config.pathSuffix
                        if not os.path.exists(outputDirectory): os.makedirs(outputDirectory)
                        newOutputFile = os.path.join(outputDirectory,fileName+config.filenameSuffix+fileExtension)
                        outFileNameList.append(newOutputFile)
                        out = open(newOutputFile,'w')
                elif not out:
                    raise PostProcessError('option separateOutput specified, no output file can be determined for the first unit',0)
                # postprocess the unit and print it
                UnitPostProcessor(aUnit).processUnit().printit(out)
            out.close()
        else: 
            out=None
            if config.output: 
                out = open(config.output,'w')
                outFileNameList.append(config.output)
            else:
                out=sys.stdout
            for aUnit in fortUnitIterator(inputFile,config.isFreeFormat):
                UnitPostProcessor(aUnit).processUnit().printit(out)
            if config.output: 
                out.close()

        if (config.timing):
            print 'SourceProcessing: timing: '+str(datetime.datetime.utcnow()-startTime)

    except PostProcessError,e:
        sys.stderr.write('\nERROR: PostProcessError')
        if (e.lineNumber>0) :
            sys.stderr.write(' on line '+str(e.lineNumber))
        sys.stderr.write(': '+e.msg+'\n')
        cleanup(outFileNameList)
        return 1

    except SymtabError,e:
        print >>sys.stderr,'\nERROR: SymtabError at line '+str(e.lineNumber)+':',e.msg
        if e.entry:
            symbolNameStr = e.symbolName or '<symbol name unknown>'
            print >>sys.stderr,'For entry', e.entry.debug(symbolNameStr)
        cleanup(outFileNameList)
        return 1
    except UserError,e:
        print >>sys.stderr,'\nERROR: UserError:',e.msg
        cleanup(outFileNameList)
        return 1 
    except ScanError,e: 
        print >>sys.stderr,'\nERROR: ScanError: scanner fails at line '+str(e.lineNumber)+':'
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
        cleanup(outFileNameList)
        return 1 
    except ParseError,e: 
        print >>sys.stderr,'\nERROR: ParseError: parser fails to assemble tokens in scanned line '+str(e.lineNumber)+':'
        print >>sys.stderr,e.scannedLine
        if e.details: print >>sys.stderr,e.details
        if e.target: print >>sys.stderr,"tried to parse as",e.target
        cleanup(outFileNameList)
        return 1 
    except AssemblerException,e:
        print >>sys.stderr,"\nERROR: AssemblerError: parser failed:",e.msg
        cleanup(outFileNameList)
        return 1 
    except ListAssemblerException,e:
        print >>sys.stderr,"\nERROR: ListAssemblerError: parser failed:",e.msg
        cleanup(outFileNameList)
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
    # import cProfile
    # cProfile.runctx( 'main()', globals(), locals(), filename="postProcess.profile" )
    sys.exit(main())
