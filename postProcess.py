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
from PP.templateExpansion import TemplateExpansion

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
    opt.add_option('-m','--mode',dest='mode',
                   type='choice', choices=modeChoices,
                   help='set default options for transformation mode with MODE being one of: '+ modeChoicesHelp+ ' (default is \'f\')',
                   default='f')
    opt.add_option('-o',
                   '--output',
                   dest='output',
                   help='redirect output to  file OUTPUT (default output is stdout); cannot be specified together with --width',
                   default=None)
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
    opt.add_option('--concreteType',
                   dest='concreteType',
                   help='replace abstract active string (see also --abstractType ) with concrete active type CONCRETETYPE; defaults to \'active\'',
                   default='active')
    opt.add_option('--free',
                   dest='free',
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
    opt.add_option('--width',
                   dest='width',
                   help='write one compile unit per output file with WIDTH digits prepended to the extension of <input_file>, e.g. for -n 2 and three compile units in an input file named \'a.f\' we create \'a.00.f\', a.01.f\', \'a.02.f\'; also creates a file named \'postProcess.make\' for reference within a makefile; cannot be specified together with -o')
    opt.add_option('--whitespace',
                   dest='whitespace',
                   help='inserts whitespaces between tokens',
                   action='store_true',
                   default=False)

    config, args = opt.parse_args()

    # Set input file
    if len(args) != 1:
        opt.error("expect exactly one argument <input_file>, given are "+str(len(args))+" which are:"+str(args)+" and the following options: "+str(config))
    inputFile = args[0]

    if (config.width and config.output):
        opt.error("cannot specify both --width and -o.")

    # set symtab type defaults
    Symtab.setTypeDefaults((fs.RealStmt,[]),(fs.IntegerStmt,[]))

    # set __deriv__ output format(__deriv__(v) -> "(v)%d" if -d option or "v" by default)
    UnitPostProcessor.setDerivType(config.deriv)

    # set free/fixed format
    free_flow(config.free)

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
        UnitPostProcessor.setFreeFlow(config.free)
        if (config.inline):
            if (config.noInline):
                opt.error("option --noInline conflicts with option -i")
            UnitPostProcessor.setInlineFile(inlineFile)
        if (config.noInline):
            UnitPostProcessor.setInlineFile(None)
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

    # set whitespace
    fe.Ops.setWhitespace(config.whitespace)

    outFileNameList=[]
    try:
        if splitUnits:
            (base,ext) = os.path.splitext(inputFile)
            unitNumExt = "%0"+str(unitNameWidth)+"d"
            unit_num = 0
            for aUnit in fortUnitIterator(inputFile,config.free):
                output = base + unitNumExt % unit_num + ext
                out = open(output,'w')
                outFileNameList.append(output)
                UnitPostProcessor(aUnit).processUnit().printit(out)
                out.close()
                unit_num += 1
            makeOut = open('postProcess.make','w')
            makeOut.write("POSTPROCESSEDFILES=")
            for outFileName in outFileNameList:
                makeOut.write(" \\\n"+outFileName)
            makeOut.write("\n")
            makeOut.close()
        else: 
            out=None
            if config.output: 
                out = open(config.output,'w')
                outFileNameList.append(config.output)
            else:
                out=sys.stdout
            for aUnit in fortUnitIterator(inputFile,config.free):
                UnitPostProcessor(aUnit).processUnit().printit(out)
            if config.output: 
                out.close()
    except PostProcessError,e:
        sys.stderr.write('\nERROR: PostProcessError')
        if (e.lineNumber>0) :
            sys.stderr.write(' on line '+str(e.lineNumber))
        sys.stderr.write(': '+e.msg+'\n')
        cleanup(outFileNameList)
        return 1

    except SymtabError,e:
        debugstr = e.entry and e.entry.debug('unknown') \
                            or ''
        print >>sys.stderr,'\nERROR: SymtabError on line '+str(e.lineNumber)+':\n',e.msg,'\nfor entry',debugstr
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
        print >>sys.stderr,"This failure is likely due to possibly legal but unconventional Fortran,"
        print >>sys.stderr,"such as unusual spacing. Please consider modifying your source code."
        cleanup(outFileNameList)
        return 1 
    except ParseError,e: 
        print >>sys.stderr,'\nERROR: ParseError: parser fails to assemble tokens in scanned line '+str(e.lineNumber)+':'
        print >>sys.stderr,e.scannedLine
        print >>sys.stderr,"as",e.target
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
    return 0

if __name__ == "__main__":
    # import cProfile
    # cProfile.runctx( 'main()', globals(), locals(), filename="postProcess.profile" )
    sys.exit(main())

