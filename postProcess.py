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
from PyUtil.options import addPostProcessorOptions,PostProcessorOptErrors,setPostProcessFlags

from PyIR.prog1 import Prog1

from PyFort.flow import setOutputLineLength, setInputLineLength, setOutputFormat
import PyFort.flow as flow
from PyFort.fortUnit import Unit,fortUnitIterator
from PyFort.fortFile import Ffile
import PyFort.fortExp as fe
import PyFort.fortStmts as fs

from PP.unitPostProcess import UnitPostProcessor,PostProcessError
from PP.templateExpansion import TemplateExpansion

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

# initSet: a set of common blocks occurring in the file which need variables initialized
# initNames: a list of names of initialization subroutines to be called by the global
# init procedure
# typeDecls: a list of variable type declarations from the common blocks
# output: the file to which new units are printed
# base,unitNumExt,unit_num,ext: used to create a new output file if splitUnits is true
# splitUnits: True if units are being split and printed to different files
def addInitProcedures(initSet,initNames,typeDecls,output=None,base='',unitNumExt='',unit_num=0,ext='',splitUnits=False):
    '''creates active variable derivative initialization procedures and prints them to specified output file(s)'''
    for elt in initSet:
        newUnit = UnitPostProcessor.createInitProcedure(elt,typeDecls)
        if newUnit is not None:
            # print new output file
            if splitUnits:
                output = base + unitNumExt % unit_num + ext; unit_num+=1
                ourOutFileHandle = open(output,'w')
                ourOutFileNameList.append(output)
                # print new output file
                newUnit.printit(ourOutFileHandle)
                ourOutFileHandle.close()
            else:
                newUnit.printit(output)
    if len(initNames) > 0:
        newUnit = UnitPostProcessor.createGlobalInitProcedure(initNames)
        if splitUnits:
            output = base + unitNumExt % unit_num + ext
            ourOutFileHandle = open(output,'w')
            ourOutFileNameList.append(output)
            newUnit.printit(ourOutFileHandle)
            ourOutFileHandle.close()
        else:
            newUnit.printit(output)
    return

def main():
    global ourOutFileNameList
    global ourOutFileHandle
    usage = '%prog [options] <input_file> [additional input files]'
    opt = OptionParser(usage=usage)
    addPostProcessorOptions(opt)

    try:
        config, args = opt.parse_args()

        startTime=None
        if (config.timing):
            startTime=datetime.datetime.utcnow()

        PostProcessorOptErrors(opt,config,args)
        setPostProcessFlags(config,args)
        # set options for splitting compile units
        if config.width:
            splitUnits = True
            unitNameWidth = config.width
        else:
            splitUnits = False
        DebugManager.debug("running for <input_file>:"+args[0]+" and the following options: "+str(config))

        inputFile = args[0]
        initSet = set([]); initNames = []; typeDecls = set([])
        if splitUnits:
            (base,ext) = os.path.splitext(inputFile)
            unitNumExt = "%0"+str(unitNameWidth)+"d"
            unit_num = 0
            unitStartTime=None
            if (config.timing):
                unitStartTime=datetime.datetime.utcnow()
            for aUnit in fortUnitIterator(inputFile,config.inputFormat):
                if (config.explicitInit):
                    UnitPostProcessor(aUnit).getInitCommonStmts(initSet,initNames,typeDecls)
                outputFile = base + unitNumExt % unit_num + ext; unit_num+=1
                ourOutFileHandle = open(outputFile,'w')
                ourOutFileNameList.append(outputFile)
                UnitPostProcessor(aUnit).processUnit().printit(ourOutFileHandle)                    
                ourOutFileHandle.close()
                if (config.progress):
                    msg='SourceProcessing: progress: done with unit '+aUnit.uinfo.name
                    if (config.timing):
                        nTime=datetime.datetime.utcnow()
                        msg+=' took: '+str(nTime-unitStartTime)
                        unitStartTime=nTime
                    print msg
            # add new init procedures & global init procedure at end
            if (config.explicitInit):
                addInitProcedures(initSet,initNames,typeDecls,base=base,unitNumExt=unitNumExt,\
                                      unit_num=unit_num,ext=ext,splitUnits=splitUnits)

            makeOut = open('postProcess.make','w')
            makeOut.write("POSTPROCESSEDFILES=")
            for outFileName in ourOutFileNameList:
                makeOut.write(" \\\n"+outFileName)
            makeOut.write("\n")
            makeOut.close()
        # SEPARATE OUTPUT INTO FILES AS SPECIFIED BY PRAGMAS
        elif config.separateOutput:
            ourOutFileHandle = None
            setFormat = False
            if config.outputFormat == None:
                setFormat = True
            for aUnit in fortUnitIterator(inputFile,config.inputFormat):
                if (config.explicitInit):
                    UnitPostProcessor(aUnit).getInitCommonStmts(initSet,initNames,typeDecls)
                # We expect to find file pragmas in the cmnt section of units exclusively
                if aUnit.cmnt:
                    if (re.search('openad xxx file_start',aUnit.cmnt.rawline,re.IGNORECASE)):
                        # close the previous output file (if any)
                        if ourOutFileHandle: ourOutFileHandle.close()
                        # extract the new output file location (and add path and filename suffixes)
                        (head,tail) = os.path.split(aUnit.cmnt.rawline.split('start [')[1].split(']')[0])
                        (fileName,fileExtension) = os.path.splitext(tail)
                        outputDirectory = config.pathPrefix+head+config.pathSuffix
                        if outputDirectory == '': outputDirectory = './'
                        if not os.path.exists(outputDirectory): os.makedirs(outputDirectory)
                        newOutputFile = os.path.join(outputDirectory,fileName+config.filenameSuffix+fileExtension)
                        if setFormat:
                            config.outputFormat = Ffile.get_format(fileExtension)
                            setOutputFormat(config.outputFormat)
                        ourOutFileNameList.append(newOutputFile)
                        ourOutFileHandle = open(newOutputFile,'w')
                elif not ourOutFileHandle:
                    raise PostProcessError('option separateOutput specified, no output file can be determined for the first unit',0)
                # postprocess the unit and print it
                UnitPostProcessor(aUnit).processUnit().printit(ourOutFileHandle)
            # add new init procedures & global init procedure after module declarations
            if (config.explicitInit):
                addInitProcedures(initSet,initNames,typeDecls,ourOutFileHandle)
            ourOutFileHandle.close()
        else: 
            ourOutFileHandle=None
            if config.outputFile: 
                ourOutFileHandle = open(config.outputFile,'w')
                ourOutFileNameList.append(config.outputFile)
            else:
                ourOutFileHandle=sys.stdout
            for aUnit in fortUnitIterator(inputFile,config.inputFormat):
                if (config.explicitInit):
                    UnitPostProcessor(aUnit).getInitCommonStmts(initSet,initNames,typeDecls)
                UnitPostProcessor(aUnit).processUnit().printit(ourOutFileHandle)
            # add new init procedures & global init procedure after module declarations
            if (config.explicitInit):
                addInitProcedures(initSet,initNames,typeDecls,ourOutFileHandle)
            if config.outputFile: 
                ourOutFileHandle.close()

        if (config.timing):
            print 'SourceProcessing: timing: '+str(datetime.datetime.utcnow()-startTime)

    except (PostProcessError,UserError,AssemblerException,ListAssemblerException,ParseError,ScanError),e:
        sys.stderr.write(str(e))
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
    # use the following to generate profile data that can be viewed e.g. with RunSnakeRun
    # import cProfile
    # cProfile.runctx( 'main()', globals(), locals(), filename="postProcess.profile" )
    sys.exit(main())

