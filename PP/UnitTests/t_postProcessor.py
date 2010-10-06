#!/usr/bin/env python

import tempfile
from Setup import *
from _Setup import *
from unittest import *
from PyUtil.errors import UserError
from PyUtil.symtab import Symtab
from PyFort.fortUnit import fortUnitIterator
from PyFort.fortStmts import RealStmt,IntegerStmt,SubroutineStmt
from PyFort.flow import setOutputFormat
from PyUtil.debugManager import DebugManager
from unitPostProcess import UnitPostProcessor,PostProcessError
from PP.templateExpansion import TemplateExpansion

'''
Unit tests for post-processor

'''

DebugManager.setQuiet(True)

Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))

def addInitProcedures(initSet,initNames,typeDecls,output,splitUnits=False,output2=None):
    for elt in initSet:
        newUnit = UnitPostProcessor.createInitProcedure(elt,typeDecls)
        if newUnit is not None:
            # print new output file
            if splitUnits:
                out = open(output,'w')
                outFileNameList.append(output)
                # print new output file
                newUnit.printit(out)
                out.close()
                unit_num += 1                
            else:
                newUnit.printit(output)
    if len(initNames) > 0:
        newUnit = UnitPostProcessor.createGlobalInitProcedure(initNames)
        if splitUnits:
            out = open(output2,'w')
            outFileNameList.append(output2)
            newUnit.printit(out)
            out.close()
        else:
            newUnit.printit(output)
        return (newUnit!=None)

def compareFiles(assertFunc,originalFileName,RefFileName,format='fixed',mode='forward',templateFile='dummy.template.f',inlineFile='dummy.inline.f'):
    try:
        (fd,testFileName) = tempfile.mkstemp()
        testFile  = open(testFileName,'w')
        UnitPostProcessor.setMode(mode)
        setOutputFormat(format)
        insertGlobalInitCall = False
        initSet = set([])
        initNames = []
        typeDecls = set([])
        if (mode=='reverse'):
            if (inlineFile):
                UnitPostProcessor.setInlineFile(fname_t(inlineFile))
                UnitPostProcessor.processInlineFile()
            if (templateFile):
                TemplateExpansion.setTemplateFile(fname_t(templateFile))
            # get common block variables to initialize if processing in reverse mode
            for aUnit in fortUnitIterator(fname_t(originalFileName),format):
                UnitPostProcessor(aUnit).getInitCommonStmts(initSet,initNames,typeDecls)
            insertGlobalInitCall = (len(initNames) > 0)
        # add new init procedures & global init procedure
        addInitProcedures(initSet,initNames,typeDecls,testFile)
        for aUnit in fortUnitIterator(fname_t(originalFileName),format):
            if isinstance(aUnit.uinfo,SubroutineStmt) and len(initNames) > 0 and insertGlobalInitCall:
                UnitPostProcessor(aUnit).processUnit(insertGlobalInitCall).printit(testFile)
                insertGlobalInitCall = False
            else:
                UnitPostProcessor(aUnit).processUnit().printit(testFile)
        testFile.close()
        testFile = open(testFileName,'r')
        testFileLines = testFile.readlines()
        refFile = open(fname_t(RefFileName),'r')
        refFileLines = refFile.readlines()
        #assertFunc(len(testFileLines),len(refFileLines),'transformation result ('+testFileName+') and reference file have disparate line counts')
        for testLine,refLine in zip(testFileLines,refFileLines):
            assertFunc(testLine,refLine)
        refFile.close()
        testFile.close()
        os.remove(testFileName)
    except PostProcessError,e:
        print >>sys.stderr,'\nPost-processing Error on line '+str(e.lineNumber)+':\n',e.msg
        refFile.close()
        testFile.close()
        os.remove(testFileName)
        raise e
    except UserError,e:
        print >>sys.stderr,"Error: ",e.msg
        refFile.close()
        testFile.close()
        os.remove(testFileName)
        raise e
    except AssertionError,e:
        print >>sys.stderr,"Error: "+str(e)+"; comparing files: "+testFileName+" "+os.path.join("Tfiles",RefFileName)+"\n" 
        refFile.close()
        testFile.close()
        raise e


class C1(TestCase):

    def test0(self):
        'post-process empty file: empty.f'
        compareFiles(self.assertEquals,'empty.f','empty.post.f')

    def test1(self):
        'post-process file with declarations which should not change: implicit.f'
        #implicit, save, real, integer, etc
        compareFiles(self.assertEquals, 'implicit.f','implicit.post.f')

    def test2(self):
        'post-process a file with included modules: modules.f'
        #active module should be added
        compareFiles(self.assertEquals, 'modules.f','modules.post.f')

    def test3(self):
        'test type replacements: active.f'
        compareFiles(self.assertEquals,'active.f','active.post.f')

    def test4(self):
        'test inline replacement of __value__ and __deriv__ in declaration statements: decl_inline.f'
        compareFiles(self.assertEquals,'decl_inline.f','decl_inline.post.f')

    def test5(self):
        'test inline replacement of __value__ and __deriv__ in execution statements: exec_inline.f'
        compareFiles(self.assertEquals,'exec_inline.f','exec_inline.post.f')

    def test6(self):
        'test inline replacement of nested __value__ and __deriv__ in decl and execution statements: nested_inline.f'
        compareFiles(self.assertEquals,'nested_inline.f','nested_inline.post.f')

    def test7(self):
        'test replacement of __value__ and __deriv__ in simple I/O statements: ioSimple.f'
        compareFiles(self.assertEquals,'ioSimple.f','ioSimple.post.f')

    def test8(self):
        'test replacement of __value__ and __deriv__ in expressions in I/O statements : ioExpr.f'
        compareFiles(self.assertEquals,'ioExpr.f','ioExpr.post.f')

    def test9(self):
        'test replacement of __value__ and __deriv__ in implicit do loops in I/O statements : ioImplDo.f'
        compareFiles(self.assertEquals,'ioImplDo.f','ioImplDo.post.f')

    def test10(self):
        'test inlining: inlinepush2_simple.f'
        compareFiles(self.assertEquals,'inlinepush2_simple.f','inlinepush2_simple.post.f',mode='reverse',templateFile='inlinepush2.template.f',inlineFile='inlinepush2.inline.f')

    def test11(self):
        'test inlining: inlinepush2.f'
        compareFiles(self.assertEquals,'inlinepush2.f','inlinepush2.post.f',mode='reverse',templateFile='inlinepush2.template.f',inlineFile='inlinepush2.inline.f')

    def test12(self):
        'test initialization of derivative components of active variables within a module'
        compareFiles(self.assertEquals,'module_init.f90','module_init.post.f90',format='free',mode='reverse',templateFile='dummy.template.f',inlineFile='dummy.inline.f')

    def test13(self):
        'test initialization of derivative components of active variables within a common block'
        compareFiles(self.assertEquals,'common_init.f90','common_init.post.f90',format='free',mode='reverse',templateFile='dummy.template.f',inlineFile='dummy.inline.f')

suite = asuite(C1)

if __name__ == "__main__":
    sys.exit(runit(suite))

