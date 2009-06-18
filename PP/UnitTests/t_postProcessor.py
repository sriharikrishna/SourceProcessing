#!/usr/bin/env python

import tempfile
from Setup import *
from _Setup import *
from unittest import *
from PyUtil.errors import UserError
from PyUtil.symtab import Symtab
from PyFort.fortUnit import fortUnitIterator
from PyFort.fortStmts import RealStmt,IntegerStmt
from unitPostProcess import UnitPostProcessor,PostProcessError
from PyUtil.debugManager import DebugManager

'''
Unit tests for post-processor

'''

DebugManager.setQuiet(True)

Symtab.setTypeDefaults((RealStmt,[]),(IntegerStmt,[]))

def compareFiles(assertFunc,originalFileName,RefFileName,free):
    try:
        (fd,testFileName) = tempfile.mkstemp()
        testFile  = open(testFileName,'w')
        for aUnit in fortUnitIterator(fname_t(originalFileName),free):
            UnitPostProcessor(aUnit).processUnit().printit(testFile)
        testFile.close()
        testFile = open(testFileName,'r')
        testFileLines = testFile.readlines()
        refFile = open(fname_t(RefFileName),'r')
        refFileLines = refFile.readlines()
        assertFunc(len(testFileLines),len(refFileLines),'transformation result and reference file have disparate line counts')
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

    def test00(self):
        'post-process empty file: empty.f'
        compareFiles(self.assertEquals,'empty.f','empty.post.f',free=False)

    def test1(self):
        'post-process file with declarations which should not change: implicit.f'
        #implicit, save, real, integer, etc
        compareFiles(self.assertEquals, 'implicit.f','implicit.post.f',free=False)

    def test2(self):
        'post-process a file with included modules: modules.f'
        #active module should be added
        compareFiles(self.assertEquals, 'modules.f','modules.post.f',free=False)

    def test3(self):
        'test type replacements: active.f'
        compareFiles(self.assertEquals,'active.f','active.post.f',free=False)

    def test4(self):
        'test inline replacement of __value__ and __deriv__ in declaration statements: decl_inline.f'
        compareFiles(self.assertEquals,'decl_inline.f','decl_inline.post.f',free=False)

    def test5(self):
        'test inline replacement of __value__ and __deriv__ in execution statements: exec_inline.f'
        compareFiles(self.assertEquals,'exec_inline.f','exec_inline.post.f',free=False)

    def test6(self):
        'test inline replacement of nested __value__ and __deriv__ in decl and execution statements: nested_inline.f'
        compareFiles(self.assertEquals,'nested_inline.f','nested_inline.post.f',free=False)

suite = asuite(C1)

if __name__ == "__main__":
    sys.exit(runit(suite))

