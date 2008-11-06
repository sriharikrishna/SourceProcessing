#!/usr/bin/env python

import tempfile
from Setup import *
from _Setup import *
from unittest import *
from PyUtil.errors import UserError
from PyFort.fortUnit import fortUnitIterator
from canon import UnitCanonicalizer,CanonError

'''
Unit tests for canonicalizer

'''

Symtab.setTypeDefaults((fs.RealStmt,[]),(fs.IntegerStmt,[]))

def compareFiles(assertFunc,originalFileName,RefFileName,free):
    UnitCanonicalizer.setVerbose(False) 
    try:
        (fd,testFileName) = tempfile.mkstemp()
        testFile  = open(testFileName,'w')
        for aUnit in fortUnitIterator(fname_t(originalFileName),free):
            UnitCanonicalizer(aUnit).canonicalizeUnit()
            aUnit.printit(testFile)
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
    except CanonError,e:
        print >>sys.stderr,'\nCanonicalization Error on line '+str(e.lineNumber)+':\n',e.msg
        return 1
    except UserError,e:
        print >>sys.stderr,"Error: ",e.msg
        return 1

class C1(TestCase):

    def test00(self):
        'canonicalize empty file'
        compareFiles(self.assertEquals,'empty.f','empty.pre.f',free=False)

    def test2(self):
        'canon of max using real consts w embedded kinds'
        compareFiles(self.assertEquals,'realConst_withKind.f90','realConst_withKind.pre.f90',free=True)

    def test3(self):
        'canon array of derived types'
        compareFiles(self.assertEquals,'derived-type-arr.f90','derived-type-arr.ok.f90',free=True)

    def test5(self):
        'preserve inline comments for statements that have been altered during canonicalization'
        compareFiles(self.assertEquals,'inlineComment.f90','inlineComment.pre.f90',free=True)

    def test5(self):
        'preserve inline comments for statements that have been altered during canonicalization'
        compareFiles(self.assertEquals,'inlineComment.f90','inlineComment.pre.f90',free=True)

    def test6(self):
        'canonicalize a subunit (subroutine contained in program)'
        compareFiles(self.assertEquals,'canonicalizeSubunit.f90','canonicalizeSubunit.pre.f90',free=True)

class C2(TestCase):
    '''Coverage for particular kinds of statements'''
    def test0(self):
        'Hoist function call from if statement (without "then")'
        compareFiles(self.assertEquals,'ifNonThenStmt.f','ifNonThenStmt.pre.f',free=False)

    def test1(self):
        'Hoist function call from if-then statement'
        compareFiles(self.assertEquals,'ifThenStmt.f','ifThenStmt.pre.f',free=False)

    def test2(self):
        'Hoist function calls from do and do while statements'
        compareFiles(self.assertEquals,'doDoWhile.f','doDoWhile.pre.f',free=False)

class TestCanonicalizeSubroutineCall(TestCase):
    '''Subroutine call statements'''
    def test1(self):
        'Hoist intrinsic function call from subroutine call statement'
        compareFiles(self.assertEquals,'subCall_hoistIntrinsic.f90','subCall_hoistIntrinsic.pre.f90',free=True)

    def test2(self):
        'Hoist nonintrinsic function call from subroutine call statement'
        compareFiles(self.assertEquals,'subCall_hoistNonintrinsic.f90','subCall_hoistNonintrinsic.pre.f90',free=True)

suite = asuite(C1,C2,TestCanonicalizeSubroutineCall)

if __name__ == "__main__":
    runit(suite)

