#!/usr/bin/env python

import tempfile
from Setup import *
from _Setup import *
from unittest import *
from PyUtil.errors import UserError
from PyFort.fortUnit import fortUnitIterator
from canon import UnitCanonicalizer

'''
Unit tests for canonicalizer

'''

class C1(TestCase):

    def canonTest(self,originalFileName,RefFileName,free):
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
            self.assertEquals(len(testFileLines),len(refFileLines))
            for testLine,refLine in zip(testFileLines,refFileLines):
                self.assertEquals(testLine,refLine)
            refFile.close()
            testFile.close()
            os.remove(testFileName)
        except UserError,e:
            print >>sys.stderr,"Error: ",e.msg
            return 1

    def test00(self):
        'canonicalize empty file'
        self.canonTest('empty.f','empty.pre.f',free=False)

    def test0(self):
        'hoist function call from if statement (without "then")'
        self.canonTest('ifNonThenStmt.f','ifNonThenStmt.pre.f',free=False)

    def test1(self):
        'hoist function call from if-then statement'
        self.canonTest('ifThenStmt.f','ifThenStmt.pre.f',free=False)

    def test2(self):
        'canon of max using int consts w embedded kinds'
        self.canonTest('int-const-w-kind.f90','int-const-w-kind.ok.f90',free=True)

    def test3(self):
        'canon array of derived types'
        self.canonTest('derived-type-arr.f90','derived-type-arr.ok.f90',free=True)

#   def test5(self):
#       'canonicalization of conditional statements'
#       self.canonTest('conditionals.f','conditionals.pre.f',free=False)

    def test5(self):
        'preserve inline comments for statements that have been altered during canonicalization'
        self.canonTest('inlineComment.f90','inlineComment.pre.f90',free=True)

suite = asuite(C1)

if __name__ == "__main__":
    runit(suite)

