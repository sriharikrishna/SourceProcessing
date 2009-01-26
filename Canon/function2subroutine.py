'''
Function to Subroutine conversion
'''

from _Setup import *

#from PyUtil.symtab import SymtabEntry,SymtabError

#from PyFort.typeInference import TypeInferenceError,expressionType,functionType,isArrayReference
#import PyFort.fortExp as fe
#import PyFort.fortStmts as fs
from PyFort.fortUnit import Unit

def convertFunction(functionUnit):
    newSubUnit = Unit(parent=functionUnit.parent,fmod=functionUnit.fmod)
    # iterate over decls for functionUnit
    for aDecl in functionUnit.decls:
        print '\tencountered decl "'+str(aDecl)+'"'
    print
    # iterate over execs for functionUnit
    for anExec in functionUnit.execs:
        print '\tencountered exec "'+str(anExec)+'"'
    return newSubUnit

