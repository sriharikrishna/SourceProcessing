'''
Duplicate (most of) the functionality of perl Ffile.pm

In other words, read in a Fortran file, assembling the
line objects.
Provide mappers, rewriters, string conversion facilities
'''
from _Setup import *
from cStringIO import StringIO
#from fortLine  import a_line
from fortLine  import fortLine
from PyUtil.assembler import vgen
from PyUtil.buf_iter  import buf_iter
from PyUtil.errors import UserError
from freefmt       import freefmt
from fixedfmt      import fixedfmt

def _ident(s):
    return [s]

class Ffile(object):
    def __init__(self,fobj,free=False):
        fmt = (free and freefmt) or fixedfmt
        a_line = fortLine(fmt).a_line
        self.lines = vgen(a_line,buf_iter(fobj))
        self.fobj  = fobj

    @staticmethod
    def file(name,free=False):
        try:
          file=open(name)
        except IOError:
          msg="Error cannot open file named: "+name
          raise UserError(msg)
        return Ffile(open(name),free)

    @staticmethod
    def here(str,free=False):
        return Ffile(StringIO(str),free)

    def str(self):
        '''return all of the original file lines concatenated together
        WARNING: do not use on large files !!
        '''
        return ''.join([l.rawline for l in self.lines])

    def readlines(self):
        '''Make Ffile function like readlines for regular files'''
        return [ ll for l in self.lines for ll in l.rawline.splitlines(True) ]

    def iterlines(self):
        '''instead of the whole line list, return an iterator
        that yields 1 rawline at a time
        '''
        return ( (ll for l in self.lines for ll in l.rawline.splitlines(True)) )

    def printit(self,out=None):
        if out:
            for l in self.iterlines():
                print >> out,l,
        else:
            for l in self.iterlines():
                print l,
    
    def write(self,fname):
        ff = open(fname,'w')

        self.printit(ff)

        ff.close()

    def map(self,lexi):
        for (cls,meth) in lexi:
            cls.map = meth
        for l in self.lines:
            for ll in l.map():
                yield ll
        for (cls,meth) in lexi:
            cls.map = _ident
