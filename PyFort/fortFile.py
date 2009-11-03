'''
Duplicate (most of) the functionality of perl Ffile.pm

In other words, read in a Fortran file, assembling the
line objects.
Provide mappers, rewriters, string conversion facilities
'''
from _Setup import *
from cStringIO import StringIO
from fortLine  import fortLine,cline,fline
from PyUtil.assembler import vgen,treat,disj
from PyUtil.buf_iter  import buf_iter
from PyUtil.errors import UserError
from freefmt       import freefmt
from fixedfmt      import fixedfmt
from process_fort_stmt import process_fort_stmt,process_fort_cmnt
from flow import setFixedOrFreeFormat
import os

def _ident(s):
    return [s]

class Ffile(object):
    def __init__(self,fobj,free=False,c_action=cline,s_action=fline):
        fmt      = (free and freefmt) or fixedfmt
        fl       = fortLine(fmt)
        s      = lambda x:process_fort_stmt(x,self.rawBufIter.myCounter,s_action)
        c      = lambda x:process_fort_cmnt(x,self.rawBufIter.myCounter,c_action)
        cblk   = treat(fl.cblk,c)
        # produces multiple statements, if multiple statements were on
        # the same line in the original file
        stmt   = treat(fl.stmt,s)
        a_line = disj(cblk,stmt)

        self.rawBufIter = buf_iter(fobj)
        self.lines = vgen(a_line,self.rawBufIter,True)
        self.fobj  = fobj

    @staticmethod
    def file(name,c_action=cline,s_action=fline,inputFormat=None,outputFormat=None):
        import PyUtil.debugManager
        try:
          f=open(name)
        except IOError:
          msg="Error cannot open file named: "+name
          raise UserError(msg)
        PyUtil.debugManager.DebugManager.setProcessedFile(name)
        #set formatting based on name extension
        ext = os.path.splitext(name)[1]
        if not inputFormat:
            if ext in ['.f90','.f95','.f03']:
                inputFormat='free'
                setFixedOrFreeFormat('free',outputFormat)
            else:
                inputFormat='fixed'
                setFixedOrFreeFormat('fixed',outputFormat)
        else:
            setFixedOrFreeFormat(inputFormat,outputFormat)
        return Ffile(f,inputFormat=='free',c_action,s_action)

    @staticmethod
    def here(str,free=False,c_action=cline,s_action=fline):
        return Ffile(StringIO(str),free,c_action,s_action)

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

    def printit(self,out=sys.stdout):
        for l in self.iterlines():
            print >> out,l,
    
    def write(self,fname):
        ff = open(fname,'w')

        self.printit(ff)

        ff.close()

    def map(self,lexi,*args,**kwargs):
        for (cls,meth) in lexi:
            cls.map = meth
        for l in self.lines:
            for ll in l.map(*args,**kwargs):
                yield ll
        for (cls,meth) in lexi:
            cls.map = _ident
