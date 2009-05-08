import sys

from _Setup import *

from PyUtil.assembler import *
from PyUtil.buf_iter import buf_iter
from PyUtil.debugManager import DebugManager
from PyUtil.flatten import flatten
from PyUtil.symtab import Symtab

import fortStmts as fs
from fortFile import Ffile
from fortParse import parse_stmt,parse_cmnt
from module_handler import ModuleHandler

import stmt2unit

def instance_pred(*class_list):
    '''define a predicate that checks if argument is an instance of 1 of the
    classes listed in class_list
    '''
    def the_pred(x):
        for c in class_list:
            if isinstance(x,c):
                return True
        return False
    return the_pred

def install_pat(cur):

    def _show(s):
        print 'looking at:',s
        return s

    def action(cur):
        def _action(self):
            return self.unit_action(cur)
        return _action
    
    _ustart    = lambda s: s.is_ustart()
    _uend      = lambda s: s.is_uend()

    startunit  = treat(pred(_ustart))
    endunit    = pred(_uend)
    cblk       = pred(lambda s: s.is_comment())
    ulist_pre  = pred(lambda s: s.is_contains())
    adecl      = treat(pred(lambda s: s.is_decl(cur)),action(cur))
    aexec      = pred(lambda s: s.is_exec())


    (c,u,d,x,n,e) = (cblk,
                     startunit,
                     adecl,
                     aexec,
                     ulist_pre,
                     endunit)

    fmod = cur.module_handler

    def handle_mod(aUnit):
        if isinstance(aUnit.uinfo,fs.ModuleStmt):
            fmod.add_module(aUnit.uinfo.name,aUnit)
        return aUnit

    def cmst(p): return treat(seq(zo1(c),p),flatten)

    def _ul(s):
        'eta expansion for recursive patterns'
        return uu(s)

    uh    = treat(seq(zo1(c),u),cur.uhead)
    udcl  = treat(star(cmst(d)),flatten,cur.udecl)
    uexc  = treat(star(cmst(x)),flatten,cur.uexec)
    cnth  = treat(cmst(n),cur.ucont)
    cntl  = treat(star(_ul),cur.ulist)
    uctn  = zo1(seq(cnth,cntl))
    uend  = treat(cmst(e),cur.uend,handle_mod)

    uu    = treat(seq(uh,udcl,uexc,uctn,uend),lambda s:cur.fval)
    ucm   = treat(c,cur.ucm,lambda s:cur.fval)
    return disj(uu,ucm)

def _symtab_of(v):
    return v and v.symtab or None

class Unit(object):
    '''unit object
    cmnt       unit comment block (may not exist)
    uinfo      unit type (module,subroutine,function,blockdata,program)
    decls      list of declaration statements (including comments)
    execs      list of executable statements (including comments)
    contains   contains list (including comments)
    ulist      unit list (the units "contain"ed within this one)
    end        end list (including comments)
    symtab     hierachical symbol table
    fmod       ???? (fmod = cur.module_handler)
    _in_iface  whether or not we are currently in an interface block
    '''

    def __init__(self,parent=None,fmod=None):
        'create a unit'
        self.cmnt      = None
        self.uinfo     = None
        self.decls     = []
        self.execs     = []
        self.contains  = []
        self.ulist     = []
        self.end       = []
        self.parent    = parent
        self.symtab    = Symtab(_symtab_of(parent))
        self.fmod      = fmod
        self._in_iface = False

        DebugManager.debug('new unit created: '+str(self)+', new symtab being created = '+str(self.symtab))

    def name(self):
        return self.uinfo.name

    def printit(self,out=sys.stdout):
        if self.cmnt:
            print >> out,self.cmnt.rawline,
        if self.uinfo:
            print >> out,self.uinfo.rawline,
        for aDeclStmt in self.decls:
            print >> out,aDeclStmt.rawline,
        for anExecStmt in self.execs:
            print >> out,anExecStmt.rawline,
        for aContainsEntry in self.contains:
            print >> out,aContainsEntry.rawline,
        for aSubUnit in self.ulist:
            aSubUnit.printit(out)
        if self.end:
            for anEndListEntry in self.end:
                print >> out,anEndListEntry.rawline,

# end class Unit

class _curr(object):
    '''helper object:
    embodies the notion of "current" object being built
    incrementally via the subpatterns in a seq pattern
    '''
    def __init__(self,module_handler=None):
        self.val            = Unit(None,module_handler)
        self.fval           = None  
        self.module_handler = module_handler

    def uhead(self,s):
        self.uinfo(s[1])
        if s[0]:
            self.ucomm(s[0][0])
        return self.val

    def ucomm(self,s):
        self.val.cmnt = s
        return self.val

    def uinfo(self,s):
        self.val.uinfo = s
        s.unit_entry(self)
        return self.val

    def udecl(self,s):
        self.val.decls = s
        return self.val

    def uexec(self,s):
        self.val.execs = s
        return self.val

    def uend(self,s):
        u = self.val
        p = u.parent
        u.end = s
        self.fval = u
        self.val  = Unit(p,self.module_handler)
        return u

    def ucont(self,s):
        u = self.val
        u.contains = s
        self.val = Unit(u,self.module_handler)
        return u

    def ulist(self,l):
        p = self.val.parent
        self.val = p
        self.val.ulist = l
        return self.val

    def ucm(self,cmnt):
        u         = self.val
        p         = u.parent
        u.cmnt    = cmnt
        self.fval = u
        self.val  = None # no need for Unit(p), as this must be last
        return u

def fortUnitIterator(fileName,free):
    return vgen(install_pat(_curr(ModuleHandler())),
                buf_iter(Ffile.file(fileName,free,parse_cmnt,parse_stmt).lines))

if __name__ == '__main__':
    from _Setup.testit import *
    from itertools import *

    fn0 = 'units1.f90'
    i0  = fortUnitIterator(fname_t(fn0),True)
    fn1 = 'units2.f90'
    i1  = fortUnitIterator(fname_t(fn1),True)
    fn2 = 'units3.f90'
    i2  = fortUnitIterator(fname_t(fn2),True)
    fn3 = 'units4.f90'
    i3  = fortUnitIterator(fname_t(fn3),True)
    fn4 = 'units5.f90'
    i4  = fortUnitIterator(fname_t(fn4),True)
    fn5 = 'units6.f90'
    i5  = fortUnitIterator(fname_t(fn5),True)

    fn  = 'if.f90'
    ii  = fortUnitIterator(fname_t(fn),True)
