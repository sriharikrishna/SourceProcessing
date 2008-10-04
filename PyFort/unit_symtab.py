'''symtab with specific stuff for units:

    Inheirit from PyUtil.Symtab, but add various entry types, and manipulations
    to the main symtab methods
'''

import _Setup

from PyUtil.symtab import Symtab as Base_symtab
from PyUtil.caselessDict import caselessDict as cDict


class Symtab(Base_symtab):
    def __init__(self,*args,**kws):
        Base_symtab.__init__(self,*args,**kws)
        self.dbg = False
        self.default_implicit()

    def default_implicit(self):
        if self.parent:
            self.implicit = self.parent.implicit
            return

        self.implicit = cDict()
        for l in '_abcdefghopqrstuvwxyz':
            self.implicit[l] = self._default_real
        for l in 'ijklmn':
            self.implicit[l] = self._default_int
    
    def implicit_none(self):
        for l in '_abcdefghijklmnopqrstuvwxyz':
            self.implicit[l] = None

    def _set_dbg(self,on=False):
        self.dbg = on

    def enter_name(self,name,val):
        if self.dbg:
            print "entering %s = %s in %s" % (name,val,self)
        Base_symtab.enter_name(self,name,val)

    def lookup_dims(self,name):
        entry = self.lookup_name(name)
        if entry: return entry.lookup_dims()
        return None

    def lookup_lngth(self,name):
        entry = self.lookup_name(name)
        if entry: return entry.lookup_lngth()
        return None

    def lookup_type(self,name):
        (entry,level) = self.lookup_name_level(name)
        if entry: return entry.lookup_type(level.implicit[name[0]])
        
        return self.implicit[name[0]]

    def update_w_module(self,unit):
        'update self with ids from module "unit" symtab. module name = "name"'
        module_ids = unit.symtab.ids

        origin_label = 'module:'+unit.name()

        self.ids.update(module_ids)

        for n in module_ids:
            self.ids[n].origin = origin_label
        
if __name__ == '__main__':
    def check_it(self):
        return 'I am unit_symtab' + str(self)

    Symtab.check_it = check_it
    
    Symtab._default_real = 'real'
    Symtab._default_int  = 'int'
    s = Symtab()
    s.enter_name('foo','top level foo')
    s1 = Symtab(s)
    s1.enter_name('bar','level 2 bar')

    res1 = s1.lookup_name('bar')
    res2 = s1.lookup_name('foo')
    res3 = s.lookup_name('bar')
    res4 = s.lookup_name('foo')

    res5 = s.check_it()
    res6 = s1.check_it()
