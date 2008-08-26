'''Hierachical symbol table objects =
   Linked list of caselessDicts
'''

import _Setup

from PyUtil.caselessDict import caselessDict as cDict

class Symtab(object):
    def __init__(self,parent=None):
        self.ids    = cDict()
        self.parent = parent

    def lookup_name_level(self,name):
        S = self
        while S:
            D = S.ids
            if name in D:
                return (D[name],S)
            S = S.parent
        return (None,None)

    def lookup_name(self,name):
        (entry,dc) = self.lookup_name_level(name)
        return entry

    def enter_name(self,name,v):
        self.ids[name] = v
