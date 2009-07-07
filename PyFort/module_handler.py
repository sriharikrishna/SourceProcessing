'''Save and get module symbol tables
Currently only a thin veneer over caseless dict
'''
import _Setup
from PyUtil.caselessDict import caselessDict as cDict

class ModuleHandler(object):
    def __init__(self):
        self.modules = cDict()
    def add_module(self,name,stab):
        self.modules[name] = stab
    def get_module(self,name):
        return self.modules.get(name)

ourModuleHandler = ModuleHandler()

