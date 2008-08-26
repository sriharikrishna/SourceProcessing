'''Define the various kinds of symbol table entries
'''
class gen_entry(object):
    def enter_type(self,*args,**kw):
        print 'cannot enter type for %s',self

    def lookup_dims(self,*a,**k):
        return None

    def lookup_type(self,*a,**k):
        return None

class subr(gen_entry):
    def __init__(self,name,params,lcl,vtype=None):
        self.name   = name
        self.params = params
        self.lcl    = lcl
        self.vtype  = vtype

class func(gen_entry):
    def __init__(self,name,params,lcl,vtype=None):
        self.name   = name
        self.params = params
        self.lcl    = lcl
        self.vtype  = vtype

    def enter_type(self,vtype):
        print 'entering |%s| into functional vtype' % vtype
        self.vtype   = vtype

    def lookup_type(self,default_t):
        return self.vtype or self.lcl.implicit[self.name[0]]

class var(gen_entry):
    def __init__(self,vtype,dims,lngth=None,origin='local'):
        self.vtype   = vtype
        self.dims    = dims
        self.lngth   = lngth
        self.origin  = origin

    def enter_type(self,vtype,typemod):
        self.rtype   = vtype
        self.typemod = typemod

    def lookup_dims(self):
        return self.dims

    def lookup_type(self,default_t):
        return self.vtype or default_t

class dtype(gen_entry):
    def __init__(self,name,fields):
        self.name = name
        self.fields = fields

class iface(gen_entry):
    def __init__(self,name,entries):
        self.name = name
        self.entries = entries

class stmtfn(gen_entry):
    def __init__(self,args,body):
        self.args = args
        self.body = body
