'''
Module to list the intrinsic functions, and
a predicate to test for a function being an intrinsic
'''
__intrinsics = (
    'abs',
    'acos',
    'adjustl',
    'adjustr',
    'aimag',
    'allocated',
    'alog',
    'amax0',
    'amax1',
    'amin0',
    'amin1',
    'any',
    'asin',
    'associated',
    'atan',
    'atan2',
    'close',
    'cmplx',
    'cos',
    'dabs',
    'dble',
    'dcos',
    'dexp',
    'dfloat',
    'dlog',
    'dmax1',
    'dmin1',
    'dmod',
    'dot_product',
    'dsign',
    'dsin',
    'dsqrt',
    'exp',
    'float',
    'idint',
    'index',
    'int',
    'iabs',
    'lbound',
    'len',
    'len_trim',
    'lge',
    'lgt',
    'lle',
    'llt',
    'log',
    'log10',
    'max',
    'max0',
    'maxval',
    'min',
    'min0',
    'minloc',
    'minval',
    'mod',
    'nint',
    'open',
    'present',
    'read',
    'real',
    'repeat',
    'shape',
    'sign',
    'sin',
    'sinh',
    'size',
    'sqrt',
    'sum',
    'tan',
    'tanh',
    'transfer',
    'transpose',
    'trim',
    'ubound',
    )

__nonStandard = (
                 'getuid',
                 'getpid',
                 'hostnam',
                 'loc',
                 'sleep'   
                 )

def getNonStandard():
    return __nonStandard

__useNonStandard=[]
def useNonStandard(aList):
    global __useNonStandard
    if aList: 
        __useNonStandard=aList
    
__inquiry = (
    'any',
    'associated',
    'allocated',
    'present',
    'shape',
    'size',
    )

def is_intrinsic(op):
    return (op.lower() in __intrinsics) or (op.lower() in __useNonStandard)

def is_inquiry(op):
    return op.lower() in __inquiry

def getGenericName(op):
    ''' convert F77 archaic specific names to generic names '''
    if not is_intrinsic(op):
        raise Exception("argument "+op+" is not an intrinsic")
    archaicName=op.lower()
    if (archaicName[0] == 'a' and archaicName[-1] in ('0','1') and archaicName[1:-1] in ('max','min') ):
        return archaicName[1:-1]
    elif (archaicName[0] == 'd' and archaicName[-1] == '1' and archaicName[1:-1] in ('max','min') ):
        return archaicName[1:-1]
    elif (archaicName[-1] in ('0','1') and archaicName[:-1] in ('max','min') ):
        return archaicName[:-1]
    elif (archaicName[0] == 'd' and archaicName[1:] in ('sign','abs','log','sqrt','mod','sin','cos') ):
        return archaicName[1:]
    elif (archaicName[0] == 'i' and archaicName[1:] in ('abs') ):
        return archaicName[1:]
    elif (archaicName[0] == 'a' and archaicName[1:] in ('log') ):
        return archaicName[1:]
    elif (archaicName == 'idint') :
        return 'int'
    elif (archaicName == 'dfloat') :
        return 'dble'
    else :
        return archaicName
    
def isPolymorphic(op):
    '''  true for intrinscis with generic names '''
    if not is_intrinsic(op):
        raise Exception("argument "+op+" is not an intrinsic")
    return op.lower() in ('max','min','sign')
