'''
Module to list the intrinsic functions, and
a predicate to test for a function being an intrinsic
'''
__intrinsics = (
    'abs',
    'adjustl',
    'adjustr',
    'aimag',
    'allocated',
    'alog',
    'amax0',
    'amax1',
    'amin0',
    'amin1',
    'associated',
    'atan',
    'atan2',
    'close',
    'cmplx',
    'cos',
    'dabs',
    'dble',
    'dexp',
    'dlog',
    'dmax1',
    'dmin1',
    'dsign',
    'dsqrt',
    'exp',
    'float',
    'index',
    'int',
    'iabs',
    'len',
    'len_trim',
    'log',
    'log10',
    'max',
    'max0',
    'maxval',
    'min',
    'min0',
    'minval',
    'mod',
    'nint',
    'open',
    'present',
    'read',
    'real',
    'sign',
    'sin',
    'size',
    'sqrt',
    'sum',
    'tan',
    'tanh',
    'transpose',
    'trim',
    'ubound',
    )

__inquiry = (
    'associated',
    'allocated',
    'present',
    'size',
    )

def is_intrinsic(op):
    return op.lower() in __intrinsics

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
    elif (archaicName[0] == 'd' and archaicName[1:] in ('sign','abs','log','sqrt') ):
        return archaicName[1:]
    elif (archaicName[0] == 'i' and archaicName[1:] in ('abs') ):
        return archaicName[1:]
    elif (archaicName[0] == 'a' and archaicName[1:] in ('log') ):
        return archaicName[1:]
    else :
        return archaicName
    
def isPolymorphic(op):
    '''  true for intrinscis with generic names '''
    if not is_intrinsic(op):
        raise Exception("argument "+op+" is not an intrinsic")
    return op.lower() in ('max','min','sign')

    
