'''
Module to list the intrinsic functions, and
a predicate to test for a function being an intrinsic
'''
__intrinsics = (
    'abs',
    'allocate',
    'allocated',
    'associated',
    'atan',
    'atan2',
    'close',
    'cos',
    'dble',
    'deallocate',
    'dsign',
    'exp',
    'float',
    'int',
    'len',
    'log',
    'max',
    'min',
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
    'trim',
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
