'''
Module to list the intrinsic functions, and
a predicate to test for a function being an intrinsic
'''
__intrinsics = (
    'abs',
    'atan',
    'atan2',
    'close',
    'cos',
    'dble',
    'exp',
    'float',
    'int',
    'len',
    'mod',
    'nint',
    'open',
    'read',
    'sign',
    'sin',
    'sqrt',
    'tan',
    'tanh',
    'trim',
    )

def is_intrinsic(op):
    return op.lower() in __intrinsics
