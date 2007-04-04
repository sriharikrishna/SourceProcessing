from _Setup import *

import PyFort.fortStmts as fs
import PyFort.fortExp   as fe
from PyFort.fortContextFile import fortContextFile

def reslice_assign(l,pat,subst):
    rhs = fe.subst(l.rhs,pat,subst)
    new_l = l.same(fs.AssignStmt(l.lhs,rhs))
    return [new_l]

def reslice_call(l,pat,subst):
    args = [fe.subst(a,pat,subst) for a in l.args]
    new_l = l.same(fs.CallStmt(l.head,args))
    return [new_l]

def reslice_ifthen(l,pat,subst):
    test = fe.subst(l.test,pat,subst)
    new_l = l.same(fs.IfThenStmt(test))
    return [new_l]

def gen_reslice_fns(undo_dict):
    def pat(e):
        return isinstance(e,str) and e in undo_dict

    def subst(e):
        return undo_dict[e]

    return (pat,subst)

reslice_lexi = [(fs.AssignStmt,reslice_assign),
                (fs.CallStmt,reslice_call),
                (fs.IfThenStmt,reslice_ifthen)]

def reslice(f):
    '''Given a filename f, re-introduce the slicing operations
    removed by the canonicalization process
    '''

    import cPickle as cp

    upf       = open('reslice.dat')
    reslc_up  = cp.Unpickler(upf)
    reslice_d = reslc_up.load()
    upf.close()

    fns  = gen_reslice_fns(reslice_d)
    fcr  = fortContextFile(f)
    
    fcr2 = fcr.rewrite(reslice_lexi,*fns)
    return fcr2
