'''
Buffered iterator -- try to avoid itertool.chain for backtracking,
as I think the closure captures the values
'''
import copy as c

class buf_iter(object):
    def __init__(self,iterable):

        self.iterable   = iter(iterable)
        self.lookahead  = []

    def putback(self,lst):
        ll = c.copy(lst)
        ll.reverse()
        self.lookahead.extend(ll)

        return self

    def next(self):
        if not self.lookahead:
            return self.iterable.next()
        v = self.lookahead.pop()
        return v

    def __iter__(self):
        return self
