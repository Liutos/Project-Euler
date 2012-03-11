# -*- coding: utf-8 -*-

def rec(st, nd, sum, limit):
    if (nd > limit):
        return sum
    else:
        if (0 == nd % 2):
            return rec(nd, st+nd, sum+nd, limit)
        else:
            return rec(nd, st+nd, sum, limit)

def pro2(limit):
    return rec(1, 2, 0, limit)
