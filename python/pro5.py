# -*- coding: utf-8 -*-

def tmp(n, d):
    if 0 == n % d:
        return n / d
    else:
        return n

def helper(n, lst):
    return reduce(tmp, lst, n)

def pro5(n):
    m = 2
    lst = [1]
    while m < n:
        m = m + 1
        lst.append(helper(m, lst))
    return reduce(lambda a, b: a * b, lst, 1)

print pro5(20)
