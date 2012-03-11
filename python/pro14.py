# -*- coding: utf-8 -*-

def rec(m, times):
    if (1 == m):
        return times
    else:
        if (0 == m % 2):
            return rec(m/2, times+1)
        else:
            return rec(3*m+1, times+1)

def collatz_length(n):
    return rec(n, 1)

def pro14(n):
    max = []
    win = 0
    i = 2
    while i <= n:
        len = collatz_length(i)
        if (len > win):
            max = i
            win = len
        i = i + 1
    return max
