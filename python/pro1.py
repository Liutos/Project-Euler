# -*- coding: utf-8 -*-

def pro1(num):
    sum = 0
    i = 1
    while i < num:
        if (0 == (i % 3) or 0 == (i % 5)):
            sum = sum + i
        i = i + 1
    return sum


