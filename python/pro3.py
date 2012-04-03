# -*- coding: utf-8 -*-

def smallest_factor(n):
    test = 2
    while test < n:
        if 0 == n % test:
            return test
        test = test + 1
    return n

def largest_prime_factor(n):
    num = n
    acc = 1
    while num != 1:
        if num <= 2:
            return num
        else:
            fac = smallest_factor(num)
            num = num / fac
            acc = fac
    return acc
