# -*- coding: utf-8 -*-

def is_palindromic(number):
    tmp = str(number)
    return tmp == tmp[::-1]

def pro4():
    top = 0
    for i in range(100, 999):
        for j in range(100, 999):
            mul = i * j
            if is_palindromic(mul) and (mul > top):
                top = mul
    return top
