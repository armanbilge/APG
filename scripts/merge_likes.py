#!/usr/bin/env python3

import sys
import functools as ft
import itertools as it
import numpy as np
from numpy.polynomial.polynomial import polymul

def print_join(iter):
    print(next(iter), end='')
    for x in iter:
        print('\t', end='')
        print(x, end='')
    print()

def manypolymul(coeffs):
    return ft.reduce(polymul, coeffs, [1])

fs = map(open, sys.argv[1:])
z = None
for l in zip(*fs):
    coeffs = [list(map(float, x.split())) for x in l]
    p = manypolymul(coeffs)
    if z is None:
        z = manypolymul([1] * len(x) for x in coeffs)
    p /= z[:len(p)]
    mp = np.max(p)
    p /= mp
    pad = sum(map(len, coeffs)) - len(coeffs) - p.shape[0] + 1
    print_join(it.chain(map(str, p), it.repeat('0', pad)))

for f in fs:
    f.close()
