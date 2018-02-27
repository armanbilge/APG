#!/usr/bin/env python3

import sys
from pysam import VariantFile

l = [10**(-i/10) for i in range(256)]

with VariantFile(sys.argv[1]) as f:
    for record in f:
        pl = record.samples[0]['PL']
        pl = (l[pl[0]], l[pl[1]], l[pl[2]])
        z = max(pl[0], max(pl[1], pl[2]))
        print(pl[0]/z, end='')
        print('\t', end='')
        print(pl[1]/z, end='')
        print('\t', end='')
        print(pl[2]/z)
