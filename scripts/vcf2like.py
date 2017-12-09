#!/usr/bin/env python3

import sys
from pysam import VariantFile

l = [str(10**(-i/10)) for i in range(256)]

with VariantFile(sys.argv[1]) as f:
    for record in f:
        pl = record.samples[0]['PL']
        print(l[pl[0]], end='')
        print('\t', end='')
        print(l[pl[1]], end='')
        print('\t', end='')
        print(l[pl[2]])
