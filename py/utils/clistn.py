#! /usr/bin/python2.3

from optese import *
from pprint import pprint

"""
Prints total number of units in the given frequency distribution file.
"""

if __name__ == '__main__':
    phonfn = 'got_these_diphones_sn.txt'
    infp = open(phonfn)
    indata = list(infp)[2:]

##     blocks = {}
##     for i in range(10):
##         blocks[i] = 0
    total = 0
    for line in indata:
        count, dps = line.split()
        count = int(count)
        total += count
##         blocks[count] += int(dps)

    print total
##     bk = blocks.keys()
##     bk.sort()
##     for k in bk:
##         print '%s\t%s' % (k, blocks[k])
