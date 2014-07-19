#! /usr/bin/python2.3

from optese import *
from pprint import pprint

"""
Prints a frequency distribution of the units in the given file.
"""


if __name__ == '__main__':
    infn = 'sentences.txt'
    infp = open(infn)
    indata = list(infp)
    infp.close()
    
    slist = []
    ### Import the sentences
    for i in xrange(len(indata)):
        s = Sentence(indata[i])
        s.set_src_id(i)
        s.delete_src()
        slist.append(s)
    
    corpus = Corpus(slist)
    corpus.set_freqs()
    corpus.set_freq_order()

    keys = corpus.freqs.keys()
    #keys.sort()
    for k in keys:
        print '%s\t%s' % (corpus.freqs[k], k)
