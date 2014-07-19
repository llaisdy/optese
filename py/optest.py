#! /usr/bin/python2.3

from optese import *

## orthData = list(open('corpus_orth.txt.selected'))
## phonData = []

## orthin = list(open('corpus_orth.txt'))
## phonin = list(open('corpus_phon.txt'))

## for line in orthData:
##     if line in orthin:
##         nl = phonin[orthin.index(line)]
##         phonData.append(nl)

## open('corpus_phon.txt.selected','w').write(''.join(phonData))


in_prefix = 'selected'
out_prefix = 'selected_'

opt = Opteser()
opt.reset_in_fns(in_prefix)
opt.reset_out_fns(out_prefix)

opt.import_corpus()
opt.selected_sentences = opt.corpus

dps = opt.selected_sentences.freqs.keys()
dps.sort()
out = ['%s\t%s\n' % (opt.selected_sentences.freqs[dp], dp)
       for dp in dps]

print opt.unit_out_fn

open(opt.unit_out_fn, 'w').writelines(out)

opt.output_not_covered(dps)
