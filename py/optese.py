#! /usr/bin/python2.3

# if python pre 2.4
from sets import Set as set

import os, re

#### WARNING: Phoneset is hardcoded!
#### TODO: fix this!

seps = ['/', '#', '$'] # and '-' is word-internal separator

stressed_starters = ['@', 'a', 'e', 'i', 'o', 'u', 'y']

phones = ['@', '@@',
          'a', 'aa', 'aay', 'ai', 'au', 'ay',
          'e', 'ee', 'ei', 'eu', 'ey',
          'i', 'ii', 'iu',
          'o', 'oa', 'oi', 'oo', 'ou', 'oy',
          'u', 'uu', 'uy',
          'y', 'yu', 'yy',
          'b', 'ch', 'd', 'dh', 'f', 'g',
          'hh', 'j', 'jh', 'k', 'l', 'lh',
          'lw', 'm', 'mh', 'n', 'ng', 'ngh',
          'nh', 'nw', 'p', 'r', 'rh', 'rw',
          's', 'sh', 't', 'th', 'v', 'w',
          'x', 'z', 'zh'
          ]

doc = """\
<html><head></head><body>
 
<h1>Optese: Haven't Got These Units</h1>

TOC

<h2 id="monophones">Monophones</h2>

<table border>
MONOPHONES
</table>

<h2 id="diphones">Diphones</h2>

<table border>
DIPHONES
</table>

</body></html>
"""

class Corpus(list):
    def __init__(self, slist=[], debug=True):
        list.__init__(self, slist)
        self.debug = debug
        self.freqs = {}
        self.freq_order = []

    def __add__(self, other):
        self.extend(other[:])
        self.reset_freqs()
        return self

    def __sub__(self, other):
        for s in other[:]:
            if s in self:
                self.remove(s)
        self.reset_freqs()
        return self

    def add_sentence(self, s):
        self.append(s)
        for dp in s:
            self.freqs[dp] = self.freqs.setdefault(dp, 0) + s[dp]
        self.set_freq_order

    def remove_sentence(self, s):
        if s in self:
            self.remove(s)
        for dp in s:
            self.freqs[dp] = self.freqs.setdefault(dp, 0) - s[dp]
        self.set_freq_order

    def get_sentence_by_ID(self, sid):
        # improve this!
        s = [s for s in self if s.src_id == sid][0]
        return s
            
    def set_freqs(self):
        self.freqs = {}
        for s in self:
            for dp in s:
                self.freqs[dp] = self.freqs.setdefault(dp, 0) + s[dp]

    def set_freq_order(self):
        self.freq_order = list(set(self.freqs.values()))
        self.freq_order.sort()
    
    def reset_freqs(self):
        self.set_freqs()
        self.set_freq_order()

    def ots(self, min_to_get=1):
        if self.debug is True: count = 1
        use_these_sentences = Corpus()
        # 2.4 has DSU builtin, but for 2.3:
        import copy
        slist = copy.deepcopy(self[:]) # this always catches me out
        slist = [(len(s), s) for s in slist] # Decorate; assume len(s)
        while(slist):
            slist.sort()                     # Sort
            slist = [i[1] for i in slist]    # Un-decorate

            next_to_use = slist.pop()
            s = self.get_sentence_by_ID(next_to_use.src_id)
            use_these_sentences.add_sentence(s)

            to_remove = [dp for dp in next_to_use
                         if (use_these_sentences.freqs[dp] >= min_to_get)]
            for s in slist:            
                s.remove_dps(to_remove)

            if self.debug is True:
                print '\t%d\tnext: %d\tlen(slist): %d\tlen(to_remove): %d' \
                      % (count, next_to_use.src_id,
                         len(slist), len(to_remove))
                count += 1
            slist = [(len(s), s) for s in slist if len(s)]
        if self.debug is True:
            print 'Done: found %d diphones; using %d sentences.' \
                  % (len(use_these_sentences.freqs),
                     len(use_these_sentences))
        return use_these_sentences

    def export_units(self):
        dps = self.freqs.keys()
        dps.sort()
        units = ['%s\t%s\n' % (self.freqs[dp], dp)
                 for dp in dps]
        return units

    def export_units_not_covered(self):
        got_these = self.freqs.keys()

        monophones = {}
        for ph in phones:
            mplist = []
            for sep1 in seps:
                for sep2 in seps:
                    mp = '%s%s%s' % (sep1, ph, sep2)
                    if mp not in got_these:
                        mplist.append(mp)
            monophones[ph] = mplist

        diphones = {}
        for p1 in phones:
            for p2 in phones:
                dp = '%s-%s' % (p1, p2)
                contexts = ['-%s-' % dp]
                for s in seps:
                    contexts.append('%s%s' % (s, dp))
                    contexts.append('%s%s' % (dp, s))
                diphones[dp] = [d for d in contexts
                                if d not in got_these]

        # html report
        mtoc = []
        mk = monophones.keys()
        mk.sort()
        mtab = ''
        for k in mk:
            mtoc.append('<li><a href="#%s">%s</a>(%d missing)</li>\n' \
                        % (k, k, len(monophones[k])))
            mtab = '%s<tr><td id="%s">%s</td><td>%s</td></tr>\n' \
                   % (mtab, k, k, '</td><td>'.join(monophones[k]))
        global doc
        doc = doc.replace('MONOPHONES', mtab)
        dtoc = []
        dk = diphones.keys()
        dk.sort()
        dtab = ''
        for k in dk:
            dtoc.append('<li><a href="#%s">%s</a>(%d missing)</li>\n' \
                        % (k, k, len(diphones[k])))
            dtab = '%s<tr><td id="%s">%s</td><td>%s</td></tr>\n' \
                   % (dtab, k, k, '</td><td>'.join(diphones[k]))
        doc = doc.replace('DIPHONES', dtab)

        # toc
        toc = """\
    <h2> Table of Contents</h2>

    <ol>
    <li><a href="#monophones">Monophones</a></li>
        <ol>
        %s
        </ol>
    <li><a href="#diphones">Diphones</a></li>
        <ol>
        %s
        </ol>
    </ol>
    """ % (''.join(mtoc), ''.join(dtoc))

        doc = doc.replace('TOC', toc)

        return doc
                

separators = ['-', '/', '#', '$']

class Sentence(dict):
    def __init__(self, src=''):
        dict.__init__(self) # {unit: n}
        self.src = src
        self.src_id = 0

        if self.src:
            self.src2dict()

    def src2dict(self, format='hpi'):
        src = self.src
        for sep in separators:
            src = src.replace(sep, ' %s ' % sep)
        if format == 'hpi':
            words = self.parse_into_words(src)
            for word in words:
                # - get start & end units
                if len(word) == 3:
                    self.add_unit(''.join(word))
                else:
                    self.add_unit(''.join(word[:4]))
                    self.add_unit(''.join(word[-4:]))
                    word = word[2:-2]
                    if len(word) > 3:
                        self.parse_simple(word)
        else:
            src = src.strip().split()
            self.parse_simple(src)

    def parse_into_words(self, src):
        seps = re.findall(r'[/#$]', src)
        words = []
        while len(seps) > 1:
            x = src.index(seps[0])
            y = src.index(seps[1], x+1)
            word = src[x:y+1].split()
            words.append(word)
            src = src[y:]
            seps.pop(0)
        return words

    def parse_simple(self, plist):
        for i in range(0, (len(plist) - 3), 2):
            dp = ''.join(plist[i:i+5])
            self.add_unit(dp)

    def add_unit(self, unit):
        self[unit] = self.setdefault(unit, 0) + 1

    def delete_src(self):
        self.src = ''

    def set_src_id(self, i):
        self.src_id = i

    def get_src_id(self):
        return self.src_id

    def remove_dps(self, to_remove):
        for dp in to_remove:
            if dp in self:
                del self[dp]



class Opteser:
    def __init__(self):
        self.min_examples_to_get = 1
        # Get at least this many of every unit
        self.min_coverage = 1
        # Gather only units which have at least this many examples
        self.get_not_covered = True
        # Do produce 'units not covered' report
        self.phon_out = True  # Do output phonetised text
        self.verbose = True   # Do print progress indicators
        
        self.in_prefix = 'corpus'
        self.out_prefix = 'subCorpus'
        self.phon_in_fn = '%s_phon.txt' % self.in_prefix
        self.orth_in_fn = '%s_orth.txt' % self.in_prefix
        self.phon_out_fn = '%s_phon.txt' % self.out_prefix
        self.orth_out_fn = '%s_orth.txt' % self.out_prefix
        self.unit_out_fn = '%s_units.txt' % self.out_prefix

        self.corpus = None
        self.selected_sentences = None

    def print_usage(self):
        out = """\
* Optese: Optimal Text Selection

** Usage: optese.py (options) (input_prefix)

   Options:
       -c fn: name of config file containing parameters
       -h:    print this message and exit

      inputs:
       -f n:  min frequency for input units
       -e n:  min examples to collect of each unit
       -g fn: got these diphones from earlier iteration

      outputs:
       -v:    verbose mode
       -o fn: prefix for output filenames
       -p:    give phonetised text output (as well as orthography)
       -n:    run not_covered.py
          """    
        print out

    def print_defaults(self):
        out = """\
** Default values for Optese parameters:

min_examples_to_get = %d   # Get at least this many of every unit
min_coverage = %d          # Gather only units which have
                          # at least this many examples
get_not_covered = %s  # Do produce 'units not covered' report
phon_out = %s         # Do output phonetised text
verbose = %s          # Do print progress indicators
in_prefix = %s
phon_in_fn = %s
orth_in_fn = %s
out_prefix = %s
phon_out_fn = %s
orth_out_fn = %s
unit_out_fn = %s
""" % (self.min_examples_to_get,
       self.min_coverage,
       self.get_not_covered,
       self.phon_out,
       self.verbose,
       self.in_prefix,
       self.phon_in_fn,
       self.orth_in_fn,
       self.out_prefix,
       self.phon_out_fn,
       self.orth_out_fn,
       self.unit_out_fn
       )
        print out
               
    def reset_in_fns(self, in_prefix):
        self.in_prefix = in_prefix
        self.phon_in_fn = '%s_phon.txt' % self.in_prefix
        self.orth_in_fn = '%s_orth.txt' % self.in_prefix

    def reset_out_fns(self, out_prefix):
        self.out_prefix = out_prefix
        self.phon_out_fn = '%s_phon.txt' % self.out_prefix
        self.orth_out_fn = '%s_orth.txt' % self.out_prefix
        self.unit_out_fn = '%s_units.txt' % self.out_prefix

    def import_config(self, fn):
        print 'importing config from', fn
        exec(open(fn).read())
        for line in list(open(fn)):
            if line[0] !='\n':
                term = line.split()[0]
                if term != '#':
                    exec('self.%s = %s' % (term, term))
                
    
    def import_corpus(self):
        if self.verbose is True:
            print 'Reading corpus ...',
        fp = open(self.phon_in_fn)
        indata = list(fp)
        fp.close()
        slist = []
        for i in xrange(len(indata)):
            s = Sentence(indata[i])
            s.set_src_id(i)
            s.delete_src()
            slist.append(s)
        self.corpus = Corpus(slist, self.verbose) # verbose aka debug
        self.corpus.reset_freqs()
        if self.min_coverage > 1:
            to_remove = [dp for dp in self.corpus.freqs
                         if self.corpus.freqs[dp] < self.min_coverage]
            if self.verbose is True:
                print 'Removing %d diphones ... ' % len(to_remove),
            for s in self.corpus:
                s.remove_dps(to_remove)
            self.corpus.reset_freqs()
            if self.verbose is True:
                print 'Done: read in %d sentences.\nRunning OTS ...\n' \
                      % len(self.corpus)

    def select_sentences(self):
        self.selected_sentences = self.corpus.ots(self.min_examples_to_get)

    def save_selection(self, which='orth'):
        if which == 'orth':
            in_fn = self.orth_in_fn
            out_fn = self.orth_out_fn
        elif which == 'phon':
            in_fn = self.phon_in_fn
            out_fn = self.phon_out_fn
            
        data = list(open(in_fn))
        out = []
        for i in [s.src_id for s in self.selected_sentences]:
            out.append(data[i])
        open(out_fn, 'w').writelines(out)

        if self.phon_out is True:
            self.phon_out = False
            self.save_selection('phon')

        ### Save 'got these diphones'
        out = self.selected_sentences.export_units()
        open(self.unit_out_fn, 'w').writelines(out)

        if self.get_not_covered is True:
            doc = self.selected_sentences.export_units_not_covered()
            open('%s_not.html' % \
                 os.path.splitext(self.unit_out_fn)[0],
                 'w').write(doc)


if __name__ == '__main__':
    import getopt, sys
    argv = sys.argv[1:]

    options, arg = getopt.getopt(argv, 'c:e:f:g:hno:pv')
    odict = dict(options)

    opt = Opteser()
    
    if '-h' in odict:
        opt.print_usage()
        opt.print_defaults()
    else:
        if odict.get('-c'):
            opt.import_config(odict['-c'])
        if odict.get('-e'):            
            opt.min_examples_to_get = int(odict['-e'])
        if odict.get('-f'):
            opt.min_coverage = int(odict['-f'])
        if odict.get('-g'):
            opt.unit_out_fn = odict['-g']
        if odict.get('-n'):
            opt.get_not_covered = True
        if odict.get('-o'):
            opt.out_prefix = odict['-o']
        if odict.get('-p'):
            opt.phon_out = True
        if odict.get('-v'):
            opt.verbose = True
        if arg:
            opt.reset_in_fns(arg[0])

        opt.import_corpus()
        opt.select_sentences()
        opt.save_selection()
