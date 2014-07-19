* Optese: Optimal Text Selection

:Author: Ivan A. Uemlianin
:Contact: ivan@llaisdy.com
:Copyright: 2005, University of Wales, Bangor

.. contents::

** Acknowledgements

This software was produced as part of the WISPR project funded by Interreg IIIA European Union Programme and the Welsh Language Board.

This software was released by Bangor University under a BSD-style license.  A local copy of the license is here: `LICENSE.txt`_.  

.. _`LICENSE.txt`: LICENSE.txt

The original web page for the WISPR project is/was at `http://www.e-gymraeg.org/wispr/index_en.htm`_.

.. _`http://www.e-gymraeg.org/wispr/index_en.htm`: http://www.e-gymraeg.org/wispr/index_en.htm

** Overview

Optese implements the 'greedy-set-cover' algorithm from Cormen *et al.* (1990) [CORMEN90]_ for optimal text selection.  Given a list of sentences in two formats - orthographic and phonemic - optese will return a minimal list of sentences (orthographic) which contains all of the diphones in the input list.

** Set up

*** Download

# todo

*** Installation

As long as you have python, there is no installation necessary.  The script optese.py should just run as it is.

** Usage

Calling ``optese.py -h`` will output this brief usage reminder:

  ::

    * Optese: Optimal Text Selection

    ** Usage: optese.py (options) (input_prefix)

       Options:
           -c fn: name of config file containing parameters
           -c:    print default parameters
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
          
    ** Default values for Optese parameters:

    min_examples_to_get = 1   # Get at least this many of every unit
    min_coverage = 1          # Gather only units which have
                              # at least this many examples
    get_not_covered = True  # Do produce 'units not covered' report
    phon_out = True         # Do output phonetised text
    verbose = True          # Do print progress indicators
    in_prefix = corpus
    phon_in_fn = corpus_phon.txt
    orth_in_fn = corpus_orth.txt
    out_prefix = subCorpus
    phon_out_fn = subCorpus_phon.txt
    orth_out_fn = subCorpus_orth.txt
    unit_out_fn = subCorpus_units.txt


Calling ``optese.py -h`` also shows the format necessary for config files: the general idea is one 'name = value' statement per line, and anything after a '#' is ignored.  The statements should follow Python syntax: for example, strings should be quoted.

When ``optese.py`` is called without arguments it will look for the files ``corpus_orth.txt`` (list of sentences) and ``corpus_phon.txt`` (sentences in 'phonetized' form: see below) in the current directory.  It will out put progress indicators looking something like this:

::

  $ ../optese.py
  Reading corpus ... Done: read in 9321 sentences.
  Running OTS ...

    1	next: 3893	len(slist): 9320	len(to_remove): 302
    2	next: 8630	len(slist): 9319	len(to_remove): 189
    3	next: 5964	len(slist): 9318	len(to_remove): 169
    4	next: 8655	len(slist): 9317	len(to_remove): 131
    5	next: 1336	len(slist): 9316	len(to_remove): 124

    ...

    1993	next: 9179	len(slist): 7	len(to_remove): 1
    1994	next: 2320	len(slist): 6	len(to_remove): 1
    1995	next: 8120	len(slist): 5	len(to_remove): 1
    1996	next: 4689	len(slist): 4	len(to_remove): 1
  Done: found 9268 diphones; using 1996 sentences.
  Getting not covered ... Done.
  $

Depending on your machine, a corpus of 9321 sentences should be processed in a few minutes.

The output will comprise these files:

``subCorpus_orth.txt``
    List of selected sentences (orthographic).

``subCorpus_phon.txt``
    List of selected sentences (phonemic).

``subCorpus_units.txt``
    List of chosen units, with their counts. The line format is count unit, as in: ``61   #1@-ng``.

``subCorpus_units_not.html``
    List of units *not* found in the input.  As this list is likely to be quite large, it is formatted in html for ease of navigation.  A table of contents lists 'basic' (*i.e.*, context-independent) monophones and diphones.  The body of the report is two tables showing, for each 'basic' unit, which context-dependent units are missing.  Anything with more than half of its context-dependent units missing is highlighted in red in both the toc and the body of the report.  An example is here: `subCorpus_units_not.html`_.

.. _`subCorpus_units_not.html`: subCorpus_units_not.html

*** Format of 'phonetized' sentences

Here is an example of an orthographic sentence (of Welsh) and its phonemic representation:


  Ewch i fyny, ac fei cewch ar unwaith.

  ``$1eu-x#i#v-4@-n-y/a-k#v-1ei#k-1eu-x#a-r#4y-n-w-ai-th$``

As well as the phones themselves there are:

- Stress markers: numbers 1-4, before vowels;
- Word separators: '\#';
- Phrase separators: '/';
- Utterance boundaries: '$'.


** TODO

(This section may not be very readable;)

*** inputs

- allow user to:
  - provide orthographic input only, and specify a pronunciation dictionary
  - specify units other than diphones (eg n-phones/grams, longer chunks)

*** command-line user interface

parameters:

outputs:

- haven't got these units 
  - inc. units which were present but below inclusion threshold?


*** Experiments

- min_examples_to_get > 1


*** Documentation

- discussion/definitions:
  - naive/generalised units (old-style optese)
  - context-sensitive units (hpi)



** References

.. [CORMEN90]  Cormen, Thomas, H., Charles E. Leiserson, and Ronald L. Rivest. (1990).  *Introduction to Algorithms*.  Cambridge, Ma.: MIT Press.



