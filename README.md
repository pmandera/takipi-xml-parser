% TaKIPI XML output parser
% Paweł Mandera

Parses output generated by [TaKIPI](http://nlp.pwr.wroc.pl/takipi/) into more compact format
with each token in a new line and its POS tag and lemma separated by tabs.

Bound morphemes which are split by the tagger and marked as 'aglt' are re-attached to the words.
E.g. 'urządziłem' which is split into 'urządził' and 'em' by TaKIPI are put back together.

The input file must have unambiguous assignment of POS tag and lemma for each token so TaKIPI
should be run with an option `--force one`.
