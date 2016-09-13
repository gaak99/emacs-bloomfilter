(under construction)

A very basic bloom filter implementation in Elisp.
NO WARRANTY. Not extensively tested yet.

# What is a Bloom Filter?
From wikipedia [^1]:

> A *Bloom filter* is a space-efficient probabilistic data structure,
> conceived by Burton Howard Bloom in 1970, that is used to test whether
> element is a member of a set. False positive matches are possible,
> but false negatives are not, thus a Bloom filter has a 100% recall
> rate. In other words, a query returns either "possibly in set" or
> "definitely not in set".

# User Setable Options
Hash function fitness is discussed here [^2] and here [^3]:

```el
;; List of hash functions. See below [?] for refs to which
;; ones are best.
(defvar bfilter-hashers '(sxhash; shipped w/emacs
			               (lambda (in)
				              (bfilter--jenkins-hash in (length in)))))

;; The bit vector data structure size.
(defvar bfilter-bv-size 1024)
```

# Public API

```el
;; Given a string key INPUT, set the appro bloom filter bit vector BV slots.
;; Return the slot indexes set (can be ignored).
(bfilter-set input-key bv)

;; Given key as INPUT, return t if all associated hashed slots in BV equal t, else nil.
(bfilter-isset? input-key bv)
```

# Tests
The few tests can be run like so:

```sh
CASK_EMACS=/usr/local/Cellar/emacs/25.0.94/bin/emacs cask emacs --batch   -l ert --script test/test-bfilter.el -f ert-run-tests-batch-and-exit
```

[^1]: wiki url
[^2]: fn?
[^3]: fn?
