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
Hash functions, number of hash functions, and size of the filter should be taken into consideration. [^2]

```el
;; List of hash functions. See below [?] for refs to which
;; ones are best.
(defvar bfilter-hashers '(sxhash; shipped w/emacs
			               (lambda (in)
				              (bfilter--jenkins-hash in (length in)))))

;; The bit vector data structure size.
(defvar bfilter-bv-size 1024)
```

## Note Jenkins hash function

The included el implementation results don't match the one in the C
version on the wikipedia page (but is probably ok for non-production
use) but Emacs 25 dynamic module feature may solve it.[^3]

# Public API

```el
;; Construct/return bloom filter of size bfilter-size.
(bfilter-init)

;; Given a string key INPUT, set the appro bloom filter bit vector BF slots.
;; Return the slot indexes set (can be ignored).
(bfilter-set input-key bf)

;; Given key as INPUT, return t if all associated hashed slots in BF equal t, else nil.
(bfilter-isset? input-key bf)
```

# Tests
The few tests can be run like so:

```sh
CASK_EMACS=/usr/local/Cellar/emacs/25.0.94/bin/emacs cask emacs --batch   -l ert --script test/test-bfilter.el -f ert-run-tests-batch-and-exit
```

[^1]: wikipedia url
[^2]: http://billmill.org/bloomfilter-tutorial/
[^3]: https://gbxcx-labs.appspot.com/posts-dir/post-emacs25-ffi.html
