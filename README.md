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
Hash function fitness and size of the filter are covered nicely here [^2]

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

The included el implementation is not exactly the one in the C version
on the wikipedia page -- see here[^3] for full details and how the Emacs
25 dynamic module can be used to run the C one (if needbe).

# Public API

```el
;; Construct/return bf bit vector of size bfilter-bv-size.
(bfilter-init-bv)

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

[^1]: wikipedia url
[^2]: http://billmill.org/bloomfilter-tutorial/
[^3]: https://gbxcx-labs.appspot.com/posts-dir/post-emacs25-ffi.html
