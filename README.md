(under construction)

A very basic bloom filter implementation in Elisp.
NO WARRANTY. Not extensively tested yet.

# User Setable Options
```el
(defvar bfilter-hashers '(sxhash; shipped w/emacs
			               (lambda (in)
				              (bfilter--jenkins-hash in (length in)))))
(defvar bfilter-bv-size 1024)
```

# Public API

```el
;; "Given a string key INPUT, set the appro bloom filter bit vector BV slots.
;; Return the slot indexes set (can be ignored)."
(bfilter-set input-key bv)

;; "Given key as INPUT, return t if all associated hashed slots in BV equal t, else nil."
(bfilter-isset? input-key bv)
```

# Tests
The few tests can be run like so:

```sh
CASK_EMACS=/usr/local/Cellar/emacs/25.0.94/bin/emacs cask emacs --batch   -l ert --script test/test-bfilter.el -f ert-run-tests-batch-and-exit
```
