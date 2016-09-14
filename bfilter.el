;;; bfilter.el --- Bloom filter in elisp

;; Copyright ?

;; Author: github.com/gaak99
;; Version: 0.5.0
;; Package-Version: ??
;; Keywords: bloomfilter

;;; Commentary:

;; A very basic bloom filter implementation in Elisp.
;; NO WARRANTY.  Not extensively tested.

;;; Code:

;; Suggestion to developers: M-x occur ";;;"

(require 'dash)

;;; User Setable Options

(defvar bfilter-hashers '(sxhash; shipped w/emacs
			      (lambda (in)
				(bfilter--jenkins-hash in (length in)))))
(defvar bfilter-bv-size 1024)
(defvar bfilter-size bfilter-bv-size)

;;; Public API

(defun bfilter-init ()
  "Construct/return of bloom filter structure (of size bfilter-size)."
  (make-bool-vector bfilter-size nil))

(defun bfilter-set (input bf)
  "Given a string key INPUT, set the appro bloom filter bit vector BF slots.
Return the slot indexes set (can be ignored)."
  (-let ((vi (-map (lambda (f); vector indexes
		     (mod (funcall f input) (length bf)))
		   bfilter-hashers)))
    (-each vi
      (lambda (i)
	(progn
	  ;;(message "debug set: %s" i)
	  (aset bf i t))))
    vi))

(defun bfilter-isset? (input bf)
  "Given key as INPUT, return t if associated all hashed slots in BF equal t, else nil."
  (-let* ((vi (-map (lambda (f);  vector indices
		      (mod (funcall f input) (length bf)))
		    bfilter-hashers))
	  (vv (-map (lambda (i);  vector values
		      (aref bf i))
		    vi))
	 (any-nils? (--find-indices (equal nil it ) vv)))
    (if any-nils? nil t)))

;;; Private functions

;;; Hashers (no elisp version avail so added here)

(defun bfilter--jenkins-hash (input maxlen)
  "Jenkins(ish) hash - INPUT is string (of MAXLEN) to be
hashed and returned.

Based on (and direct port of) C version
at https://en.wikipedia.org/wiki/Jenkins_hash_function
Warning: due to elisp fixnum != C uint32_t
this does not have same results as C version."
  (let ((key input)
	(hash 0)
	(i 0))
    (while (and
	    (< i maxlen))
      (setq hash (+ hash (aref key i)))
      (setq hash (+ hash (lsh hash 10)))
      (setq hash (logxor hash (ash hash 6)))
      (setq i (1+ i)))
    (setq hash (+ hash (lsh hash 3)))
    (setq hash (logxor hash (ash hash 11)))
    (setq hash (+ hash (lsh hash 15)))
    hash))

(provide 'bfilter)

;;; bfilter.el ends here
