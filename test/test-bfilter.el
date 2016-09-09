;;; test-bfilter.el --- Bloom filter test suite
;;; gb Winter16

;; Commentary:
;; --todo: use real test frameworkz aka ert.

;;; Code:

;(require 'dash)

(setq load-path
      (cons (expand-file-name ".") load-path))
(require 'bfilter)

(setq test-bfilter-bv-size 1024); bug if smaller (ie 32) get epic fail

(ert-deftest bfilter-basic ()
  (should (-let* ((dat "bob.dobbs@cheetahmagicinc.com")
		  (foo (message "bftest debug: start bfilter-0: %s" dat))
		  (bvt (make-bool-vector test-bfilter-bv-size nil))
		  (setrt (bfilter-set dat bvt))
		  (getrt (bfilter-isset? dat bvt)))
	    getrt)))

;;; test-bfilter.el ends here
