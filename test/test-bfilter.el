;;; test-bfilter.el --- Bloom filter test suite
;;; gb Winter16

;; Commentary:
;; --todo: use real test frameworkz aka ert.

;;; Code:

;(require 'dash)

(setq load-path
      (cons (expand-file-name ".") load-path))
(require 'bfilter)

(setq bfilter-test-bv-size 1024); bug if smaller (ie 32) get epic fail

;; strings from random.org
(setq bfilter-test-rand-strings-l1
      '("YTcwMxwHeG" "pEzgJKLL9t" "OjSM4sHnG7" "9GB9W70GAC" "bxlVCIM6Xo" "7XS39dSSoT" "8QBCz7UxSm" "uAlmW3eBRr" "w8Esi3gpJi" "LDdGBYq4mH"))
(setq bfilter-test-rand-strings-l2
      '("LTBgjlDsR6" "2Js3OMjWw0" "eavUOuz7cn" "AOU7vNpMzT" "34sVSMHuFY" "VgqVATJZqm" "eFuflOQ3fz" "vGMWPfw9hW" "ywY9rfNvvx" "8KZJGaaL9y"))

(ert-deftest bfilter-test-basic-once ()
  (should (-let* ((dat "bob.dobbs@cheetahmagicinc.com")
		  (foo (message "bftest debug: start bfilter-0: %s" dat))
		  (bvt (bfilter-init-bv))
		  (setrt (bfilter-set dat bvt))
		  (getrt (bfilter-isset? dat bvt)))
	    getrt)))

(ert-deftest  bfilter-test-basic-many ()
  (should (-let* ((z1 (-zip-with (lambda (a b)
				   (concat a "@" b))
				 bfilter-test-rand-strings-l1
				 bfilter-test-rand-strings-l2))
		  (foo (message
			"bftest debug: start bfilter-test-basic-many: %s" (car z1)))
		  (bvt (bfilter-init-bv))
		  (setrt (-each z1
			   (lambda (s)
			     (bfilter-set s bvt))))
		  (isset (-map (lambda (s)
				 (bfilter-isset? s bvt))
			       z1))
		  ;; use --find-indices so can display all nils found
		  (any-nils? (--find-indices (equal nil it) isset)))
	    (if any-nils? nil t))))

(ert-deftest bfilter-test-noset-once ()
  "No set before get so should fail modulo false positives."
  (should (-let* ((dat "bob.dobbs@magiccheetahmagicinc.com")
		  (foo (message "bftest debug: start bfilter-test-noset-once: %s" dat))
		  (bvt (bfilter-init-bv))
		  (getrt (bfilter-isset? dat bvt)))
	    (not getrt))))

(ert-deftest  bfilter-test-noset-many ()
  "No set before get so should fail modulo false positives."
  (should (-let* ((z1 (-zip-with (lambda (a b)
				   (concat a "@" b))
				 bfilter-test-rand-strings-l1
				 bfilter-test-rand-strings-l2))
		  (foo (message
			"bftest debug: start bfilter-test-basic-many: %s" (car z1)))
		  (bvt (bfilter-init-bv))
		  (isset (-map (lambda (s)
				 (bfilter-isset? s bvt))
			       z1))
		  (any-true? (--find-indices (equal t it) isset)))
	    (if any-true? nil t))))

;;; test-bfilter.el ends here
