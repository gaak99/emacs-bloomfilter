;;; test-bfilter.el --- Bloom filter test suite
;;; gb Winter16

;; Commentary:
;; --todo: use real test frameworkz aka ert.

;;; Code:

(require 'dash)

(setq test-bfilter-bv-size 1024); bug if smaller (ie 32) get epic fail
(setq my-bfilter-tests '(test-bfilter-0 test-bfilter-1))

(defun bfilter-run-tests ()
  (-each
    my-bfilter-tests
    (lambda (f)
     (funcall f))))

(defun test-bfilter-0 ()
  "simple/quick one-off bf get&set"
  (-let* ((dat "bob.dobbs@cheetahmagicinc.com")
	  (foo (message "bftest debug: start bfilter-0: %s" dat))
	  (bvt (make-bool-vector test-bfilter-bv-size nil))
	  (setrt (bfilter-set dat bvt))
	  (getrt (bfilter-found? dat bvt)))
    (if (not getrt) (message "test-bfilter-0 FAIL"))))

(defun test-bfilter-1 ()
  "test mult inputs for misc order get/set"
  (let* ((dat '("foo@bar")); todo beef up
	 (foo (message "bftest debug: start bfilter-1: %s" (car dat)))
	 (bvt (make-bool-vector test-bfilter-bv-size nil))
	 (should-fail (-map
		       (lambda (s) ; get no set
			 (bfilter-found? s bvt))
		       dat))
	 (notFail (-filter ; (t/nil...)
		   (lambda (x)
		     (and x))
		   should-fail))
	 (should-pass (-map  ; doh false positives r possible w/Bf
		       (lambda (s)
			 (progn
			   (bfilter-set s bvt)
			   (bfilter-found? s bvt)))
		       dat))
	 (notSuccess (-filter ; (t/nil ...)
		      (lambda (x)
			(not (and x)))
		      should-pass)))
    (if notFail
	(message "should-fail fail"))
    (if notSuccess
	(message "should-pass fail"))
    notSuccess))
(bfilter-run-tests)

;;; test-bfilter.el ends here
