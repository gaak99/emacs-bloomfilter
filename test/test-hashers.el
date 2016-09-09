;;; test-hashers.el --- a modicum of hash fn sanity check for collisions

;; strings from random.org
(setq rando-string-l1
      '("YTcwMxwHeG" "pEzgJKLL9t" "OjSM4sHnG7" "9GB9W70GAC" "bxlVCIM6Xo" "7XS39dSSoT" "8QBCz7UxSm" "uAlmW3eBRr" "w8Esi3gpJi" "LDdGBYq4mH"))
(setq rando-string-l2
      '("LTBgjlDsR6" "2Js3OMjWw0" "eavUOuz7cn" "AOU7vNpMzT" "34sVSMHuFY" "VgqVATJZqm" "eFuflOQ3fz" "vGMWPfw9hW" "ywY9rfNvvx" "8KZJGaaL9y"))

(defun test-hashers-3 (hfn l1 l2)
  "Quick sanity check on hash fn on range, collisions, etc."
  (-let* ((z1 (-zip-with (lambda (a b)
			 (concat a "@" b)) l1 l2))
	  (bv-size 1024)
	  (h1 (-map (lambda (s)
		      (mod (funcall hfn s) bv-size))
		    z1))
	  (len1 (length h1))
	  (h2 (-uniq h1))
	  (len2 (length h2))
	  (len12 (list len1 len2)); eq?
	  (mm (list (-min h1) (-max h1)))); range
   mm)) ; eyeball results -- todo automate&add ert support
(test-hashers-3 'sxhash  rando-string-l1 rando-string-l2)
(test-hashers-3 'sxhash  rando-string-l2 rando-string-l1)

(test-hashers-3 (lambda (k)
		  (bfilter--jenkins-hash k (length k)))
		rando-string-l2 rando-string-l1)
(test-hashers-3 (lambda (k)
		  (bfilter--jenkins-hash k (length k)))
		rando-string-l1 rando-string-l2)
