;;; tty-image-test.el --- Tests for tty-image.el -*- lexical-binding: t -*-

;; Run with:
;;   emacs -Q -batch -L ~/dotfiles -l tty-image-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'tty-image)

(defun tty-image-test--png (width height &optional extra)
  "Return a fake PNG header for WIDTH x HEIGHT followed by EXTRA bytes."
  (concat tty-image--png-signature
          (unibyte-string 0 0 0 13 ?I ?H ?D ?R)
          (unibyte-string (logand (ash width -24) 255) (logand (ash width -16) 255)
                          (logand (ash width -8) 255) (logand width 255)
                          (logand (ash height -24) 255) (logand (ash height -16) 255)
                          (logand (ash height -8) 255) (logand height 255))
          (or extra "")))

(ert-deftest tty-image-test-png-size-reads-ihdr ()
  (should (equal (tty-image--png-size (tty-image-test--png 640 70000))
                 '(640 . 70000))))

(ert-deftest tty-image-test-png-size-rejects-non-png ()
  (should-error (tty-image--png-size "GIF89a not a png at all......")))

(ert-deftest tty-image-test-small-image-keeps-natural-size ()
  (let ((tty-image-cell-size '(10 . 20)))
    (should (equal (tty-image--fit '(100 . 100) 80 24) '(10 . 5)))))

(ert-deftest tty-image-test-large-image-scales-to-fit-rows ()
  (let ((tty-image-cell-size '(10 . 20)))
    (should (equal (tty-image--fit '(1000 . 1000) 80 24) '(48 . 24)))))

(ert-deftest tty-image-test-wide-image-scales-to-fit-cols ()
  (let ((tty-image-cell-size '(10 . 20)))
    (should (equal (tty-image--fit '(4000 . 400) 80 24) '(80 . 4)))))

(ert-deftest tty-image-test-fit-never-returns-zero ()
  (let ((tty-image-cell-size '(10 . 20)))
    (should (equal (tty-image--fit '(10000 . 1) 80 24) '(80 . 1)))))

(ert-deftest tty-image-test-single-chunk-transmit ()
  (should (equal (tty-image--transmit-codes 42 "abc" 3 2)
                 (list (concat "\e_Ga=T,U=1,f=100,i=42,p=1,c=3,r=2,q=2,m=0;"
                               (base64-encode-string "abc")
                               "\e\\")))))

(ert-deftest tty-image-test-chunks-are-at-most-4096-and-flag-more ()
  (let* ((png (make-string 7000 ?x))
         (codes (tty-image--transmit-codes 7 png 1 1))
         (payloads (mapcar (lambda (c) (string-trim-right
                                        (cadr (split-string c ";")) "\e\\\\"))
                           codes)))
    (should (= (length codes) 3))
    (should (cl-every (lambda (p) (<= (length p) 4096)) payloads))
    (should (equal (apply #'concat payloads) (base64-encode-string png t)))
    (should (string-match-p "m=1;" (nth 0 codes)))
    (should (string-prefix-p "\e_Gq=2,m=1;" (nth 1 codes)))
    (should (string-prefix-p "\e_Gq=2,m=0;" (nth 2 codes)))))

(ert-deftest tty-image-test-tmux-wrap-doubles-escapes ()
  (should (equal (tty-image--wrap-for-tmux "\e_Ga=d\e\\")
                 "\ePtmux;\e\e_Ga=d\e\e\\\e\\")))

(ert-deftest tty-image-test-placeholder-rows-carry-row-diacritics ()
  (should (equal (tty-image--placeholder-text 3 2)
                 (concat (string #x10EEEE #x0305 #x0305 #x10EEEE #x10EEEE)
                         "\n"
                         (string #x10EEEE #x030D #x0305 #x10EEEE #x10EEEE)))))

(ert-deftest tty-image-test-placeholder-composes-with-diacritics ()
  ;; The rule Emacs uses to attach combining marks for display.
  (should (eq 0 (string-match-p "\\c.\\c^+"
                                (tty-image--placeholder-text 1 2)))))

(ert-deftest tty-image-test-placeholder-rejects-too-many-rows ()
  (should-error (tty-image--placeholder-text 1 (1+ (length tty-image--diacritics)))))

(ert-deftest tty-image-test-not-supported-in-batch ()
  ;; Batch Emacs has no tty frame; never claim support there.
  (let ((tty-image-force nil)
        (process-environment (cons "TERM=xterm-256color" process-environment)))
    (should-not (tty-image-supported-p))))

;;; tty-image-test.el ends here
