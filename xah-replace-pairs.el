;;; xah-replace-pairs.el --- Multi-pair find/replace in strings and region.

;; Copyright © 2010-2015, by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 2.0.1
;; Created: 17 Aug 2010
;; Keywords: lisp, tools
;; Homepage: http://ergoemacs.org/emacs/elisp_replace_string_region.html

;; This file is not part of GNU Emacs.

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; Commentary:

;; This package provides elisp function xah-replace-pairs-region and variations that do find/replace with multiple pairs of strings.

;; xah-replace-pairs-region
;; xah-replace-pairs-in-string
;; xah-replace-regexp-pairs-region
;; xah-replace-regexp-pairs-in-string
;; xah-replace-pairs-region-recursive
;; xah-replace-pairs-in-string-recursive

;; Please Buy Xah Emacs Tutorial
;; http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html
;; Thanks.

;;; INSTALL:

;; Place the file in emacs's load path (see: `load-path')  (typically ~/emacs.d/lisp/). Then (require 'xah-replace-pairs) in your lisp code.

;;; History:

;; 2015-04-28 major rewrite. This package was xfrp_find_replace_pairs
;; version 1.0, 2010-08-17. First version.


;;; Code:

(defun xah-replace-pairs-region (φbegin φend φpairs)
  "Replace multiple ΦPAIRS of find/replace strings in region ΦBEGIN ΦEND.

ΦPAIRS is a sequence of pairs
 [[findStr1 replaceStr1] [findStr2 replaceStr2] …]
It can be list or vector, for the elements or the entire argument.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)

The replacement are literal and case sensitive.

Once a subsring in the buffer is replaced, that part will not change again.  For example, if the buffer content is “abcd”, and the φpairs are a → c and c → d, then, result is “cbdd”, not “dbdd”.

Note: the region's text or any string in ΦPAIRS is assumed to NOT contain any character from Unicode Private Use Area A. That is, U+F0000 to U+FFFFD. And, there are no more than 65534 pairs."
  (let (
        (ξunicodePriveUseA #xf0000)
        (ξi 0)
        (ξtempMapPoints '()))
    (progn
      ;; generate a list of Unicode chars for intermediate replacement. These chars are in  Private Use Area.
      (setq ξi 0)
      (while (< ξi (length φpairs))
        (push (char-to-string (+ ξunicodePriveUseA ξi)) ξtempMapPoints)
        (setq ξi (1+ ξi))))
    (save-excursion
      (save-restriction
        (narrow-to-region φbegin φend)
        (progn
          ;; replace each find string by corresponding item in ξtempMapPoints
          (setq ξi 0)
          (while (< ξi (length φpairs))
            (goto-char (point-min))
            (while (search-forward (elt (elt φpairs ξi) 0) nil t)
              (replace-match (elt ξtempMapPoints ξi) t t))
            (setq ξi (1+ ξi))))
        (progn
          ;; replace each ξtempMapPoints by corresponding replacement string
          (setq ξi 0)
          (while (< ξi (length φpairs))
            (goto-char (point-min))
            (while (search-forward (elt ξtempMapPoints ξi) nil t)
              (replace-match (elt (elt φpairs ξi) 1) t t))
            (setq ξi (1+ ξi))))))))

(defun xah-replace-pairs-in-string (φstr φpairs)
  "Replace string ΦSTR by find/replace ΦPAIRS sequence.
Returns the new string.
This function is a wrapper of `xah-replace-pairs-region'. See there for detail."
  (with-temp-buffer
    (insert φstr)
    (xah-replace-pairs-region 1 (point-max) φpairs)
    (buffer-string)))

(defun xah-replace-regexp-pairs-region (φbegin φend φpairs &optional φfixedcase-p φliteral-p)
  "Replace regex string find/replace ΦPAIRS in region.

ΦBEGIN ΦEND are the region boundaries.

ΦPAIRS is
 [[regexStr1 replaceStr1] [regexStr2 replaceStr2] …]
It can be list or vector, for the elements or the entire argument.

The optional arguments ΦFIXEDCASE-P and ΦLITERAL-P is the same as in `replace-match'.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)"
  (save-restriction
      (narrow-to-region φbegin φend)
      (mapc
       (lambda (ξx)
         (goto-char (point-min))
         (while (search-forward-regexp (elt ξx 0) (point-max) t)
           (replace-match (elt ξx 1) φfixedcase-p φliteral-p)))
       φpairs)))

(defun xah-replace-regexp-pairs-in-string (φstr φpairs &optional φfixedcase-p φliteral-p)
  "Replace string ΦSTR recursively by regex find/replace pairs ΦPAIRS sequence.

This function is a wrapper of `xah-replace-regexp-pairs-region'. See there for detail.

See also `xah-replace-pairs-in-string'."
  (with-temp-buffer 
    (insert φstr)
    (goto-char (point-min))
    (xah-replace-regexp-pairs-region (point-min) (point-max) φpairs φfixedcase-p φliteral-p)
    (buffer-string)))

(defun xah-replace-pairs-region-recursive (φbegin φend φpairs)
  "Replace multiple ΦPAIRS of find/replace strings in region ΦBEGIN ΦEND.

This function is similar to `xah-replace-pairs-region', except that the replacement is done recursively after each find/replace pair.  Earlier replaced value may be replaced again.

For example, if the input string is “abcd”, and the pairs are a → c and c → d, then, the result is “dbdd”, not “cbdd”.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)

The replacement are literal and case sensitive."
  (save-restriction
    (narrow-to-region φbegin φend)
    (mapc
     (lambda (x)
       (goto-char (point-min))
       (while (search-forward (elt x 0) (point-max) 'NOERROR)
         (replace-match (elt x 1) t t)))
     φpairs)))

(defun xah-replace-pairs-in-string-recursive (φstr φpairs)
  "Replace string ΦSTR recursively by find/replace pairs ΦPAIRS sequence.

This function is is a wrapper of `xah-replace-pairs-region-recursive'. See there for detail."
  (with-temp-buffer
    (insert φstr)
    (goto-char (point-min))
    (xah-replace-pairs-region-recursive (point-min) (point-max) φpairs)
    (buffer-string)))

(provide 'xah-replace-pairs)

;;; xah-replace-pairs.el ends here
