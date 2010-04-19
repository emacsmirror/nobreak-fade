;;; nobreak-fade.el --- some functions for `fill-nobreak-predicate'

;; Copyright 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 4
;; Keywords: convenience
;; URL: http://user42.tuxfamily.org/nobreak-fade/index.html

;; nobreak-fade.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; nobreak-fade.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.
 
;;; Commentary:

;; This is a few "nobreak" functions for `fill-nobreak-predicate'.
;; See each docstring for details.

;;; Install:

;; Put nobreak-fade.el in one of your `load-path' directories and in your
;; .emacs put for instance
;;
;;     (autoload 'nobreak-fade-single-letter-p "nobreak-fade")
;;     (add-hook 'fill-nobreak-predicate 'nobreak-fade-single-letter-p)
;;
;; or in Emacs 20 and 21 an equivalent setq or lambda (since
;; `fill-nobreak-predicate' is not a hook until Emacs 22).
;;
;; There's autoload cookies for the functions if you know how to use
;; `update-file-autoloads' and friends, after which just add-hook or
;; customize.

;;; Emacsen:

;; Designed for Emacs 21 and up.  Does nothing in XEmacs 21 as it doesn't
;; have a nobreak.

;;; History:

;; Version 1 - the first version
;; Version 2 - custom-option for nobreak-fade-single-letter-p too
;; Version 3 - nobreak-fade prefix for all funcs
;;           - new nobreak-fade-emacs-M-x-p
;; Version 4 - new nobreak-fade-add

;;; Code:

;;;###autoload
(defun nobreak-fade-add (pred &optional local)
  "Add PRED to `fill-nobreak-predicate'.
If PRED is already in `fill-nobreak-predicate' then do nothing.
If LOCAL is non-nil then add it buffer-local.

This is a handy way to cope with different
`fill-nobreak-predicate' in different versions of Emacs.  In
Emacs 22 up it's simply

    (add-hook 'fill-nobreak-predicate PRED nil LOCAL)

If you always use 22 and up then you may as well write that
directly, or use customize.

In Emacs 21 `nobreak-fade-add' manipulates a single-function
`fill-nobreak-predicate' value, and in XEmacs 21 it does
nothing.

The new PRED is always prepended to any existing
`fill-nobreak-predicate' checks.  The order checks are made
normally doesn't matter."

  ;; The flavour of `fill-nobreak-predicate' is locked down at compile-time.
  ;; Is there any merit in allowing for it added later in xemacs?
  ;;
  (cond ((eval-when-compile
           (eq 'hook (get 'fill-nobreak-predicate 'custom-type)))
         ;; emacs22 up, defcustom hook
         (add-hook 'fill-nobreak-predicate pred nil local))

        ((eval-when-compile (not (boundp 'fill-nobreak-predicate)))
         ;; xemacs21, no such variable
         )

        (t
         ;; emacs21, single predicate function
         (if local (make-local-variable 'fill-nobreak-predicate))
         (cond ((not fill-nobreak-predicate)
                ;; no value yet, store
                (setq fill-nobreak-predicate pred))
               ((eq pred fill-nobreak-predicate)
                ;; already equal, do nothing
                )
               ((and (eq   'lambda (car-safe fill-nobreak-predicate))
                     (null (nth 1 fill-nobreak-predicate))
                     (eq   'or (car-safe (nth 2 fill-nobreak-predicate)))
                     (null (nthcdr 3 fill-nobreak-predicate)))
                ;; lambda of `or'
                (unless (member (list pred)
                                (cdr (nth 2 fill-nobreak-predicate)))
                  (setq fill-nobreak-predicate
                        `(lambda () (or ,(list pred)
                                        ,@(cdr (nth 2 fill-nobreak-predicate)))))))
               (t ;; existing value, add to it
                (set 'fill-nobreak-predicate
                     `(lambda ()
                        (or ,(list pred)
                            ,(list fill-nobreak-predicate)))))))))

;;-----------------------------------------------------------------------------

;;;###autoload
(defun nobreak-fade-single-letter-p ()
  "Don't dangle a single letter at start or end of sentence.
This function is designed for use in `fill-nobreak-predicate'.
It prevents a line break after a single-letter word at the start
of a sentence, like

    This is some text.  A     <--- no break after \"A\"
    lot more is possible.

and similarly before a single letter word at the end of a
sentence,

    `Wandering minstrel,
    I'.                       <--- no break before \"I\"

The idea is that it's easier on the eye not to have a small word
dangling, especially at the start of a sentence.

A single punctuation character can be present too, such as a
quotation mark, comma, or bracket.  More than one is not made a
nobreak as the visual effect isn't as bad.

See `fill-single-word-nobreak-p' (new in Emacs 23) for nobreak of
any first or last word, not just single letter ones."

  ;; punct \\s. and also parens \\s( and \\s)
  ;; pattern then "( letter punct | punct ? letter )"
  (let ((letter-and-punct
         "\\(\\sw\\(\\s.\\|\\s(\\|\\s)\\)\\|\\(\\s.\\|\\s(\\|\\s)\\)?\\sw\\)"))
    (or
     ;; Following word at end of sentence: single char + punct, and then
     ;; `sentence-end'.  For flexibility as well as `sentence-end' allow
     ;;     - next line a `paragraph-start'
     ;;     - end of buffer
     ;;     - some newlines then end of buffer
     ;;
     (looking-at (concat letter-and-punct "[ \t]*\
\\(\
\\(" (if (eval-when-compile (fboundp 'sentence-end))
         (sentence-end) ;; new in emacs22
       sentence-end) "\\)\
\\|\
\\(\n" paragraph-start "\\)\
\\|\
\n*\\'\
\\)"))

     ;; Preceding word at start of sentence: `backward-sentence' to go to
     ;; what then should be single char + punct and then original `point'.
     (let ((pos (point)))
       (save-excursion
         (backward-sentence)
         (and (looking-at (concat letter-and-punct "[ \t]*"))
              (equal pos (match-end 0))))))))

;;;###autoload
(defun nobreak-fade-emacs-M-x-p ()
  "Don't break after an Emacs M-x.
This function is designed for use in `fill-nobreak-predicate'.
It keeps an M-x on the same line as the command name that follows.

    M-x some-sort-of-command
       ^
       | no break here

This is meant to be easier on the eye than an M-x dangling at the
end of a line, and help make it clear the command name is indeed
a command name.

A quoted `M-x', or an M-x at the end of sentence etc is not
affected, only if followed by a word or symbol.

This is Emacs specific but should be rare otherwise and so should
do no harm if enabled globally.  Having it in all modes is good
if writing about Emacs things in `text-mode', `texinfo-mode',
etc."

  (and (>= (- (point) 4) (point-min))
       (save-excursion
         (goto-char (- (point) 4))
         (let ((case-fold-search nil))
           (looking-at "\\bM-x \\sw")))))

;;;###autoload
(defun nobreak-fade-emacs-url-p ()
  "Don't break after URL of \"URL `http://...'\".
This function is designed for use in `fill-nobreak-predicate'.
It avoids a line break after the URL in an Emacs docstring form
like

    URL `http://...'
       ^
       | no break here

This is only for visual effect.  The help display system
recognises a URL like this perfectly well with a newline.

This nobreak is specific to Emacs docstrings but a URL and
backquote should be rare in other text and should do no harm if
enabled globally."

  (and (>= (- (point) 4) (point-min))
       (save-excursion
         (goto-char (- (point) 4))
         (let ((case-fold-search nil))
           (looking-at "\\bURL `\\S.")))))



;;;###autoload
(custom-add-option 'fill-nobreak-predicate 'nobreak-fade-single-letter-p)
;;;###autoload
(custom-add-option 'fill-nobreak-predicate 'nobreak-fade-emacs-M-x-p)
;;;###autoload
(custom-add-option 'fill-nobreak-predicate 'nobreak-fade-emacs-url-p)

(provide 'nobreak-fade)

;;; nobreak-fade.el ends here
