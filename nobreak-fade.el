;;; nobreak-fade.el --- some functions for `fill-nobreak-predicate'

;; Copyright 2009, 2010, 2011, 2014 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 8
;; Keywords: convenience, filling
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

;; This is some "nobreak" functions for `fill-nobreak-predicate'.
;; See each docstring for details.

;;; Emacsen:

;; Designed for Emacs 20 and up.  Does nothing in XEmacs 21 as it doesn't
;; have a nobreak.

;;; Install:

;; Put nobreak-fade.el in one of your `load-path' directories and in your
;; .emacs put for instance
;;
;;     (autoload 'nobreak-fade-single-letter-p "nobreak-fade")
;;     (add-hook 'fill-nobreak-predicate 'nobreak-fade-single-letter-p)
;;
;; For Emacs 20 and 21 try `nobreak-fade-add' below which copes with
;; `fill-nobreak-predicate' being a plain variable in those versions,
;;
;;     (require 'nobreak-fade)
;;     (nobreak-fade-add 'nobreak-fade-single-letter-p)
;;
;; There's autoload cookies for the functions if you know how to use
;; `update-file-autoloads' and friends, after which just add-hook or
;; customize.

;;; History:

;; Version 1 - the first version
;; Version 2 - custom-option for nobreak-fade-single-letter-p too
;; Version 3 - nobreak-fade prefix for all funcs
;;           - new nobreak-fade-emacs-M-x-p
;; Version 4 - new nobreak-fade-add
;; Version 5 - new nobreak-fade-emacs-info-link-p
;; Version 6 - interactive fun for nobreak-fade-add
;; Version 7 - emacs20 (thing-at-point 'symbol) instead of symbol-at-point
;; Version 8 - new nobreak-fade-tex-math-start-p, nobreak-fade-tex-math-end-p

;;; Code:

(defvar help-xref-info-regexp) ;; help.el or help-mode.el

(defun nobreak-fade-completing-read (symbol-list require-match)
  "Read a nobreak function name from the user in the minibuffer.
This is an internal part of nobreak-fade.el.

SYMBOL-LIST is a list of function symbols.  If REQUIRE-MATCH is
nil then the return can be any function symbol.  If REQUIRE-MATCH
is non-nil then it's one of SYMBOL-LIST."

  (let ((default (thing-at-point 'symbol))
        (table   (mapcar (lambda (symbol)
                           (let (desc)
                             ;; first line of docstring for
                             ;; completing-help.el or icicles
                             (and (fboundp symbol)
                                  (setq desc (documentation symbol))
                                  (string-match "\n" desc)
                                  (setq desc (substring desc 0
                                                        (match-beginning 0))))
                             ;; Crib note: completion table keys must be
                             ;; strings for emacs21, and completing-help.el
                             ;; 3.13 `completing-help-alist-p' only
                             ;; recognises strings too
                             (cons (symbol-name symbol) desc)))
                         symbol-list)))
    (unless (member default symbol-list)
      (setq default nil))

    (let* ((str (completing-read
                 (if default
                     (format "Nobreak function (default %s): " default)
                   "Nobreak function name: ")
                 table
                 nil           ;; predicate
                 require-match
                 nil           ;; initial input
                 'nobreak-fade-history
                 default))
           (ret (intern-soft str)))
      (unless (fboundp ret)
        (error "Not a function: %s" str))
      ret)))

;;;###autoload
(defun nobreak-fade-add (pred &optional local)
  "Add PRED to `fill-nobreak-predicate'.
If PRED is already in `fill-nobreak-predicate' then do nothing.
If LOCAL is non-nil then add buffer-local.

This is a handy way to cope with `fill-nobreak-predicate' being a
single function in Emacs 21 and earlier.  For Emacs 22 it's a
hook and this function is simply

    (add-hook 'fill-nobreak-predicate PRED nil LOCAL)

If you always use 22 and up then you may as well write that
directly, or use customize.

In Emacs 21 this function manipulates a single lambda form in
`fill-nobreak-predicate'.  And in XEmacs 21 it does nothing as
there's no such variable.

The new PRED is always prepended to any existing
`fill-nobreak-predicate' checks.  The order checks are made
normally doesn't matter."

  (interactive
   (list (nobreak-fade-completing-read
          (get 'fill-nobreak-predicate 'custom-options)
          nil)))

  ;; The flavour of `fill-nobreak-predicate' is locked down at compile-time.
  ;; Would there be merit allowing for it as an add-on in xemacs?
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
dangling at the start or end of a sentence, especially at the
start.

A single punctuation character can be present too, such as a
quotation mark, comma, or bracket.  More than one is not made a
nobreak as the visual effect isn't as bad.

See `fill-single-word-nobreak-p' (new in Emacs 23) for a nobreak
of any first or last word, not just single letter ones."

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

;;-----------------------------------------------------------------------------
;; Emacs bits

;;;###autoload
(defun nobreak-fade-emacs-M-x-p ()
  "Don't break after an Emacs M-x.
This function is designed for use in `fill-nobreak-predicate'.
It keeps an M-x on the same line as the command name that follows.

    M-x some-sort-of-command
       ^
       | no break here

This is meant to be easier on the eye than an M-x dangling at the
end of a line, and can help make it clear the command name is
indeed a command name.

A quoted `M-x' without a command name, or an M-x at the end of
sentence etc is not affected, only if followed by a word or
symbol.

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

This is only for visual effect.  The help system recognises a URL
link like this perfectly well with or without a newline.

This nobreak is specific to Emacs docstrings but a URL and
backquote should be rare in other text and so should do no harm
if enabled globally."

  (and (>= (- (point) 4) (point-min))
       (save-excursion
         (goto-char (- (point) 4))
         (let ((case-fold-search nil))
           (looking-at "\\bURL `\\S.")))))

;;;###autoload
(defun nobreak-fade-emacs-info-link-p ()
  "Don't break within an Emacs docstring info link.
This function is designed for use in `fill-nobreak-predicate'.
It keeps the node name part of an info link on one line,

    See Info node `(elisp)Documentation Tips'
                                       ^
                         no break here |

Prior to Emacs 23.2 if there's a newline within the node name
then pressing Ret to follow it only takes the name up to the
newline.  This is fixed in Emacs 23.2 but avoiding a newline
helps earlier versions.

The \"Info node\" part introducing the link can have newlines,
the problem is only in the node name.

This nobreak is specific to Emacs docstrings but an Info node
form will be rare in other text and so this nobreak should do no
harm if enabled globally."

  ;; emacs20,21 help-xref-info-regexp is in help.el and is pre-loaded.
  ;; No (require 'help) since emacs20 help.el lacks a `provide'.
  ;;
  ;; emacs22 up help-xref-info-regexp is in help-model.el which must be
  ;; required.
  ;;
  ;; xemacs21 doesn't have help-xref-info-regexp (and there's no
  ;; help-mode.el), so will get an error, but don't worry about that as
  ;; there's no fill-nobreak-predicate at all
  ;;
  (unless (boundp 'help-xref-info-regexp)
    (require 'help-mode))

  (eval-and-compile ;; quieten byte compiler call to looking-at
    (require 'thingatpt))

  (and (thing-at-point-looking-at help-xref-info-regexp)
       (progn
         (save-excursion
           (search-backward "`" (match-beginning 0) t)))))

;;;###autoload
(custom-add-option 'fill-nobreak-predicate 'nobreak-fade-single-letter-p)
;;;###autoload
(custom-add-option 'fill-nobreak-predicate 'nobreak-fade-emacs-M-x-p)
;;;###autoload
(custom-add-option 'fill-nobreak-predicate 'nobreak-fade-emacs-url-p)
;;;###autoload
(custom-add-option 'fill-nobreak-predicate 'nobreak-fade-emacs-info-link-p)

;;-----------------------------------------------------------------------------
;; TeX maths $x ... y$

(defun nobreak-fade-tex-math-start-p ()
  "Don't break after the first symbol of a TeX math $foo ...$.
This function is designed for use in `fill-nobreak-predicate'.

A break is suppressed just after a short word or symbol at the
start of a TeX math form.  For example

    $a \\=\\ge b$
      ^---- no break here

This helps to source readability by keeping a short initial
symbol together with the rest of the expression.

A word or symbol of 5 or fewer chars gives a no-break.  5 has the
advantage of keeping a \\=\\bigl opening together with the
following expression, as for example

    $\\=\\bigl( \\=\\begin{smallmatrix} ... $
           ^---- no break here

Perhaps exactly how many characters etc will change.  The
intention is no break after a small initial symbol or number.

An opening $ is distinguished from a closing $ by having
preceding whitespace or start of line.

$$ and LaTeX \\=\\[ display math are treated the same as $,
though they're usually at the start of a line anyway.

    \\=\\[123 \\=\\times 456\\=\\]
         ^---- no break here

A literal $ in math mode can be escaped as \\=\\$ and this is
recognised as not a closing $ and so can be part of a no-break,

    $\\=\\$10 \\=\\times N$
         ^---- no break here, literal $

This predicate is TeX-specific.  In particular it would be
undesirable in plain text where $123 would be a monetary amount,
or in Perl where \"$x\" is a variable.  Both would happily have a
following line break.

For this reason `nobreak-fade-tex-math-start-p' will normally
be added buffer-local to `fill-nobreak-predicate' in `tex-mode'
or similar.  See `nobreak-fade-tex-math-add' for a convenient
way to do that.

----
See `nobreak-fade-tex-math-end-p' for similar on closing $."

  (save-excursion
    ;; Point is after the whitespace between words, so for example
    ;;    foo bar  quux
    ;;             ^----point here at q
    ;; Go to start of preceding word, where word means run of non-white,
    ;; not word syntax as such since the "$" etc which are of interest are
    ;; not word chars.  Also \n is endcomment syntax rather than
    ;; whitespace, so cannot use `skip-syntax-backward' for the non-white
    ;; as it would go back past the current line.
    ;;
    (skip-chars-backward " \t")
    (let ((after (point)))
      (skip-chars-backward "^ \t\r\n")

      ;; $ or $$ or \( begins math mode.
      ;; Further $ or \) ends, except \$ is a literal $ and so does not end.
      ;; So match \ followed by non-) non-white, otherwise any non-$ non-white.
      ;;
      ;; Mixtures "\( $" or "$ \)" work and are handled here, though
      ;; actually using that would be confusing.
      ;;
      ;; \[ is ended by \] and not by $ or $$.
      ;; Usually \[, or $$ for that matter, would start at the start of a
      ;; line anyway so would not arise here.
      ;;
      (and (looking-at "\
\\(\
\\(\\$\\$?\\|\\\\(\\)\\(\\\\[^) \t]\\|[^\\$ \t]\\)\\{1,5\\}\
\\|\
\\\\\\[\\(\\\\[^] \t]\\|[^\\ \t]\\)\\{1,5\\}\
\\)")
           (= (match-end 0) after)))))

(defun nobreak-fade-tex-math-end-p ()
  "Don't break before the last symbol of a TeX math $... foo$.
This function is designed for use in `fill-nobreak-predicate'.

A break is suppressed just before the last symbol etc of a TeX
math form.  For example

    $a \\=\\ge b$
          ^---- no break here

This helps to source readability by keeping a short final symbol
together with the rest of the expression.

A word or symbol of 5 or fewer chars gives a no-break.  5 has the
advantage of keeping a \\=\\bigr closing together with the
following expression, as for example

    $ ... \\=\\end{smallmatrix} \\=\\bigl)$
                           ^---- no break here

Perhaps exactly how many characters etc will change.  The
intention is no break before a small final symbol or number.

$$ and LaTeX \\=\\[ display math are treated the same as $,
though they're usually at the start of a line anyway.

    \\=\\[123 \\=\\times 456\\=\\]
                ^---- no break here

A literal $ in math mode can be escaped as \\=\\$ and this is
recognised as not a closing $ and so does not cause a no-break,

    $a + 10\\=\\$ + b + c + d$
             ^---- break allowed here, literal $

This predicate is TeX-specific and for this reason will normally
be added buffer-local to `fill-nobreak-predicate' in `tex-mode'
or similar.  See `nobreak-fade-tex-math-add' for a convenient
way to do that.

----
See `nobreak-fade-tex-math-start-p' for similar on closing $."

  (looking-at "\
\\([^ \t$\\]\\|\\\\.\\)\\{1,5\\}\
\\(\\$\\$?\\|\\\\[])]\\)\
\\([ \t]\\|$\\)"))

;;;###autoload
(defun nobreak-fade-tex-math-add ()
  "Add Tex math nobreak predicates to `fill-nobreak-predicate'.
This function is designed for use in `tex-mode-hook' or similar.

`nobreak-fade-tex-math-start-p' and
`nobreak-fade-tex-math-end-p' are added to
`fill-nobreak-predicate', but only buffer-local since these
predicates are TeX-specific.

This function is interactive so \\[nobreak-fade-tex-math-add]
can add the predicates to try temporarily before putting them in
a hook, or perhaps if editing TeX in the middle of some other
mode."

  (interactive)
  (nobreak-fade-add 'nobreak-fade-tex-math-start-p
                     t)  ;; buffer-local
  (nobreak-fade-add 'nobreak-fade-tex-math-end-p
                     t)) ;; buffer-local

;;;###autoload
(custom-add-option 'tex-mode-hook 'nobreak-fade-tex-math-add)

;;-----------------------------------------------------------------------------

;; LocalWords: nobreak docstring docstrings Ret minibuffer foo bigl bigr ge

(provide 'nobreak-fade)

;;; nobreak-fade.el ends here
