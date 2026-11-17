;;;; zustral-mode.el - Major mode for editing Zustral source files.
;;;;
;;;; Copyright (C) 2021 Fernando Borretti
;;;;
;;;; Author: Fernando Borretti <fernando@borretti.me>
;;;; Maintainer: Fernando Borretti <fernando@borretti.me>
;;;; Created: 24 July, 2021
;;;; Version: 0.0.1
;;;; Keywords: languages
;;;;
;;;; Structure
;;;; ---------
;;;;
;;;;     1. Constants
;;;;     1. Syntax Highlighting
;;;;     1. Mode Definition
;;;;

(require 'cl-lib)

;;;; Section 1. Constants

(defconst zustral-keywords
  '("and" "or" "not" "module" "is" "body" "import" "as" "end" "constant" "type"
    "function" "generic" "record" "union" "case" "of" "when" "typeclass"
    "instance" "method" "if" "then" "else" "let" "var" "while" "for" "do" "from"
    "to" "borrow" "borrow!" "in" "return" "skip" "Free" "Linear" "Type" "Region"
    "pragma" "nil" "true" "false"))

(defconst zustral-indent-1-regexp
  "^[ \t]*\\(module\\|import\\|record\\|union\\|interface\\|function\\|if\\|else\\|when\\|for\\|while\\|borrow\\)\\>")
(defconst zustral-indent-2-regexp
  "^[ \t]*\\(case\\)\\>")
(defconst zustral-deindent-2-regexp
  "^[ \t]*\\(end case;\\)")
(defconst zustral-deindent-1-regexp
  "^[ \t]*\\(end[ \t_[:alnum:]]*[;.]\\|[)];\\|when\\>\\|else\\>\\)")

(defconst zustral-default-tab-width 4)

;;;; Section 1. Syntax Highlighting
;;;;

;; Build a new syntax-table from scratch to avoid any defaults:
(defconst zustral-syntax-table
  (let ((table (make-char-table 'syntax-table)))
    (modify-syntax-entry   ?\#        "w"  table) ;; hex/bin/oct constants.
    (modify-syntax-entry   ?\!        "."  table)
    (modify-syntax-entry   ?\&        "."  table)
    (modify-syntax-entry   ?\(        "()" table)
    (modify-syntax-entry   ?\)        ")(" table)
    (modify-syntax-entry '(?\* . ?\/) "."  table)
    (modify-syntax-entry '(?0  . ?9 ) "w"  table)
    (modify-syntax-entry '(?\: . ?\>) "."  table)
    (modify-syntax-entry   ?@         "w"  table) ;; @embed
    (modify-syntax-entry '(?A  . ?Z ) "w"  table)
    (modify-syntax-entry   ?\[        "(]" table)
    (modify-syntax-entry   ?\]        ")[" table)
    (modify-syntax-entry   ?_         "w"  table)
    (modify-syntax-entry '(?a  . ?z ) "w"  table)
    (modify-syntax-entry   ?\{        "(}" table)
    (modify-syntax-entry   ?\}        "){" table)
    (modify-syntax-entry   ?\~        "."  table) ;; &~ reborrow.
    table))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
(defvar zustral-font-lock-keywords
  (list
   ;; Comments
   (cons "--.*" font-lock-comment-face)
   ;; Strings, using the syntax-table (above) would interfere with comments.
   (cons "\"\"\"\\([^\"]*\\(\"[^\"]\\|\"\"[^\"]\\)\\)*[^\"]*\"\"\"" font-lock-doc-face)
   (cons "\"\\([^\\\\\n]*[\\\\].\\)*[^\"\n]*\"" font-lock-string-face)
   ;; Keywords
   (cons (regexp-opt zustral-keywords 'words) font-lock-keyword-face)
   ;; Types
   (cons ":[ \\t]*\\(\\<[[:alpha:]][][&!,_[:alnum:]]*\\)" '(1 font-lock-type-face))
   (cons "\\(\\<[[:alpha:]][_[:alnum:]]*\\):[ \t]*Type\\>" '(1 font-lock-type-face))
   ;; Functions
   (cons "\\(\\<[[:alpha:]][_[:alnum:]]*\\>\\)(" '(1 font-lock-function-name-face))
   ;; Identifiers
   (cons "\\<[[:alpha:]][_[:alnum:]]*\\>" font-lock-variable-name-face)
   ))

;;;; Section 1. Indentation
;;;;

(defun zustral-find-previous-non-empty-line ()
  "Returns a cons cell, whose car is the indentation of the closest non-blank
line before the current one, and whose cdr is the text of that line.

If there are no blank-lines, returns zero and the empty string."
  ;; Go to the end of the previous line to avoid matching the current line:
  (save-excursion
    (beginning-of-line)
    (if (= (point) 1)
        (cons 0 "") ;; Beginning of the buffer. So we indent to zero.
      (progn
        (backward-char)
        (if (re-search-backward "^[ \t]*." nil t)
            (let ((text (buffer-substring (line-beginning-position)
                                          (line-end-position))))
              (cons (current-indentation) text))
          (cons 0 ""))))))

(defun zustral-indent-line ()
  "Indent the current line of Zustral code."
  ;; previous-indent is the indentation of the closest non-empty line
  ;; before the current line. previous-text is the text of said line.
  (let* ((non-empty (zustral-find-previous-non-empty-line))
         (previous-indent (car non-empty))
         (previous-text   (cdr non-empty))
         (indent
          (cond
           ((string-match zustral-indent-2-regexp previous-text)
            ;; The previous line opens a new indent block.
            (+ (* 2 zustral-default-tab-width) previous-indent))
           ((string-match zustral-indent-1-regexp previous-text)
            ;; The previous line opens a new indent block.
            (+      zustral-default-tab-width  previous-indent))
           (t
            ;; Indent same as the previous line.
            previous-indent))))
    (save-excursion
      (beginning-of-line)
      (cond
       ((looking-at zustral-deindent-2-regexp)
        ;; This line should be indented 2 levels less.
        (setq indent (max (- indent (* 2 zustral-default-tab-width)) 0)))
       ((looking-at zustral-deindent-1-regexp)
        ;; This line should be indented less.
        (setq indent (max (- indent      zustral-default-tab-width ) 0)))
       ))
    (indent-line-to indent)
    ))

;;;; Section 1. Mode Definition
;;;;

(define-derived-mode zustral-mode fundamental-mode "Zustral"
  :syntax-table zustral-syntax-table
  "Major mode for editing Zustral source text."
  (setq font-lock-defaults '((zustral-font-lock-keywords)))
  (setq indent-line-function 'zustral-indent-line))


(add-to-list 'auto-mode-alist '("\\.zui\\'" . zustral-mode))
(add-to-list 'auto-mode-alist '("\\.zum\\'" . zustral-mode))

(provide 'zustral-mode)
