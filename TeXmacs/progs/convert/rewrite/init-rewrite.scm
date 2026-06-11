
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-rewrite.scm
;; DESCRIPTION : setup texmacs converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert rewrite init-rewrite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main TeXmacs format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs-recognizes? s)
  (and (string? s)
       (or (string-starts? s "<TeXmacs")
           (string-starts? s "\\(\\)(TeXmacs")
           (string-starts? s "TeXmacs")
           (string-starts? s "edit"))))

(define-format texmacs
  (:name "TeXmacs")
  (:suffix "tm" "ts" "tp")
  (:must-recognize texmacs-recognizes?))

(converter texmacs-tree texmacs-stree
  (:function tree->stree))

(converter texmacs-stree texmacs-tree
  (:function stree->tree))

(converter texmacs-document texmacs-tree
  (:function parse-texmacs))

(converter texmacs-tree texmacs-document
  (:function serialize-texmacs))

(converter texmacs-snippet texmacs-tree
  (:function parse-texmacs-snippet))

(converter texmacs-tree texmacs-snippet
  (:function serialize-texmacs-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format code
  (:name "Source code"))

(tm-define (texmacs->code t . enc)
  (if (null? enc) (set! enc (list (get-locale-charset))))
  (if (tree? t)
      (cpp-texmacs->verbatim t #f (car enc))
      (texmacs->code (tm->tree t) (car enc))))

(tm-define (code->texmacs x . opts)
  (verbatim->texmacs x (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(tm-define (code-snippet->texmacs x . opts)
  (verbatim-snippet->texmacs x (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(converter texmacs-tree code-document
  (:function texmacs->code))

(converter code-document texmacs-tree
  (:function code->texmacs))
  
(converter texmacs-tree code-snippet
  (:function texmacs->code))

(converter code-snippet texmacs-tree
  (:function code-snippet->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format markdown
  (:name "Markdown")
  (:suffix "md" "markdown"))

(define (markdown-latex-special? c)
  (or (char=? c #\#)
      (char=? c #\%)
      (char=? c #\&)))

(define (markdown-escape-latex-text s)
  (let loop ((chars (string->list s)) (r '()))
    (if (null? chars)
      (list->string (reverse r))
      (let ((c (car chars)))
        (if (markdown-latex-special? c)
          (loop (cdr chars) (cons c (cons #\\ r)))
          (loop (cdr chars) (cons c r)))))))

(define (markdown-leading-heading-level s)
  (let loop ((i 0))
    (if (and (< i (string-length s)) (char=? (string-ref s i) #\#))
      (loop (+ i 1))
      (if (and (> i 0)
               (< i (string-length s))
               (char-whitespace? (string-ref s i)))
        i
        0))))

(define (markdown-heading-command level)
  (cond ((= level 1) "section*")
        ((= level 2) "subsection*")
        ((= level 3) "subsubsection*")
        (else "paragraph*")))

(define (markdown-unordered-item-text line)
  (and (>= (string-length line) 2)
       (or (char=? (string-ref line 0) #\-)
           (char=? (string-ref line 0) #\*)
           (char=? (string-ref line 0) #\+))
       (char-whitespace? (string-ref line 1))
       (substring line 2 (string-length line))))

(define (markdown-ordered-item-text line)
  (let ((n (string-length line)))
    (let loop ((i 0))
      (cond ((>= i n) #f)
            ((char-numeric? (string-ref line i)) (loop (+ i 1)))
            ((and (> i 0)
                  (< (+ i 1) n)
                  (char=? (string-ref line i) #\.)
                  (char-whitespace? (string-ref line (+ i 1))))
             (substring line (+ i 2) n))
            (else #f)))))

(define (markdown-lines->latex lines)
  (let ((result '())
        (list-mode #f)
        (code-mode? #f))
    (define (emit s) (set! result (cons s result)))
    (define (close-list)
      (when list-mode
        (emit (string-append "\\end{" list-mode "}\n\n"))
        (set! list-mode #f)))
    (define (open-list mode)
      (when (not (== list-mode mode))
        (close-list)
        (emit (string-append "\\begin{" mode "}\n"))
        (set! list-mode mode)))
    (for-each
      (lambda (line)
        (let ((s (string-trim-spaces line)))
          (cond
            (code-mode?
             (if (string-starts? s "```")
               (begin
                 (emit "\\end{verbatim}\n\n")
                 (set! code-mode? #f))
               (begin
                 (emit line)
                 (emit "\n"))))
            ((string-starts? s "```")
             (close-list)
             (emit "\\begin{verbatim}\n")
             (set! code-mode? #t))
            ((string-null? s)
             (close-list)
             (emit "\n"))
            ((> (markdown-leading-heading-level s) 0)
             (let* ((level (markdown-leading-heading-level s))
                    (body (substring s (+ level 1) (string-length s)))
                    (cmd (markdown-heading-command level)))
               (close-list)
               (emit (string-append "\\" cmd "{"
                                    (markdown-escape-latex-text body)
                                    "}\n\n"))))
            ((markdown-unordered-item-text s)
             (open-list "itemize")
             (emit (string-append "\\item "
                                  (markdown-escape-latex-text
                                    (markdown-unordered-item-text s))
                                  "\n")))
            ((markdown-ordered-item-text s)
             (open-list "enumerate")
             (emit (string-append "\\item "
                                  (markdown-escape-latex-text
                                    (markdown-ordered-item-text s))
                                  "\n")))
            (else
             (close-list)
             (emit (string-append (markdown-escape-latex-text line) "\n\n"))))))
      lines)
    (when code-mode? (emit "\\end{verbatim}\n\n"))
    (close-list)
    (string-join (reverse result) "")))

(tm-define (markdown-snippet->texmacs x . opts)
  (let* ((text (string-replace x "\r\n" "\n"))
         (lines (string-split text #\newline))
         (latex (markdown-lines->latex lines)))
    (generic->texmacs latex "latex-snippet")))

(converter markdown-snippet texmacs-tree
  (:function-with-options markdown-snippet->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbatim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->verbatim x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (let* ((wrap? (== (assoc-ref opts "texmacs->verbatim:wrap") "on"))
         (enc (or (assoc-ref opts "texmacs->verbatim:encoding") "auto")))
    (cpp-texmacs->verbatim x wrap? enc)))

(tm-define (texmacs->verbatim-snippet x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (let* ((wrap? (== (assoc-ref opts "texmacs->verbatim:wrap") "on"))
         (enc (or (assoc-ref opts "texmacs->verbatim:encoding") "auto")))
    (if (or (== (get-env "mode") "prog") (== (get-env "font-family") "tt"))
        ;; FIXME: dirty hacks for "copy to verbatim" of code snippets
        (let ((conv (cpp-texmacs->verbatim x #f enc))
              (tick (cpp-texmacs->verbatim (tm->tree "`") #f enc)))
          (string-replace conv tick "`"))
        (cpp-texmacs->verbatim x wrap? enc))))

(tm-define (verbatim->texmacs x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (let* ((wrap? (== (assoc-ref opts "verbatim->texmacs:wrap") "on"))
         (enc (or (assoc-ref opts "verbatim->texmacs:encoding") "utf-8")))
    (cpp-verbatim->texmacs x wrap? enc)))

(tm-define (verbatim-snippet->texmacs x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (let* ((wrap? (== (assoc-ref opts "verbatim->texmacs:wrap") "on"))
         (enc (or (assoc-ref opts "verbatim->texmacs:encoding") "utf-8")))
    (cpp-verbatim-snippet->texmacs x wrap? enc)))

(define-format verbatim
  (:name "Verbatim"))
;;(:suffix "txt"))

(converter verbatim-document texmacs-tree
  (:function-with-options verbatim->texmacs)
  (:option "verbatim->texmacs:wrap" "off")
  (:option "verbatim->texmacs:encoding" "utf-8"))

(converter verbatim-snippet texmacs-tree
  (:function-with-options verbatim-snippet->texmacs)
  (:option "verbatim->texmacs:wrap" "off")
  (:option "verbatim->texmacs:encoding" "utf-8"))

(converter texmacs-tree verbatim-document
  (:function-with-options texmacs->verbatim)
  (:option "texmacs->verbatim:wrap" "off")
  (:option "texmacs->verbatim:encoding" "auto"))

(converter texmacs-tree verbatim-snippet
  (:function-with-options texmacs->verbatim-snippet)
  (:option "texmacs->verbatim:wrap" "off")
  (:option "texmacs->verbatim:encoding" "auto"))
