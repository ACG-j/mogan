;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 222_55.scm
;; DESCRIPTION : Unit tests for smart paste format detection
;; COPYRIGHT   : (C) 2026 Mogan STEM authors
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(use-modules (generic generic-edit)
             (kernel texmacs tm-convert)
             (data html)
             (data latex))

(check-set-mode! 'report-failed)

(define (test-smart-paste-detect-text-format)
  (check (smart-paste-detect-text-format "# Title\n\n- item")
         => "markdown")
  (check (smart-paste-detect-text-format "```scheme\n(display 1)\n```")
         => "markdown")
  (check (smart-paste-detect-text-format "\\frac{a}{b}")
         => "latex")
  (check (smart-paste-detect-text-format "# Formula\n\n- $\\frac{a}{b}$")
         => "markdown")
  (check (smart-paste-detect-text-format "<p>Hello</p>")
         => "html")
  (check (smart-paste-detect-text-format "Just ordinary text.")
         => "verbatim")
  (check (smart-paste-detect-text-format #f)
         => "verbatim"))

(define (test-paste-as-markdown-does-not-insert-upsell)
  (let* ((source (string-load (unix->url "$TEXMACS_PATH/progs/generic/generic-edit.scm"))))
    (check (string-contains? source "plugins/account/data/md.tex")
           => #f)))

(tm-define (test_222_55)
  (test-smart-paste-detect-text-format)
  (test-paste-as-markdown-does-not-insert-upsell)
  (check-report))
