;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 222_57.scm
;; DESCRIPTION : Unit tests for local OCR provider integration
;; COPYRIGHT   : (C) 2026 Mogan STEM authors
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(use-modules (liii ocr)
             (kernel texmacs tm-convert)
             (data latex))

(check-set-mode! 'report-failed)

(define (test-ocr-provider-format)
  (check (ocr-provider-format "pix2text" #f)
         => "markdown")
  (check (ocr-provider-format "pix2text" #t)
         => "latex")
  (check (ocr-provider-format "rapid-latex-ocr" #f)
         => "latex"))

(define (test-ocr-clean-output)
  (check (ocr-clean-output
           "INFO: loading\nIn image: /tmp/a.png\nOuts: \n\\frac{a}{b}\ncost: 0.1\n")
         => "\\frac{a}{b}"))

(define (test-ocr-result-conversion)
  (let* ((tree (ocr-result->texmacs "\\frac{a}{b}" "latex"))
         (serialized (object->string (tree->stree tree))))
    (check (string-contains? serialized "frac")
           => #t)))

(define (test-ocr-provider-missing-result)
  (when (and (not (ocr-command-available? "p2t"))
             (not (ocr-command-available? "rapid_latex_ocr")))
    (let* ((result (ocr-image->result "/tmp/nonexistent.png" #f))
           (provider (car result))
           (format (cadr result))
           (message (caddr result)))
      (check provider => #f)
      (check format => "markdown")
      (check (string-contains? message "OCR backend not found")
             => #t))))

(tm-define (test_222_57)
  (test-ocr-provider-format)
  (test-ocr-clean-output)
  (test-ocr-result-conversion)
  (test-ocr-provider-missing-result)
  (check-report))
