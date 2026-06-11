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
  (check (ocr-provider-format "paddleocr" #f)
         => "markdown")
  (check (ocr-provider-format "easyocr" #f)
         => "markdown")
  (check (ocr-provider-format "rapid-latex-ocr" #f)
         => "latex"))

(define (test-ocr-provider-selection)
  (cond ((ocr-paddleocr-available?)
         (check (ocr-select-provider #f) => "paddleocr"))
        ((ocr-easyocr-available?)
         (check (ocr-select-provider #f) => "easyocr")))
  (when (and (ocr-command-available? "rapid_latex_ocr")
             (!= (get-preference "ocr.provider") "pix2text"))
    (check (ocr-select-provider #t) => "rapid-latex-ocr")))

(define (test-ocr-pix2text-gpu-probe)
  (when (ocr-command-available? "p2t")
    (check (boolean? (ocr-pix2text-gpu-available?))
           => #t)))

(define (test-ocr-pix2text-tool-paths)
  (when (ocr-command-available? "p2t")
    (let ((site-library (ocr-tool-python-site-library "p2t"))
          (library-path (ocr-tool-library-path "p2t")))
      (check (string? site-library) => #t)
      (check (string? library-path) => #t)
      (when (!= site-library "")
        (check (string-contains? site-library "site-packages")
               => #t)))))

(define (test-ocr-paddleocr-probe)
  (when (ocr-command-available? "paddleocr")
    (check (boolean? (ocr-paddleocr-available?))
           => #t)))

(define (test-ocr-clean-output)
  (check (ocr-clean-output
           "INFO: loading\nIn image: /tmp/a.png\nOuts: \n\\frac{a}{b}\ncost: 0.1\n")
         => "\\frac{a}{b}"))

(define (test-ocr-easyocr-line-filter)
  (check (ocr-keep-easyocr-text-line?
           "V(p,t) = Eah~H(shh)bh~T(shh) H-1 YhE(r (Sh; ahs bh))")
         => #f)
  (check (ocr-keep-easyocr-text-line?
           "We denote the set of potential partner policies as H* which is")
         => #t)
  (check (ocr-keep-easyocr-text-line?
           "RegAlg ' (K,H,t\") = [V* (j*) - V(pk,n*)].")
         => #f))

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
  (test-ocr-provider-selection)
  (test-ocr-pix2text-gpu-probe)
  (test-ocr-pix2text-tool-paths)
  (test-ocr-paddleocr-probe)
  (test-ocr-clean-output)
  (test-ocr-easyocr-line-filter)
  (test-ocr-result-conversion)
  (test-ocr-provider-missing-result)
  (check-report))
