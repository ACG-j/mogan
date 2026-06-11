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
             (convert rewrite init-rewrite)
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

(define (test-markdown-snippet-converter)
  (let* ((input "# Formula\n\n- $\\frac{a}{b}$\n\nPlain text.")
         (tree (generic->texmacs input "markdown-snippet"))
         (serialized (object->string (tree->stree tree))))
    (check (string-contains? serialized "bad format or data")
           => #f)
    (check (string-contains? serialized "Formula")
           => #t)
    (check (string-contains? serialized "Plain text")
           => #t)))

(define (test-markdown-snippet-converter-with-llm-output)
  (let* ((input (string-append "区域 A 上的积分为：\n\n"
                               "$$\\iint_A 1\\,dA = area(A).$$\n\n"
                               "计算面积：\n\n"
                               "- 当 $0 \\le x \\le \\frac12$ 时，$y$ 从 0 到 2。\n"
                               "- 当 $\\frac12 \\le x \\le 2$ 时，$y$ 从 0 到 $\\frac1x$。\n\n"
                               "因此\n\n"
                               "$$area(A)=1+2\\ln 2.$$"))
         (tree (generic->texmacs input "markdown-snippet"))
         (serialized (object->string (tree->stree tree))))
    (check (string-contains? serialized "bad format or data")
           => #f)
    (check (string-starts? serialized "(document")
           => #t)))

(define (test-markdown-escape-latex-line-preserves-math)
  ;; Text-mode specials (#, %, &) are escaped, but math spans are verbatim.
  (check (markdown-escape-latex-line "a & b # c")
         => "a \\& b \\# c")
  (check (markdown-escape-latex-line "x $a & b$ y")
         => "x $a & b$ y")
  (check (markdown-escape-latex-line "$$\\begin{aligned}a&=b\\\\&=c\\end{aligned}$$")
         => "$$\\begin{aligned}a&=b\\\\&=c\\end{aligned}$$")
  (check (markdown-escape-latex-line "see $\\mathcal{H}^{*}$ & more")
         => "see $\\mathcal{H}^{*}$ \\& more"))

(define (test-markdown-snippet-converter-with-ocr-output)
  ;; Mirrors PaddleOCR PPStructureV3 structured output for a mixed
  ;; text + formula screenshot: aligned display math, inline math in
  ;; the body, and a second display formula must all convert cleanly.
  (let* ((input (string-append
                  "$$\\begin{aligned}V(\\mu,\\pi)=&\\mathbb{E}"
                  "_{a_h\\sim\\mu(s_h,h)}\\\\&\\left[\\sum_{h=0}^{H-1}"
                  "\\gamma^h\\mathbb{E}(r(s_h,a_h,b_h))\\right]"
                  "\\end{aligned}$$\n\n"
                  "We denote the set of potential partner policies as "
                  "$\\mathcal{H}^{*}$, which is a subset of $\\Pi$.\n\n"
                  "$$\\mathrm{Reg}_{\\mathbf{Alg}}(K,\\mathcal{H},\\pi^{*})"
                  "=\\sum_{k\\in[K]}[V^{*}(\\pi^{*})-V(\\mu^{k},\\pi^{*})].$$"))
         (tree (generic->texmacs input "markdown-snippet"))
         (serialized (object->string (tree->stree tree))))
    (check (string-contains? serialized "bad format or data")
           => #f)
    (check (string-starts? serialized "(document")
           => #t)
    (check (string-contains? serialized "potential partner policies")
           => #t)))

(tm-define (test_222_55)
  (test-smart-paste-detect-text-format)
  (test-paste-as-markdown-does-not-insert-upsell)
  (test-markdown-snippet-converter)
  (test-markdown-snippet-converter-with-llm-output)
  (test-markdown-escape-latex-line-preserves-math)
  (test-markdown-snippet-converter-with-ocr-output)
  (check-report))
