;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 222_56.scm
;; DESCRIPTION : Unit tests for magic paste shortcuts
;; COPYRIGHT   : (C) 2026 Mogan STEM authors
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(use-modules (generic generic-kbd)
             (kernel gui kbd-define))

(check-set-mode! 'report-failed)

(define (test-magic-paste-shortcut)
  (check (in? '("C-V" ()) (get-bindings-by-command '(kbd-magic-paste)))
         => #t))

(tm-define (test_222_56)
  (test-magic-paste-shortcut)
  (check-report))
