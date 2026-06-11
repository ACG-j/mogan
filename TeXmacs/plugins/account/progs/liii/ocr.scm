
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ocr.scm
;; DESCRIPTION : ocr
;; COPYRIGHT   : (C) 2025  Mogan STEM authors
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (liii ocr))
(import (liii os))
(import (liii base64))
(import (liii time))

(define temp-dir (os-temp-dir))
(define ocr-default-languages "en,ch_sim")

(define-preferences
  ("ocr.provider" "auto" noop)
  ("ocr.languages" ocr-default-languages noop))

(define-public (ocr-command-available? cmd)
  (and (string? cmd) (url-exists-in-path? cmd)))

(define (ocr-command-output cmd)
  (tm-string-trim-both (eval-system (string-append cmd " 2>&1"))))

(define-public (ocr-shell-quote s)
  (string-quote s))

(define (ocr-python-command script)
  (string-append "python -c " (ocr-shell-quote script)))

(define (ocr-tool-python-command tool script)
  (string-append "tool_path=$(command -v "
                 (ocr-shell-quote tool)
                 "); tool_python=$(sed -n '1s/^#!//p' \"$tool_path\"); "
                 "\"$tool_python\" -c "
                 (ocr-shell-quote script)))

(define (ocr-tool-python-site-library tool)
  (ocr-command-output
    (ocr-tool-python-command
      tool
      "import sysconfig; print(sysconfig.get_paths()['purelib'])")))

(define (ocr-tool-library-path tool)
  (let ((site-library (ocr-tool-python-site-library tool)))
    (if (== site-library "")
        ""
        (ocr-command-output
          (string-append "find "
                         (ocr-shell-quote (string-append site-library "/nvidia"))
                         " -type d -name lib 2>/dev/null | paste -sd ':' -")))))

(define (ocr-command-with-tool-libraries tool command)
  (let ((library-path (ocr-tool-library-path tool)))
    (if (== library-path "")
        command
        (string-append "LD_LIBRARY_PATH="
                       (ocr-shell-quote
                         (if (== (getenv "LD_LIBRARY_PATH" "") "")
                             library-path
                             (string-append library-path ":"
                                            (getenv "LD_LIBRARY_PATH" ""))))
                       " "
                       command))))

(define (ocr-python-string s)
  (string-append "'" s "'"))

(define (ocr-python-module-available-with command-maker module-name)
  (and (ocr-command-available? "python")
       (== (ocr-command-output
             (command-maker
               (string-append "import importlib.util; "
                              "print('yes' if importlib.util.find_spec("
                              (ocr-python-string module-name)
                              ") else 'no')")))
           "yes")))

(define (ocr-python-module-available? module-name)
  (ocr-python-module-available-with ocr-python-command module-name))

(define (ocr-python-cuda-available-with command-maker)
  (and (== (ocr-command-output
             (command-maker
               "import torch; print('yes' if torch.cuda.is_available() else 'no')"))
           "yes")))

(define (ocr-onnx-cuda-available-with command-maker)
  (and (== (ocr-command-output
             (command-maker
               (string-append "import onnxruntime as ort; "
                              "print('yes' if 'CUDAExecutionProvider' in "
                              "ort.get_available_providers() else 'no')")))
           "yes")))

(define-public (ocr-gpu-available?)
  (or (ocr-onnx-cuda-available-with ocr-python-command)
      (and (ocr-python-module-available? "torch")
           (ocr-python-cuda-available-with ocr-python-command))))

(define-public (ocr-pix2text-gpu-available?)
  (ocr-onnx-cuda-available-with
    (lambda (script)
      (ocr-command-with-tool-libraries
        "p2t"
        (ocr-tool-python-command "p2t" script)))))

(define (ocr-pix2text-device)
  (if (ocr-pix2text-gpu-available?) "gpu" "cpu"))

(define-public (ocr-available-providers)
  (let ((providers '()))
    (when (ocr-command-available? "p2t")
      (set! providers (cons "pix2text" providers)))
    (when (ocr-command-available? "rapid_latex_ocr")
      (set! providers (cons "rapid-latex-ocr" providers)))
    (reverse providers)))

(define-public (ocr-select-provider formula?)
  (let ((preferred (get-preference "ocr.provider"))
        (available (ocr-available-providers)))
    (cond ((and (== preferred "pix2text") (in? "pix2text" available))
           "pix2text")
          ((and (== preferred "rapid-latex-ocr")
                (in? "rapid-latex-ocr" available))
           "rapid-latex-ocr")
          ((and formula? (in? "rapid-latex-ocr" available))
           "rapid-latex-ocr")
          ((in? "pix2text" available)
           "pix2text")
          ((in? "rapid-latex-ocr" available)
           "rapid-latex-ocr")
          (else #f))))

(define-public (ocr-provider-format provider formula?)
  (cond ((== provider "pix2text")
         (if formula? "latex" "markdown"))
        ((== provider "rapid-latex-ocr") "latex")
        (else "verbatim")))

(define (ocr-pix2text-command image-path formula?)
  (ocr-command-with-tool-libraries
    "p2t"
    (string-append "p2t predict -l "
                   (ocr-shell-quote (get-preference "ocr.languages"))
                   " --device "
                   (ocr-pix2text-device)
                   " --file-type "
                   (if formula? "formula" "text_formula")
                   " -i "
                   (ocr-shell-quote image-path))))

(define (ocr-rapidlatex-command image-path)
  (string-append "rapid_latex_ocr " (ocr-shell-quote image-path)))

(define (ocr-output-body output)
  (let* ((text (force-string output))
         (parts (string-decompose text "Outs:")))
    (if (> (length parts) 1)
        (string-recompose (cdr parts) "Outs:")
        text)))

(define-public (ocr-clean-output output)
  (let* ((trimmed (tm-string-trim-both (ocr-output-body output)))
         (lines (string-split trimmed #\newline))
         (useful (list-filter lines
                   (lambda (line)
                     (let ((line* (tm-string-trim-both line)))
                       (and (!= line* "")
                            (not (string-starts? line* "Running"))
                            (not (string-starts? line* "Loading"))
                            (not (string-starts? line* "Using"))
                            (not (string-starts? line* "INFO:"))
                            (not (string-starts? line* "WARNING:"))
                            (not (string-contains? line* " In image:"))
                            (not (string-starts? line* "In image:"))
                            (not (string-contains? line* " Outs:"))
                            (not (string-starts? line* "Outs:"))
                            (not (string-starts? line* "cost:"))))))))
    (tm-string-trim-both (string-recompose useful "\n"))))

(define (ocr-run-provider provider image-path formula?)
  (let* ((cmd (cond ((== provider "pix2text")
                     (ocr-pix2text-command image-path formula?))
                    ((== provider "rapid-latex-ocr")
                     (ocr-rapidlatex-command image-path))
                    (else "")))
         (output (if (== cmd "") "" (ocr-command-output cmd))))
    (ocr-clean-output output)))

(define (ocr-missing-provider-message)
  (string-append
    "OCR backend not found.\n\n"
    "Install one local backend and try smart paste again:\n\n"
    "- Pix2Text: pip install pix2text\n"
    "- RapidLaTeXOCR: pip install rapid_latex_ocr\n\n"
    "Pix2Text is preferred for mixed text and formulas; RapidLaTeXOCR is a "
    "lightweight formula-only fallback. If GPU runtime packages are installed, "
    "the Python backend can use them outside the application process."))

(define (ocr-insert-message message)
  (insert (generic->texmacs message "markdown-snippet")))

(define-public (ocr-result->texmacs result format)
  (cond ((== format "latex")
         (latex->texmacs (parse-latex result)))
        ((== format "markdown")
         (generic->texmacs result "markdown-snippet"))
        (else
         (generic->texmacs result "verbatim"))))

(define (ocr-insert-result result format)
  (if (== (tm-string-trim-both (force-string result)) "")
      (ocr-insert-message "OCR produced no text.")
      (insert (ocr-result->texmacs result format))))

(define (get-image t i bool)
  (let* ((cur-t (tree-ref t i)))
    (cond 
      ((not cur-t) #f)
      ((tree-is? cur-t 'image) (get-image-tuple cur-t 0 bool))
      (else (get-image t (+ i 1) bool)))))

(define (get-image-tuple t i bool)
  (if bool
    (let* ((cur-t (tree-ref t i)))
      (cond 
        ((not cur-t) #f)
        ((tree-is? cur-t 'tuple) (get-image-name cur-t 0))
        (else (get-image-tuple t (+ i 1)))))
    (let* ((cur-t (tree-ref t i)))
      (cond 
        ((not cur-t) #f)
        ((tree-is? cur-t 'tuple) (get-image-data cur-t 0))
        (else (get-image-tuple t (+ i 1)))))))

(define (get-image-name t i)
  (let* ((cur-t (tree-ref t i)))
    (cond 
      ((not cur-t) #f)
      ((not (string=? (tree->string cur-t) "")) (tree->string cur-t))
      (else (get-image-name t (+ i 1))))))

(define (get-image-data t i)
  (let* ((cur-t (tree-ref t i)))
    (cond 
      ((not cur-t) #f)
      ((tree-is? cur-t 'raw-data) (cdr (tree->stree cur-t)))
      (else (get-image-name t (+ i 1))))))

(define (get-image-extension name)
  (let* ((parts (string-split name #\.)))
    (if (> (length parts) 1)
        (last parts)
        name)))

(define (insert-tips)
  (go-to (cursor-path))
  (go-to-next-node)
  (kbd-return)
  (ocr-insert-message (ocr-missing-provider-message)))

(define (insert-latex-by-cursor)
  (insert-tips))

(define (ocr-save-image-to-temp t)
  (let* ((image-name (get-image t 0 #t))
         (extension (if image-name (get-image-extension image-name) "png"))
         (temp-name (string-append temp-dir "/temp-" (number->string (current-time)) "." extension))
         (data-list (get-image t 0 #f)))
    (if (and (list? data-list) (not (null? data-list)))
        (let* ((base64-str (car data-list))
               (binary-data (decode-base64 base64-str)))
          (string-save binary-data temp-name)
          (display* "Image has saved to " temp-name "\n")
          temp-name)
        #f)))

(define-public (ocr-image->result image-path formula?)
  (let* ((provider (ocr-select-provider formula?)))
    (if provider
        (list provider
              (ocr-provider-format provider formula?)
              (ocr-run-provider provider image-path formula?))
        (list #f "markdown" (ocr-missing-provider-message)))))

#|
ocr-to-latex-by-cursor
将图像识别为LaTeX并在当前光标处插入

语法
----
(ocr-to-latex-by-cursor t)

参数
----
t: tree
图像的tree表示，且该图像并不在文档中

返回值
------
无返回值，有副作用。会在文档中插入LaTeX代码片段。

逻辑
----
1. 光标在数学模式中，会直接插入数学公式
2. 光标在文本模式中，插入图片对应的LaTeX代码片段
|#
(tm-define (ocr-to-latex-by-cursor t)
  (let* ((image-path (ocr-save-image-to-temp t))
         (formula? (== (get-env "mode") "math")))
    (if image-path
        (let* ((ocr-result (ocr-image->result image-path formula?))
               (format (cadr ocr-result))
               (result (caddr ocr-result)))
          (ocr-insert-result result format))
        (ocr-insert-message "No image data found in clipboard."))))

; (get-image-extension (get-image t 0 #t)) 获取文件后缀，创建对应临时文件
; (get-image t 0 #f) 获取 raw-data

#|
ocr-to-latex-by-image
将图像识别为LaTeX并在图像下方插入

语法
----
(ocr-to-latex-by-image t)

参数
----
t: tree
图像的tree表示且该图像已经在文档中

返回值
------
无返回值，有副作用。会在文档中插入LaTeX代码片段。

逻辑
----
1. 光标在数学模式中，会直接插入数学公式
2. 光标在文本模式中，插入图片对应的LaTeX代码片段
|#
(tm-define (ocr-to-latex-by-image t)
  (let* ((image-path (ocr-save-image-to-temp t))
         (formula? (== (get-env "mode") "math")))
    (tree-go-to t :end)
    (kbd-return)
    (if image-path
        (let* ((ocr-result (ocr-image->result image-path formula?))
               (format (cadr ocr-result))
               (result (caddr ocr-result)))
          (ocr-insert-result result format))
        (ocr-insert-message "No image data found in clipboard."))))
