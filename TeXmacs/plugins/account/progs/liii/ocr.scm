
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
(import (liii path))

(define temp-dir (os-temp-dir))
(define ocr-default-languages "en,ch_sim")
(define ocr-temp-counter 0)
(define ocr-image-placeholder-prefix "MOGAN_OCR_IMAGE:")

(define-preferences
  ("ocr.provider" "auto" noop)
  ("ocr.languages" ocr-default-languages noop))

(define-public (ocr-command-available? cmd)
  (and (string? cmd) (url-exists-in-path? cmd)))

(define (ocr-command-output cmd)
  (tm-string-trim-both (eval-system cmd)))

(define (ocr-command-error cmd)
  (tm-string-trim-both (check-stderr cmd)))

(define-public (ocr-shell-quote s)
  (string-quote s))

(define (ocr-sh-quote s)
  (string-append "'"
                 (string-replace (force-string s) "'" "'\"'\"'")
                 "'"))

(define (ocr-temp-path prefix suffix)
  (set! ocr-temp-counter (+ ocr-temp-counter 1))
  (string-append temp-dir
                 "/"
                 prefix
                 "-"
                 (number->string (getpid))
                 "-"
                 (number->string (current-time))
                 "-"
                 (number->string ocr-temp-counter)
                 suffix))

(define (ocr-shell-command cmd)
  (let ((script-path (ocr-temp-path "mogan-ocr" ".sh")))
    (string-save (string-append "#!/bin/sh\n" cmd "\n") script-path)
    (string-append "sh " script-path)))

(define (ocr-python-command script)
  (ocr-shell-command
    (string-append "python -c " (ocr-sh-quote script))))

(define (ocr-path-parent-name path)
  (let* ((path* (force-string path))
         (len (string-length path*))
         (end (if (and (> len 1)
                       (char=? (string-ref path* (- len 1)) #\/))
                  (- len 1)
                  len)))
    (let loop ((i (- end 1)))
      (cond ((<= i 0)
             (if (and (> end 0) (char=? (string-ref path* 0) #\/))
                 "/"
                 "."))
            ((char=? (string-ref path* i) #\/)
             (substring path* 0 i))
            (else (loop (- i 1)))))))

(define (ocr-existing-path paths)
  (cond ((null? paths) "")
        ((file-exists? (car paths)) (car paths))
        (else (ocr-existing-path (cdr paths)))))

(define (ocr-tool-python-path tool)
  (let* ((path-dirs (string-decompose (getenv "PATH" "") ":"))
         (candidates (map (lambda (dir) (path-join dir tool)) path-dirs))
         (tool-path (ocr-existing-path candidates))
         (script (if (file-exists? tool-path)
                     (string-load tool-path)
                     ""))
         (lines (string-split script #\newline))
         (first-line (if (null? lines) "" (car lines))))
    (if (string-starts? first-line "#!")
        (string-drop first-line 2)
        "")))

(define (ocr-tool-python-command tool script)
  (let ((tool-python (ocr-tool-python-path tool)))
    (if (== tool-python "")
        ""
        (string-append (ocr-shell-quote tool-python)
                       " -c "
                       (ocr-sh-quote script)))))

(define (ocr-tool-python-command-with-args tool script args)
  (let ((command (ocr-tool-python-command tool script)))
    (if (== command "")
        ""
        (string-append command
                       " "
                       (string-recompose (map ocr-sh-quote args) " ")))))

(define-public (ocr-tool-python-site-library tool)
  (let* ((tool-python (ocr-tool-python-path tool))
         (venv-root (if (== tool-python "")
                        ""
                        (ocr-path-parent-name
                          (ocr-path-parent-name tool-python))))
         (lib-root (if (== venv-root "") "" (path-join venv-root "lib")))
         (entries (if (file-exists? lib-root)
                      (vector->list (path-list lib-root))
                      '()))
         (python-dirs (list-filter entries
                        (lambda (entry)
                          (string-starts? entry "python"))))
         (site-dirs (list-filter
                      (map (lambda (entry)
                             (path-join lib-root entry "site-packages"))
                           python-dirs)
                      file-exists?)))
    (if (null? site-dirs) "" (car site-dirs))))

(define-public (ocr-tool-library-path tool)
  (let ((site-library (ocr-tool-python-site-library tool)))
    (if (or (== site-library "")
            (not (file-exists? (string-append site-library "/nvidia"))))
        ""
        (let* ((nvidia-root (string-append site-library "/nvidia"))
               (entries (vector->list (path-list nvidia-root)))
               (lib-dirs (map (lambda (entry)
                                (path-join nvidia-root entry "lib"))
                              entries))
               (existing (list-filter lib-dirs file-exists?)))
          (string-recompose existing ":")))))

(define (ocr-command-with-tool-libraries tool command)
  (let ((library-path (ocr-tool-library-path tool)))
    (if (== library-path "")
        command
        (string-append "LD_LIBRARY_PATH="
                       (ocr-sh-quote
                         (if (== (getenv "LD_LIBRARY_PATH" "") "")
                             library-path
                             (string-append library-path ":"
                                            (getenv "LD_LIBRARY_PATH" ""))))
                       " "
                       command))))

(define (ocr-python-string s)
  (string-append "\"" s "\""))

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
               "print(('no','yes')[__import__('torch').cuda.is_available()])"))
           "yes")))

(define (ocr-onnx-cuda-available-with command-maker)
  (and (== (ocr-command-output
             (command-maker
               (string-append
                 "print(('no','yes')[__import__('onnxruntime')."
                 "get_available_providers().__contains__('CUDAExecutionProvider')])")))
           "yes")))

(define-public (ocr-gpu-available?)
  (or (ocr-onnx-cuda-available-with ocr-python-command)
      (and (ocr-python-module-available? "torch")
           (ocr-python-cuda-available-with ocr-python-command))))

(define-public (ocr-pix2text-gpu-available?)
  (ocr-onnx-cuda-available-with
    (lambda (script)
      (ocr-shell-command
        (ocr-command-with-tool-libraries
          "p2t"
          (ocr-tool-python-command "p2t" script))))))

(define-public (ocr-easyocr-available?)
  (and (ocr-command-available? "p2t")
       (== (ocr-command-output
             (ocr-shell-command
               (ocr-tool-python-command
                 "p2t"
                 (string-append "import importlib.util; "
                                "print('yes' if importlib.util.find_spec("
                                (ocr-python-string "easyocr")
                                ") else 'no')"))))
           "yes")))

(define-public (ocr-paddleocr-available?)
  (and (ocr-command-available? "paddleocr")
       (== (ocr-command-output
             (ocr-shell-command
               (ocr-tool-python-command
                 "paddleocr"
                 (string-append "import importlib.util; "
                                "print('yes' if importlib.util.find_spec("
                                (ocr-python-string "paddleocr")
                                ") and importlib.util.find_spec("
                                (ocr-python-string "paddle")
                                ") else 'no')"))))
           "yes")))

(define (ocr-easyocr-gpu-available?)
  (and (ocr-easyocr-available?)
       (ocr-python-cuda-available-with
         (lambda (script)
           (ocr-shell-command
             (ocr-command-with-tool-libraries
               "p2t"
               (ocr-tool-python-command "p2t" script)))))))

(define (ocr-pix2text-device)
  (if (ocr-pix2text-gpu-available?) "gpu" "cpu"))

(define-public (ocr-available-providers)
  (let ((providers '()))
    (when (ocr-paddleocr-available?)
      (set! providers (cons "paddleocr" providers)))
    (when (ocr-easyocr-available?)
      (set! providers (cons "easyocr" providers)))
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
          ((and (== preferred "paddleocr") (in? "paddleocr" available))
           "paddleocr")
          ((and (== preferred "easyocr") (in? "easyocr" available))
           "easyocr")
          ((and (== preferred "rapid-latex-ocr")
                (in? "rapid-latex-ocr" available))
           "rapid-latex-ocr")
          ((and formula? (in? "rapid-latex-ocr" available))
           "rapid-latex-ocr")
          ((and (not formula?) (in? "paddleocr" available))
           "paddleocr")
          ((and (not formula?) (in? "easyocr" available))
           "easyocr")
          ((in? "pix2text" available)
           "pix2text")
          ((in? "rapid-latex-ocr" available)
           "rapid-latex-ocr")
          (else #f))))

(define-public (ocr-provider-format provider formula?)
  (cond ((== provider "pix2text")
         (if formula? "latex" "markdown"))
        ((== provider "paddleocr") "markdown")
        ((== provider "easyocr") "markdown")
        ((== provider "rapid-latex-ocr") "latex")
        (else "verbatim")))

(define (ocr-pix2text-command image-path formula?)
  (ocr-shell-command
    (ocr-command-with-tool-libraries
      "p2t"
      (string-append "p2t predict -l "
                     (ocr-sh-quote (get-preference "ocr.languages"))
                     " --device "
                     (ocr-pix2text-device)
                     " --file-type "
                     (if formula? "formula" "text_formula")
                     " -i "
                     (ocr-sh-quote image-path)
                     " 2>&1"))))

(define (ocr-run-command-to-file command)
  (let* ((output-path (ocr-temp-path "mogan-ocr-output" ".txt"))
         (command*
           (ocr-shell-command
             (string-append command
                            " > "
                            (ocr-sh-quote output-path)
                            " 2>&1"))))
    (os-call command*)
    (if (file-exists? output-path)
        (string-load output-path)
        "")))

(define (ocr-run-pix2text image-path formula?)
  (ocr-run-command-to-file
    (ocr-command-with-tool-libraries
      "p2t"
      (string-append "p2t predict -l "
                     (ocr-sh-quote (get-preference "ocr.languages"))
                     " --device "
                     (ocr-pix2text-device)
                     " --file-type "
                     (if formula? "formula" "text_formula")
                     " -i "
                     (ocr-sh-quote image-path)))))

(define (ocr-rapidlatex-command image-path)
  (string-append "rapid_latex_ocr " (ocr-sh-quote image-path)))

(define (ocr-run-rapidlatex image-path)
  (ocr-run-command-to-file (ocr-rapidlatex-command image-path)))

(define (ocr-easyocr-command image-path)
  (let ((script
          (string-append
            "import easyocr, re, sys\n"
            "COMMON = set('the of and to in is as that with for an a this which are be by from on or we use if true fixed unknown policy algorithm problem provided prior set input denoted produces series agent policies term represent optimal reward regret function defined collaborates across episodes instead however exactly subset potential partner'.split())\n"
            "def keep_line(line):\n"
            "    words = [w.lower().strip(\"'\") for w in re.findall(r\"[A-Za-z][A-Za-z']+\", line)]\n"
            "    if len(line.strip()) < 12:\n"
            "        return False\n"
            "    hits = sum(1 for w in words if w in COMMON)\n"
            "    return hits >= 1 and len(words) >= 2\n"
            "def to_lines(result):\n"
            "    items = []\n"
            "    for box, text, conf in result:\n"
            "        text = text.strip()\n"
            "        if not text:\n"
            "            continue\n"
            "        xs = [p[0] for p in box]\n"
            "        ys = [p[1] for p in box]\n"
            "        items.append({'x': min(xs), 'y': sum(ys) / len(ys), 'h': max(1, max(ys) - min(ys)), 'text': text})\n"
            "    items.sort(key=lambda it: (it['y'], it['x']))\n"
            "    groups = []\n"
            "    for it in items:\n"
            "        if groups and abs(it['y'] - groups[-1]['y']) <= max(12, min(28, groups[-1]['h'] * 0.8)):\n"
            "            g = groups[-1]\n"
            "            g['items'].append(it)\n"
            "            g['y'] = sum(x['y'] for x in g['items']) / len(g['items'])\n"
            "            g['h'] = max(g['h'], it['h'])\n"
            "        else:\n"
            "            groups.append({'y': it['y'], 'h': it['h'], 'items': [it]})\n"
            "    lines = []\n"
            "    for g in groups:\n"
            "        line = ' '.join(it['text'] for it in sorted(g['items'], key=lambda it: it['x']))\n"
            "        if keep_line(line):\n"
            "            lines.append(line)\n"
            "    return lines\n"
            "reader = easyocr.Reader(['en'], gpu="
            (if (ocr-easyocr-gpu-available?) "True" "False")
            ", verbose=False)\n"
            "result = reader.readtext(sys.argv[1], detail=1, paragraph=False)\n"
            "print('\\n'.join(to_lines(result)))")))
    (ocr-command-with-tool-libraries
      "p2t"
      (ocr-tool-python-command-with-args "p2t" script (list image-path)))))

(define (ocr-run-easyocr image-path)
  (ocr-run-command-to-file (ocr-easyocr-command image-path)))

(define (ocr-paddleocr-command image-path)
  (let ((script
          (string-append
            "import os, sys, tempfile\n"
            "os.environ.setdefault('PADDLE_PDX_DISABLE_MODEL_SOURCE_CHECK', 'True')\n"
            "from paddleocr import PPStructureV3\n"
            "try:\n"
            "    from PIL import Image\n"
            "except Exception:\n"
            "    Image = None\n"
            "try:\n"
            "    import paddle\n"
            "    device = 'gpu' if paddle.device.is_compiled_with_cuda() else 'cpu'\n"
            "except Exception:\n"
            "    device = 'cpu'\n"
            "def field(obj, name, default=None):\n"
            "    value = default\n"
            "    getter = getattr(obj, 'get', None)\n"
            "    if callable(getter):\n"
            "        try:\n"
            "            value = getter(name)\n"
            "        except Exception:\n"
            "            value = default\n"
            "    if value is None or value is default:\n"
            "        value = getattr(obj, name, default)\n"
            "    return value\n"
            "def strip_formula_delimiters(text):\n"
            "    text = str(text or '').strip()\n"
            "    if text.startswith('$$') and text.endswith('$$'):\n"
            "        return text[2:-2].strip()\n"
            "    if text.startswith('$') and text.endswith('$'):\n"
            "        return text[1:-1].strip()\n"
            "    return text\n"
            "def bbox_of(block):\n"
            "    bbox = field(block, 'block_bbox') or field(block, 'bbox')\n"
            "    if not bbox or len(bbox) < 4:\n"
            "        return None\n"
            "    try:\n"
            "        return [int(float(x)) for x in bbox[:4]]\n"
            "    except Exception:\n"
            "        return None\n"
            "def braces_balanced(s):\n"
            "    depth = 0\n"
            "    escape = False\n"
            "    for ch in s:\n"
            "        if escape:\n"
            "            escape = False\n"
            "            continue\n"
            "        if ch == '\\\\':\n"
            "            escape = True\n"
            "        elif ch == '{':\n"
            "            depth += 1\n"
            "        elif ch == '}':\n"
            "            depth -= 1\n"
            "            if depth < 0:\n"
            "                return False\n"
            "    return depth == 0 and not escape\n"
            "def skip_ws(s, i):\n"
            "    while i < len(s) and s[i].isspace():\n"
            "        i += 1\n"
            "    return i\n"
            "def parse_group(s, i):\n"
            "    i = skip_ws(s, i)\n"
            "    if i >= len(s) or s[i] != '{':\n"
            "        return None, i\n"
            "    start = i + 1\n"
            "    depth = 0\n"
            "    i += 1\n"
            "    while i < len(s):\n"
            "        ch = s[i]\n"
            "        if ch == '\\\\':\n"
            "            i += 2\n"
            "            continue\n"
            "        if ch == '{':\n"
            "            depth += 1\n"
            "        elif ch == '}':\n"
            "            if depth == 0:\n"
            "                return s[start:i], i + 1\n"
            "            depth -= 1\n"
            "        i += 1\n"
            "    return None, i\n"
            "def frac_groups_ok(s):\n"
            "    i = 0\n"
            "    needle = '\\\\frac'\n"
            "    while True:\n"
            "        pos = s.find(needle, i)\n"
            "        if pos < 0:\n"
            "            return True\n"
            "        num, j = parse_group(s, pos + len(needle))\n"
            "        if num is None or num.strip() == '':\n"
            "            return False\n"
            "        den, j = parse_group(s, j)\n"
            "        if den is None or den.strip() == '':\n"
            "            return False\n"
            "        if not frac_groups_ok(num) or not frac_groups_ok(den):\n"
            "            return False\n"
            "        i = j\n"
            "def formula_looks_valid(text, bbox=None):\n"
            "    formula = strip_formula_delimiters(text)\n"
            "    if not formula or formula.endswith('\\\\'):\n"
            "        return False\n"
            "    if not braces_balanced(formula) or not frac_groups_ok(formula):\n"
            "        return False\n"
            "    if bbox:\n"
            "        width = max(1, bbox[2] - bbox[0])\n"
            "        height = max(1, bbox[3] - bbox[1])\n"
            "        if len(formula) > max(240, int(width * height / 80)):\n"
            "            return False\n"
            "        if formula.count('\\\\frac') > max(8, int(width / 40)):\n"
            "            return False\n"
            "    return True\n"
            "def safe_math(text, double):\n"
            "    body = strip_formula_delimiters(text)\n"
            "    if formula_looks_valid(body):\n"
            "        return ('$$' if double else '$') + body + ('$$' if double else '$')\n"
            "    return (r'\\$\\$' if double else r'\\$') + body + (r'\\$\\$' if double else r'\\$')\n"
            "def sanitize_markdown_math(text):\n"
            "    text = str(text or '')\n"
            "    out = []\n"
            "    i = 0\n"
            "    while i < len(text):\n"
            "        if text.startswith('$$', i):\n"
            "            j = text.find('$$', i + 2)\n"
            "            if j < 0:\n"
            "                out.append(text[i:])\n"
            "                break\n"
            "            out.append(safe_math(text[i + 2:j], True))\n"
            "            i = j + 2\n"
            "        elif text[i] == '$':\n"
            "            j = text.find('$', i + 1)\n"
            "            if j < 0:\n"
            "                out.append(text[i])\n"
            "                i += 1\n"
            "            else:\n"
            "                out.append(safe_math(text[i + 1:j], False))\n"
            "                i = j + 1\n"
            "        else:\n"
            "            out.append(text[i])\n"
            "            i += 1\n"
            "    return ''.join(out)\n"
            "def content_has_math(text):\n"
            "    text = str(text or '')\n"
            "    if '$' in text or '\\\\' in text:\n"
            "        return True\n"
            "    math_chars = set('∫∑∏√≤≥≠≈∞∈∉⊂⊆∪∩×÷±∂∇θλμπσωΩαβγ')\n"
            "    return any(ch in math_chars for ch in text)\n"
            "source_image = None\n"
            "def crop_block(image_path, bbox):\n"
            "    global source_image\n"
            "    if Image is None or not bbox:\n"
            "        return None\n"
            "    try:\n"
            "        if source_image is None:\n"
            "            source_image = Image.open(image_path).convert('RGB')\n"
            "        width, height = source_image.size\n"
            "        x1, y1, x2, y2 = bbox\n"
            "        pad = 4\n"
            "        x1 = max(0, x1 - pad); y1 = max(0, y1 - pad)\n"
            "        x2 = min(width, x2 + pad); y2 = min(height, y2 + pad)\n"
            "        if x2 <= x1 or y2 <= y1:\n"
            "            return None\n"
            "        out = tempfile.NamedTemporaryFile(prefix='mogan-ocr-formula-', suffix='.png', delete=False)\n"
            "        out.close()\n"
            "        source_image.crop((x1, y1, x2, y2)).save(out.name)\n"
            "        return out.name, max(1, x2 - x1)\n"
            "    except Exception:\n"
            "        return None\n"
            "def image_placeholder(crop):\n"
            "    if not crop:\n"
            "        return None\n"
            "    path, width = crop\n"
            "    return 'MOGAN_OCR_IMAGE:' + path + '\\t' + str(width) + 'px'\n"
            "def block_sort_key(item):\n"
            "    index, block = item\n"
            "    bbox = bbox_of(block) or [0, 0, 0, 0]\n"
            "    order = field(block, 'block_order')\n"
            "    if order is None:\n"
            "        order = field(block, 'index', index)\n"
            "    try:\n"
            "        order = int(order)\n"
            "    except Exception:\n"
            "        order = index\n"
            "    return (order, bbox[1], bbox[0])\n"
            "def block_to_markdown(image_path, block):\n"
            "    label = field(block, 'block_label')\n"
            "    if label is None:\n"
            "        label = field(block, 'label', '')\n"
            "    label = str(label or '').lower()\n"
            "    content = field(block, 'block_content')\n"
            "    if content is None:\n"
            "        content = field(block, 'content', '')\n"
            "    content = str(content or '').strip()\n"
            "    if not content:\n"
            "        return ''\n"
            "    if label in ('chart', 'figure', 'image', 'table', 'seal', 'stamp'):\n"
            "        return ''\n"
            "    bbox = bbox_of(block)\n"
            "    if label == 'formula':\n"
            "        formula = strip_formula_delimiters(content)\n"
            "        if formula and formula_looks_valid(formula, bbox):\n"
            "            return '$$' + formula + '$$'\n"
            "        crop = crop_block(image_path, bbox)\n"
            "        if crop:\n"
            "            return image_placeholder(crop)\n"
            "        return '`' + content.replace('`', \"'\") + '`'\n"
            "    return sanitize_markdown_math(content)\n"
            "def result_to_markdown(image_path, res):\n"
            "    blocks = field(res, 'parsing_res_list') or []\n"
            "    parts = []\n"
            "    if blocks:\n"
            "        for _, block in sorted(enumerate(blocks), key=block_sort_key):\n"
            "            part = block_to_markdown(image_path, block).strip()\n"
            "            if part:\n"
            "                parts.append(part)\n"
            "        return '\\n\\n'.join(parts)\n"
            "    md = field(res, 'markdown')\n"
            "    if isinstance(md, dict):\n"
            "        text = md.get('markdown_texts') or md.get('markdown_text') or ''\n"
            "    else:\n"
            "        text = str(md or '')\n"
            "    return sanitize_markdown_math(text.strip())\n"
            "pipeline = PPStructureV3(device=device, use_doc_orientation_classify=False, use_doc_unwarping=False, use_textline_orientation=False)\n"
            "texts = []\n"
            "for res in pipeline.predict(sys.argv[1]):\n"
            "    text = result_to_markdown(sys.argv[1], res)\n"
            "    if text.strip():\n"
            "        texts.append(text.strip())\n"
            "print('MOGAN_OCR_MARKDOWN_BEGIN')\n"
            "print('\\n\\n'.join(texts))")))
    (string-append "PADDLE_PDX_DISABLE_MODEL_SOURCE_CHECK=True "
                   (ocr-tool-python-command-with-args
                     "paddleocr" script (list image-path)))))

(define (ocr-run-paddleocr image-path)
  (ocr-run-command-to-file (ocr-paddleocr-command image-path)))

(define ocr-english-common-words
  '("the" "of" "and" "to" "in" "is" "as" "that" "with" "for" "an" "a"
    "this" "which" "are" "be" "by" "from" "on" "or" "we" "use" "if"
    "true" "fixed" "unknown" "policy" "algorithm" "problem" "provided"
    "prior" "set" "input" "denoted" "produces" "series" "agent"
    "policies" "term" "represent" "optimal" "reward" "regret"
    "function" "defined" "collaborates" "across" "episodes" "instead"
    "however" "exactly" "subset" "potential" "partner"))

(define (ocr-ascii-alpha? c)
  (let ((n (char->integer c)))
    (or (and (>= n (char->integer #\A)) (<= n (char->integer #\Z)))
        (and (>= n (char->integer #\a)) (<= n (char->integer #\z))))))

(define (ocr-line-words line)
  (let* ((chars (string->list (force-string line)))
         (flush (lambda (word words)
                  (if (null? word)
                      words
                      (cons (list->string (reverse word)) words)))))
    (let loop ((rest chars) (word '()) (words '()))
      (cond ((null? rest)
             (reverse (flush word words)))
            ((ocr-ascii-alpha? (car rest))
             (loop (cdr rest) (cons (car rest) word) words))
            (else
             (loop (cdr rest) '() (flush word words)))))))

(define-public (ocr-keep-easyocr-text-line? line)
  (let* ((line* (tm-string-trim-both (force-string line)))
         (words (map string-downcase (ocr-line-words line*)))
         (hits (length (list-filter words
                         (lambda (word)
                           (in? word ocr-english-common-words))))))
    (and (>= (string-length line*) 12)
         (>= (length words) 2)
         (>= hits 1))))

(define (ocr-output-body output)
  (let* ((text (force-string output))
         (markdown-parts
           (string-decompose text "MOGAN_OCR_MARKDOWN_BEGIN"))
         (parts (string-decompose text "Outs:")))
    (if (> (length markdown-parts) 1)
        (string-recompose (cdr markdown-parts) "MOGAN_OCR_MARKDOWN_BEGIN")
        (if (> (length parts) 1)
        (string-recompose (cdr parts) "Outs:")
        text))))

(define-public (ocr-clean-output output)
  (let* ((text (force-string output))
         (trimmed (tm-string-trim-both (ocr-output-body text))))
    (if (> (length (string-decompose text "MOGAN_OCR_MARKDOWN_BEGIN")) 1)
        trimmed
        (let* ((lines (string-split trimmed #\newline))
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
          (tm-string-trim-both (string-recompose useful "\n"))))))

(define (ocr-run-provider provider image-path formula?)
  (ocr-clean-output
    (cond ((== provider "pix2text")
           (ocr-run-pix2text image-path formula?))
          ((== provider "paddleocr")
           ;; PPStructureV3 is tuned for multi-block pages. A single CJK line
           ;; with inline math is often misclassified (e.g. as a "chart") and
           ;; yields no usable blocks; fall back to pix2text, which detects
           ;; inline formulas and handles Chinese text. EasyOCR is English and
           ;; text-only, so it is only a last resort.
           (let ((output (ocr-run-paddleocr image-path)))
             ;; Decide on the cleaned text: a degenerate result still carries
             ;; the MOGAN_OCR_MARKDOWN_BEGIN marker and log lines, so the raw
             ;; output is never literally empty.
             (if (!= (tm-string-trim-both (ocr-clean-output output)) "")
                 output
                 (cond ((ocr-command-available? "p2t")
                        (ocr-run-pix2text image-path formula?))
                       ((ocr-easyocr-available?)
                        (ocr-run-easyocr image-path))
                       (else "")))))
          ((== provider "easyocr")
           (let ((output (ocr-run-easyocr image-path)))
             (if (== (tm-string-trim-both output) "")
                 (ocr-run-pix2text image-path formula?)
                 output)))
          ((== provider "rapid-latex-ocr")
           (ocr-run-rapidlatex image-path))
          (else ""))))

(define (ocr-missing-provider-message)
  (string-append
    "OCR backend not found.\n\n"
    "Install one local backend and try smart paste again:\n\n"
    "- PaddleOCR: pip install paddleocr paddlepaddle\n"
    "- Pix2Text: pip install pix2text\n"
    "- EasyOCR: pip install easyocr\n"
    "- RapidLaTeXOCR: pip install rapid_latex_ocr\n\n"
    "PaddleOCR is preferred for structured screenshots, Pix2Text for mixed text and formulas; RapidLaTeXOCR is a "
    "lightweight formula-only fallback. If GPU runtime packages are installed, "
    "the Python backend can use them outside the application process."))

(define (ocr-insert-message message)
  (insert (generic->texmacs message "markdown-snippet")))

(define (ocr-image-placeholder-line? line)
  (string-starts? (tm-string-trim-both (force-string line))
                  ocr-image-placeholder-prefix))

(define (ocr-image-placeholder-path line)
  (let* ((body (string-drop (tm-string-trim-both (force-string line))
                            (string-length ocr-image-placeholder-prefix)))
         (parts (string-split body #\tab)))
    (if (null? parts) body (car parts))))

(define (ocr-image-placeholder-width line)
  (let* ((body (string-drop (tm-string-trim-both (force-string line))
                            (string-length ocr-image-placeholder-prefix)))
         (parts (string-split body #\tab)))
    (if (> (length parts) 1) (cadr parts) "")))

(define (ocr-markdown-lines->texmacs lines)
  (let* ((markdown (tm-string-trim-both (string-recompose lines "\n"))))
    (if (== markdown "")
        #f
        (generic->texmacs markdown "markdown-snippet"))))

(define (ocr-image-node path width)
  (stree->tree `(image ,path ,width "" "" "")))

(define (ocr-structured-markdown->texmacs result)
  (let* ((lines (string-split (force-string result) #\newline)))
    (stree->tree
      `(document
         ,@(let loop ((rest lines) (pending '()) (nodes '()))
             (cond ((null? rest)
                    (let ((node (ocr-markdown-lines->texmacs
                                  (reverse pending))))
                      (reverse (if node
                                   (cons (tree->stree node) nodes)
                                   nodes))))
                   ((ocr-image-placeholder-line? (car rest))
                    (let* ((node (ocr-markdown-lines->texmacs
                                   (reverse pending)))
                           (nodes* (if node
                                       (cons (tree->stree node) nodes)
                                       nodes))
                           (image-path (ocr-image-placeholder-path
                                         (car rest)))
                           (image-width (ocr-image-placeholder-width
                                          (car rest))))
                      (loop (cdr rest)
                            '()
                            (cons (tree->stree
                                    (ocr-image-node image-path image-width))
                                  nodes*))))
                   (else
                    (loop (cdr rest)
                          (cons (car rest) pending)
                          nodes))))))))

(define-public (ocr-result->texmacs result format)
  (cond ((== format "latex")
         (latex->texmacs (parse-latex result)))
        ((== format "markdown")
         (ocr-structured-markdown->texmacs result))
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
