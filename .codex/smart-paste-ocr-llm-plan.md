# Smart Paste, OCR, and LLM Chat Plan

## Goal

Enable reliable smart paste, add local OCR support, and keep the LLM chat box focused on API-based chat integration.

Do not introduce opencode, Claude Code, Codex SDK, or a full agent runner in the first phase.

## Current Understanding

- `Ctrl+Shift+V` / `Magic paste` already exists through `kbd-magic-paste`.
- Text-mode magic paste already routes to `smart-format-paste`.
- Existing paste logic can detect plain text, Markdown, LaTeX, HTML, internal TeXmacs snippets, and images.
- The LLM chat box is separate from smart paste. It uses `texmacs_input_widget` backed by `tmfs://chat/{session}/message` and `tmfs://chat/{session}/input`.
- LLM chat should use a CJK-capable default font for display/input, not the document editor's Euler New Roman default.

## Phase 1: Smart Paste

### Scope

Make smart paste deterministic and robust before adding OCR or LLM involvement.

### Behavior

Paste format priority:

1. Image
2. TeXmacs internal format
3. HTML
4. Markdown
5. LaTeX
6. Plain text

### Tasks

1. Trace and clean up the `kbd-magic-paste` path.
2. Fix format detection so HTML and plain text are not mixed incorrectly.
3. When clipboard contains HTML:
   - Read HTML content for HTML validation.
   - Read plain text as fallback.
   - Use HTML converter only when the HTML is valid enough.
4. Keep normal paste behavior unchanged.
5. Keep `Paste special` as the manual override path.
6. Add or update focused tests for:
   - Plain text
   - Markdown
   - LaTeX
   - HTML
   - Image with mocked OCR provider

## Phase 2: OCR

### Recommendation

Use local OCR providers. Avoid cloud OCR as the default.

### Provider Model

```text
ocr.provider = pix2text | pp-formulanet-s | pp-formulanet-l | rapidlatexocr | none
ocr.mode     = auto | text | formula
ocr.output   = texmacs | markdown | latex | plain
```

### Default Provider

Use `Pix2Text` as the default provider.

Reasons:

- Good fit for screenshot-to-document workflows.
- Supports text, layout, tables, and mathematical formulas.
- Can output Markdown/LaTeX that can be converted into TeXmacs.
- Practical local alternative to Mathpix-style workflows.

### Formula OCR Providers

Use formula-specific providers as optional backends:

- `PP-FormulaNet-S`: faster formula OCR.
- `PP-FormulaNet-L`: higher-accuracy formula OCR.
- `RapidLaTeXOCR`: lighter ONNXRuntime-based formula fallback.

### OCR Actions

Expose separate user actions:

- Smart OCR Paste: image to mixed Markdown/LaTeX/TeXmacs via Pix2Text.
- Formula OCR Paste: image to LaTeX via PP-FormulaNet or RapidLaTeXOCR.
- Image Only: insert the image without OCR.

### Tasks

1. Add OCR provider preferences.
2. Implement a single provider interface:

```text
image path -> provider -> normalized OCR result -> TeXmacs insertion
```

3. Implement `Pix2Text` first.
4. Implement `RapidLaTeXOCR` second as a low-dependency formula fallback.
5. Implement `PP-FormulaNet-S/L` after the base flow is stable.
6. Add clear error handling:
   - Provider not installed
   - Model missing
   - Timeout
   - Invalid output
7. If OCR fails, fall back to inserting the image.

## Phase 3: LLM Chat API

### Scope

Use direct chat API adapters only. Do not add agent SDKs in the first phase.

### Recommended First Provider

Implement `openai-compatible` first.

This covers:

- OpenAI
- DeepSeek
- Kimi
- Qwen
- OpenRouter
- SiliconFlow
- local vLLM/Ollama OpenAI-compatible endpoints

### Optional Second Provider

Add `anthropic` later if native Claude API behavior is needed.

### Configuration

```text
llm.provider
llm.api_base
llm.api_key
llm.model
llm.temperature
llm.stream
llm.timeout
```

### Tasks

1. Keep the existing chat UI and chat buffer model.
2. Add an API adapter layer for chat completion.
3. Stream text deltas back into the existing chat output path.
4. Keep API responses as UTF-8 at the boundary.
5. Convert to TeXmacs internal text exactly once before inserting into trees.
6. Review `reasoning-delta` handling to avoid unnecessary `cork->utf8` / `utf8->cork` round trips.
7. Add error messages for:
   - Missing API key
   - Invalid model
   - Network failure
   - Rate limit
   - Timeout
8. Add a small settings UI:
   - Provider
   - Base URL
   - API key
   - Model
   - Test connection

## Not In Scope For Phase 1

Do not implement these yet:

- opencode runner
- Claude Code SDK
- Codex SDK
- Agent loop
- Tool execution
- File-editing agent workflow

Reason: the current goal is smart paste, local OCR, and basic API chat. Agent runtimes add process lifecycle, permissions, state synchronization, and UI complexity that should be handled as a separate project.

## Suggested Implementation Order

1. Stabilize smart paste format detection.
2. Add OCR provider abstraction.
3. Implement Pix2Text.
4. Add Formula OCR through RapidLaTeXOCR.
5. Add PP-FormulaNet-S/L.
6. Add OpenAI-compatible LLM chat API.
7. Add settings UI for OCR and LLM.
8. Add tests and provider diagnostics.

## References

- Pix2Text: https://github.com/breezedeus/Pix2Text
- PP-FormulaNet: https://paddlepaddle.github.io/PaddleX/3.1/en/module_usage/tutorials/ocr_modules/formula_recognition.html
- RapidLaTeXOCR: https://github.com/RapidAI/RapidLaTeXOCR
- LaTeX-OCR / pix2tex: https://github.com/lukas-blecher/LaTeX-OCR
- UniMERNet: https://github.com/opendatalab/UniMERNet
