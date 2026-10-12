---
paths: [code-review-testimpact.el]
priority: P3
---

WHEN the no-test-coverage mapping extracts definitions from
elisp hunks THE MAPPING SHALL drop variable definitions
(defvar, defconst, defcustom, defface) before classification —
configuration and data are not behavior and never need a
covering test.
