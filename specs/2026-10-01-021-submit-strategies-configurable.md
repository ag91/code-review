---
paths: [code-review-criteria.el, code-review-comment.el]
---

WHEN a criterion is captured with C-c C-t THE SYSTEM SHALL
submit it through the first firing strategy of
code-review-criteria-submit-strategies (agent, push, suggest,
local — the local strategy writing the criteria file and nothing
else, for criteria directories that are not version
controlled), and WHEN no strategy fires THE SYSTEM SHALL still
record the verdict sidecar.
