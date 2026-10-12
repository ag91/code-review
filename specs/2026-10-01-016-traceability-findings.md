---
paths: [code-review-criteria.el, code-review-section-analysis.el]
priority: P2
---

WHEN a reviewed diff modifies a criteria file whose req id no
test hunk of the diff references THE ANALYSIS SECTION SHALL flag
a strong traceability finding, and WHEN a test hunk references a
req id whose criteria file the diff does not touch THE ANALYSIS
SECTION SHALL flag a hint traceability finding; both with jump
buttons, and never as a signal into the render.
