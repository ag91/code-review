---
paths: [code-review-criteria.el, code-review-comment.el]
priority: P2
---

WHEN a criterion is captured THE SYSTEM SHALL submit it through
the first firing strategy of
code-review-criteria-submit-strategies (the strategies spanning:
the author's agent, a push to the author's branch, a suggestion
block for the author to apply, and a local-only write for
criteria directories that are not version controlled), and WHEN
no strategy fires THE SYSTEM SHALL still record the verdict
sidecar.
