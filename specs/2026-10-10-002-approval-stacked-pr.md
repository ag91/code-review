---
paths: [code-review-criteria-regression.el]
---

WHEN the reviewer approves an explored regression draft THE
SYSTEM SHALL branch `review/req-<id>' off the PR head, commit
the criteria file and the drafted tests with `Invariant-Ref:'
and `Regression-Test:' trailers, and on a GitHub PR open a
STACKED PR whose base is the contributor's PR branch (the
contributor decides; never the reviewer's push to the
contributor's branch, never main) — while a local review ships
L0-only, staging the criteria file and test-matching files
rather than a blanket add on the user's own working tree.
