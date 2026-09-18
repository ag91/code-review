# AGENTS.md

Guidance for AI agents (and humans) working in this repository.

## What this is

`code-review` is a fork of
[`wandersoncferreira/code-review`](https://github.com/wandersoncferreira/code-review):
an Emacs package to review GitHub/GitLab/Bitbucket pull requests in a
magit-section based read/write buffer.  It is *installed and actively
used* from this checkout (`~/.emacs.d/elpa/code-review`), so changes here
affect the user's live Emacs immediately.

`Improvements.org` is the roadmap and engineering log: phase plans,
status (done / design / todo), and hard-won gotchas.  Read it before
changing anything in `code-review-section.el`, and update it whenever a
phase moves forward or a new gotcha is discovered.

## Layout

| File | Role |
|---|---|
| `code-review.el` | entrypoints, `code-review-mode`, transient menus |
| `code-review-section.el` | the big one: section rendering, diff wash, classification engine (~3.1k lines) |
| `code-review-db.el` | sqlite persistence via closql (singleton db) |
| `code-review-github.el` / `-gitlab.el` / `-bitbucket.el` | forge backends |
| `code-review-repo.el`, `code-review-comment.el`, `code-review-actions.el`, `code-review-utils.el`, `code-review-faces.el`, `code-review-parse-hunk.el`, `code-review-interfaces.el` | support |
| `test/` | ERT tests (`make test`) |

## Build and test

Dependencies are ordinary package.el packages (installed in
`~/.emacs.d/elpa`, or via `make deps` on a fresh machine).

```sh
make compile   # byte-compile all package files
make test      # ERT suite in batch emacs (isolated sqlite per test)
make deps      # install dependencies from MELPA (fresh machines / CI)
```

There is no cask/buttercup anymore: tests are plain ERT, run by
`test/run-tests.el` in a batch `emacs -Q`.

### Test conventions

- DB-touching tests must run inside `code-review-test--with-db`
  (see `test/code-review-test-helpers.el`); it swaps the closql
  singleton onto a fresh `/tmp` sqlite file so the user's real
  database is never touched.
- Section-rendering assertions use
  `code-review-test--sections-match` in the same helpers file.
- ERT test names use `file-name/description` (slash form) so
  `ert-run-tests-batch` output and selectors stay readable.

## Verification protocol (follow when changing any source file)

1. Byte-compile every touched file: `make compile` (or in the user's
   daemon — it has all deps loaded).
2. **Purge stale native-comp caches** after changing `.el` sources:
   `rm -f ~/.emacs.d/eln-cache/*/code-review-*.eln`.  Emacs will not
   recompile on its own and stale `.eln` files silently mask your
   changes.  This has bitten repeatedly.
3. Reload all package files in the running daemon via `emacsclient --eval`
   (`(load "code-review-section")` etc.), then render a fresh test buffer
   and check section counts (files/hunks/painted lines/folded/bots) plus
   a re-render stability check on the user's existing review buffer.
4. In a *live* daemon: `defun` removal does not unbind the old function
   (`fmakunbound` it), and re-loading a `defcustom`/`defvar` does not
   update its value (set it explicitly when testing new defaults).
5. The user reviews and commits their own work; leave changes staged-in-
   working-tree only, never commit unless asked.

## Gotchas specific to this codebase

- `magit-wash-sequence` loops `(while (and (not (eobp)) (funcall function)))`
  — a washer that returns nil stops the whole wash.  Section inserters
  wrapped into the wash pipeline must return the section.
- magit 4.x paints hunk faces lazily via `magit-section-paint` from the
  highlight machinery, which never runs in code-review buffers; we paint
  eagerly (`code-review--magit-diff-paint-hunk`).
- The dual-role comment classes (`code-review-base-comment-section` /
  `code-review-comment-section` are used both as sections and as data
  objects) need the `magit-section-ident-value` methods to delegate
  through `value` — magit's visibility cache keys on them.
- The db is an `eieio-singleton`: once connected, changing
  `code-review-db-database-file` has no effect until the singleton and
  its connection are reset (see the test helper).
- Two `:override` advices on magit-diff internals remain (phase 11b in
  `Improvements.org` plans to replace them with an owned wash pipeline).
  They are the known fragility point across magit upgrades.
