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
| `code-review-section.el` | section rendering, the owned diff wash, comment/reaction section classes (~2.4k lines) |
| `code-review-diff.el` | diff classification engine: pure functions on raw diff text + file-order/noise rule defcustoms |
| `code-review-reactions.el` | reaction toggle machinery (one engine, three contexts: description/conversation/code-comment) |
| `code-review-local.el` | local diff review (`code-review-review-local-diff`): working tree as a read-only pseudo-PR (state LOCAL) |
| `code-review-db.el` | sqlite persistence via closql (singleton db) |
| `code-review-github.el` / `-gitlab.el` / `-bitbucket.el` | forge backends |
| `code-review-repo.el`, `code-review-comment.el`, `code-review-actions.el`, `code-review-utils.el`, `code-review-faces.el`, `code-review-parse-hunk.el`, `code-review-interfaces.el` | support (`actions.el` also holds the interactive/navigation commands) |
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
  eagerly (`code-review-wash--paint-hunk`).
- The dual-role comment classes (`code-review-base-comment-section` /
  `code-review-comment-section` are used both as sections and as data
  objects) need the `magit-section-ident-value` methods to delegate
  through `value` — magit's visibility cache keys on them.
- The db is an `eieio-singleton`: once connected, changing
  `code-review-db-database-file` has no effect until the singleton and
  its connection are reset (see the test helper).
- `oset` on the closql pullreq objects is memory-only (no per-slot
  write-through).  Persist with an explicit `(closql-insert db obj t)`;
  that is what every `code-review-db--pullreq-*-update` does.
- `magit-git-string` returns the FIRST line only.  For multi-line
  git output (diffs!) use `magit-git-output`.
- The build chain runs in a timer: never rely on ambient
  `default-directory` or on `code-review-db--pullreq-id` surviving
  into `code-review--internal-build` (it re-asserts the id from the
  dispatched obj).
- `(require 'foo)` is a no-op once `foo` is loaded: when reloading in
  the daemon use `(load "foo")` for EVERY touched file, or stale
  definitions (including native-jit subrs) survive and lie to you.
- A long-lived daemon makes ANY "it still works" check unreliable
  after moving functions between files: old defuns survive in
  memory and byte-compile stays quiet behind `declare-function`.
  Prove extracted functions exist with a fresh batch emacs
  (`make test`) — the regression tests in
  `test/code-review-diff-test.el` exist for exactly this reason.
- Never trim the trailing newline of stored diff text
  (`string-trim` on a diff does this): the reorder joins file
  blocks, and a block missing its final newline glues onto the
  next block's `diff --git` header, breaking the wash downstream.
- The diff wash is OWNED (phase 11b): `code-review-wash-diff`,
  `code-review-wash-insert-file-section`, `code-review-wash-hunk`,
  `code-review-wash--paint-hunk` read plain diff text and insert
  magit sections.  Do NOT reintroduce advices on magit-diff
  internals (`magit-diff-wash-diff`, `magit-diff-wash-hunk`,
  `magit-diff-insert-file-section`) — their contracts changed across
  magit 4.x and that is what phase 11b removed.  What we use from
  magit is magit-SECTION (stable): `magit-insert-section`, section
  classes, `magit-wash-sequence`, `magit-section-hide/show`, the
  visibility cache, and optional `magit-section-paint`.
