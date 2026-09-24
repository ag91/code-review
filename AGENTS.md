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
| `code-review-analysis.el` | phase 5 heuristics: duplicate/dead-code/dangling findings (worktree greps, byte-capped, cached per PR+diff) |
| `code-review-hunkhighlight.el` | phase 10: tree-sitter semantic hunk faces (python first; reusable on any magit diff buffer) |
| `code-review-local.el` | local diff review (`code-review-review-local-diff`): working tree as a read-only pseudo-PR (state LOCAL) |
| `code-review-browse.el` | phase 7: `browse-url` integration — GitHub PR links (with `#diff-`/comment anchors) open inside Emacs; `code-review-open-pr-at-point` finds PR URLs in any buffer (email workflow) |
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
- NEVER run heavy or unbounded compute in the live daemon: the
  phase 5 analysis once froze the user's Emacs for two full minutes
  (git grep on a packed base tree).  Measure anything heavy in
  batch emacs (`make test`), and design every engine so its cost
  is bounded and measurable before it ever reaches the daemon.
- `magit-git-output` / `magit-git-string` run git with
  GIT_LITERAL_PATHSPECS=1: glob-style pathspecs (`:(exclude)*.el`)
  are taken literally and match nothing.  Use plain `call-process`
  when a git call needs glob pathspecs.
- The match-data is GLOBAL and gets clobbered by any nested
  `string-match` (even inside a helper): bind `match-string` results
  to variables BEFORE calling anything that might match.  And
  `match-string` WITHOUT an explicit string argument, when the last
  match was on a STRING, reads the positions against the CURRENT
  BUFFER — a `replace-regexp-in-string` replacement lambda doing
  this spliced HUNK TEXT into a query string it was rewriting.
  Always pass the string explicitly: `(match-string 1 STR)`.
- `?` is a regexp QUANTIFIER: a literal question mark in a regexp
  must be escaped (`#match\\?`, not `#match?` — the unescaped form
  silently made the preceding char optional and never matched,
  which is an easy trap when the literal you search for ENDS in
  `?`).
- cl-loop traps: an `unless` clause placed BEFORE a `for` clause
  does not filter (the body still runs for every iteration — use
  `cl-remove-if` on the list instead); referencing a `_`-prefixed
  destructured `for` variable raises void-variable at runtime —
  name it normally if you use it.
- `git grep -n -F -f probe-file` reports every match of every
  probe and can take minutes on big worktrees; `git grep -l` stops
  at the first match per file and is ~25x faster when you only
  need which files matched.
- When an edit loops (paren surgery, region transplants): stop and
  ask the user instead of iterating — they will fix it faster than
  the loop will.
- treesit gotchas (phase 10): `treesit-query-capture`
  returns `(CAPTURE-NAME . NODE)` pairs (name first!); the query
  predicate spelling is VERSION-EXCLUSIVE: Emacs 30 only supports
  `#match` (no `?`) with the REGEXP FIRST
  (`(#match "\\`test" @capture)`), Emacs 31 only accepts the
  standard `(#match? @capture "\\`test")` (and `#match` does not
  compile there).  The package therefore probes the contract at
  runtime by capture-exercising both spellings
  (`code-review-hunkhighlight--contract`) and rewrites the
  predicates (`--old-style-query`) when running the old contract:
  author queries in the `#match?` form and let the rewrite handle
  30.  Predicates may NOT be attached to one alternative inside
  a `[...]` alternation — write such patterns as separate
  top-level query patterns.  (Structural forms — wrapped or
  unwrapped patterns, sibling captures, list queries — work on
  both; only the predicate spelling differs.)
- treesit predicates: `treesit-query-compile` ACCEPTS predicates
  that are unsupported at RUNTIME (e.g. `#not-match` and `#eq` —
  Emacs 30.2 only supports equal/match/pred at capture time, and
  the error fires from `treesit-query-capture`).  So a compiling query is
  not a valid query: always exercise the capture, and isolate
  per-query captures in a condition-case so one bad query only
  disables itself (see `code-review-hunkhighlight--ranges`).
  Same story for NODE names: e.g. the scala grammar has NO
  `(number)` node (it is `integer_literal` /
  `floating_point_literal`) and compile is happy to accept it;
  the node-type error only surfaces at capture time.  PROBE node
  names with a real capture against a sample before shipping a
  query.
- When rebinding a `defcustom` in the live daemon after changing
  its default, `setq` it to `(eval (car (get 'VAR
  'standard-value)))` — the standard-value cell holds the
  UNevaluated default form, so a plain `(car ...)` installs the
  form instead of the value (the queries list then "works" as an
  alist keyed by `quote` and every language silently loses its
  highlights). A bare `(eval (car ...))` without the `setq` is
  equally broken in the opposite direction: it evaluates the new
  default and DISCARDS it, leaving the stale value bound while
  looking like a fix (this slip cost an hour of phantom
  debugging; the rebind test must print the bound value, not
  just run).
- Face display precedence (phase 10 post-mortem): a text property
  holding a LIST of faces merges with EARLIER faces winning
  attribute conflicts — appending a semantic face after
  `magit-diff-added` is INVISIBLE on screen because that face sets
  its own foreground (#22aa22), yet `get-text-property` happily
  reports the semantic face as present.  Apply semantic faces as
  OVERLAYS (overlay faces override text-property faces; mark them
  with a per-face property for idempotency, `evaporate t`; they
  also survive magit 4.x replacing hunk face properties).  See
  `code-review-hunkhighlight--put-face`.
- `search-forward` does NOT set match data: never read
  `match-beginning`/`match-end` after it, use `point` (this cost
  an hour of phantom test failures once).
- After a daemon restart, package code is only autoloaded:
  `code-review.el` (which defines `code-review-sections-hook`)
  is NOT loaded until an entry command runs, so probing/verifying
  in a restarted daemon requires loading it explicitly.
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
- When transcribing magit internals, copy patterns EXACTLY and
  prove the copy against real input in a fresh batch emacs: the
  11b entry regex dropped magit's optional backref group
  `\\(?:\\(?2:.+?\\) \\2\\)?`, silently matched NO `diff --git`
  line, and every fresh render landed as raw uncolored text for
  three days before anyone noticed.  The regression test
  `code-review-section-test/wash-diff-rename-block` guards it.
- `code-review-wash-hunk` must bind its `match-string` results
  immediately after `looking-at`: the db writes it performs in
  between clobber the global match data (emacsql compiles
  statements with string-matches on a COLD cache, leaving
  string-relative positions), so reading groups afterwards returns
  garbage or signals args-out-of-range.  This sat latent because
  the db ERT tests run early in the suite and warm the statement
  cache — any test whose name sorts before `code-review-db-test`
  hits the wash cold, which is exactly how the browse tests
  exposed it.  `code-review-browse-test/jump-to-file-anchor`
  guards it.
- The dual-role comment classes (`code-review-base-comment-section` /
  `code-review-comment-section` are used both as sections and as data
  objects) need the `magit-section-ident-value` methods to delegate
  through `value` — magit's visibility cache keys on them.  Their
  DATA (author/id/msg) also lives on the VALUE object, not on the
  rendered section: read a comment's databaseId from
  `(oref section value)`, never from the section itself.
- Daemon eval output: `(load FILE)` returns `t`, NOT the file's
  last form's value, and a script that wraps itself in
  `with-output-to-string` swallows its own report when loaded
  inside another one.  Have daemon scripts save their report into
  a defvar and read that variable back from emacsclient.
- Run `check-parens` on every /tmp elisp script BEFORE loading it
  into the daemon (batch emacs + `insert-file-contents` +
  `check-parens`): a surplus closer can silently end a `let` so
  half the script runs at outer scope with void variables, and a
  `cl-labels` whose definitions list closes early leaves its body
  calling a void `walk`.  Note check-parens also catches
  unmatched STRING quotes (a missing closing quote silently
  swallows half the file into one string, which shows up as a
  paren error a screen away from the real typo).
- EIEIO `-p` predicates (e.g. `code-review-base-comment-section-p`)
  are EXACT-CLASS checks, not subclass-aware (`cl-typep` is the
  subclass-aware one): a parent-class predicate returns nil for a
  child instance.  Never dispatch on a parent-class predicate;
  enumerate the concrete classes.  Related trap: the `local?` SLOT
  lies — `code-review-outdated-comment-section` sets `local?` t
  even for comments FETCHED from the forge, so classify by class,
  never by that slot (phase 6).
- In ERT tests, `(cl-letf (((fn) val)) ...)` requires a
  `(setf fn)` expander and signals `void-function ((setf fn))`;
  rebind with `fset` + `unwind-protect` instead (phase 6).
- Never walk ALL sections of ALL live review buffers from a
  verify script: a `magit-map-sections` probe over every open
  buffer hung the daemon for minutes after a class redefining
  reload (user had to C-g).  The minimal reload script (loads +
  keymap patches + report into a defvar) is instant; keep daemon
  probes bounded and targeted (phase 6 incident).
- NEVER filter `make compile` output when you changed a source
  file (phase 10 incident): a `grep ... | head -n 5` hid
  "Error: End of file during parsing" for an unbalanced insert,
  so the STALE `.elc` kept serving old bytecode — batch tests,
  daemon reloads and probes all ran the old code while the
  source looked right, and an hour went into "debugging" a
  feature that was never loaded.  When a test contradicts a
  correct-looking implementation, prove the ARTIFACT is fresh
  first: `ls -la foo.el foo.elc` (mtimes) and grep the `.elc`
  for a symbol only the new source defines.
- Find paren unbalance with a `syntax-ppss` DEPTH-WALK instead of
  hand-counting closers (in the daemon or batch: insert the file,
  `emacs-lisp-mode`, report `(nth 0 (syntax-ppss (line-end-position)))`
  per line): the depth that fails to return to 0 at the defun
  boundary points straight at the missing/extra paren.  Hand
  counting was wrong twice in a row on the same 15-closer line.
- Emacs 31.1 daemon: RE-loading a `.el` source whose path was
  loaded before fails with "End of file during parsing:
  #<killed buffer>" (the reader's buffer dies mid-read); FRESH
  paths and explicit `.elc` loads are fine.  To verify a reload
  took effect, check `(documentation 'fn)` for a marker word
  from the new docstring, not just that the `load` returned `t`.
- "Stack overflow in regexp matcher" on a review render is almost
  always a per-line analysis regexp meeting a MEGABYTE single
  line (Jupyter notebook JSON, minified bundles: 500KB in one
  line; litellm incident, see Improvements.org phase 5).  The
  matcher recurses per character.  Any regexp consuming raw
  lines/diff text must cap the line first
  (`code-review-analysis--cap-line`); use plain `string-search`
  (not a regexp) when the FULL line must be searched.  And
  `code-review-analysis-run` must stay condition-cased: an
  analysis failure logged as "no findings" is fine, one that
  kills the render reads as a forge error ("Got an error from
  your VC provider") and leaves the user bufferless.
- Never drive interactive commands that prompt (`y-or-n-p`,
  completing-read) via `emacsclient --eval` in the user's live
  daemon: the prompt pops in THEIR frame mid-work.  Call the
  underlying machinery with explicit state instead (e.g. set
  `code-review-db--pullreq-id` (a GLOBAL defvar — the last build
  wins it) and let-bind `code-review-section-full-refresh?`
  around `code-review--build-buffer`).
- magit 4.x section accessors are oref-based:
  `magit-section-value` / `magit-section-start` /
  `magit-section-end` are VOID functions; use
  `(oref section value)` etc.  `magit-map-sections` takes
  (FUNCTION &optional SECTION), NOT a buffer — call it inside
  `with-current-buffer` with no second arg.  It RETURNS the
  section it walked from (NOT a list of results) — collect
  results via side effects in the callback.  The semantic
  highlight overlays carry the marker property `cr-hh-face`
  (see `--put-face`), not a package-named property.
- `magit-section-show-level` (magit's own global folding) keys on
  raw NESTING DEPTH, which differs between the real render (root >
  files-report > files-chnged > file > hunk) and the wash-test
  harness (root > files-chnged > file > hunk): depth-based levels
  shift by two between the two.  Fold by section TYPE instead
  (see `code-review-fold-show-level`, phase 9).
- Re-defining a derived mode does NOT update pre-existing buffers:
  after reloading `code-review.el` in the daemon, live review
  buffers keep the OLD keymap OBJECT (the defvar rebinds the
  variable, not the buffers' local maps) and never ran the new
  mode body (no local `eldoc-documentation-functions` entry).
  Patch live buffers explicitly: `(use-local-map
  code-review-mode-map)` + re-run any local `add-hook`s.
- Comment sections are dual-role data objects: slots like `hidden`
  and `end` are initialized by `magit-insert-section` during a real
  wash; a hand-built instance in tests must `(oset ... hidden nil)`
  and give it `start`/`end` markers or magit's show/hide machinery
  signals `unbound-slot`.
