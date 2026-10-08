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
changing anything in the `code-review-section-*` render files, and
update it whenever a phase moves forward or a new gotcha is
discovered.

## Layout

| File | Role |
|---|---|
| `code-review.el` | entrypoints, `code-review-mode`, transient menus |
| `code-review-section.el` | the section-rendering FACADE + review-buffer build/orchestration: `--trigger-hooks` render phases, `--internal-build` per forge, `--build-buffer`, patch-line/hunk navigation |
| `code-review-section-shared.el` | shared section-render primitives: the render defcustoms/defvars (indent, fill, display toggles, grouped/written-comment bookkeeping), `--hide-if-hidden`, html rendering, hunk painting |
| `code-review-section-header.el` | the PR header section inserters (title/state/milestone/labels/assignees/reviewers/commits+CI checks/description/feedback) |
| `code-review-section-analysis.el` | the analysis + review-order sections and their jump/goto helpers |
| `code-review-section-comment.el` | the comment section classes and renderers (conversation, top-level, inline code comments, outdated hunks, bot folding, fringe markers) |
| `code-review-section-wash.el` | the owned diff wash (phase 11b): `wash-diff` / `wash-insert-file-section` / `wash-hunk` + the comment interleaving, and the diff report |
| `code-review-diff.el` | diff classification engine: pure functions on raw diff text + file-order/noise rule defcustoms |
| `code-review-reactions.el` | reaction toggle machinery (one engine, three contexts: description/conversation/code-comment) |
| `code-review-analysis.el` | phase 5 heuristics (duplicate/dead-code/dangling findings) + phase 15 hunk delicacy (blast radius, bounded blame age/ownership, branch delta, dead defs; badge on delicate hunk headings, top-K jump list, C-c C-d cycling; worktree greps, byte-capped, cached per PR+diff) |
| `code-review-history.el` | phase 14 harvest + review heat: per-repo history cache (ONE `git log --name-only` parsed twice — code-compass metrics + the phase 18 coupling matrix — async child emacs, TTL, format-versioned: old caches re-harvest once), churn-percentile x complexity x knowledge score, HOT/WARM/COLD buckets feeding the phase 3 tags/order/focus (soft dependency on code-compass) |
| `code-review-coupling.el` | phase 18 change-coupling completeness: co-change matrix from the history harvest (code-maat style: degree threshold, min co-changes, max changeset size), findings when a changed file's >=threshold peer (star case: its coupled TEST file) is untouched in the PR; cached per PR+diff, rendered in the Analysis section with jump buttons into the peer |
| `code-review-dossier.el` | phase 16 hunk dossier: `C-c C-h` inserts an on-demand, cached, collapsible "Context (dossier)" section after a hunk (`git log -L` history of exactly those lines at the base rev, blame authors/last-touch, touched-def call sites with jump buttons, tests split, file heat, hunk risk); `C-u` appends the OFF-by-default LLM garnish |
| `code-review-testimpact.el` | phase 17 test impact: CI-gaming detector (pure diff-text scan: language-scoped skip markers, `\|\| true`, gated/removed CI steps, lowered coverage thresholds → hard `[CI-GAME]` file tags; DOC-classified files and comment-only lines are skipped — they mention the markers legitimately), test mapping (changed defs → covering test files, `[NO-TEST]` hunk tags + Analysis entries), the `T` command running exactly the mapped subset via `compilation-start` (command resolution: user alist → any projectile/project.el test command already loaded and defined → built-in conventions, Makefile `test` target/pytest/jest/go/cargo; C-u: fake-fix check, running the subset at the BASE rev in a detached temp worktree too) |
| `code-review-hunkhighlight.el` | phase 10 + 20: tree-sitter semantic hunk faces (reusable on any magit diff buffer) — the ENGINE: reconstruct, parse, merge the general/test/security/BEACON query lists with per-entry condition-range classification (`--entry-conds`/`--entry-face`), the flat `--cache-key`, the treesit-optional `--region`, `--apply` overlays |
| `code-review-hunkhighlight-queries.el` | the per-language treesit query VOCABULARIES + faces: general (phase 10), test-file, security source/sink lists (phase 20a; sparse by design), and the phase 20b BEACON list (conditions captured as @_cond, comparison operators and literals in `(STRONG . DIM)` cons mappings: strong inside conditions and in test files, dim outside) |
| `code-review-hunkhighlight-intraline.el` | phase 20c: intra-line changed-token marks — grammar-free tokenizer, token-LCS alignment, raw-hunk `-`/`+` block pairing, underline face on exactly the changed tokens of modified added lines (line/token caps; pure-add and whitespace-only pairs unmarked) |
| `code-review-registry.el` | phase 22 incident registry: commit TRAILERS (Incident/Invariant-Ref/Regression-Test/Paths) are the source of truth, the `incidents/` markdown is GENERATED from them (`code-review-registry-generate`); one bounded `git log` trailer scan + byte-capped `incidents/*.md` fallback, cached per (REPO . HEAD); incident paths feed the phase 15 hunk badge (`:incidents` score ingredient, `N incident(s)` reason) and `[N incident(s)]` file-heading tags, the phase 5 dead-code never-flag, and the phase 16 dossier jump lines; keyword detection (`code-review-registry--detect` in `code-review-post-hook`) prompts ONCE per PR to tag the review (`code-review-incident-tag`: PR-prefilled paths, submission chain); `code-review-install-conventions` appends the idempotent sentinel-marked AGENTS.md block |
| `code-review-criteria.el` | phase 23 criteria engine: criteria files (default `specs/*.md`, EARS shape, stable date-numbered req IDs `YYYY-MM-DD-NNN`) matched against a change's paths; ONE bounded `git ls-files --cached --others --exclude-standard` pass (criteria are drafted BEFORE the commit, so untracked-but-not-ignored files count; output UNSORTED); `--covers-p` declared exact/parent-dir match with undeclared text/base-name fallback (declared-but-uncovering never falls back); `--enforce-local` (`criteria-required`: nil/warn/require, require REFUSES the local review); the EARS `--template`/`insert-template` (paths prefilled from the diff) and `criteria-draft` calling the soft `criteria-generate-function` AI hook (never implemented by the mode) |
| `code-review-section-criteria.el` | phase 23 criteria CHECKLIST section (local reviews only, a reading aid — nothing submitted): per-item section class with a `req` slot, `RET` cycling `[ ]/[x]/[!]` in place, id jump buttons, covered paths, and the warn banner when no criteria cover the change |
| `code-review-local.el` | local diff review (`code-review-review-local-diff`): working tree as a read-only pseudo-PR (state LOCAL) |
| `code-review-browse.el` | phase 7: `browse-url` integration — GitHub PR links (with `#diff-`/comment anchors) open inside Emacs; `code-review-open-pr-at-point` finds PR URLs in any buffer (email workflow) |
| `code-review-db.el` | sqlite persistence via closql (singleton db) |
| `code-review-github.el` / `-gitlab.el` / `-bitbucket.el` | forge backends |
| `code-review-repo.el`, `code-review-comment.el`, `code-review-actions.el`, `code-review-utils.el`, `code-review-faces.el`, `code-review-parse-hunk.el`, `code-review-interfaces.el` | support (`actions.el` also holds the interactive/navigation commands) |
| `test/` | ERT tests (`make test`) |

## File size guideline

Keep every source file **500–800 lines**.  When a file crosses
800 lines — or a single function grows past ~80 — stop adding and
refactor first: extract cohesive helpers, then split by concern
(the `code-review-section-*` split of phase 15c is the template:
a thin facade that `(require)`s the parts in dependency order, so
every existing `(require 'code-review-section)` site keeps
working; cross-part function calls go through `declare-function`,
classes are plain symbols at compile time).  This applies even to
files that were ALREADY over the limit when the guideline landed —
`code-review-actions.el`, `code-review-github.el`,
`code-review-analysis.el`, `code-review-hunkhighlight.el` are the
current offenders to shrink when touched.  Two smaller-than-500
files are fine when they are one coherent concern
(`-section-shared`, `-section-analysis`); do not pad or merge
files just to hit the range.

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
- Text properties ride STRINGS out of chat buffers and into the
  db (phase 7 incident): emacs-slack/lui link buttons hand
  `browse-url` URLs carrying `lui-raw-text` and keymap
  properties with the whole message; `match-string` PRESERVES
  them, and `emacsql-escape-scalar` encodes every scalar with
  `prin1-to-string`, so a propertized slot serializes its entire
  property payload into the column — tens of MB per row, and
  the row can never be read back (the reader dies on the
  embedded unreadable objects: "EmacSQL had an unhandled
  condition").  Any string destined for a closql slot must be
  `substring-no-properties`'d at the parse boundary
  (`code-review-utils-pr-from-url` and
  `code-review-browse--canonical-url` do this now); never
  `match-string` buffer text straight into a slot.
- Never write a probe loop that moves point BACKWARD from each
  match it collects: `search-forward` re-finds the same match
  forever, and the loop conses without bound — this hung the
  live daemon until the user C-g'd (RSS was ~10GB and climbing).
  Collect match positions forward in ONE pass (record
  positions, then extract), and keep daemon probes strictly
  single-pass.
- An unbalanced TEST file (not just a package file) kills the
  whole `make test` batch at load time, and the symptom is easy
  to misread: make echoes the huge `command-line-1` `-L` list and
  the real error ("Error: end-of-file ... loading
  test/foo-test.el") drowns inside it — exit 255 with only
  `command-line-1`/`command-line`/`normal-top-level` frames.
  Redirect `make test` to a file and read the frames ABOVE the
  `-L` echo line.  Pre-flight every test-file edit with the same
  paren check used for /tmp scripts (the depth-walk or an
  external counter) BEFORE running the suite.
- `git log --exclude=refs/remotes/code-review/*` placement:
  `--exclude` is a revision option of the `log` SUBCOMMAND —
  placed before it (global-option position) git errors out, and
  `call-process` DISCARDS stderr, so the harvest silently
  returned an empty log and empty metrics (phase 14).  When a
  git call through `call-process` returns surprising emptiness,
  re-run it with stderr captured before debugging the parse.
- A `make compile` KILLED by a command timeout leaves a stale
  `.elc` behind, and the next `make test` runs the old bytecode
  while the source looks fixed (the phase 14 accessor bug looked
  "not fixed" for exactly this reason — 113/113 only after a
  clean recompile).  After any interrupted compile, re-run
  `make compile` and verify the `.elc` mtime BEFORE trusting a
  test verdict.
- The same trap with an UNBALANCED SOURCE file (not just an
  interrupted compile): `make compile` fails, the stale `.elc`
  keeps serving OLD bytecode, and `make test` still passes GREEN
  on the old code — the suite result is meaningless exactly when
  you need it most (a one-closer-short splice did exactly this).
  `check-parens` every source edit BEFORE running the suite, and
  after compiling verify the `.elc` mtime is newer than the `.el`.
  For big block edits, splice by line range with boundary
  ASSERTIONS (start-with/end-with the expected form text) rather
  than hand-transcribing `old_str` blocks — the assertions refuse
  to write on any mismatch.
- magit's `magit-insert-heading` AUTO-APPENDS the child count to a
  heading ("Commits:" renders as "Commits (1)"; "Reviewed by
  alice[COMMENTED]:" as "Reviewed by alice[COMMENTED] (1)"), and
  `shr` wraps rendered comment bodies even at short widths: pin
  heading/body text in tests with prefix matches and
  `[[:space:]\n]*` between words, never a trailing colon.
- `search-forward` (and `search-backward`) search for a LITERAL
  STRING: passing a regexp like "\\[HOT\\]" searches for that
  exact text and finds nothing (use `re-search-forward`).  The
  existing `search-forward` gotcha (no match data) has a second
  face: it is not a regexp search at all.
- `code-review-db-pullreq` is `:abstract`: ERT fixtures cannot
  instantiate it ("Class code-review-db-pullreq is abstract") —
  build rows with a concrete subclass
  (`code-review-github-repo`, `code-review-local-diff`, ...).
- shr wraps rendered HTML at `shr-width`
  (`code-review-fill-column` in `code-review--insert-html`): an
  assertion on rendered words like "Rendered from" fails when
  shr breaks the line between them — match with
  `[[:space:]\n]*` between words.
- A KEYMAP built with `defvar` does not REBIND when its file is
  reloaded in a live daemon (the defvar no-op gotcha): the
  variable still holds the OLD map object, so
  `(use-local-map code-review-mode-map)` patches live buffers
  with a STALE map.  Patch the SHARED old map object in place
  with `(define-key code-review-mode-map ...)` instead: every
  existing buffer (same object) and every future one (the mode
  uses the variable) sees the key.
- ERT's `(equal RESULT "*...@ HEAD*")` does NOT glob: `.*` inside
  the expected string is a literal dot-star and the assertion
  fails against a real name.  Assert generated buffer names with
  `string-match-p` and a real regexp.
- The deferred render chain is ASYNC even in batch emacs
  (`deferred:parallel` posts its callbacks into
  `deferred:queue`, consumed by timer ticks): an ERT end-to-end
  test must poll (`(sit-for 0.05)`, with a deadline) until the
  row's `raw-diff` is non-nil AND `deferred:queue` is nil.  The
  `closql-insert` in `--internal-build` landing means the DB
  assertions are safe, and a drained queue means no async work
  survives the test's db reset (see
  `code-review-local/review-commit-end-to-end`).
- magit 4.x buffer names have NO leading star: a revision buffer
  is named "magit: commit <rev>", so `(get-buffer "*magit:
  commit*")` returns nil.  Find magit buffers by name prefix
  "magit:" or by `derived-mode-p` over `(buffer-list)`.
- A surplus closer AFTER a complete top-level form does not stop
  that form from being read and EVALUATED: the reader closes the
  form early, its side effects land, and the `load` only signals
  on the NEXT top-level read.  A daemon probe can therefore
  return correct results from an unbalanced file — pre-flight
  every probe with check-parens regardless of how well it ran.
- `(interactive "p")` passes ARG 1 — NOT nil — for no prefix: any
  no-prefix behavior keyed on `(not arg)` silently never runs
  interactively (only direct calls with no argument exercise it,
  which is exactly what an ERT test written naively does).  Call
  the command with 1 in end-to-end tests, and key no-prefix
  detection on `(member arg '(nil 1))` (this bug shipped the
  magit-buffer local review invisibly broken for every real
  keypress; see Improvements.org phase 12).
- magit's log machinery chokes on split `-n` args in batch:
  `(magit-log-head (list "-n" "3"))` dies inside
  `magit-log-get-commit-limit` with `stringp nil` (the `"-n"`
  form matches magit's `"^-n\\([0-9]+\\)?$"` with an EMPTY group,
  and `(string-to-number nil)` explodes).  Pass the merged form
  `(list "-n3")`, which is what the transient actually produces.
- magit-log commit sections carry SHORT shas as values (not the
  40-char form): expect short shas in anything derived from them
  (stored diff args, buffer-name hints).
- "the diff of the commits in the region" is NOT magit's
  `d`-on-region convention: magit diffs the endpoint trees
  (`OLD..NEW`), which EXCLUDES the oldest commit's own changes.
  Including them needs `OLD^..NEW` — and a ROOT-commit oldest
  needs the git EMPTY TREE trick (`4b825dc...` is the same sha in
  every repository and a valid range endpoint without the object
  existing).  Probe range semantics with real git before shipping.
- `apply` requires its LAST argument to be a LIST: spreading
  subprocess args with `apply` and appending a trailing PATH
  STRING signals `(wrong-type-argument listp "lib.py")`.  Collect
  the trailing scalars into the spread list with `append` (see
  `code-review-analysis--blame`).
- Raw diff ranges text inside a REGEXP: the `+` of `+1,4` is a
  QUANTIFIER — `"^@@ -1,3 +1,4"` matches nothing (the space
  before `+` swallows it as "one or more spaces"); escape it:
  `"^@@ -1,3 \\+1,4"`.  (Or use `search-forward`, which is a
  literal search.)
- `(string-prefix-p "\\" line)` guards the `git diff` "\ No
  newline at end of file" marker line; writing `"\\\\"` (a
  TWO-character string) does not guard anything and the marker
  leaks into the hunk's context/blame lines.
- `recenter` acts on the SELECTED window: guarding with
  `(get-buffer-window (current-buffer))` (displayed SOMEWHERE)
  still errors with "recenter'ing a window that does not display
  current-buffer" when the buffer is displayed but not selected —
  exactly the `emacsclient --eval` probe case.  Guard on `(eq
  (window-buffer (selected-window)) (current-buffer))` (see
  `code-review-section--goto-hunk-section`).
- A daemon verification script that polls for an async
  (deferred) render can be satisfied INSTANTLY by the buffer's
  PREVIOUS content: "buffer exists, non-empty, has a root
  section" was all true of the stale render, so the probes read
  the old buffer while the new render was still queued.  Poll on
  something only the new render produces (a marker string), or
  re-probe in a separate emacsclient call after the queue drains
  (phase 15).
- On a promisor/partial clone (`remote.*.promisor` /
  `blob:none`), `git blame` of an old, frequently-modified file
  lazy-fetches a blob for EVERY historical version of the blamed
  lines — minutes of network fetches inside a SYNCHRONOUS
  `call-process` in the render, which blocks Emacs completely
  (litellm PR 43310: 60 lines of `proxy_server.py` at the base
  branch, >90s and unfinished).  Detect the promisor config and
  skip (`code-review-analysis--partial-clone-p`); the same
  applies to ANY blob-reading git call in a render path.
- An async child (`make-process` + `emacs -Q`) resolves
  dependencies from the STALE `.elc` on disk, while the
  long-lived daemon has the restored `.el` loaded and answers
  `fboundp` TRUE: the child dies instantly and its sentinel
  reports a failure that is really a stale-artifact problem (the
  phase 14 history child "failed" on litellm for exactly this
  reason after code-compass's primitives were restored to the
  `.el` without recompiling).  Verify child-visible
  dependencies from a FRESH batch emacs, never from the daemon.
- A form can be BALANCED but MIS-NESTED: one missing closer
  mid-function is absorbed by a surplus closer at the end, and
  `check-parens` stays green while the code means something
  else.  This shipped a phase 16 command whose whole body sat
  inside `(when (equal arg '(4)))` — a no-prefix keypress was a
  complete no-op while every artifact looked fresh.  When a
  function "runs but does nothing", `(disassemble 'fn BUF)`
  FIRST (output goes to BUF or `*Disassembly*`, NOT
  `standard-output`): the `goto-if-nil` targets show the real
  control flow, and they cannot be lied to by stale artifacts
  once you have proven the `.elc` is fresh.  Phase 20 shipped a
  second instance that produced NO symptom at all instead of a
  no-op: the beacon-merge restructure of
  `code-review-hunkhighlight--ranges` left `(nreverse res)`
  inside the `(when (memq lang ...fragment-line-languages...))`
  gate, so every non-yaml language silently returned nil — no
  error, no message (the caller's condition-case never fired),
  check-parens green, compile clean, and every treesit ERT test
  failing while its individual pieces probed perfect in
  isolation.  When helpers work standalone but the composed
  defun returns nil, disassemble the defun before debugging the
  helpers; and do big-block edits with line-range splices plus
  closer-count ASSERTIONS (the awk closer-count check), never
  hand-transcribed `old_str` blocks.
- Deeply nested data building is a paren-count bug farm (phase
  20, third confirmed instance): a cache key built as nested
  `(cons a (cons b (cons c ...)))` cost three failed hand-counted
  closer fixes on the same defun — hand counting was wrong
  three times, exactly the documented trap.  Restructure to a
  FLAT helper with a plain `list` of the parts
  (`code-review-hunkhighlight--cache-key`) instead of fighting
  the nesting; balance with the depth-walk/unmatched-opener
  probes and closer-count assertions, never by eye.
- ERT `face-at`-style helpers read the LAST char of the search
  match: an assertion about a single-char token (a comparison
  operator) must use a search string that ENDS ON that char
  (`" (="` ends on `=`), not on a neighboring operand — a
  search ending on a space or an identifier silently asserts
  the wrong position and the test proves nothing.
- Pattern-anchored block splices can EAT a neighboring block:
  splicing several blocks bottom-up, an earlier splice's END
  anchor ("the next defun") can match a defun INSIDE the content
  a later splice just inserted (the phase 20 engine refactor: the
  cache-key splice's region anchor found the region defun that
  now sat after its own helper defuns, and the splice deleted
  them — the byte-compile "function not known to be defined"
  warnings pointed straight at it).  Splice one block at a time
  and re-derive the anchors from the CURRENT file between
  splices, or anchor on the LAST line of the block you are
  replacing instead of the first line of the next one.
- `make compile` exits 0 even when a byte-compile FAILS (an
  `--eval`'d `byte-compile-file` never fails make), and the
  stale `.elc` serves the old code while the source looks
  fixed.  After EVERY compile: `grep -c Error` the log AND
  check the `.elc` mtime is newer than the `.el`.  Exit 0
  alone proves nothing (bit twice in phase 16).
- Elisp `\x1f` in a string literal greedily reads up to FOUR
  hex digits: `"\x1falice"` reads as U+01FA + `"lice"`.  Build
  \x1f-joined fixture strings with `concat` and a standalone
  `"\x1f"`.
- Piping probe output through `grep` without `-a`: one
  `prin1` of byte-code (control characters) makes grep declare
  the WHOLE stream binary and print only "binary file matches",
  silently swallowing exactly the lines you grepped for.  Use
  `grep -a` on emacs probe output.
- Same-second fixture commits make "most recent commit" a tie
  decided by line order, not authorship: give fixture commits
  deterministic `GIT_AUTHOR_DATE`/`GIT_COMMITTER_DATE` via
  `process-environment` bound around `call-process`.
- emacsql `%` escaping (phase 21, cost a broken migration
  branch): a RAW SQL string is a format string at execution
  (`emacsql-format` runs `format` over the prepared statement),
  so every literal `%` must be written `%%`.  A `$s1` PARAMETER
  or a vector string CONSTANT can never carry a pattern that
  ends in `%` at all: `emacsql-escape-scalar` prin1-wraps values
  for STORAGE, so the arg `"https://%` reaches sqlite as the
  pattern `"\"https://%"` (a literal backslash) and matches
  nothing.  And do not build the raw string with elisp `format`
  (it collapses the `%%` before emacsql ever sees it) — build
  with `concat` and keep the raw `%%` literal (see
  `code-review-db--migrate-v10`).  Assert wildcards against
  real rows: a broken pattern silently matches ZERO rows and
  every test that also matches another branch stays green.
- closql's marker bridge (phase 21): closql stores the eieio
  unbound marker as the plain TEXT `eieio-unbound`
  (`closql--intern-unbound` on write) and post-processes every
  decoded row with `closql--extern-unbound`, which returns the
  LIVE marker (`eieio--unbound` after the Emacs 31 rename;
  always the value of `eieio-unbound`).  A decoded buffer
  column therefore eq's the EVALUATED `eieio-unbound`, never
  the quoted literal `'eieio-unbound` — assert with
  `(should (eq (nth N row) eieio-unbound))`.
- `substr(col, 1, N)` on stored prin1 text CUTS a string
  literal mid-quote (the leading `"` with no closing `"`), and
  emacsql's `read` then dies on the row ("EmacSQL had an
  unhandled condition", data nil).  Verify stored strings with
  `length(col)` (a number, reads back cleanly) or read the file
  with the sqlite3 CLI.
- Makefile recipes MANGLE `$s1` (make expands `$s`, then the
  shell expands the rest): keep `$`-free SQL in makefile probes
  and put probe code in a real `.el` file that the recipe just
  `load`s — never emacsql params inside `--eval` strings.
- Batch emacs probes must set `native-comp-jit-compilation nil`
  as their FIRST eval: a batch process JIT-compiling the
  package `.elc`s writes the shared `~/.emacs.d/eln-cache` and
  races the live daemon (crash-correlation during phase 21 was
  traced to daemon-side Emacs 31 GC bugs, but the guard removes
  the shared surface entirely).  Loading probe `.el` files via
  a makefile recipe needs the FULL elpa load path (the project
  Makefile's `$(wildcard $(PKG_DIR)/*)` form), not a hand-
  picked `-L` list (test files `(require 'a)`, `uuidgen`, ...).
- `delete-dups` is DESTRUCTIVE and `(append a b)` returns a list
  whose tail SHARES `b`'s conses: deduping an appended list
  splices the shared tail — corrupting the cached plist a later
  reader walks (an ERT test caught this in the phase 17 engine).
  Always `(delete-dups (copy-sequence ...))` when the source may
  be cached or shared.
- check-parens reports where the damage SURFACES (usually the
  NEXT top-level form), not the line of the missing/extra closer;
  hand-counting closers was wrong three times in a row on the
  same line.  Write daemon probe scripts FLAT (bind results into
  a plain `let`, assemble the report at the very end) — deep
  nesting is what makes them hard to balance — and prefer
  `how-many` to a hand-rolled match-count loop.
- A `search-forward` probe must search something UNIQUE to the
  target: the Analysis section's own "no test coverage" line
  mentions def names too, so searching the def name landed point
  in the Analysis section (and the hunk-target resolution
  correctly refused).  Search the exact hunk code line instead.
- Local review buffer names are `*Code Review: local: REPO*` —
  NO `@ RANGE` part for HEAD diffs; find review buffers by name
  REGEXP, never an exact guess.
- Test files have NO `provide` form: `require` fails with
  "failed to provide feature".  `load` them (as `run-tests.el`
  does) — in probes, `load` the `.el` directly.
- Cached data must be validated on BOTH the memory and the disk
  path (phase 18 live-daemon bug): a `--load` that checks the
  format version only on the disk file silently serves
  VERSIONLESS in-memory entries loaded by pre-migration code,
  and the migration never runs while everything "works".  Stamp
  the same `:version`/`:window`/timestamp plist into BOTH
  `--store` destinations and run one validator over whichever
  path serves.
- `how-many` counts matches FROM POINT (phase 18 probe): after
  a render point can sit at point-max, so a verification count
  reads 0 while the text exists.  `(goto-char (point-min))`
  before `how-many`, or collect with an anchored
  `re-search-forward` — and note a plain `how-many "foo:"`
  count also matches the LITERAL string inside new source
  shown in the diff; anchor the pattern
  (`"^[ \t]*foo: "`) to count actual rendered findings.
- `slot-boundp` on a NIL object signals wrong-type-argument:
  the closql singleton can legitimately be nil (mid-reset, in
  a fresh child).  Guard probes with
  `(and db (slot-boundp db :connection))`, never a bare
  `slot-boundp`.
- git fast-import fixtures (phase 18): `from :mark` must come
  AFTER the message data (a `from` before `data` starts from
  the EMPTY tree, not the previous commit), reusing a blob mark
  across commits makes later commits EMPTY (mark new blobs
  per commit), and fast-import writes objects only — `git
  reset --hard` is needed afterwards to materialize the
  worktree.  The Execute shield wants ONE git invocation per
  call: build the fixture with a single heredoc fast-import
  spanning ALL commits.
- A truthy cache-miss SENTINEL inside `or` short-circuits the
  compute and LEAKS the symbol to every consumer (phase 22):
  `(or (gethash key cache 'missing) (compute-and-store ...))`
  never falls through — the cache hands back `'missing` itself
  and downstream `dolist`/`gethash` die with
  `wrong-type-argument listp missing`, or worse, an
  `ignore-errors` upstream swallows it into a silent nil (the
  file-tag bug: nil tag, no error, no clue).  Use a two-step
  `(let ((cached (gethash key cache 'missing))) (if (eq
  cached 'missing) (compute-and-store) cached))` whenever the
  cached value may legitimately be nil.
- A text block ending in a newline splits into a trailing
  EMPTY line (phase 22): `--parse-trailers` rejected every
  well-formed commit message because the final `""` element
  fell into the `t` branch and nulled the ok flag.  Split with
  OMIT-NULLS (`(split-string s "\n" t)`) whenever empty lines
  are not data — folded-continuation parsers still work,
  whitespace-led continuation lines are non-empty.
- `code-review-section--setup-worktree` gates on
  `code-review-repo-enable`: a render-level test that binds it
  nil (copied from a db-only end-to-end) gets NO worktree, so
  the analysis engine, hunk badge, incident registry tag and
  dossier all silently render nothing.  Render-level tests
  need it at its default `t`; db-field-only tests bind it nil
  to skip the worktree setup.
- `git ls-files --cached --others --exclude-standard` output
  is UNSORTED (the tracked group, then the untracked group,
  each in internal order): any test or consumer that expects
  a deterministic order must compare `sort`ed copies of the
  list (the phase 23 criteria scan test bit on exactly this).
- Rendering a LOCAL review spawns the phase 14 async
  history-harvest child, and its process sentinel later
  RE-RENDERS the review buffer: if a subsequent test has reset
  the db by then, rendering a second LOCAL review DELETES the
  previous LOCAL row (the local key is unique), the re-render
  reads a nil pullreq, and `oref` on nil signals from inside
  the sentinel — killing the whole batch suite at a test that
  passes in isolation.  Two lessons: a process sentinel must
  NEVER signal (async errors land wherever the user happens to
  be — wrap the sentinel body in `condition-case` and log), and
  a render-level ERT test must be HERMETIC: bind
  `code-review-history-enabled` nil inside it (no harvest child
  at all) and `kill-buffer` the review buffer at test end so
  nothing async outlives the db reset.
- Deeply nested trailing-closer edits (appending a form at the
  end of a long `let*` test body) are the classic hand-count
  trap AGAIN: two of three kill-buffer insertions landed wrong
  by eye and only the awk net-opener/closer count +
  `check-parens` settled it.  Never balance closers by
  counting — assert the net count per region with awk and let
  `check-parens` be the verdict.
- Regenerating an INSTALLED package's autoloads with a bare
  `(loaddefs-generate DIR OUT ...)` call breaks the next fresh
  Emacs restart (this bit for real): package.el's own
  `package-generate-autoloads` passes EXTRA-DATA (the 4th
  argument) — a printed
  `(add-to-list 'load-path (or (and load-file-name ...)))`
  form — and `loaddefs-generate` inserts that string ONLY when
  the output file is created FRESH; in update mode (file
  exists, `generate-full` nil) it reads the existing file and
  silently DROPS extra-data.  A regenerated file that was
  written without the form looks identical, registers every
  autoload symbol — and on restart `package-activate-1` loads
  it by absolute path without the dir on `load-path`, so
  helm-M-x happily offers the commands and executing one dies
  with "Cannot open load file: No such file or directory,
  code-review".  The daemon kept working the whole time because
  its `load-path` was set at ITS startup with the OLD file —
  the failure only surfaces on a fresh start.  Regenerate the
  package.el way: delete the old file first, then
  `(package-generate-autoloads "code-review" PKG-DIR)`, and
  VERIFY the `add-to-list` form is present in the result
  before moving on.
- The section class's `keymap` slot DOES route keys: magit 4.x
  applies it (EVALUATED) over the section's text at render time.
  What breaks keys is the MARKER REWRITE: `replace-match` writes
  the new text with NO properties — losing BOTH the `magit-section`
  MATCHER property (then `magit-current-section` at the rewritten
  text falls to the root and the command silently no-ops) and the
  routing keymap.  Capture `(text-properties-at BEG)` before
  `replace-match` and `set-text-properties` the whole region back
  after (then overlay the new state's face).  And the ERT test
  must press the actual key — `(execute-kbd-macro "\r")` at point
  ON the rewritten marker, not just call the command: a direct
  call proves the command, not the routing, and ships broken keys
  green.
- A symbol-valued `keymap` TEXT PROPERTY is INERT (neither
  `key-binding` nor the command loop resolves it): the value must
  be the EVALUATED keymap object
  (`(propertize TEXT 'keymap MAP-VARIABLE)` — the variable, not
  `'MAP-VARIABLE`).  Diagnostic: the neighbors carry
  `(keymap (13 . ...))` (evaluated, routes) while the broken chars
  carry the bare symbol.  The phase-1 header sections propertize
  with `'code-review-*-section-map` SYMBOLS — their RET bindings
  are likely silently dead the same way (not yet fixed; flagged,
  see Improvements.org phase 23 postscript).
- `propertize` (and plist consumers) take KEY-VALUE pairs: a
  helper spreading extra PROPS must `(apply #'propertize TEXT
  'keymap MAP PROPS)`.  Passing `PROPS` as one argument leaves
  an ODD-length plist, `put-text-property` gets the list as the
  property NAME, signals `wrong-type-argument symbolp` — and
  the render chain's condition-case reports it as "Got an error
  from your VC provider", the documented false signature of an
  engine error killing the render.
- Any ERT test whose expectation embeds a date-numbered id
  generated from TODAY (criteria req ids, incident ids) must
  compute the date: `(format-time-string "%Y-%m-%d")`.  A
  hardcoded date passes on the day the test is written and
  fails the next day (the criteria draft test bit exactly so:
  green on 2026-10-01, red on 2026-10-02 with zero source
  changes).
- A render-level ERT test must be BOTH hermetic AND leak-proof,
  or its failure poisons LATER tests: (a) hermetic — bind
  `code-review-history-enabled` nil so no async harvest child is
  spawned (its sentinel's re-render of a LOCAL review DELETES the
  current LOCAL row, stalling a later test's deferred chain with
  "raw-diff missing" while logging "history re-render failed");
  (b) leak-proof — wrap the assertion phase in
  `unwind-protect` and `kill-buffer` the review buffer in the
  cleanup, because an assertion failure otherwise leaks the
  buffer, and the same sentinel re-render then deletes whatever
  LOCAL row the LATER test is mid-flight on.  The sentinel's
  `condition-case` (already in place) only stops the batch CRASH;
  it does not stop the row-deletion side effect.
- ghub 5.1 semantics (the mention fix, 2026-10-07): a
  callback-less `ghub-query` is ASYNC by default — it returns nil
  immediately and the answer is dropped (`ghub--retrieve` uses
  `url-retrieve` unless `:synchronous t`).  With `:synchronous t`
  the return is the single `(data CONTENTS)` PAIR, NOT the full
  response alist older ghub returned: an old let-alist `.data`
  reader silently reads nil (the `C-c @` @mention completion
  listed no users for exactly this reason).  Unwrap with
  `(cdr res)`.  And beware the fixture trap: the sync return
  PRINTS as `(data (repository ...))` with the data contents
  spliced as elements — writing an ERT stub that QUOTES the
  contents wrapped (`'(data ((repository ...)))`) adds one list
  level, `assq` then walks into the wrong level, and every test
  against the stub reports empty with no error anywhere (the
  first version of the mention tests failed both on exactly
  this).  Stub the spliced shape: `'(data (repository ...))`.
- Chunk-probing paren balance with byte ranges: `insert-file-contents`
  with BEG/END takes BYTE offsets into the file, not line numbers —
  convert a 1-based line position with a `(1- ...)` guard (or
  `position-bytes`), or the chunk drops its first line and a
  broken chunk "passes" while the file fails check-parens.
- `magit-insert-heading` (magit 4.x) inserts the heading's OWN
  trailing newline and leaves point at the FIRST BODY line's bol —
  and in the interleaved diff wash the text BELOW that bol is
  already-inserted HUNK text, so `line-end-position` there returns
  a HUNK line's eol (2026-10-08: the comment background paint sat
  one line low and its "heading" overlay swallowed the body, the
  next comment and a hunk line — every comment looked shifted down
  one line).  To paint the HEADING line go UP from the body bol:
  `(save-excursion (forward-line -1) (make-overlay (point) (1+
  (line-end-position))))` — never `(forward-line 0)` at the body
  bol.  The body overlays were correct all along (start = body
  bol, end = point after the insert); only the heading overlays
  were broken.

<!-- code-review-conventions v1 -->

## Code review conventions

### Incident registry

- Every production incident leaves a REGISTRY ENTRY under
  `incidents/` and a regression test.  Entries are GENERATED
  from commit trailers (`M-x code-review-registry-generate`) —
  never hand-curated; edit only the human body below the
  front matter.
- Fix commits carry the trailers:
  `Incident: <year-numbered id>`, `Invariant-Ref: <req id>`,
  `Regression-Test: <test names>`, `Paths: <files the incident
  touched>`.
- Changes touching incident paths get ELEVATED review
  attention (delicacy badge, dossier incidents, review
  budget).

### Criteria (requirements)

- Criteria live under `specs/` as markdown, one requirement
  per file, EARS/Gherkin shape (`WHEN ... THE SYSTEM SHALL
  ...`), with a stable date-numbered req id in the file name.
- Every requirement binds to its guard tests: the req id
  appears in the ERT test name (`file/req-<id>-description`)
  or the test docstring.

### If you are an AI agent working in this repository

- Author registry entries, criteria files and test tags
  yourself, in the formats above, as part of the change.
- PRs touching incident paths get elevated review attention.
<!-- end code-review-conventions v1 -->
