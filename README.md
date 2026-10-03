[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/code-review-badge.svg)](https://melpa.org/#/code-review)
![Tests](https://github.com/wandersoncferreira/code-review/actions/workflows/ci.yml/badge.svg)

# Code Review

Package to help you perform code reviews from your VC provider. Currently
supports Github and basic Gitlab and Bitbucket workflows.

![Demo of code review package](./docs/code_review_demo.png)

Link to same PR on Github: https://github.com/wandersoncferreira/dotfiles/pull/5

# Overview

The Emacs everywhere goal continues. These are the main features of
`code-review` to help you never leave Emacs to do Pull Request reviews.

- Start review from URL via `code-review-start`
- Modern UI using [magit-section](https://emacsair.me/2020/01/23/magit-section/) and [transient](https://github.com/magit/transient)
- Read Pull Request comments
- Reply to comments
- Include code suggestions
- View `outdated` comments with the right diff hunk context
- Approve, Reject or Request Changes for your PRs
- Integrated with `forge-topic-view` via `code-review-forge-pr-at-point`
- Fast track commands like "LGTM! Approved"
- Review using single commits to focus on diff
- Set labels on RET. See details [Multi value selection](./docs/multi-value-selection.md)
- Set assignee. Use transient `sy` option to `assign yourself` to the PR.
- Set milestone. See details [push access required](./docs/milestone.md)
- Edit PR title
- Edit PR description body
- Merge your PR. _(beta feature) See details [merge](./docs/merge.md)_
- Reactions. See details [react to comments](./docs/reactions.md)
- Promote comments to new issues.
- Save/Resume in-progress Reviews
- Visit binary files in Dired or Remote. Example [here](https://github.com/wandersoncferreira/code-review/pull/90)
- Mention user with `C-c @` in `*code-review-comment*` buffer.

Highly recommend using the transient menu in the `*Code Review*` buffer by pressing `r`.

The basic workflow:

- `RET` on a hunk diff line to add a comment
- `RET` on a local comment to edit
- `RET` on a previous sent comment to include a reply
- `C-c C-k` on a local comment to remove it
- `r s f` to enable transient and Set a feedback
- `r a` to approve the PR | `r r` to reject the PR | `r c` to add comments in the PR

# Solo criteria loop (local reviews)

When you review your own changes (`M-x code-review-review-local-diff`), the
package can show you the REQUIREMENTS that govern the diff, so you judge
conformance to intent — not just style. Requirements live under `specs/` as
markdown, one per file, in EARS shape (`WHEN ... THE SYSTEM SHALL ...`), with a
stable date-numbered id in the file name:

```markdown
---
paths: [lib/payments.py]
---
WHEN a change touches the payment export
THE SYSTEM SHALL keep the export pure (no network calls).
```

In a local review, a `Criteria (solo loop)` section renders next to the diff
with every requirement whose `paths:` cover the changed files:

- `RET` on a requirement row cycles its verdict: `[ ]` pending -> `[x]`
  satisfied -> `[!]` violated. Your own reading aid — nothing is submitted.
- `RET`/click on the req id jumps to the criteria file in the worktree.
- No requirement covers the change? `warn` mode (the default) shows a banner
  inviting `M-x code-review-criteria-insert-template`, which drafts a new
  criteria file (paths prefilled from your diff) and visits it in another
  window. `M-x code-review-criteria-draft` additionally calls
  `code-review-criteria-generate-function` (FILE PATHS) when wired — point it
  at your LLM/agent of choice to draft the EARS sentence for you to review.
- `code-review-criteria-required` = `require` refuses to open a local review
  for uncovered changes at all.

Criteria files count before you commit them (tracked and
untracked-but-not-ignored), so the loop works while drafting. Every
requirement binds to its guard test: the req id goes into the ERT test name
(`file/req-<id>-description`) so traceability holds forever.

### Playing with issue trackers (Jira, etc.): no duplication

A ticket is the CONVERSATION (context, negotiation, priority); the `specs/`
file is the INVARIANT (the thing future reviews judge the code against). The
ticket may rot — the spec may not. So the reference points one way: translate
the ticket's acceptance criteria into one durable EARS sentence when you
insert the template, keep at most a pointer (`refs: JIRA-123` in the front
matter, or the `Invariant-Ref: <req id>` commit trailer) in the ticket's
direction, and never maintain the same requirement in two places — two copies
drift apart, and the drift is invisible until one is wrong.

The loop is deliberately solo-first: it lives entirely in your local review
and your working tree, so you can use it fully without team adoption.

You can include your own bindings to functions like
`code-review-set-feedback`, `code-review-submit-approve`,
`code-review-submit-request-changes`, and `code-review-submit-comments` to not rely on the
transient panel. But I think you should see it :]

Take a look at which features are available to each integrated forge [here](./docs/forge_support.md).

Missing something? Please, [let us know](https://github.com/wandersoncferreira/code-review/issues/new).

# Installation

I highly recommend installing `code-review` through `package.el`.

It's available on `MELPA`.

`M-x package-install code-review`

Then you can either `M-x code-review-start` and provide a PR URL or `M-x
code-review-forge-pr-at-point` if you are in a forge buffer over a PR.

# Configuration

### Code Review

If you want to see pretty symbols enable `emojify` package:

``` emacs-lisp
(add-hook 'code-review-mode-hook #'emojify-mode)
```

Define line wrap in comment sections.

``` emacs-lisp
(setq code-review-fill-column 80)
```

Change how `code-review` splits the buffer when opening a new PR. Defaults to
`#'switch-to-buffer-other-window`.

``` emacs-lisp
(setq code-review-new-buffer-window-strategy #'switch-to-buffer)
```

Change the destination where binary files is downloaded.

``` emacs-lisp
(setq code-review-download-dir "/tmp/code-review/")
```


#### Experimental

Use passwords configured for forge. The default is `'code-review`.

``` emacs-lisp
(setq code-review-auth-login-marker 'forge)
```

#### Doom Emacs users

I've noticed that `*Code Review*` buffer is not added into the current workspace
in Doom emacs. If you have `workspaces` in your `$DOOMDIR/init.el` file,
consider the following snippet:

``` emacs-lisp
(add-hook 'code-review-mode-hook
          (lambda ()
            ;; include *Code-Review* buffer into current workspace
            (persp-add-buffer (current-buffer))))
```

#### Insecure private instances

If your private instance is HTTP not HTTPS, then you need to add the host to the following variable.

```emacs-lisp
(setq ghub-insecure-hosts '("hostname.com"))
```

### Forge specific

Follow the documentation to your version control provider to see more details
for the setup and configuration.

- [Github](./docs/github.md)
- [Gitlab](./docs/gitlab.md)
- [Bitbucket](./docs/bitbucket.md)

# Keybindings

You can access the transient panel by hitting `r` from any place of the `Code
Review` buffer.

![Transient keybindings](./docs/code_review_transient.png)

| Binding | Object                                | Action                      |
|:-------:|:-------------------------------------:|:---------------------------:|
| RET     | hunk                                  | Add Comment                 |
| RET     | comment                               | Add Reply                   |
| RET     | local comment (not sent to forge yet) | Edit local comment          |
| C-c C-k | local comment                         | Delete local comment        |
| C-c C-c | Comment Buffer                        | Register your local comment |
| C-c C-k | Comment Buffer                        | Cancel your local comment   |
| C-c C-r | comment                               | Add Reaction                |
| C-c C-i | comment                               | Promote to new issue        |
| C-c C-r | pr description                        | Add Reaction                |
| RET     | reaction (on emoji symbol)            | Endorse or Remove Reaction  |
| RET     | Request Reviewer                      | Request reviewer at point   |
| C-c C-n | anywhere in buffer                    | Jump to next diff hunk (skips comments and collapsed noise files) |
| C-c C-p | anywhere in buffer                    | Jump to previous diff hunk |
| N       | anywhere in buffer                    | Toggle focus mode: hide auto-flagged noise files (lockfiles, docs, whitespace-only changes); the "Files changed" heading reports what is hidden |
| D       | file section                          | Difftastic drill-down: zoom into one file's real changes in a structural view (read-only; C-u for the whole PR; needs the `difftastic` package and the `difft` command) |
| V       | anywhere in buffer                    | View-only diff of the whole PR ignoring whitespace |
| u       | anywhere in buffer (transient)        | Copy the PR URL into the kill ring (C-u: open it in the browser) |


## Binding suggestions

You can place `code-review-forge-pr-at-point` to a key binding for your convenience:

``` emacs-lisp
(define-key forge-topic-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point)
```

If you are not an Evil user you can set the letter `k`, for example, to delete a
local comment or feedback at point.

``` emacs-lisp
(define-key code-review-feedback-section-map (kbd "k") 'code-review-section-delete-comment)
(define-key code-review-local-comment-section-map (kbd "k") 'code-review-section-delete-comment)
(define-key code-review-reply-comment-section-map (kbd "k") 'code-review-section-delete-comment)
```

Move between hunks with the built-in `C-c C-n` and `C-c C-p`.  If you
prefer to move between comments instead, rebind the jump commands:

``` emacs-lisp
(define-key code-review-mode-map (kbd "M-n") 'code-review-comment-jump-next)
(define-key code-review-mode-map (kbd "M-p") 'code-review-comment-jump-previous)
```

# Extension to other forges

The package allows you to write integration with other forges to leverage these
functionalities. Take a look at `code-review-interfaces.el` to see which functions
need to be implemented.


# Thanks

Thanks [Laurent Charignon](https://github.com/charignon) for the awesome
[github-review](https://github.com/charignon/github-review) package and
stewardship. Github Review made me more familiar with the problem domain and
`code-review` is an attempt to build on top of it.

Thanks [Ag Ibragimov](https://github.com/agzam) for the amazing idea to use
`magit-section` to build a more suitable interface to this problem.
