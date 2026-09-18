# Makefile for code-review
#
# Replaces the old cask-based build.  Dependencies are expected to be
# installed as ordinary package.el packages (see `make deps' which
# installs them from MELPA).  Point PKG_DIR at your package-user-dir.

EMACS   ?= emacs
PKG_DIR ?= $(HOME)/.emacs.d/elpa
LOAD_PATH := -L . -L test $(addprefix -L ,$(wildcard $(PKG_DIR)/*))

DEPS = a closql magit transient ghub uuidgen deferred markdown-mode forge emojify s dash

.PHONY: deps compile test check clean

deps: ## install package dependencies from MELPA
	$(EMACS) -Q --batch \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	  --eval "(package-initialize)" \
	  --eval "(package-refresh-contents)" \
	  --eval "(dolist (p (quote ($(DEPS)))) \
	           (unless (package-installed-p p) (package-install p)))" \
	  --eval "(message \"deps ok\")"

compile: ## byte-compile all package files
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval "(byte-compile-file \"code-review.el\")" \
	  --eval "(dolist (f (cdr (directory-files \".\" nil \"^code-review-.*\\\\.el\\\\'\"))) \
	           (byte-compile-file f))"

test: ## run the ERT test suite in batch mode
	$(EMACS) -Q --batch $(LOAD_PATH) -l test/run-tests.el

check: compile test ## compile then test

clean: ## remove build artifacts
	rm -f *.elc test/*.elc
