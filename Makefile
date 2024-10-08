export EMACS ?= $(shell which emacs)
export CASK := $(shell which cask)
ifeq ($(CASK),)
$(error Please install CASK at github.com/cask/cask)
endif
CASK_DIR := $(shell $(CASK) package-directory || exit 1)
SRC = $(shell $(CASK) files)
UNLINTABLE=private-comments-mode-autoloads.el private-comments-pkg.el

# pulls the version from private-comments-mode.el
VERSION = $(shell $(CASK) version)
ELCFILES = $(SRC:.el=.elc)
TESTS = $(shell git ls-files tests/test*el)
TESTSSRC = $(TESTS) tests/private-comments-mode-test.el
ELCTESTS = $(TESTSSRC:.el=.elc)

.DEFAULT_GOAL := test-compile

.PHONY: autoloads
autoloads: cask
	$(EMACS) -Q --batch -f package-initialize --eval "(package-generate-autoloads \"private-comments-mode\" default-directory)"

README.org: README.in.org private-comments-mode.el
	$(CASK) eval "(progn \
	             (load \"private-comments-mode\") \
	             (describe-minor-mode \"private-comments-mode\") \
	             (with-current-buffer \"*Help*\" (princ (buffer-string))))" 2>/dev/null \
	| scripts/readme-sed.sh "KEYS NOTEBOOK" README.in.org "key.*binding" > README.org0
	grep ';;' private-comments-mode.el \
	    | gawk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | gsed -e 's/^\s*;;*\s*//g' \
	    | scripts/readme-sed.sh "COMMENTARY" README.org0 > README.org
	rm README.org0

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: clean
clean:
	$(CASK) clean-elc
	rm -rf tests/config

.PHONY: test-compile
test-compile: autoloads
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | ggrep -E -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | ggrep -E -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)

.PHONY: lint
lint: cask
	bash -ex scripts/melpazoid.sh

.PHONY: test-unit
test-unit:
	$(CASK) exec ert-runner -L . -L tests tests/test*.el

.PHONY: test
test: test-compile test-unit

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean autoloads
	$(CASK) package

.PHONY: install
install: dist
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/private-comments-mode-$(VERSION).tar\")))"
