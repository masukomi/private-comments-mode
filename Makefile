export EMACS ?= $(shell which emacs)
export CASK := $(shell which cask)
ifeq ($(CASK),)
$(error Please install CASK at github.com/cask/cask)
endif
CASK_DIR := $(shell $(CASK) package-directory || exit 1)
SRC = $(shell $(CASK) files)
UNLINTABLE=private-comments-mode-autoloads.el private-comments-pkg.el
VERSION = $(shell $(CASK) version)
ELCFILES = $(SRC:.el=.elc)
TESTS = $(shell git ls-files tests/test*el)
TESTSSRC = $(TESTS) tests/private-comments-mode-test.el
ELCTESTS = $(TESTSSRC:.el=.elc)

.DEFAULT_GOAL := test-compile

.PHONY: autoloads
autoloads: cask
	$(EMACS) -Q --batch -f package-initialize --eval "(package-generate-autoloads \"private-comments-mode\" default-directory)"

README.rst: README.in.rst private-comments-mode.el
	grep ';;' private-comments-mode.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | scripts/readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: clean
clean:
	$(CASK) clean-elc
	rm -f tests/log/*

.PHONY: test-compile
test-compile: autoloads
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)

.PHONY: lint
lint: test-compile
	bash -ex scripts/melpazoid.sh

.PHONY: test-unit
test-unit:
	$(CASK) exec ert-runner -L . -L tests tests/test*.el

.PHONY: test-int
test-int:
	$(CASK) exec ecukes --reporter magnars

.PHONY: test
test: test-compile test-unit test-int

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	$(CASK) package

.PHONY: install
install: dist
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/private-comments-mode-$(VERSION).tar\")))"
