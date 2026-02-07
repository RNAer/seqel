.DEFAULT_GOAL := test

# robust emacs detection
EMACS ?= $(shell command -v emacs 2>/dev/null || echo /opt/homebrew/bin/emacs)

VERSION:=1.0
PACKAGE_NAME:=seqel-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)
PACKAGE:=/tmp/$(PACKAGE_NAME).tar

TEST_FILES := $(wildcard test/*-test.el)

.PHONY: test clean package

test:
	@printf "\n------- Checking Emacs Version...\n"
	@$(EMACS) --version | head -1
	@printf "\n------- Byte-Compiling elisp files...\n"
	${EMACS} -Q --batch -L . -f batch-byte-compile *.el
	@printf "\n------- Testing...\n"
	${EMACS} -Q --batch -L . -L test -l ert $(foreach file,$(TEST_FILES),-l $(file)) -f ert-run-tests-batch-and-exit

package: $(PACKAGE_DIR)
	tar cvf $(PACKAGE) -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	cp -r *.el $@

clean:
	rm -f $(PACKAGE)
	rm -rf $(PACKAGE_DIR)
	rm -f *.elc
