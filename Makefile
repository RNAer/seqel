# ----------------------------------------------------------------------------
# Copyright (c) 2013--, Zech XU
#
# Distributed under the terms of the Modified BSD License.
#
# The full license is in the file COPYING.txt, distributed with this software.
# ----------------------------------------------------------------------------

.DEFAULT_GOAL := test

VERSION:=1.0
PACKAGE_NAME:=seqel-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)
PACKAGE:=/tmp/$(PACKAGE_NAME).tar

# Allow EMACS to be set via environment variable, default to an empty string
EMACS ?=

# If EMACS is not set, determine the default based on the OS
ifeq ($(EMACS),)
	ifeq ($(OS),Windows_NT)
		EMACS = emacs
	else
		UNAME_S := $(shell uname -s)
		ifeq ($(UNAME_S),Linux)
			EMACS = emacs
		endif
		ifeq ($(UNAME_S),Darwin)
			EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
		endif
	endif
endif


package: $(PACKAGE_DIR)
	tar cvf $(PACKAGE) -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	cp -r *.el $@

clean:
	rm -f $(PACKAGE)
	rm -rf $(PACKAGE_DIR)
	rm -f *.elc

# learned from https://github.com/xuchunyang/pinyin.el

.PHONY: test
test:
	@printf "\n------- Checking Emacs Version...\n"
	@$(EMACS) --version | head -1
	@printf "\n------- Byte-Compiling elisp files...\n"
	${EMACS} -Q --batch -L . -f batch-byte-compile *.el
	@printf "\n------- Testing...\n"
	${EMACS} -Q --batch -L . -L test -l ert -l all-tests.el -f ert-run-tests-batch-and-exit

# end
