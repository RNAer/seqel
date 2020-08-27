# ----------------------------------------------------------------------------
# Copyright (c) 2013--, Zech XU
#
# Distributed under the terms of the Modified BSD License.
#
# The full license is in the file COPYING.txt, distributed with this software.
# ----------------------------------------------------------------------------

.DEFAULT_GOAL := package
VERSION:=1.0
PACKAGE_NAME:=seqel-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)
PACKAGE:=/tmp/$(PACKAGE_NAME).tar

package: $(PACKAGE_DIR)
	tar cvf $(PACKAGE) -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	cp -r *.el $@

clean:
	rm -f $(PACKAGE)
	rm -rf $(PACKAGE_DIR)

# https://github.com/xuchunyang/pinyin.el
EMACS ?= emacs

.PHONY: all
all:
	@printf "* Checking Emacs Version...\n"
	@$(EMACS) --version | head -1
	@printf "\n* Byte-Compiling elisp files...\n"
	${EMACS} -Q --batch -L . -f batch-byte-compile *.el
	@printf "\n* Testing...\n"
	${EMACS} -Q --batch -L . -l test/unit-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f pinyin.elc

emacs_versions:
	@for cmd in emacs emacs-24.4.2 emacs-24.5.2 emacs-25.1.1 emacs-25.3.1; do \
	    make EMACS=$$cmd ;\
	done
# end
