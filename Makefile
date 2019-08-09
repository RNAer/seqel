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

package: $(PACKAGE_DIR)
	tar cvf /tmp/$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" --exclude="*.elc" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	cp -r * $@

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)

# end
