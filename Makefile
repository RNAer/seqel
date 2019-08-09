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

# end
