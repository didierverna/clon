### Makefile --- Toplevel directory

## Copyright (C) 2010, 2011 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>

## This file is part of CLon

## Redistribution and use in source or binary form, with or without
## modification, are permitted provided that the following conditions are met:

## Redistributions of source code must retain the above copyright notice, this
## list of conditions and the following disclaimer.

## Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.

## Neither the names of the authors or copyright holders, nor the names of any
## contributor or organization may be used to endorse or promote products
## derived from Clon without specific prior written permission.

## CLON IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
## EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
## LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
## CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
## SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
## INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
## CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
## ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
## POSSIBILITY OF SUCH DAMAGE.


### Commentary:

## Contents management by FCM version 0.1.

## Please use GNU Make with this makefile.


### Code:

TOP_DIR := .

include Makefile.cnf

hack: all

include Makefile.inc
include version.inc


SUBDIRS     := src sbcl clisp	\
	       share doc	\
	       demos
SYSTEMS_DIR := $(SHARE)/common-lisp/systems
ASDF_FILE   := com.dvlsoft.clon.asd

DIST_NAME := clon-$(SHORT_VERSION)
TARBALL   := $(DIST_NAME).tar.gz
SIGNATURE := $(TARBALL).asc

install-system:
	ln -fs "`pwd`/$(ASDF_FILE)" "$(SYSTEMS_DIR)/"

all-formats dvi ps ref all-formats-ref dvi-ref ps-ref:
	cd doc && $(MAKE) $@

# Needed because we have an INSTALL file which fucks up the gen mechanism
# (remember that Mac OSX is case-insensitive).
install:
	$(MAKE) gen TARGET=install

uninstall:
	-rm -f "$(SYSTEMS_DIR)/$(ASDF_FILE)"
	$(MAKE) gen TARGET=uninstall

clean:
	-rm *~
	$(MAKE) gen TARGET=clean

# #### NOTE: be sure to propagate to the subdirs first, otherwise, version.inc
# will keep on being reconstructed.
distclean: clean
	$(MAKE) gen TARGET=distclean
	-rm *.tar.gz *.tar.gz.asc
	-rm -fr version.inc
	-rm -fr $($(LISP)_BINLOC)-*
	-rm -fr "${HOME}"/.cache/common-lisp/$($(LISP)_CACHE)-*"`pwd`"

tag:
	git tag -a -m 'Version $(LONG_VERSION)' 'version-$(SHORT_VERSION)'

tar: $(TARBALL)
gpg: $(SIGNATURE)
dist: tar gpg

install-www: dist
	-install -m 644 $(TARBALL)   "$(W3DIR)/attic/"
	-install -m 644 $(SIGNATURE) "$(W3DIR)/attic/"
	echo "\
<? lref (\"clon/attic/clon-$(SHORT_VERSION).tar.gz\", \
	 contents (\"Dernière version\", \"Latest version\")); ?> \
| \
<? lref (\"clon/attic/clon-$(SHORT_VERSION).tar.gz.asc\", \
	 contents (\"Signature GPG\", \"GPG Signature\")); ?>" \
	  > "$(W3DIR)/latest.txt"
	chmod 644 "$(W3DIR)/latest.txt"
	git push --tags "$(W3DIR)/clon.git" :
	$(MAKE) gen TARGET=install-www
	cd "$(W3DIR)"					\
	  && ln -fs attic/$(TARBALL) latest.tar.gz	\
	  && ln -fs attic/$(SIGNATURE) latest.tar.gz.asc

update-version:
	cd doc && $(MAKE) $@

gen:
	@for i in $(SUBDIRS) ; do                 \
	   echo "making $(TARGET) in $${i} ..." ; \
	   ( cd $${i} && $(MAKE) $(TARGET) ) ;    \
	 done

$(TARBALL):
	git archive --format=tar --prefix=$(DIST_NAME)/	\
	    --worktree-attributes HEAD			\
	  | gzip -c > $@

$(SIGNATURE): $(TARBALL)
	gpg -b -a $<

.DEFAULT:
	$(MAKE) gen TARGET=$@

.PHONY: hack							\
	all-formats dvi ps ref all-formats-ref dvi-ref ps-ref	\
	install-system install install-ref uninstall		\
	clean distclean						\
	tag tar gpg dist install-www				\
	update-version						\
	gen


### Makefile ends here
