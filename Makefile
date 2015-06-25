### Makefile --- Toplevel directory

## Copyright (C) 2010-2012, 2015 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

## This file is part of Clon

## Permission to use, copy, modify, and distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies.

## THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
## WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
## MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
## ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
## WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
## ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
## OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


### Commentary:

## Contents management by FCM version 0.1.

## Please use GNU Make with this makefile.


### Code:

# Needed in include.make
TOP_DIR := .

include make/config.make
hack: all
include make/include.make
ifeq ($(LISP),CLISP)
  include make/clisp.make
endif
include make/version.make

SUBDIRS   := setup core termio demos share doc
DIST_NAME := $(PROJECT)-$(SHORT_VERSION)
TARBALL   := $(DIST_NAME).tar.gz
SIGNATURE := $(TARBALL).asc


all:
	$(MAKE) gen TARGET=all
	$(MAKE) INSTALL

all-formats dvi ps ref all-formats-ref dvi-ref ps-ref:
	cd doc && $(MAKE) $@

# Needed because we have an INSTALL file which fucks up the gen mechanism
# (remember that Mac OSX is case-insensitive).
install:
	$(MAKE) gen TARGET=install

uninstall:
	$(MAKE) gen TARGET=uninstall

clean:
	-rm *~
	$(MAKE) gen TARGET=clean

distclean: clean
	$(MAKE) gen TARGET=distclean
	-rm -f .clisp.cnf
	-rm *.tar.gz *.tar.gz.asc
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
<? lref (\"$(PROJECT)/attic/$(PROJECT)-$(SHORT_VERSION).tar.gz\", \
	 contents (\"Dernière version\", \"Latest version\")); ?> \
| \
<? lref (\"$(PROJECT)/attic/$(PROJECT)-$(SHORT_VERSION).tar.gz.asc\", \
	 contents (\"Signature GPG\", \"GPG Signature\")); ?>" \
	  > "$(W3DIR)/latest.txt"
	chmod 644 "$(W3DIR)/latest.txt"
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

INSTALL: doc/$(PROJECT)-user.info
	info --file=./doc/$(PROJECT)-user.info	\
	     -n Installation			\
	     -n Configuration			\
	     -n 'Non-ANSI Features'		\
	     -n 'Supported Platforms'		\
	     --output=$@
	perl -pi -e 's/^File:.*\n//g' $@

doc/$(PROJECT)-user.info:
	cd doc && $(MAKE) $(PROJECT)-user.info

$(TARBALL):
	git archive --format=tar --prefix=$(DIST_NAME)/	\
	    --worktree-attributes HEAD			\
	  | gzip -c > $@

$(SIGNATURE): $(TARBALL)
	gpg -b -a $<

.DEFAULT:
	$(MAKE) gen TARGET=$@

.PHONY: hack all						\
	all-formats dvi ps ref all-formats-ref dvi-ref ps-ref	\
	install install-ref uninstall				\
	clean distclean						\
	tag tar gpg dist install-www				\
	update-version						\
	gen


### Makefile ends here
