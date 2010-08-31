### Makefile --- Toplevel directory

## Copyright (C) 2010 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Sun May 31 17:13:07 2009
## Last Revision: Sat Jun 12 17:50:23 2010

## This file is part of CLon

## Clon is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License version 3,
## as published by the Free Software Foundation.

## Clon is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


### Commentary:

## Contents management by FCM version 0.1.

## Please use GNU Make with this makefile.


### Code:

SUBDIRS := src sbcl  \
	   share doc \
	   demo

include Makefile.cnf
include Makefile.inc

SYSTEMS_DIR := $(SHARE)/common-lisp/systems
ASDF_FILE   := com.dvlsoft.clon.asd


all:
	$(MAKE) gen TARGET=all

install:
	ln -fs "`pwd`/$(ASDF_FILE)" "$(SYSTEMS_DIR)/"
	$(MAKE) gen TARGET=install

uninstall:
	-rm -f "$(SYSTEMS_DIR)/$(ASDF_FILE)"
	$(MAKE) gen TARGET=uninstall

clean:
	-rm *~
	$(MAKE) gen TARGET=clean

distclean:
	$(MAKE) clean
	-rm -fr sbcl-* **/sbcl-*
	$(MAKE) gen TARGET=clean

update-version:
	$(MAKE) gen TARGET=update-version

gen:
	@for i in $(SUBDIRS) ; do                 \
	   echo "making $(TARGET) in $${i} ..." ; \
	   ( cd $${i} && $(MAKE) $(TARGET) ) ;    \
	 done

.DEFAULT:
	$(MAKE) gen TARGET=$@

.PHONY: all install uninstall clean update-version gen


### Makefile ends here
