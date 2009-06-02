### Makefile --- Makefile for Clon's src directory

## Copyright (C) 2009 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Sun May 31 17:24:02 2009
## Last Revision: Sun May 31 17:24:02 2009

## This file is part of Clon.

## Clon is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## Clon is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


### Commentary:

## Contents management by FCM version 0.1.


### Code:

include ../Makefile.cnf
include ../Makefile.inc

SYSTEMS_DIR := $(SHARE)/common-lisp/systems
ASDF_FILE   := clon.asd

all:

install:
	ln -fs $(ASDF_FILE) $(SYSTEMS_DIR)/

uninstall:
	-rm -f $(SYSTEMS_DIR)/$(ASDF_FILE)

clean:
	-rm *~

distclean: clean

.PHONY: all install uninstall clean distclean


### Makefile ends here
