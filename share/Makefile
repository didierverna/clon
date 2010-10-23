### Makefile --- Share directory

## Copyright (C) 2010 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Sun Jan  4 14:49:06 2009
## Last Revision: Tue Sep 21 15:06:33 2010

## This file is part of Clon.

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


### Code:

TOP_DIR := ..

include $(TOP_DIR)/Makefile.cnf

all:

include $(TOP_DIR)/Makefile.inc

CLON_SHARE := $(SHARE)/clon
THEMES_DIR := $(CLON_SHARE)/themes


install:
	install -d -m 755 "$(THEMES_DIR)"
	install -m 644 themes/* "$(THEMES_DIR)/"

uninstall:
	-rm -fr "$(CLON_SHARE)"

clean:
	-rm *~ */*~ */*/*~

.PHONY: all install uninstall clean


### Makefile ends here
