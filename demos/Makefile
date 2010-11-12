### Makefile --- Demo directory

## Copyright (C) 2010 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Sun May 31 17:24:02 2009
## Last Revision: Sat Jun 12 17:56:14 2010

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

DEMOS := simple advanced

demos: $(DEMOS)

$(DEMOS): %: %.lisp
	$($(LISP)_PATH) $($(LISP)_DUMP) $($(LISP)_LOAD) $<

clean:
	-rm *~

distclean: clean
	-rm -fr $(DEMOS)

.PHONY: all demos clean distclean


### Makefile ends here
