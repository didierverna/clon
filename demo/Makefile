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
include $(TOP_DIR)/Makefile.inc

# #### NOTE: at least for the demo program, we do want the debugger enabled,
# (to demonstrate the restarts) so I can't use the --script option here.
# #### PORTME:
all:
	CC=$(CC) sbcl \
	--noinform --disable-ldb --lose-on-corruption --end-runtime-options \
	--no-userinit --no-sysinit --load demo.lisp

clean:
	-rm *~

distclean: clean
	-rm -fr sbcl-* # for ASDF Binary Locations
	-rm -fr demo

.PHONY: all clean distclean


### Makefile ends here
