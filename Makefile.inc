### Makefile.inc --- Inclusion part

## Copyright (C) 2010 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Tue Jun  2 17:48:53 2009
## Last Revision: Sat Jun 12 17:53:10 2010

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

PERL := perl

SHARE := $(PREFIX)/share

W3DIR := $(HOME)/www/software/lisp/clon

SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_DUMP   := --disable-ldb --lose-on-corruption	\
	       --end-runtime-options --no-userinit --no-sysinit
SBCL_LOAD   := --load

CMUCL_CACHE  := cmu
CMUCL_BINLOC := cmu
CMUCL_DUMP   := -noinit -nositeinit
CMUCL_LOAD   := -load

CCL_CACHE  := ccl
CCL_BINLOC := openmcl
CCL_DUMP   := --no-init
CCL_LOAD   := --load

ECL_CACHE  := ecl
ECL_BINLOC := ecl
ECL_DUMP   := -norc
ECL_LOAD   := -load

CLISP_CACHE  := clisp
CLISP_BINLOC := clisp
CLISP_DUMP   := -norc
CLISP_LOAD   := -i

ABCL_CACHE  := abcl
ABCL_BINLOC := abcl
ABCL_DUMP   := --batch --eval '(defvar cl-user::com.dvlsoft.clon.dump t)'
ABCL_LOAD   := --load

$(TOP_DIR)/version.inc: $(TOP_DIR)/version.cl $(TOP_DIR)/com.dvlsoft.clon.asd
	$($(LISP)_PATH) $($(LISP)_LOAD) $(TOP_DIR)/version.cl | tail -2 > $@


### Makefile.inc ends here
