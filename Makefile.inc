### Makefile.inc --- Inclusion part

## Copyright (C) 2010, 2011, 2012 Didier Verna.

## Author:     Didier Verna <didier@lrde.epita.fr>
## Maintainer: Didier Verna <didier@lrde.epita.fr>

## This file is part of Clon.

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


### Code:

PROJECT   := clon
PACKAGE   := com.dvlsoft.$(PROJECT)
ASDF_FILE := $(PACKAGE).asd

PERL := perl

SHARE := $(PREFIX)/share

W3DIR := $(HOME)/www/software/lisp/$(PROJECT)

SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_LOAD   := --load
SBCL_DUMP   := --script

CMUCL_CACHE  := cmu
CMUCL_BINLOC := cmu
CMUCL_LOAD   := -load
CMUCL_DUMP   := -noinit -nositeinit $(CMUCL_LOAD)

CCL_CACHE  := ccl
CCL_BINLOC := openmcl
CCL_LOAD   := --load
CCL_DUMP   := --no-init $(CCL_LOAD)

ECL_CACHE  := ecl
ECL_BINLOC := ecl
ECL_LOAD   := -load
ECL_DUMP   := -norc $(ECL_LOAD)

CLISP_CACHE  := clisp
CLISP_BINLOC := clisp
CLISP_LOAD   := -i
CLISP_DUMP   := -norc $(CLISP_LOAD)

ABCL_CACHE  := abcl
ABCL_BINLOC := abcl
ABCL_LOAD   := --load
ABCL_DUMP   := --batch --eval '\
(progn (defvar cl-user::$(PACKAGE).configuration nil)\
       (setf (getf cl-user::$(PACKAGE).configuration :dump) t))' \
	       $(ABCL_LOAD)

$(TOP_DIR)/version.inc: $(TOP_DIR)/version.cl $(TOP_DIR)/$(ASDF_FILE)
	$($(LISP)_PATH) $($(LISP)_LOAD) $(TOP_DIR)/version.cl | tail -2 > $@


### Makefile.inc ends here
