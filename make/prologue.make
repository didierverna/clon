### prologue.make --- Prologue Makefile

## Copyright (C) 2010-2012, 2015, 2021 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

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
PACKAGE   := net.didierverna.$(PROJECT)
ASDF_FILE := $(PACKAGE).asd

TYPESET_COPYRIGHT_YEARS := $(subst -,--,$(COPYRIGHT_YEARS))

PERL := perl

SHARE := $(PREFIX)/share

W3DIR := $(HOME)/Documents/Science/Sites/lrde/software/lisp/$(PROJECT)
NET_DIR := $(HOME)/Documents/Science/Sites/didierverna.net/content/projects/$(PROJECT)

SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_LOAD   := --load
SBCL_EVAL   := --eval
SBCL_DUMP   := $(SBCL_LOAD)

CMUCL_CACHE  := cmu
CMUCL_BINLOC := cmu
CMUCL_LOAD   := -load
CMUCL_EVAL   := -eval
CMUCL_DUMP   := $(CMUCL_LOAD)

CCL_CACHE  := ccl
CCL_BINLOC := openmcl
CCL_LOAD   := --load
CCL_EVAL   := --eval
CCL_DUMP   := $(CCL_LOAD)

ECL_CACHE  := ecl
ECL_BINLOC := ecl
ECL_LOAD   := -load
ECL_EVAL   := -eval
ECL_DUMP   := $(ECL_LOAD)

CLISP_CACHE  := clisp
CLISP_BINLOC := clisp
CLISP_LOAD   := -i
CLISP_EVAL   := -x
CLISP_DUMP   := $(CLISP_LOAD)

ABCL_CACHE  := abcl
ABCL_BINLOC := abcl
ABCL_LOAD   := --load
ABCL_EVAL   := --eval
#### NOTE: multiple usage of the eval option to avoid a funcall/intern mess.
ABCL_DUMP   := --batch						   \
	       $(ABCL_EVAL) '(require "asdf")'			   \
	       $(ABCL_EVAL) '(asdf:load-system :$(PACKAGE).setup)' \
	       $(ABCL_EVAL) '($(PACKAGE).setup:configure :dump t)' \
	       $(ABCL_LOAD)

ACL_CACHE  := acl
ACL_BINLOC := acl
ACL_LOAD   := -L
ACL_EVAL   := -e
ACL_DUMP   := $(ACL_LOAD)

LW_CACHE  := lw
LW_BINLOC := lw
LW_LOAD   := -load
LW_EVAL   := -eval
LW_DUMP   := $(LW_LOAD)

BINLOC := $($(LISP)_BINLOC)

ifeq ($(RESTRICTED),t)
#### NOTE: multiple usage of the eval option to avoid a funcall/intern mess.
CONFIG_1 := '(require "asdf")'
CONFIG_2 := '(asdf:load-system :$(PACKAGE).setup)'
CONFIG_3 := '($(PACKAGE).setup:configure :restricted t)'
EVAL_CONFIG := $($(LISP)_EVAL) $(CONFIG_1) \
	       $($(LISP)_EVAL) $(CONFIG_2) \
	       $($(LISP)_EVAL) $(CONFIG_3)
else
CONFIG_1 :=
CONFIG_2 :=
CONFIG_3 :=
EVAL_CONFIG :=
endif

### prologue.make ends here
