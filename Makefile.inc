### Makefile.inc --- Inclusion part

## Copyright (C) 2010, 2011 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>

## This file is part of Clon.

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
