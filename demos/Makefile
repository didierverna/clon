### Makefile --- Demo directory

## Copyright (C) 2010, 2011, 2015 Didier Verna

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

TOP_DIR := ..

include $(TOP_DIR)/make/config.make
include $(TOP_DIR)/make/prologue.make

DEMOS := simple advanced


all: $(DEMOS)
ifeq ($(LISP),ABCL)
# ABCL is java based, so we produce a jar, and a wrapper shell script around
# it.
$(DEMOS): %: %.jar
	echo '#! /bin/sh' > $@
	echo '$(JAVA) -jar $< $${1+$$@}' >> $@
	chmod 755 $@
else ifeq ($(LISP),ECL)
# ECL also has a different scheme for producing executables.
$(DEMOS): %: %.lisp
	$($(LISP)_PATH) $(EVAL_CONFIG) $($(LISP)_DUMP) dump.lisp -- $@
else ifeq ($(LISP),ACL)
# ACL's application delivery is complicated and only available in the
# Enterprise edition, so we just use the dumplisp facility here.
$(DEMOS): %: %.dxl
	echo '#! /bin/sh' > $@
	echo '$(ACL_PATH) -I $< -e "(main)" -- $${1+$$@}' >> $@
	chmod 755 $@
else
# The default way of doing things in pure lisp:
$(DEMOS): %: %.lisp
	$($(LISP)_PATH) $(EVAL_CONFIG) $($(LISP)_DUMP) $<
endif


# ------------------
# ACL specific part:
# ------------------
%.dxl: %.lisp
	$(ACL_PATH) $(EVAL_CONFIG) $(ACL_DUMP) $<


# --------------------------
# Java (ABCL) specific part:
# --------------------------

# This upcases the first letter of $(1):
java_name = $(shell echo $(1) | $(SED) 's/\([a-z]\)\([a-zA-Z0-9]*\)/\u\1\2/')
# This defines how to create foo.jar from its dependencies Foo.txt and
# Foo.class. Note the additional class file ending in $1.class. That's because
# of the Runnable inner class defined by the dumping macro. We need to delete
# it by hand because it's not seen as a target/dependency by make so
# .INTERMEDIATE doesn't work on it.
define jar_rule
$(1).jar: $(call java_name,$(1)).txt $(call java_name,$(1)).class $(1).lisp
	cp $(ABCL_JAR) $$@
	$(JAR) -ufm $$@ $$^ $(call java_name,$(1))\$$$$1.class $(1).lisp
	rm $(call java_name,$(1))\$$$$1.class
endef
# This defines how to create Foo.java from foo.lisp:
define java_rule
$(call java_name,$(1).java): $(1).lisp
	$(ABCL_PATH) $(EVAL_CONFIG) $(ABCL_DUMP) $$<
endef

$(foreach demo,$(DEMOS),$(eval $(call jar_rule,$(demo))))
$(foreach demo,$(DEMOS),$(eval $(call java_rule,$(demo))))
$(foreach demo,$(DEMOS),$(call java_name,$(demo).txt)):
	echo "Main-Class: $(@:%.txt=%)" > $@

%.class: %.java
	$(JAVAC) -cp $(ABCL_JAR) $<


# ---------
# The rest:
# ---------

clean:
	-rm *~ $(DEMOS:%=%.o) # this last bit is for ECL

distclean: clean
	-rm -fr $(DEMOS) $(DEMOS:%=%.jar) $(DEMOS:%=%.exe) $(DEMOS:%=%.dxl)

install:
uninstall:
install-www:

include $(TOP_DIR)/make/epilogue.make

.INTERMEDIATE: $(foreach demo,$(DEMOS), $(call java_name,$(demo)).txt)	 \
	       $(foreach demo,$(DEMOS), $(call java_name,$(demo)).java)	 \
	       $(foreach demo,$(DEMOS), $(call java_name,$(demo)).class)

.PHONY: hack all clean distclean install uninstall install-www

### Makefile ends here
