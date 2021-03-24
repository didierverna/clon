### Makefile --- Doc directory

## Copyright (C) 2010, 2011, 2015, 2017, 2021 Didier Verna

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
include $(TOP_DIR)/make/version.make
include $(TOP_DIR)/make/prologue.make

INFO_DIR        := $(PREFIX)/share/info
DOC_DIR	:= $(SHARE)/doc
PROJECT_DOC_DIR := $(DOC_DIR)/$(PROJECT)

MAN   := user enduser
REF   := reference
FILES := $(MAN) $(REF)

TEXI_MAN       := $(MAN:%=%.texi)
TEXI_REF       := $(REF:%=%.texi)
TEXI_FILES     := $(TEXI_MAN) $(TEXI_REF)
INFO_FILES     := $(TEXI_FILES:%.texi=$(PROJECT)-%.info)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)

PDF_MAN   := $(MAN:%=%.pdf)
PDF_REF   := $(REF:%=%.pdf)
PDF_FILES := $(PDF_MAN) $(PDF_REF)

HTML_MAN  := $(MAN)
HTML_REF  := $(REF)
HTML_DIRS := $(HTML_MAN) $(HTML_REF)

DVI_FILES := $(FILES:%=%.dvi)
PS_FILES  := $(FILES:%=%.ps)

CSS := https://sirgazil.bitbucket.io/static/docs/css/texinfo/tip/document.css


all: info pdf html

webref: pdf-webref html-webref

all-formats: all dvi ps

# Auxiliary target to install the PDF and HTML documentation at $(LOCATION),
# excluding the reference manual. This is because different versions of the
# reference are used according to the location (web or not).
install-at: $(PDF_MAN) $(HTML_MAN)
	install -d "$(LOCATION)"
	install -m 644 $(PDF_MAN) "$(LOCATION)"/
	for i in $(HTML_MAN); do			\
	  install -d -m 755 "$(LOCATION)"/$$i;		\
	  install -m 644 $$i/* "$(LOCATION)"/$$i/;	\
	done

install: $(INFO_FILES) $(PDF_FILES) $(HTML_DIRS)
	install -d "$(INFO_DIR)"
	install -m 644 $(ALL_INFO_FILES) "$(INFO_DIR)"/
	for i in $(INFO_FILES); do					\
	  install-info --info-dir="$(INFO_DIR)" $$i;			\
	done
	$(MAKE) install-at LOCATION="$(PROJECT_DOC_DIR)"
	install -m 644 $(PDF_REF) "$(PROJECT_DOC_DIR)"/
	install -d -m 755 "$(PROJECT_DOC_DIR)"/$(HTML_REF)
	install -m 644 $(HTML_REF)/* "$(PROJECT_DOC_DIR)"/$(HTML_REF)/
	for i in $(DVI_FILES) $(PS_FILES); do				\
	  test -f $$i && install -m 644 $$i "$(PROJECT_DOC_DIR)"/;	\
	done

uninstall:
	rm -fr "$(PROJECT_DOC_DIR)"
	for i in $(INFO_FILES); do					\
	  install-info --remove --info-dir="$(INFO_DIR)" $$i;		\
	done
	cd "$(INFO_DIR)" && rm -f $(ALL_INFO_FILES)

info: $(INFO_FILES)

pdf: splash.jpg $(PDF_FILES)
pdf-webref: splash.jpg webreference.pdf

html: $(HTML_DIRS)
html-webref: webreference

dvi: splash.eps $(DVI_FILES)

ps: splash.eps $(PS_FILES)

SPLASH_INSERT := 's/^\@author/\@vskip 2cm\n\@image\{splash,16cm\}\n\@author/'

ifeq ($(CONFIG),)
EVAL_CONFIG :=
else
EVAL_CONFIG := $(SBCL_EVAL) $(CONFIG)
endif

reference.texi: generate.cl
	$(SBCL_PATH) $(EVAL_CONFIG) $(SBCL_LOAD) ./generate.cl
	$(PERL) -pi -e $(SPLASH_INSERT) $@

webreference.texi: generate.cl
	$(SBCL_PATH) $(EVAL_CONFIG) $(SBCL_LOAD) ./generate.cl --web
	$(PERL) -pi -e $(SPLASH_INSERT) $@

$(PROJECT)-%.info: %.texi
	$(MAKEINFO) $<

%.pdf: %.texi
	$(TEXI2DVI) -p $<

$(HTML_DIRS) webreference: %: %.texi
	$(MAKEINFO) --css-ref=$(CSS) --html -o $@ $<
	touch $@

%.dvi: %.texi
	$(TEXI2DVI) $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

%.eps: %.jpg
	$(CONVERT) $< $@

clean:
	-rm *.tmp *.fn* *.vr* *.ky* *.tp* *.cp* *.pg *.toc *.aux *.log *~

distclean: clean
	-rm -fr splash.eps webreference* $(TEXI_REF)			\
		$(HTML_DIRS)						\
		$(ALL_INFO_FILES) $(PDF_FILES) $(DVI_FILES) $(PS_FILES)

$(TEXI_MAN): $(TOP_DIR)/make/version.make
	$(PERL) -pi -e 's/@set VERSION .*/@set VERSION $(LONG_VERSION)/' $@
	$(PERL) -pi -e 's/@set COPYRIGHT_YEARS .*/@set COPYRIGHT_YEARS $(TYPESET_COPYRIGHT_YEARS)/' $@

install-www: $(PDF_MAN) $(HTML_MAN) webref
	$(MAKE) install-at LOCATION="$(W3DIR)"
	install -m 644 webreference.pdf "$(W3DIR)"/$(PDF_REF)
	install -d -m 755 "$(W3DIR)"/$(HTML_REF)
	install -m 644 webreference/* "$(W3DIR)"/$(HTML_REF)/

include $(TOP_DIR)/make/epilogue.make

.PHONY: all webref all-formats		\
	install-at install uninstall	\
	info				\
	pdf pdf-webref			\
	html html-webref		\
	dvi				\
	ps				\
	clean distclean		\
	install-www

### Makefile ends here
