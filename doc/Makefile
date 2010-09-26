### Makefile --- Doc directory

## Copyright (C) 2010 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Tue Jun  2 18:05:20 2009
## Last Revision: Sat Jun 12 18:00:59 2010

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

include ../Makefile.cnf
include ../Makefile.inc

INFO_DIR     := $(PREFIX)/share/info
DOC_DIR	     := $(SHARE)/doc
CLON_DOC_DIR := $(DOC_DIR)/clon
TEXI_FILES   := user.texi enduser.texi

INFO_FILES     := $(TEXI_FILES:%.texi=clon-%.info)
PDF_FILES      := $(TEXI_FILES:%.texi=%.pdf)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)
DVI_FILES      := $(TEXI_FILES:%.texi=%.dvi)
PS_FILES       := $(TEXI_FILES:%.texi=%.ps)
HTML_DIRS      := $(TEXI_FILES:%.texi=%)


all: info pdf html

ref: info-ref pdf-ref html-ref

webref: pdf-webref html-webref

all-formats: all dvi ps

all-formats-ref: ref dvi-ref ps-ref

install-at: $(PDF_FILES) $(HTML_DIRS)
	install -d "$(LOCATION)"
	install -m 644 $(PDF_FILES) "$(LOCATION)/"
	install -d -m 755 "$(LOCATION)/user"
	install -m 644 user/* "$(LOCATION)/user/"
	install -d -m 755 "$(LOCATION)/enduser"
	install -m 644 enduser/* "$(LOCATION)/enduser/"

install: $(INFO_FILES)
	install -d "$(INFO_DIR)"
	install -m 644 $(ALL_INFO_FILES) "$(INFO_DIR)/"
	for i in $(INFO_FILES); do			\
	  install-info --info-dir="$(INFO_DIR)" $$i;	\
	done
	$(MAKE) install-at LOCATION="$(CLON_DOC_DIR)"

install-ref: clon-reference.info reference.pdf reference
	install -d "$(INFO_DIR)"
	install -m 644 clon-reference.info* "$(INFO_DIR)/"
	install-info --info-dir="$(INFO_DIR)" clon-reference.info
	install -d "$(CLON_DOC_DIR)"
	install -m 644 reference.pdf "$(CLON_DOC_DIR)/"
	install -d -m 755 "$(CLON_DOC_DIR)/reference"
	install -m 644 reference/* "$(CLON_DOC_DIR)/reference/"

uninstall:
	-rm -fr "$(CLON_DOC_DIR)"
	for i in $(INFO_FILES); do				\
	  install-info --remove --info-dir="$(INFO_DIR)" $$i;	\
	done
	cd "$(INFO_DIR)" && rm -f $(ALL_INFO_FILES)

info: $(INFO_FILES)
info-ref: clon-reference.info

pdf: clon.jpg $(PDF_FILES)
pdf-ref: clon.jpg reference.pdf
pdf-webref: clon.jpg webreference.pdf

html: $(HTML_DIRS)
html-ref: reference
html-webref: webreference

dvi: clon.eps $(DVI_FILES)
dvi-ref: clon.eps reference.dvi

ps: clon.eps $(PS_FILES)
ps-ref: clon.eps reference.ps

reference.texi webreference.texi: generate.sh
	./generate.sh
	$(PERL) -pi -e 's/^\@author/\@vskip 2cm\n\@image\{clon,16cm\}\n\@author/' reference.texi webreference.texi

clon-%.info: %.texi
	$(MAKEINFO) $<

%.pdf: %.texi
	$(TEXI2DVI) -p $<

$(HTML_DIRS) reference webreference: %: %.texi
	$(MAKEINFO) --html -o $@ $<
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
	-rm -fr clon.eps webreference* clon-reference* reference*	\
		$(HTML_DIRS)						\
		$(ALL_INFO_FILES) $(PDF_FILES) $(DVI_FILES) $(PS_FILES)

update-version:
	for f in $(TEXI_FILES); do					   \
	  $(PERL) -pi -e 's/@set VERSION .*/@set VERSION $(LONG_VERSION)/' \
	    $$f;							   \
	done

install-www: webref
	$(MAKE) install-at LOCATION="$(W3DIR)"
	install -m 644 webreference.pdf "$(W3DIR)/reference.pdf"
	install -d -m 755 "$(W3DIR)/reference"
	install -m 644 webreference/* "$(W3DIR)/reference/"


.PHONY: all all-formats			\
	install-at install uninstall	\
	info pdf dvi ps html		\
	clean distclean			\
	update-version install-www


### Makefile ends here
