### Makefile --- Makefile for Clon's doc directory

## Copyright (C) 2009 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Tue Jun  2 18:05:20 2009
## Last Revision: Tue Jun  2 18:05:20 2009

## This file is part of Clon.

## Clon is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

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

include ../Makefile.cnf
include ../Makefile.inc

INFO_DIR     := $(PREFIX)/info
DOC_DIR	     := $(SHARE)/doc
CLON_DOC_DIR := $(DOC_DIR)/clon
TEXI_FILES   := $(wildcard *.texi)

INFO_FILES     := $(TEXI_FILES:%.texi=%.info)
PDF_FILES      := $(TEXI_FILES:%.texi=%.pdf)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)
DVI_FILES      := $(TEXI_FILES:%.texi=%.dvi)
PS_FILES       := $(TEXI_FILES:%.texi=%.ps)
HTML_DIRS      := $(TEXI_FILES:%.texi=%_html)


all: info pdf

all-formats: all dvi ps html

install: $(INFO_FILES) $(PDF_FILES)
	install -d $(INFO_DIR)
	install -m 644 $(ALL_INFO_FILES) $(INFO_DIR)/
	for i in $(INFO_FILES); do			\
	  install-info --info-dir=$(INFO_DIR) $$i;	\
	done
	install -d $(CLON_DOC_DIR)
	for i in $(PDF_FILES); do					   \
	  install -m 644 $$i $(CLON_DOC_DIR)/`echo $$i | sed 's/clon-//'`; \
       done

uninstall:
	-rm -fr $(CLON_DOC_DIR)
	for i in $(INFO_FILES); do				\
	  install-info --remove --info-dir=$(INFO_DIR) $$i;	\
	done
	cd $(INFO_DIR) && rm -f $(ALL_INFO_FILES)

info: $(INFO_FILES)

pdf: $(PDF_FILES)

dvi: $(DVI_FILES)

ps: $(PS_FILES)

html: $(HTML_DIRS)

%.info: %.texi
	$(MAKEINFO) $<

%.pdf: %.texi
	$(TEXI2DVI) -p $<

%.dvi: %.texi
	$(TEXI2DVI) $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

$(HTML_DIRS): %_html: %.texi
	$(MAKEINFO) --html -o $@ $<
	touch $@

clean:
	-rm *.tmp *.fn* *.vr* *.ky* *.tp *.cp *.pg *.toc *.aux *.log *~

distclean: clean
	-rm -fr *.info* *.pdf *.dvi *.ps *_html

.PHONY: all all-formats			\
	install uninstall		\
	info pdf dvi ps html		\
	clean distclean


### Makefile ends here
