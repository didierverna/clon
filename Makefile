### Makefile --- Makefile for Clon's toplevel directory

## Copyright (C) 2009 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Sun May 31 17:13:07 2009
## Last Revision: Sun May 31 17:30:16 2009

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License version 2,
## as published by the Free Software Foundation.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


### Commentary:

## Contents management by FCM version 0.1.

## Please use GNU Make with this makefile.


### Code:

SUBDIRS := src share

include Makefile.cnf
include Makefile.inc

all:
	$(MAKE) gen TARGET=all

gen:
	@for i in $(SUBDIRS) ; do                 \
	   echo "making $(TARGET) in $${i} ..." ; \
	   ( cd $${i} && $(MAKE) $(TARGET) ) ;    \
	 done

.DEFAULT:
	$(MAKE) gen TARGET=$@

.PHONY: all gen local.mak


### Makefile ends here
