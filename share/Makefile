### Makefile --- Clon share/ infrastructure

## Copyright (C) 2009 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Sun Jan  4 14:49:06 2009
## Last Revision: Sun Jan  4 14:49:06 2009

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

SYSTEM := $(shell uname)

ifeq ($(SYSTEM), Darwin)
SHARE := /Library/Application\ Support
CLON_SHARE := $(SHARE)/Clon
THEMES := $(CLON_SHARE)/Themes
else
SHARE := /usr/local/share
CLON_SHARE := $(SHARE)/clon
THEMES := $(CLON_SHARE)/themes
endif

all:

install:
	install -d -m 755 $(THEMES)
	install -m 644 themes/* $(THEMES)/

### Makefile ends here
