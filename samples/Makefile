# SPDX-FileCopyrightText: 2022 G. Weinholt
#
# SPDX-License-Identifier: EUPL-1.2+

SUBDIRS = \
  driveinfo \
  etherdump \
  hello \
  irc-server \
  lspci \
  lsusb \
  ps2demo \
  video \
  web-server

default: all

$(SUBDIRS)::
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(SUBDIRS)

all clean: $(SUBDIRS)
