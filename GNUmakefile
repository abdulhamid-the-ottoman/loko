# SPDX-License-Identifier: EUPL-1.2+
# This file is a part of Loko Scheme, an R6RS Scheme system
# Copyright © 2019-2021 G. Weinholt

DESTDIR =
PREFIX = /usr/local
INSTALL = install
INSTALLINFO = install-info
VERSION = $(shell awk -F'"' '/akku-package/ {print $$4}' Akku.manifest)
DIST_TREEISH = HEAD

DEFAULT_TARGET = pc+linux
ifeq ($(origin PREFIX),default)
    GDB_AUTOLOAD_PATH = /usr/share/gdb/auto-load
else ifeq ($(origin GDB_AUTOLOAD_PATH),undefined)
    GDB_AUTOLOAD_PATH = $(PREFIX)/share/gdb/auto-load
endif

export LOKO_HEAP = 1024

#DEFAULT_TARGET = netbsd
#GDB_AUTOLOAD_PATH = /usr/pkg/share/gdb/auto-load

# Chez Scheme
BOOTSTRAPSCHEME = ./scheme-wrapper --program
SCHEMESCRIPT = scheme --program

#BOOTSTRAPSCHEME = ikarus --r6rs-script
#SCHEMESCRIPT = ikarus --r6rs-script

do_subst = sed -e 's,[@]PREFIX[@],$(PREFIX),g' \
               -e 's,[@]VERSION[@],$(VERSION),g' \
               -e 's,[@]DEFAULT_TARGET[@],$(DEFAULT_TARGET),g' \
               -e 's,[@]GDB_AUTOLOAD_PATH[@],$(GDB_AUTOLOAD_PATH),g'

all: loko scheme-script

config.sls: config.sls.in Akku.manifest
	$(do_subst) < config.sls.in > config.sls
	ln -sf ../../../config.sls .akku/lib/loko/config.sls

.akku/env:
	akku install

bootstrap: config.sls loko-prebuilt
loko-prebuilt: .akku/env
	LOKO_SOURCE=.akku/lib .akku/env $(BOOTSTRAPSCHEME) compile-loko.sps
	chmod +x loko-prebuilt.out
	if [ -f loko-prebuilt ]; then mv -f loko-prebuilt loko-prebuilt.old; fi
	mv -f loko-prebuilt.out loko-prebuilt

loko: .akku/env config.sls loko-prebuilt
	LOKO_SOURCE=.akku/lib .akku/env ./loko-prebuilt -ftarget=$(DEFAULT_TARGET) -feval -fcp0-effort-limit=1000 --compile loko.sps --output loko.out
	if [ -f loko ]; then mv -f loko loko.old; fi
	mv -f loko.out loko

# Bootstrap Loko from Loko by compiling compile-loko and having it compile Loko.
compile-loko: config.sls loko compile-loko.sps
	LOKO_SOURCE=.akku/lib .akku/env ./loko -feval --compile compile-loko.sps
.INTERMEDIATE: compile-loko
rebootstrap: compile-loko
	LOKO_SOURCE=.akku/lib .akku/env ./compile-loko
	chmod +x loko-prebuilt.out
	if [ -f loko-prebuilt ]; then mv -f loko-prebuilt loko-prebuilt.old; fi
	mv -f loko-prebuilt.out loko-prebuilt
rebootstrap-eval: config.sls loko compile-loko.sps
	LOKO_HEAP=4096 LOKO_SOURCE=.akku/lib .akku/env ./loko --program compile-loko.sps
	chmod +x loko-prebuilt.out
	if [ -f loko-prebuilt ]; then mv -f loko-prebuilt loko-prebuilt.old; fi
	mv -f loko-prebuilt.out loko-prebuilt

scheme-script: loko
	if [ -f scheme-script ]; then mv -f scheme-script scheme-script.old; fi
	ln loko scheme-script

# Header snarfing (extraction of magic numbers)

linux-snarfer.c: tools/header-snarfer.sps tools/linux.snarf
	cat /lib/modules/$(shell uname -r)/source/include/uapi/asm-generic/errno* | \
	  $(SCHEMESCRIPT) tools/header-snarfer.sps tools/linux.snarf loko arch > $@

netbsd-snarfer.c: tools/header-snarfer.sps tools/netbsd.snarf
	cat /usr/include/sys/errno.h | \
	  $(SCHEMESCRIPT) tools/header-snarfer.sps tools/netbsd.snarf loko arch > $@

net-snarfer.c: tools/net.snarf tools/header-snarfer.sps
	$(SCHEMESCRIPT) tools/header-snarfer.sps $< loko net > $@ </dev/null

linux-snarfer: linux-snarfer.c
netbsd-snarfer: netbsd-snarfer.c
net-snarfer: net-snarfer.c

arch/amd64/linux-numbers.sls: linux-snarfer
	./linux-snarfer > $@
arch/amd64/netbsd-numbers.sls: netbsd-snarfer
	./netbsd-snarfer > $@
net/numbers.sls: net-snarfer
	./net-snarfer > $@

.INTERMEDIATE: linux-snarfer linux-snarfer.c
.INTERMEDIATE: netbsd-snarfer netbsd-snarfer.c
.INTERMEDIATE: net-snarfer net-snarfer.c

# Documentation

manual:: Documentation/manual/loko.info Documentation/manual/loko.html Documentation/manual/loko.pdf

Documentation/manual/loko.info: Documentation/manual/*.texi Documentation/manual/version.texi
	cd Documentation/manual && makeinfo loko

Documentation/manual/loko.html: Documentation/manual/*.texi Documentation/manual/version.texi
	cd Documentation/manual && makeinfo --no-split --html loko

Documentation/manual/loko.pdf: Documentation/manual/*.texi Documentation/manual/version.texi
	cd Documentation/manual && texi2pdf loko.texi

Documentation/manual/version.texi: Documentation/manual/mkversion.sps loko Akku.manifest
	cd Documentation/manual && ../../.akku/env ../../loko --program mkversion.sps

samples:: loko
	$(MAKE) -C samples

# XXX: Requires "akku install --dev wak-fmt" first
fonts:
	.akku/env loko --program tools/bdf2sls.sps lib/font/8x13.bdf '(loko font font-8x13)' > lib/font/font-8x13.sls
	.akku/env loko --program tools/bdf2sls.sps lib/font/6x13.bdf '(loko font font-6x13)' > lib/font/font-6x13.sls

dist: loko-$(VERSION).tar.gz

loko-$(VERSION).tar:
	git archive --format=tar --prefix=loko-$(VERSION)/ $(DIST_TREEISH) > loko-$(VERSION).tar
	rm -rf dist
	mkdir dist
	(cd dist && \
	 tar -xf ../loko-$(VERSION).tar && \
	 cd loko-$(VERSION) && \
	 akku install && \
	 make bootstrap loko; \
	 mv -f loko loko-prebuilt )
	(cd dist && \
	 tar --group=0 --owner=0 -rf ../loko-$(VERSION).tar \
	   loko-$(VERSION)/loko-prebuilt \
	   loko-$(VERSION)/.akku/env \
	   loko-$(VERSION)/.akku/bin/activate \
	   loko-$(VERSION)/.akku/list \
	   loko-$(VERSION)/.akku/notices \
	   loko-$(VERSION)/.akku/lib )
	rm -rf dist

loko-$(VERSION).tar.gz: loko-$(VERSION).tar
	gzip -v9 < $< > $@

.INTERMEDIATE: loko-$(VERSION).tar

distimg: loko-hdd-$(VERSION).img.gz

loko-hdd-$(VERSION).img: loko
	make -C bin/pc-repl loko-hdd.img EMBED_SRC="$(EMBED_SRC)"
	ln bin/pc-repl/loko-hdd.img $@

loko-hdd-$(VERSION).img.gz: loko-hdd-$(VERSION).img
	gzip -v9 < $< > $@

.INTERMEDIATE: loko-hdd-$(VERSION).img

clean:
	rm -f loko loko.out loko.old loko-prebuilt.old
	rm -f scheme-script scheme-script.old
	rm -f config.sls
	rm -f Documentation/manual/version.texi Documentation/manual/loko*.info Documentation/manual/loko.pdf
	rm -f Documentation/manual/loko.aux Documentation/manual/loko.cp Documentation/manual/loko.cps Documentation/manual/loko.html Documentation/manual/loko.log Documentation/manual/loko.toc Documentation/manual/loko.vr Documentation/manual/loko.vrs
	$(MAKE) -C samples clean

install: all
	$(INSTALL) -m 0755 -d   $(DESTDIR)$(PREFIX)/bin
	$(INSTALL) -m 0755 loko $(DESTDIR)$(PREFIX)/bin
# Libraries for users
	$(INSTALL) -m 0755 -d                                  $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/base.loko.sls            $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/base.loko.sls            $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/case-lambda.loko.sls     $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/char.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/complex.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/cxr.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/eval.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/file.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/inexact.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/lazy.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/load.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/process-context.loko.sls $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/r5rs.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/read.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/repl.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/time.loko.sls	       $(DESTDIR)$(PREFIX)/share/r6rs/scheme
	$(INSTALL) -m 0644 lib/scheme/write.loko.sls           $(DESTDIR)$(PREFIX)/share/r6rs/scheme

	$(INSTALL) -m 0755 -d                             $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
	$(INSTALL) -m 0644 arch/amd64/linux-numbers.sls   $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
	$(INSTALL) -m 0644 arch/amd64/linux-syscalls.sls  $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
	$(INSTALL) -m 0644 arch/amd64/netbsd-numbers.sls  $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
	$(INSTALL) -m 0644 arch/amd64/netbsd-syscalls.sls $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/keyboard.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/keymaps.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/mouse.sls             $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/net.sls               $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/rtc.sls               $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/storage.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/utils.sls             $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/virtio.sls            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers

	$(INSTALL) -m 0644 drivers/pci.sls               $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/pci
	$(INSTALL) -m 0644 drivers/pci/roms.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/pci

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/early
	$(INSTALL) -m 0644 drivers/early/debugcon.sls    $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/early
	$(INSTALL) -m 0644 drivers/early/ns8250.sls      $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/early
	$(INSTALL) -m 0644 drivers/early/vga.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/early

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/ahci.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/atapi.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/core.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/drive.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/ide.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/identify.sls      $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/sata.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/net
	$(INSTALL) -m 0644 drivers/net/eepro100.sls      $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/net
	$(INSTALL) -m 0644 drivers/net/rtl8139.sls       $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/net
	$(INSTALL) -m 0644 drivers/net/rtl8169.sls       $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/net
	$(INSTALL) -m 0644 drivers/net/tun.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/net
	$(INSTALL) -m 0644 drivers/net/virtio.sls        $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/net

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/scsi
	$(INSTALL) -m 0644 drivers/scsi/core.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/scsi
	$(INSTALL) -m 0644 drivers/scsi/block.sls        $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/scsi

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/video
	$(INSTALL) -m 0644 drivers/video/bga.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/video
	$(INSTALL) -m 0644 drivers/video/vbe.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/video

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/rtc
	$(INSTALL) -m 0644 drivers/rtc/mc146818.sls      $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/rtc

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/core.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/hid.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/hid-numbers.sls   $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/hid-parser.sls    $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/hid-keyboard.sls  $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/hid-mouse.sls     $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/mass-storage.sls  $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/hub.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/uhci.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/ehci.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/ohci.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2
	$(INSTALL) -m 0644 drivers/ps2/core.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2
	$(INSTALL) -m 0644 drivers/ps2/i8042.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2
	$(INSTALL) -m 0644 drivers/ps2/keyboard.sls      $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2
	$(INSTALL) -m 0644 drivers/ps2/mouse.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2

	$(INSTALL) -m 0644 lib/apropos.sls               $(DESTDIR)$(PREFIX)/share/r6rs/loko
	$(INSTALL) -m 0644 lib/dlists.sls                $(DESTDIR)$(PREFIX)/share/r6rs/loko
	$(INSTALL) -m 0644 lib/match.sls                 $(DESTDIR)$(PREFIX)/share/r6rs/loko
	$(INSTALL) -m 0644 lib/queues.sls                $(DESTDIR)$(PREFIX)/share/r6rs/loko
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/font
	$(INSTALL) -m 0644 lib/font/font-6x13.sls        $(DESTDIR)$(PREFIX)/share/r6rs/loko/font
	$(INSTALL) -m 0644 lib/font/font-8x13.sls        $(DESTDIR)$(PREFIX)/share/r6rs/loko/font

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/net
	$(INSTALL) -m 0644 net/dhcpv4-client.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/net
	$(INSTALL) -m 0644 net/internet.sls              $(DESTDIR)$(PREFIX)/share/r6rs/loko/net
	$(INSTALL) -m 0644 net/numbers.sls               $(DESTDIR)$(PREFIX)/share/r6rs/loko/net
	$(INSTALL) -m 0644 net/tcp.sls                   $(DESTDIR)$(PREFIX)/share/r6rs/loko/net

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/kernel
	$(INSTALL) -m 0644 kernel/binfmt-elf.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/kernel
	$(INSTALL) -m 0644 kernel/osabi-linux.sls        $(DESTDIR)$(PREFIX)/share/r6rs/loko/kernel
	$(INSTALL) -m 0644 kernel/storage.sls            $(DESTDIR)$(PREFIX)/share/r6rs/loko/kernel

	$(INSTALL) -m 0644 valand/valand.sls             $(DESTDIR)$(PREFIX)/share/r6rs/loko
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/valand
	$(INSTALL) -m 0644 valand/valand/drawing.sls     $(DESTDIR)$(PREFIX)/share/r6rs/loko/valand
	$(INSTALL) -m 0644 valand/valand/internal.sls    $(DESTDIR)$(PREFIX)/share/r6rs/loko/valand

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/srfi
	$(INSTALL) -m 0644 srfi/19.loko.sls              $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a19.loko.sls
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a19
	$(INSTALL) -m 0644 srfi/19/time.loko.sls         $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a19

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/srfi
	$(INSTALL) -m 0644 srfi/170.loko.sls             $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a170.loko.sls
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a170
	$(INSTALL) -m 0644 srfi/170/posix.loko.sls       $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a170
	$(INSTALL) -m 0644 srfi/170/linux.loko.sls       $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a170/compat.loko.sls

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/srfi
	$(INSTALL) -m 0644 srfi/174.loko.sls             $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a174.loko.sls
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a198
	$(INSTALL) -m 0644 srfi/198.loko.sls             $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a198.loko.sls
	$(INSTALL) -m 0644 srfi/198/private.loko.sls     $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a198/private.loko.sls
	$(INSTALL) -m 0644 srfi/38.loko.sls              $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a38.loko.sls
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a38
	$(INSTALL) -m 0644 srfi/38/with-shared-structure.loko.sls \
	                                                 $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a38

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/pre-srfi
	$(INSTALL) -m 0644 srfi/pre-srfi/processes.loko.sls \
	                                                 $(DESTDIR)$(PREFIX)/share/r6rs/pre-srfi/processes.loko.sls
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/pre-srfi/processes
	$(INSTALL) -m 0644 srfi/pre-srfi/processes/processlib.scm \
	                                                 $(DESTDIR)$(PREFIX)/share/r6rs/pre-srfi/processes
	$(INSTALL) -m 0644 srfi/pre-srfi/processes/linux.loko.sls \
	                                                 $(DESTDIR)$(PREFIX)/share/r6rs/pre-srfi/processes/compat.loko.sls

# Libraries needed when compiling programs
	(cd .akku/lib; find * -type d -a ! -name scheme | \
	  while read fn; do \
	    $(INSTALL) -m 0755 -d $(DESTDIR)$(PREFIX)/lib/loko/$$fn; \
	  done)
# FIXME: Install only those libraries used by (loko compiler static).
	(cd .akku/lib; find * ! -type d -a \
            ! \( -name '*.chezscheme.sls' -o -name '*.ikarus.sls' -o -wholename 'scheme/*' \) | \
	  while read fn; do \
	    $(INSTALL) -m 0644 $$fn $(DESTDIR)$(PREFIX)/lib/loko/$$fn; \
	  done)
# GDB scripts for debugging Loko and compiled applications
	$(INSTALL) -m 0755 -d $(DESTDIR)$(GDB_AUTOLOAD_PATH)
	$(INSTALL) -m 0644 arch/amd64/loko-gdb.py \
	    $(DESTDIR)$(GDB_AUTOLOAD_PATH)/loko-amd64-$(VERSION)-gdb.py
# Manual page
	$(INSTALL) -m 0755 -d                   $(DESTDIR)$(PREFIX)/share/man/man1
	$(INSTALL) -m 0644 Documentation/loko.1 $(DESTDIR)$(PREFIX)/share/man/man1

install-info: Documentation/manual/loko.info
	mkdir -p $(DESTDIR)$(PREFIX)/share/info
	$(INSTALL) -m 0644 Documentation/manual/loko.info* \
	  $(DESTDIR)$(PREFIX)/share/info
	$(INSTALLINFO) --info-dir='$(DESTDIR)$(PREFIX)/share/info' \
	  '$(DESTDIR)$(PREFIX)/share/info/loko.info'

install-all: install install-info
	ln -f $(DESTDIR)$(PREFIX)/bin/loko $(DESTDIR)$(PREFIX)/bin/scheme-script

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/loko
	rm -rf $(DESTDIR)$(PREFIX)/share/loko
	rm -rf $(DESTDIR)$(PREFIX)/lib/loko
	$(INSTALLINFO) --info-dir='$(DESTDIR)$(PREFIX)/share/info' \
	  --delete '$(DESTDIR)$(PREFIX)/share/info/loko.info' || true
	rm -f $(DESTDIR)$(PREFIX)/share/info/loko.info*
