# SPDX-FileCopyrightText: 2019-2022 G. Weinholt
#
# SPDX-License-Identifier: EUPL-1.2+

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

SEABIOS_VERSION = 1.14.0

PROGRAM := kvm-virt

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) -feval -ftarget=linux --compile $< --output $@

run: $(PROGRAM) vgabios.bin bios.bin FD12LITE.img
	./$(PROGRAM)

# For building SeaBIOS

vgabios.bin: seabios-$(SEABIOS_VERSION)/out/vgabios.bin
	cp $< $@
bios.bin: seabios-$(SEABIOS_VERSION)/out/bios.bin
	cp $< $@
seabios-$(SEABIOS_VERSION).tar.gz:
	curl -LO "https://www.seabios.org/downloads/seabios-$(SEABIOS_VERSION).tar.gz"
seabios-$(SEABIOS_VERSION): seabios-$(SEABIOS_VERSION).tar.gz
	tar -xzf $<
seabios-$(SEABIOS_VERSION)/.config: seabios-$(SEABIOS_VERSION) config-seabios
	cp config-seabios seabios-$(SEABIOS_VERSION)/.config
seabios-$(SEABIOS_VERSION)/out/bios.bin: seabios
seabios-$(SEABIOS_VERSION)/out/vgabios.bin: seabios
seabios: seabios-$(SEABIOS_VERSION) seabios-$(SEABIOS_VERSION)/.config
	make -C seabios-$(SEABIOS_VERSION) PYTHON=python3

# For downloading the FreeDOS 1.2 floppy

FLOPPY.img: FD12FLOPPY.zip
	unzip -n $< $@
FD12FLOPPY.zip:
	curl -LO "https://www.ibiblio.org/pub/micro/pc-stuff/freedos/files/distributions/1.2/FD12FLOPPY.zip"

# The FD12LITE image

FD12LITE.img: FD12LITE.zip
	unzip -n $< $@
FD12LITE.zip:
	curl -LO "https://www.ibiblio.org/pub/micro/pc-stuff/freedos/files/distributions/1.2/FD12LITE.zip"

# # These paths work on Debian
# SYSLINUX_LIB = /usr/lib/SYSLINUX
# SYSLINUX_MODS_BIOS = /usr/lib/syslinux/modules/bios

# bootpart.img: #syslinux.cfg
# 	mkdir -p tmp/boot/syslinux
# 	cp $(SYSLINUX_MODS_BIOS)/* tmp/boot/syslinux/
# 	#cp syslinux.cfg tmp/boot/syslinux/syslinux.cfg
# 	dd if=/dev/zero of=$@.tmp bs=512 count=63928
# 	mformat -N 00000000 -i $@.tmp
# 	syslinux --install $@.tmp
# 	mv -f $@.tmp $@

# hdd.img: bootpart.img
# 	dd if=bootpart.img of=$@.tmp bs=512 seek=32
# 	dd if=$(SYSLINUX_LIB)/mbr.bin of=$@.tmp conv=notrunc
# 	printf "label: dos\nlabel-id: 0x88888888\n\nstart=32, size=63928, type=6, bootable" | /sbin/sfdisk $@.tmp
# 	mv -f $@.tmp $@

clean:
	rm -f $(PROGRAM)
	rm -rf seabios-$(SEABIOS_VERSION)
	rm -f debug.txt log.txt
