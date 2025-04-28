# Copyright © 2019, 2020, 2021 G. Weinholt
# SPDX-License-Identifier: EUPL-1.2+

ROOT = ../..
export LOKO_SOURCE=$(ROOT)/.akku/lib

ifeq ("$(wildcard /usr/lib/SYSLINUX)", "")
SYSLINUX_LIB := /usr/lib/syslinux/bios
SYSLINUX_MODS_BIOS := /usr/lib/syslinux/bios
else
# These paths work on Debian
SYSLINUX_LIB := /usr/lib/SYSLINUX
SYSLINUX_MODS_BIOS := /usr/lib/syslinux/modules/bios
endif

OVMF_CODE = /usr/share/ovmf/OVMF.fd
OVMF_VARS = /usr/share/OVMF/OVMF_VARS.fd

# Source files to be embedded in the image
EMBED_SRC =

QEMU_OPTIONS = \
  -enable-kvm \
  -hda loko-hdd.img \
  -m 4096 \
  -serial stdio \
  -debugcon vc \
  -M q35 \
  -k sv

all: pc-repl

.akku/env:
	akku install

pc-repl: pc-repl.sps repl-window.sls pointer.scm .akku/env $(ROOT)/loko
	.akku/env $(ROOT)/loko --verbose -feval -ftarget=pc --compile $< --output $@

run: pc-repl loko-hdd.img
	qemu-system-x86_64 $(QEMU_OPTIONS) -nic user,model=virtio-net-pci

# Run with the network attached to tap0
run-tap: pc-repl loko-hdd.img
	qemu-system-x86_64 $(QEMU_OPTIONS) \
	  -nic tap,model=virtio-net-pci,ifname=tap0,script=no,downscript=no \
	  -usb -device piix4-usb-uhci -device usb-kbd -device usb-mouse

# Use OHCI for USB, networking on tap0
run-ohci: pc-repl loko-hdd.img
	qemu-system-x86_64 $(QEMU_OPTIONS) \
	  -nic tap,model=rtl8139,ifname=tap0,script=no,downscript=no \
	  -usb -device pci-ohci -device usb-kbd -device usb-mouse

# Use QEMU as the boot loader
run-qemuboot: pc-repl loko-hdd.img
	qemu-system-x86_64 $(QEMU_OPTIONS) \
	  -kernel pc-repl \
	  -initrd '.akku/lib/zabavno/cpu/x86.sls,.akku/lib/zabavno/cpu/compat.sls,.akku/lib/zabavno/cpu/x86-utils.sls' \
	  -append 'LOKO_LIBRARY_PATH=/boot/.akku/lib CONSOLE=com1'

run-efi: loko-hdd-efi.img OVMF_VARS.fd
	qemu-system-x86_64 -enable-kvm -M pc-q35-2.5 \
	  -drive if=pflash,format=raw,unit=0,file=$(OVMF_CODE),readonly=on \
	  -drive if=pflash,format=raw,unit=1,file=OVMF_VARS.fd \
	  -net none \
	  -hda loko-hdd-efi.img \
	  -debugcon vc -serial vc -m 1024 -boot menu=off

bootpart.img: pc-repl syslinux.cfg
	mkdir -p tmp/boot/syslinux tmp/lib
	cp pc-repl tmp/boot/loko
	cp $(SYSLINUX_MODS_BIOS)/* tmp/boot/syslinux/
	cp syslinux.cfg tmp/boot/syslinux/syslinux.cfg
	cp -rL .akku/lib tmp
	make -C $(ROOT) install DESTDIR=$(CURDIR)/tmp PREFIX=/usr/local
	find tmp/lib tmp/usr -ipath '*:*' -delete
	dd if=/dev/zero of=$@.tmp bs=512 count=1000031
	mformat -N 00000000 -i $@.tmp
	mcopy -i $@.tmp -s tmp/lib tmp/usr "::"
	mcopy -i $@.tmp -s tmp/boot "::boot"
	mmd -i $@.tmp "::src"
	for x in $(EMBED_SRC); do mcopy -i $@.tmp "$$x" "::src"; done
	syslinux --install $@.tmp
	rm -rf tmp
	mv -f $@.tmp $@

loko-hdd.img: bootpart.img
	dd if=bootpart.img of=$@.tmp bs=512 seek=32
	dd if=$(SYSLINUX_LIB)/mbr.bin of=$@.tmp conv=notrunc
	printf "label: dos\nlabel-id: 0x12345678\n\nstart=32, size=1000000, type=6, bootable" | /sbin/sfdisk $@.tmp
	mv -f $@.tmp $@

# On Debian you need the grub-efi-amd64-bin package for this
grubx64.efi:
	grub-mkimage -O x86_64-efi -p /efi/boot -o $@ all_video videoinfo boot cat configfile echo efi_gop efi_uga efifwsetup ext2 fat gfxterm gfxterm_background gfxterm_menu loadenv loopback ls lsefi normal part_gpt search search_fs_file search_fs_uuid search_label test multiboot net efinet tftp serial
.INTERMEDIATE: grubx64.efi

OVMF_VARS.fd:
	cp $(OVMF_VARS) $@

startup.nsh:
	echo grubx64.efi > $@
.INTERMEDIATE: startup.nsh

efipart.img: pc-repl startup.nsh grubx64.efi grub.cfg
	mkdir -p tmp/efi/boot tmp/lib
	cp pc-repl tmp/efi/boot/loko
	cp -rL .akku/lib tmp
	make -C $(ROOT) install DESTDIR=$(CURDIR)/tmp PREFIX=/usr/local
	find tmp/lib tmp/usr -ipath '*:*' -delete
	dd if=/dev/zero of=$@.tmp bs=512 count=100031
	mformat -N 00000000 -i $@.tmp
	mcopy -i $@.tmp -s tmp/lib tmp/usr "::"
	cp grub.cfg startup.nsh grubx64.efi tmp/efi/boot
	mcopy -i $@.tmp -s tmp/efi "::EFI"
	mmd -i $@.tmp "::src"
	for x in $(EMBED_SRC); do mcopy -i $@.tmp "$$x" "::src"; done
	rm -rf tmp
	mv -f $@.tmp $@

loko-hdd-efi.img: efipart.img
	dd if=/dev/zero of=$@.tmp bs=512 count=104127
	printf "label: gpt\nlabel-id: DB4B1068-1B85-4F62-AF65-F456D2710A14\n\nunit: sectors\nfirst-lba: 34\nlast-lba: 104093\nsector-size: 512\n\nstart=2048, size=100031, type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, uuid=0FB1D292-38C6-4123-B910-3310D69C2A9A, name=\"EFI\"" | /sbin/sfdisk $@.tmp
	dd if=$< of=$@.tmp bs=512 count=100031 seek=2048 conv=notrunc
	mv -f $@.tmp $@

clean:
	rm -f bootpart.img loko-hdd.img
	rm -f efipart.img loko-hdd-efi.img
	rm -rf tmp
	rm -f pc-repl
	rm -f OVMF_VARS.fd

distclean:
	rm -rf .akku
