# SPDX-FileCopyrightText: 2019-2022 G. Weinholt
#
# SPDX-License-Identifier: EUPL-1.2+

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := driveinfo

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) -ftarget=pc --compile $< --output $@

run: $(PROGRAM) testdisk.img testdisc.iso
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -hda testdisk.img \
	  -cdrom testdisc.iso

run-usb: $(PROGRAM) testdisk.img testdisc.iso
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -usb \
	  -drive id=my_usb_disk,file=testdisk.img,if=none \
	  -device usb-storage,drive=my_usb_disk
## FIXME: Can QEMU emulate a USB CD-ROM drive?

testpart.img: README.md
	dd if=/dev/zero of=$@ bs=512 count=2880
	mformat -f 1440 -i $@
	mcopy -i $@ $^ "::"
.INTERMEDIATE: testpart.img

testdisk.img: testpart.img
	dd if=testpart.img of=testdisk.img bs=512 seek=1
	echo "start=1, size=2880, type=1" | /sbin/sfdisk testdisk.img

testdisc.iso: README.md
	xorrisofs -r -J -o ./testdisc.iso ./README.md

clean:
	rm -f $(PROGRAM)
