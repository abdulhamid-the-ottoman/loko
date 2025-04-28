# SPDX-FileCopyrightText: 2019-2022 G. Weinholt
#
# SPDX-License-Identifier: EUPL-1.2+

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := etherdump

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) -feval --compile $< --output $@

run: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -net nic,model=rtl8139 \
	  -net tap,ifname=tap0,script=no,downscript=no \
	  -append 'LOKO_LIBRARY_PATH=/boot/.akku/lib CONSOLE=com1' \
	  -initrd .akku/lib/text-mode/terminfo/builtins.sls

run-virtio: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -net nic,model=virtio-net-pci \
	  -net tap,ifname=tap0,script=no,downscript=no \
	  -append 'LOKO_LIBRARY_PATH=/boot/.akku/lib CONSOLE=com1' \
	  -initrd .akku/lib/text-mode/terminfo/builtins.sls

run-virtio-bridge: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -netdev tap,helper=/usr/lib/qemu/qemu-bridge-helper,id=hn0 \
	  -device virtio-net-pci,netdev=hn0,id=nic1 \
	  -append 'LOKO_LIBRARY_PATH=/boot/.akku/lib CONSOLE=com1' \
	  -initrd .akku/lib/text-mode/terminfo/builtins.sls

run-eepro100: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -net nic,model=i82559er \
	  -net tap,ifname=tap0,script=no,downscript=no \
	  -append 'LOKO_LIBRARY_PATH=/boot/.akku/lib CONSOLE=com1' \
	  -initrd .akku/lib/text-mode/terminfo/builtins.sls

run-rtl8139: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -net nic,model=rtl8139 \
	  -net tap,ifname=tap0,script=no,downscript=no \
	  -append 'LOKO_LIBRARY_PATH=/boot/.akku/lib CONSOLE=com1' \
	  -initrd .akku/lib/text-mode/terminfo/builtins.sls

run-native: $(PROGRAM)
	.akku/env ./etherdump tap0

clean:
	rm -f $(PROGRAM)
