# SPDX-FileCopyrightText: 2019-2022 G. Weinholt
#
# SPDX-License-Identifier: EUPL-1.2+

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := hello

all: $(PROGRAM) just-hello

$(PROGRAM): $(PROGRAM).sps $(LOKO)
	LOKO_SOURCE=$(LOKO_SOURCE) $(LOKO) --compile $< --output $@

just-hello: just-hello.sps $(LOKO)
	LOKO_SOURCE=$(LOKO_SOURCE) $(LOKO) -ftarget=linux -ffreestanding --compile $< --output $@

run: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 1024 -serial stdio -debugcon vc

run-native: $(PROGRAM)
	./$(PROGRAM)

run-just-hello: just-hello
	./just-hello

run-dynamic:
	qemu-system-x86_64 -enable-kvm -kernel $(LOKO) -m 1024 -serial stdio -debugcon vc \
	  -append 'LOKO_LIBRARY_PATH=/boot -- --program /boot/hello.sps' \
	  -initrd 'hello-lib.sls,hello.sps'

clean:
	rm -f $(PROGRAM) just-hello
