# SPDX-FileCopyrightText: 2019-2022 G. Weinholt
#
# SPDX-License-Identifier: EUPL-1.2+

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := ps2demo

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) -ftarget=pc --compile $< --output $@

run: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio -append CONSOLE=debug

clean:
	rm -f $(PROGRAM)
