# Copyright © 2022 G. Weinholt
# SPDX-License-Identifier: EUPL-1.2+

ROOT = ../..
export LOKO_SOURCE=$(ROOT)/.akku/lib

all: test-internal test-yxskaft

.akku/env:
	akku install

test-yxskaft: test-yxskaft.sps .akku/env $(ROOT)/loko
	.akku/env $(ROOT)/loko -ftarget=linux --compile $< --output $@

run: test-yxskaft
	./test-yxskaft

test-internal: test-internal.sps  $(ROOT)/loko
	.akku/env $(ROOT)/loko -ftarget=linux --compile $< --output $@

test: test-internal
	./test-internal

clean:
	rm -f test-yxskaft test-internal

distclean:
	rm -rf .akku
