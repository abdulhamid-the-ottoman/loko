# SPDX-FileCopyrightText: 2022 G. Weinholt
#
# SPDX-License-Identifier: EUPL-1.2+

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := web-server

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) -ftarget=linux --compile $< --output $@

run: $(PROGRAM)
	./$(PROGRAM)

run-dynamic:
	$(LOKO) --program $(PROGRAM).sps

clean:
	rm -f $(PROGRAM)
