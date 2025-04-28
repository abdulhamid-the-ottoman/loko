# SPDX-FileCopyrightText: 2022 G. Weinholt
#
# SPDX-License-Identifier: EUPL-1.2+

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

programs := $(patsubst %.sps,%,$(wildcard *.sps))

all: $(programs)

$(programs): %: %.sps $(LOKO)
	LOKO_SOURCE=$(LOKO_SOURCE) $(LOKO) --compile $< --output $@

clean:
	rm -f $(programs)
