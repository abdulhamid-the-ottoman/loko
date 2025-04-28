<!--
SPDX-FileCopyrightText: 2022 G. Weinholt

SPDX-License-Identifier: EUPL-1.2+
-->

# Loko Scheme ready to run on a PC

This is a graphical REPL that can run on real machines if the VBE
graphics setup works correctly (big if).

Building the disk image requires mtools and syslinux.

When all is said and done, you get a REPL running on a graphical
framebuffer and you should be able to import and use the libraries
from `.akku/lib`.

If your system has an RTL8139 or Intel e100 network card then you also
get networking. Currently this means you get a REPL on port 23.

This is all very rough in the edges but should work if you have the
right machine. You can also cheat and use QEMU: `make run`, or just
boot the disk image in another emulator.
