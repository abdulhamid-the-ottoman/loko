<!--
SPDX-FileCopyrightText: 2022 G. Weinholt

SPDX-License-Identifier: EUPL-1.2+
-->

# Version 0.12.1

This is a bug fix release:

* The bootstrapping process now looks for Chez Scheme under several
  different names.
* Fix syslinux path for Arch-based distros.
* Fix several problems with port read operations, especially
  end-of-file handling and one case where get-bytevector-n returned a
  bytevector larger than intended.
* Fix make-enumeration to remove duplicate symbols.
* Fix crash in utf8->string for truncated inputs.
* Fix fuzzing support and update it for AFL++.
* GNUmakefile: loko-prebuilt requires .akku/env.

Thanks to Amirouche and Vadym Kochan for contributing to this release.

# Version 0.12.0

This version fixes bugs, improves performance and adds features.

There is a new graphics system called Valand. It is a simple
compositing window system that takes inspiration from Wayland. The
`bin/pc-repl` program now uses Valand. It starts up with a log window
and a repl window, and it is possible to launch ELF binaries from the
repl by typing `@/path-to-the-binary`. Doom has been ported as a proof
of concept: <https://gitlab.com/weinholt/doomgeneric>.

This version is the first one that is capable of compiling itself
directly on a PC (see commit 73264dba).

This version no longer has any content licensed under CC0-1.0, which
is disfavored by some distributions. For more information,
see: <https://lwn.net/Articles/902410/>.

## Compiler

* Partial unrolling of `memq`.
* Add a shortcut for `(string-length (symbol->string sym))`.

## Runtime

* Faster `gcd` implementation.
* Fast path for `bitwise-bit-field` returns a fixnum.
* Faster code for the `bytevector-...-set!` procedures.
* Faster code for `bitwise-bit-field` and `bitwise-length` for
  negative bignums.
* Faster implementation of `number->string` for binary bases.
  printout could hang should be fixed.
* Faster string output ports.
* The default buffer size for binary ports is now 4096.
* The `read` procedure is faster.
* Memory allocations no longer require a pre-zeroed heap.
* Fix `port-transcoder` for transcoded ports (dc6b6013).
* Fix printing of the `->` symbol in write mode.
* Symbols are now printed without allocating memory.
* Handle pretty printing for all standard R6RS and R7RS forms.

## Kernel

* Logical storage devices with caching (b01d3346). This greatly
  improves file system performance.
* The Linux syscall implementation now supports passing traps to user
  space as signals.
* Some preparations have been made for I/O APIC support.

## Architecture support (amd64)

This version fixes booting on QEMU 7.1.0, which introduced an e820
memory map entry that triggered an assertion in pc startup code.

* Fix a bug in scheduler->process message queuing (81a24e50).
* Open-coding of the non-folding character predicates.
* Assume all data is in the first 2GB of memory space. This reduces
  the size of the code.

# Version 0.11.0

This version is distributed under a different licence than previous
versions. It fixes serious bugs, improves performance and adds
features. Some notable changes follow. See git for a full list of
changes.

## New licence

Loko Scheme is now licensed under the EUPL-1.2-or-later.
See <https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12>
for information on this licence.

The EUPL-1.2 is a copyleft licence with a SaaS clause, but it's more
suitable for Loko Scheme than the GNU AGPL was. The EUPL-1.2 follows a
different legal tradition, wherein the SaaS aspects of the license
apply to Loko Scheme itself; not to other code that happens to be
compiled by Loko. Please refer to the manual for more details
(available at <https://scheme.fail/manual/loko.html#Loko-License>).

The source code is now also compliant with the REUSE
specification: <https://reuse.software/spec/>.

## Compiler

* The `--verbose` flag makes Loko print information about the
  compilation process.

## Runtime

* Fixed a serious bug in number->string for binary bases.
* Fixed the return value of (angle -0.0).
* Faster implementation of fxbit-count.
* The native eol-style is now none. Automatic end-of-line conversion
  is not done for this eol-style.

## Drivers

* There is a new AHCI driver that provides SATA support.
* The video BIOS is copied from the system ROM if there is no
  expansion ROM on the video card. This means framebuffer support is
  available on more legacy PC systems than before.
* Video now works on EFI-based PC systems running GRUB 2, and other
  multiboot bootloaders that pass framebuffer information.
* The virtio-net driver is no longer broken.
* Work has been started on ACPI support, initially with an AML
  interpreter.

## Architecture support (amd64)

* The process vector is now directly available via the r14 register.
  The memory allocation information has been moved into this
  structure, which frees up r13 for register allocation.
* Fixed a crash that could be triggered by using rest arguments when
  there's so little free heap memory that the GC is invoked.

# Version 0.10.0

This version fixes bugs, improves performance and adds features.
Some notable changes follow. See git for a full list of changes.

## Runtime & libraries

* New `string-truncate!` and `bytevector-truncate!` procedures in the
  `(loko)` library.
* Fix an edge case where calling `set-port-position!` after
  encountering an eof would not remove the pending eof.

## Compiler

* Disable the non-standard `(case expr)` syntax.

## Architecture support (amd64)

* Fix `disassemble`. Thanks to Curtis Dunham for reporting that this
  procedure broke in 0.9.0.
* Fix compilation of a special case for `fx<=?`.
* Fix a crash when compiling calls to `apply` with too few arguments.

# Version 0.9.0

This version fixes bugs, improves performance and adds features.
Some notable changes follow. See git for a full list of changes.

## Compiler

* Fixed an edge case in the code generation for (fx<=? a x b).

* Fixed the inlining of `memq` for constant lists of length one.

* R6RS record optimizations have been implemented. They are not yet
  completely optimized. Notably, custom prototypes and constructors
  for inherited record are not yet as efficient as they could be.
  See <https://weinholt.se/articles/record-type-representation-trick/>
  for details on the record representation.

## Architecture support (amd64)

* Various performance improvements, including: open coding of char/u8
  port operations, open coding of all c{a,d}r procedures, and open
  coding of the base record constructor and `vector`.

## Runtime & libraries

* Fixed `textual-port?` and `binary-port?` in `(scheme base)`. They
  now handle any type of object, not just ports.

* Fixed `bytevector->{uint,sint}-list` for size 1.

* Fixed `rational?` for flonums.

* Fixed `flatan` edge cases.

* New primitives for use in drivers: `put-i/o-u8-n`, `put-i/o-u16-n`
  and `put-i/o-u32-n`. These provide access to the repeated I/O port
  write operation in the CPU.

* Hashtables are now faster and no longer leak memory for deleted keys.

* Transcoded ports are faster after some leftover debug code was
  removed.

* `eqv?` is significantly faster because of faster type comparisons.
  It also no longer allocates memory.

* The standard hash procedures are now faster, with hopefully O(1)
  time complexity, even for larger objects. To accomplish this, not
  all bits of information in the input are used.

## Samples

* Added a sample that shows how to use KVM virtualization on Linux.

# Version 0.8.0

This version fixes bugs, improves performance and adds features.
Some notable changes follow. See git for a full list of changes.

## Compiler

* Fixed improper handling of identifiers in top-level programs.

## Runtime & libraries

* Fixed `hashtable-copy` for deleted items.

* Fixed `sqrt` for rational numbers, complex numbers and exact numbers.

* Out of (heap) memory errors are handled as exceptions. In many cases
  it is possible for a program to recover from these situations. If
  there is not enough memory to even raise an exception then the
  (Loko-internal) process is exited.

* The amount of memory used for the heap can be controlled by setting
  the LOKO_HEAP environment variable to the heap size in megabytes.
  This is introduced as a means to adjust the amount of memory used by
  a program. In the current implementation the heap has a fixed size.

* Incorporated fixes for SRFI-19 errata from Takashi Kato and Shiro Kawai
  (see <https://github.com/scheme-requests-for-implementation/srfi-19>).

* The logging library is now available under the SRFI-215 name.

* Added early support for setting up processes from statically linked
  ELF binaries.

## Drivers

* Added a driver for Realtek Gigabit NICs.

* The USB UHCI driver has been rewritten and works.

* Added USB HID support.

* Added USB mass storage support (SCSI over bulk-only transport).

* Added write support in the SCSI block device driver.

## Architecture support (amd64)

* Partial inlining of generic arithmetic.

* Support for multitasking with virtual memory.

* Fix code generation for optimized div and mod.

# Version 0.7.0

This version fixes bugs, improves performance and adds features.

* A bootable hard drive image with the `bin/pc-repl` program is now
  available.

* New experimental TCP/IP stack. The `bin/pc-repl` program now has a
  network REPL that listens to port 23. The TCP/IP stack is also
  possible to use on Linux with `(loko drivers net tun)`.

* Updated SRFI 170 to match the finalized version.

* Fixed `flround` etc to not require SSE 4.1.

* Fixed transcoding when a codepoint straddles buffers.

* Added `pretty-print` to `(loko)`, based on code by Marc Feeley.

* Fixed `time-it*` for CPUs that have a lower TSC resolution.

* Fixed the condition reported when a program calls a non-procedure.

* Improved speed of `symbol->string`, `string->symbol` and various
  list procedures.

* Fixed R6RS `assert` syntax so it returns the true value.

* Fixed how R7RS `include` searches for files.

* Various fixes to the `(pre-srfi processes)` library.

* The RTC driver now has an external API.

* The `bin/pc-repl` program now attempts to set a graphics mode using
  VBE with the Video BIOS from the graphics card.

* Fixed a missed interrupt in the rtl8139 driver. The driver now also
  pads short frames before transmitting them.

See git for a full list of changes.

# Version 0.6.0

Loko Scheme 0.6.0 introduces support for R7RS-small. The release
tarballs now include a pre-built compiler and all dependencies needed
for building Loko.

This version also introduces new features, bug fixes and other
changes:

* Floating point numbers are now printed in decimal or scientific
  format (depending on the exponent).

* Improved tracking of source code location and reporting of arity
  assertions.

* Updated SRFI 170 to draft #10.

* Added SRFI 174 and SRFI 198 (draft #3).

* New `load-program` procedure that can be used to load and run
  top-level programs.

* The `--script` argument now uses script semantics (similar to
  `load`) instead of top-level semantics.

* The new `(loko apropos)` library lets you look up exported names in
  environments and libraries.

* The compiler no longer prints every file and library it is working
  with.

* Fixed an infinite loop in cp0's handling of `not`.

Please be aware that as part of introducing R7RS support, a small
inconsistency with R6RS has been introduced. Vectors are currently
self-evaluating even in R6RS code. This will be fixed in a later
version.

See git for a full list of changes.

# Version 0.5.0

With this version only a single test in the Racket R6RS test suite
fails. There should be no incompatibilities with previous versions.
Here are some of the improvements:

* The `module` syntax is exported in the `(loko)` module, allowing the
  definition of internal modules. This is provided for compatibility
  with existing code.

* The `(rnrs unicode)` library is now completely implemented thanks to
  the use of the R6RS unicode implementation by Abdulaziz Ghuloum and
  R. Kent Dybvig.

* The `(rnrs io ports)` library has received a significant speed
  boost.

* Improved support for multiple return values. Up to six return values
  can be passed directly in registers.

* Loko now uses [laesare](https://akkuscm.org/packages/laesare/) as
  its reader. This means that line and column information is available
  on assertions and procedures. This reader also supports the R7RS
  lexical syntax.

* Loko now checks variable references in `letrec` and `letrec*` to
  ensure that variables are not used before they are defined.

* The .bss section is now generated correctly, so Linux perf is now
  willing to use the symbol table.

* The `bin/pc-repl` program is a new graphical REPL that currently
  runs on virtual machines.

* `map` is now tail recursive and does not build up a large stack,
  which fixes a pathological case in some programs that use `call/cc`
  from inside map.

See git for a full list of changes.
