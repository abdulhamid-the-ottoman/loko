<!--
SPDX-FileCopyrightText: 2022 G. Weinholt

SPDX-License-Identifier: EUPL-1.2+
-->

# Loko Scheme

Loko Scheme is an implementation of the algorithmic language Scheme.
It runs on Linux/amd64, NetBSD/amd64 and on bare amd64 hardware. Both
the R6RS and the R7RS standards are supported.

Loko Scheme is intended to be a platform for application and operating
system development. It is written purely in Scheme and some assembler.
There's no C code at the bottom; Loko is the last turtle.

Loko starts quickly. The Loko REPL currently starts in 1.2 ms ± 0.2 ms
on an AMD 3950X according to [Hyperfine][hyperfine]. Loko does fairly
well in the [R7RS benchmarks][r7rs-benchmarks].

 [hyperfine]: https://github.com/sharkdp/hyperfine
 [r7rs-benchmarks]: https://ecraven.github.io/r7rs-benchmarks/

## Current status

Loko Scheme generally works well enough to be used in applications,
and is being used in at least one application the author knows about.

It passes almost everything in Racket's R6RS test suite and Chibi's
R7RS test suites. Any deviations from the reports is considered to be
a bug.

The current limitations compared to other popular Scheme
implementations are:

* The memory allocator uses a fixed heap size, which can be controlled
  at start with the `LOKO_HEAP` variable.

* There is no foreign function interface.

* There is no support for architectures other than AMD64.

* Inexact numbers use only IEEE single precision.

* The compiler is not invoked by `eval`. Compilation is only
  ahead-of-time, giving statically linked binaries.

(None of the limitations are beyond fixing).

The packages in [Akku](https://akkuscm.org) work for the most part.
Packages that use require implementation-specific extensions are
generally not yet ported to Loko.

## Documentation

[The Loko Scheme Developer's Manual](https://scheme.fail/manual/loko.html) is
available online. It is also available
in [PDF format](https://scheme.fail/manual/loko.pdf).

The manual can also be build from the Texinfo sources with `make
manual`.

## Building

See the section
[Building Loko](https://scheme.fail/manual/loko.html#Building) in
the manual.

## Resources

* The [Loko Scheme](https://scheme.fail/) website.
* The IRC channel `#loko` on [Libera.Chat](https://libera.chat/), but
  `#scheme` also works if the subject is about Scheme in general.
* The Usenet group comp.lang.scheme, available through any Usenet
  provider,
  e.g. [Eternal September](https://www.eternal-september.org/).
* [Loko Scheme on GitLab](https://gitlab.com/weinholt/loko).

## License

Copyright © 2019-2022 G. Weinholt

Licensed under the [EUPL-1.2-or-later](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12).
