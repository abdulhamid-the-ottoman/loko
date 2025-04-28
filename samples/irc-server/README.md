<!--
SPDX-FileCopyrightText: 2022 G. Weinholt

SPDX-License-Identifier: EUPL-1.2+
-->

# IRC server sample for Loko Scheme

This sample for Loko Scheme is an IRC server that runs on Linux. Only
the basics of IRC have been implemented. Clients can connect, join
channels and chat. Little else is possible.

The server demonstrates socket programming with SRFI 106 and how to
use fibers to concurrently handle many clients. Note in particular how
it reads and writes to the client sockets using timeouts.

It lacks most of the features of a real IRC server and is not prepared
for the rough world of mirc wars and l33t h4xors.
