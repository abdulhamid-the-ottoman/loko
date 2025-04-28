<!--
SPDX-FileCopyrightText: 2022 G. Weinholt

SPDX-License-Identifier: EUPL-1.2+
-->

# lsusb for Loko

This sample scans the PCI bus for a UHCI controller and starts the
driver. It then listens to events from the UHCI driver about new
devices. When new devices show up, it fetches their descriptors and
prints them.
