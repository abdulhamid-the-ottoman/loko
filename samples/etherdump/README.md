<!--
SPDX-FileCopyrightText: 2022 G. Weinholt

SPDX-License-Identifier: EUPL-1.2+
-->

# Ethernet frame dumper demo

This demonstrates several aspects of Loko in a program that reads
Ethernet frames from a network card and prints them on the
VGA text mode console. If can also run directly on Linux.

You can try it on real hardware if you have an RTL8139 lying around.
To run this in QEMU you'll need to create a tap0 network device:

```
$ sudo tunctl -t tap0 -u $USER
$ sudo ip a a 10.11.12.1/24 dev tap0
$ sudo ip link set tap0 up
```

Then type `make run` to get the sample going in QEMU. Use `make
run-virtio` to use the (much more efficient) PCI virtio network card
instead.

If you happen to have have bridging configured with QEMU then you can
try `make run-virtio-bridge` to access your real network.

Try this if you don't see any traffic at all on tap0:

```
sudo arp -i tap0 -s 10.11.12.2 aa:bb:cc:dd:ee:ff
ping 10.11.12.2
```

To use this program on Linux, follow the above but use `make
run-native`. Be prepared that Ctrl-C doesn't work at the moment.
