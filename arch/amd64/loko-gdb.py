# -*- mode: python; coding: utf-8 -*-
# SPDX-License-Identifier: EUPL-1.2+
# Copyright © 2019, 2020, 2021 G. Weinholt

"""GDB support for debugging Loko

Sadly the Guile support in gdb was bit rotten when this was written.

"""

import gdb
import struct
import re

# Matches typetags.py and (loko arch amd64 objects)
tag_fixnum = 0b000
tag_box = 0b001
tag_pair = 0b010
tag_procedure = 0b011
tag_string = 0b100
tag_vector = 0b110
tag_bytevector = 0b101
tag_immsym = 0b0111
tag_char = 0b00011111
tag_boolean = 0b10001111
tag_flonum = 0b01001111
tag_singular = 0b00101111

tag_kill_mark = 0b100000001111
tag_void = 0b0000000000001111
tag_seek_mark = 0b1000000000001111 | 1<<63
tag_move = 0b1100000000001111
tag_box_header = 0b0100000000001111

btag_bignum   = 0x40000000
btag_ratnum   = 0x43000000
btag_symbol   = 0x01000000
btag_port     = 0x02000000
btag_rtd      = 0x04000000
btag_pcompnum = 0x46000000
btag_rcompnum = 0x49000000

mask_char  = 0xff
mask_flonum = 0xff
mask_boolean = 0xff
mask_kill_mark = 0xfff
mask_void = 0xffff
mask_seek_mark = 0xffff | 1<<63
mask_move = 0xffff
mask_box_header = 0xffff

mask_boxhdr_length = 0xffffffff00000000
mask_boxhdr_refs_p = 0x0000000080000000
mask_boxhdr_type = 0x000000007f000000
mask_boxhdr_value = 0xff

shift_fixnum = 3
shift_char = 8
shift_boolean = 8
shift_singular = 8
shift_void = 16
shift_move = 16
shift_boxhdr_value = 16
shift_flonum = 32

value_false = tag_boolean

def deref(val, idx):
    """Dereference a non-immediate Scheme object."""
    val = val & ~7
    val_ptr = val.cast(val.type.pointer())
    val_ptr += idx
    value = val_ptr.dereference()
    return value

class LokoPrinter(object):
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return "#<unknown %x>" % int(self.val)


class LokoFixnumPrinter(LokoPrinter):
    def to_string(self):
        return "%d" % (self.val >> shift_fixnum)


class LokoCharPrinter(LokoPrinter):
    def to_string(self):
        return "#\\%c" % (self.val >> shift_char)

class LokoFlonumPrinter(LokoPrinter):
    def to_string(self):
        v, = struct.unpack("=f", struct.pack("=i", (self.val >> shift_flonum)))
        return "%f" % v

class LokoVoidPrinter(LokoPrinter):
    def to_string(self):
        return "#<void #x%x>" % (self.val >> shift_void)


class LokoMovePrinter(LokoPrinter):
    def to_string(self):
        return "#<move #x%x>" % (self.val >> shift_void)


class LokoPairPrinter(LokoPrinter):
    def to_string(self):
        car = deref(self.val, 0)
        cdr = deref(self.val, 1)
        return "(%s . %s)" % (car, cdr)


class LokoVectorPrinter(LokoPrinter):
    def children(self):
        length = deref(self.val, 0) >> shift_fixnum
        for i in range(length):
            yield str(i), deref(self.val, i + 1)

    def to_string(self):
        return "vector"

    def display_hint(self):
        return 'array'


class LokoBytevectorPrinter(LokoPrinter):
    def children(self):
        length = deref(self.val, 0) >> shift_fixnum
        val = (self.val & ~7) + 16
        val_ptr = val.cast(gdb.lookup_type('unsigned char').pointer())
        for i in range(length):
            value = (val_ptr + i).dereference()
            yield str(i), value

    def to_string(self):
        return "vu8@0x%x" % ((self.val & ~7) + 16)

    def display_hint(self):
        return 'array'


class LokoBoxPrinter(object):
    def __init__(self, val, boxheader):
        self.val = val
        self.boxheader = boxheader

    def to_string(self):
        return "#<%s>" % self.boxheader


class LokoGensymPrinter(LokoBoxPrinter):
    def to_string(self):
        return '#<gensym>'

    def children(self):
        header = deref(self.val, 0)
        value = (header >> shift_boxhdr_value) & mask_boxhdr_value

        prefix = deref(self.val, 1)
        yield '0', 'prefix'
        yield '1', prefix

        unique = deref(self.val, 3)
        yield '2', 'unique'
        if unique == value_false:
            yield '3', unique
        else:
            bv = bytes(int(ch) for _, ch in LokoBytevectorPrinter(unique).children())
            yield '3', bv.decode('utf-8')

        yield '4', ('value' if value & 0b1 == 0b1 else 'idx')
        yield '5', deref(self.val, 4)

    def display_hint(self):
        return 'map'


class LokoSymbolPrinter(LokoBoxPrinter):
    def to_string(self):
        name = deref(self.val, 1)
        return LokoStringPrinter(name).to_string()


class LokoPortPrinter(LokoBoxPrinter):
    def to_string(self):
        return '#<port>'

    def display_hint(self):
        return 'port'


class LokoBignumPrinter(LokoBoxPrinter):
    def to_string(self):
        return '#<bignum>'

    def display_hint(self):
        return 'bignum'


class LokoRecordPrinter(LokoBoxPrinter):
    def __init__(self, val, boxheader):
        super(LokoRecordPrinter, self).__init__(val, boxheader)
        self.name = deref(self.boxheader, 1)
        self.record_size = deref(self.boxheader, 2)
        self.parent = deref(self.boxheader, 3)
        self.uid = deref(self.boxheader, 4)
        self.names = deref(self.boxheader, 5)
        self.mutable = deref(self.boxheader, 6)

    def to_string(self):
        return '#[%s]' % (self.name or self.uid)

    def display_hint(self):
        return 'map'

    def children(self):
        i = 1
        def recurse(rtd):
            nonlocal i
            if rtd == value_false:
                return
            parent = deref(rtd, 3)
            names = LokoVectorPrinter(deref(rtd, 5)).children()
            yield from recurse(parent)
            for _, name in names:
                yield "nam%d" % i, str(name)
                yield "val%d" % i, deref(self.val, i)
                i += 1

        yield from recurse(self.boxheader)


class LokoRtdPrinter(object):
    def __init__(self, val, boxheader):
        flags = (boxheader >> shift_boxhdr_value) & mask_boxhdr_value
        self.flags = (('O' if flags&1 else 'o') +
                      ('S' if flags&2 else 's') +
                      ('G' if flags&4 else 'g'))
        self.name = deref(val, 1)
        self.record_size = deref(val, 2) >> shift_fixnum
        self.parent = deref(val, 3)
        self.uid = deref(val, 4)
        self.names = deref(val, 5)
        self.mutable = deref(val, 6)

    def to_string(self):
        return '#<rtd name=%s flags=%s size=%d parent=%s names=%s mutable=%x>' % (
            self.name, self.flags, self.record_size, self.parent,
            self.names, self.mutable)


class LokoRcompnumPrinter(object):
    def __init__(self, val, boxheader):
        self.i = deref(val, 1)
        self.r = deref(val, 2)

    def display_hint(self):
        return 'pcompnum'

    def children(self):
        yield "imag", self.i
        yield "real", self.r


class LokoProcedurePrinter(LokoPrinter):
    def __init__(self, val):
        self.val = val
        self.entry = deref(self.val, 0)
        self.info = deref(self.val, 1)

        self.name = deref(self.info, 0+1)
        self.freevars = deref(self.info, 1+1)
        self.source = deref(self.info, 2+1)

    def to_string(self):
        if self.freevars != 0:
            return "#<closure>"
        else:
            return "#<procedure>"

    def children(self):
        yield 'entry', self.entry.cast(gdb.lookup_type("void").pointer())
        yield 'info', self.info

        # TODO: Enable this once top-level values are not part of
        # closures. For now it's just too spammy.

        # for i in range(self.freevars):
        #     yield "free%i" % i, deref(self.val, 2+i)


class LokoImmsymPrinter(LokoPrinter):
    def to_string(self):
        val = int(self.val) >> 4
        chars = []
        while val:
            idx = (val & ((1 << 5) - 1)) - 1
            val >>= 5
            if val > 0:
                chars.append("abcdefghijklmnopqrstuvwxyz-/<=>"[idx])
            else:
                chars.append("acdefghklmnopqrstvxy!*+-/08<=>?"[idx])

        return "".join(chars)


class LokoStringPrinter(LokoPrinter):
    def to_string(self):
        def string_ref(val, idx):
            val = (val & ~7) + 8
            val_ptr = val.cast(gdb.lookup_type('unsigned int').pointer())
            val_ptr += idx
            value = val_ptr.dereference()
            if value & 0xff == 0x1f:
                return chr(value >> shift_char)
            return '\U0000FFFD'

        chars = ''
        length = deref(self.val, 0) >> shift_fixnum
        for i in range(length):
            chars += string_ref(self.val, i)

        return chars

    def display_hint(self):
        return 'string'


class LokoSingularPrinter(LokoPrinter):
    def to_string(self):
        return self.val


def loko_lookup_function(val):
    if str(val.type) in ('int64_t', 'int', 'long'):
        tag3 = int(val) & 0b111
        if tag3 == tag_fixnum:
            return LokoFixnumPrinter(val)

        if tag3 == tag_pair:
            return LokoPairPrinter(val)
        if tag3 == tag_procedure:
            return LokoProcedurePrinter(val)
        if tag3 == tag_string:
            return LokoStringPrinter(val)
        if tag3 == tag_vector:
            return LokoVectorPrinter(val)
        if tag3 == tag_bytevector:
            return LokoBytevectorPrinter(val)

        if tag3 == tag_box:
            boxheader = deref(val, 0)
            if int(boxheader) & 0b111 == tag_box:
                boxheader2 = deref(boxheader, 0)
                if int(boxheader2) & mask_boxhdr_type == btag_rtd:
                    return LokoRecordPrinter(val, boxheader)
            if int(boxheader) & mask_box_header == tag_box_header:
                if int(boxheader) & mask_boxhdr_type == btag_symbol:
                    if (int(boxheader) >> shift_boxhdr_value) & 0b10:
                        return LokoGensymPrinter(val, boxheader)
                    else:
                        return LokoSymbolPrinter(val, boxheader)
                elif int(boxheader) & mask_boxhdr_type == btag_port:
                    return LokoPortPrinter(val, boxheader)
                elif int(boxheader) & mask_boxhdr_type == btag_bignum:
                    return LokoBignumPrinter(val, boxheader)
                elif int(boxheader) & mask_boxhdr_type == btag_rtd:
                    return LokoRtdPrinter(val, boxheader)
                elif int(boxheader) & mask_boxhdr_type == btag_rcompnum:
                    return LokoRcompnumPrinter(val, boxheader)

            return LokoBoxPrinter(val, boxheader)

        tag4 = int(val) & 0b1111
        if tag4 == tag_immsym:
            return LokoImmsymPrinter(val)

        tag8 = int(val) & 0xff
        if tag8 == tag_singular:
            stype = int(val) >> shift_singular
            if stype == 0:
                return LokoSingularPrinter('()')
            if stype == 1:
                return LokoSingularPrinter('#!eof')
        if tag8 == tag_boolean:
            value = int(val) >> shift_boolean
            if value == 0:
                return LokoSingularPrinter('#f')
            if value == 1:
                return LokoSingularPrinter('#t')
        if tag8 == tag_char:
            return LokoCharPrinter(val)
        if tag8 == tag_flonum:
            return LokoFlonumPrinter(val)

        tag16 = int(val) & 0xffff
        if tag16 == tag_void:
            return LokoVoidPrinter(val)
        if tag16 == tag_move:
            return LokoMovePrinter(val)

        return LokoPrinter(val)

    return None

# Frame unwinding

from gdb.unwinder import Unwinder

class FrameId(object):
    def __init__(self, sp, pc):
        self.sp = sp
        self.pc = pc


class LokoUnwinder(Unwinder):
    def __init__(self, bootstrap_unwind_table):
        super(LokoUnwinder, self).__init__("loko")
        self.bootstrap_unwind_table = bootstrap_unwind_table

    def __call__(self, pending_frame):
        sp = pending_frame.read_register('rsp')
        pc = pending_frame.read_register('rip')
        unwind_info = pending_frame.create_unwind_info(FrameId(sp, pc))

        table = self.bootstrap_unwind_table.cast(
            gdb.lookup_type('unsigned long').pointer())

        # Skip over the locals (unwind)
        table_size = table.dereference() >> shift_fixnum
        for i in range(table_size / 3):
            first_label = (table + 1 + i * 3).dereference() >> 3
            last_label = (table + 2 + i * 3).dereference() >> 3
            frame_size = (table + 3 + i * 3).dereference() >> 3
            if pc >= first_label and pc < last_label:
                # Skip over the return address and the locals
                previous_sp = sp + 8 * (frame_size + 1)
                break
        else:
            previous_sp = sp + 8
        unwind_info.add_saved_register('rsp', previous_sp)

        # Return address
        sp_ptr = (previous_sp-8).cast(gdb.lookup_type('unsigned long').pointer())
        previous_rip = sp_ptr.dereference()
        unwind_info.add_saved_register('rip', previous_rip)

        return unwind_info


gdb.pretty_printers.append(loko_lookup_function)

# For some reason gdb.lookup_global_symbol does not work.
table = gdb.parse_and_eval("&bootstrap_unwind_table")
if table is None:
    print("Failed to get the address of bootstrap_unwind_table")
else:
    gdb.unwinder.register_unwinder(None, LokoUnwinder(table))
