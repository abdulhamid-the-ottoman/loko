// SPDX-License-Identifier: EUPL-1.2+
// Copyright © 2022 G. Weinholt

DefinitionBlock ("", "DSDT", 1, "Loko", "LokoACPI", 0x1)
{
    Scope(\_SB) {
        Device(PCI0) {
            Name(_HID, EisaId("PNP0A08"))
            Name(_CID, EisaId("PNP0A03"))
            Name(_UID, Zero)
            Method (_BBN)
            {
                Return (Zero)
            }
        }
    }

    // PCI device 00:01.0, an ISA bridge
    Scope(\_SB.PCI0) {
        Device(ISA) {
            Name(_ADR, 0x00010000)
            OperationRegion(P40C, PCI_Config, 0x60, 0x04)
        }
    }

    Scope(\_SB) {
        Field(PCI0.ISA.P40C, ByteAcc, NoLock, Preserve) {
            PRQ0,   8,
            PRQ1,   8,
            PRQ2,   8,
            PRQ3,   8
        }
    }
}
