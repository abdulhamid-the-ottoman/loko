// SPDX-License-Identifier: EUPL-1.2+
// Copyright © 2022 G. Weinholt

DefinitionBlock ("", "DSDT", 1, "Loko", "LokoACPI", 0x1)
{
        Method (\A0) {
            Return (1)
        }
        Method (\A1, 1) {
            Return (Arg0+1)
        }
        Method (\A2, 2) {
            Return (Arg0+Arg1+1)
        }
        Method (\A3, 3) {
            Return (Arg0+Arg1+Arg2+1)
        }
        Method (\A4, 4) {
            Return (Arg0+Arg1+Arg2+Arg3+1)
        }
        Method (\A5, 5) {
            Return (Arg0+Arg1+Arg2+Arg3+Arg4+1)
        }
        Method (\A6, 6) {
            Return (Arg0+Arg1+Arg2+Arg3+Arg4+Arg5+1)
        }
        Method (\A7, 7) {
            Return (Arg0+Arg1+Arg2+Arg3+Arg4+Arg5+Arg6+1)
        }
        Method (TEST, 0) {
            Debug = \A0 ()
            Debug = \A1 (1)
            Debug = \A2 (1,2)
            Debug = \A3 (1,2,3)
            Debug = \A4 (1,2,3,4)
            Debug = \A5 (1,2,3,4,5)
            Debug = \A6 (1,2,3,4,5,6)
            Debug = \A7 (1,2,3,4,5,6,7)
        }
}
