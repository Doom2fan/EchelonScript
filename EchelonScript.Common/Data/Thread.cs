/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using ES_ArrayIndexBase = System.Int32;

namespace EchelonScript.Common.Data;

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ES_ThreadHandle {
    private ES_ThreadData* threadPtr;

    [ES_ExcludeFromStackTrace]
    [DebuggerNonUserCode]
    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public ES_StackOffset Add (ES_StackOffset basePos, ES_StackOffset offs) {
        var ret = basePos.Value + offs.Value;
        if (ret >= threadPtr->StackSize)
            throw new StackOverflowException ($"Offset = {offs.Value}, stack size = {threadPtr->StackSize}.");

        return ret;
    }
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ES_ThreadData {
    public ES_StackOffset StackFrame;
    public nint StackSize;
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ES_StackOffset {
    public ES_ArrayIndexBase Value;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static implicit operator ES_ArrayIndexBase (ES_StackOffset idx) => idx.Value;
    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static implicit operator ES_StackOffset (ES_ArrayIndexBase idx) => new () { Value = idx };
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ES_StackFrame {
    public ES_StackOffset PreviousFrame;

    public ES_StackOffset ArgsList;
    public int ArgsCount;

    public ES_StackOffset RefsList;
    public int RefsCount;
}

