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
using System.Runtime.InteropServices;
using ChronosLib.Pooled;
using EchelonScript.Common.Utilities;

namespace EchelonScript.Common.Data.Types;

[Flags]
public enum ES_FunctionFlags : int {
    /// <summary>The function is a member function of a type, and has an implicit self pointer.</summary>
    Method = 1,
    /// <summary>Function is virtual and may be overridden in derived classes. (May only be used in classes)</summary>
    Virtual = 1 << 1,
    /// <summary>Function has no function body and must be overridden in derived classes. (May only be used in classes)</summary>
    Abstract = 1 << 2,
}

public enum ES_ArgumentKind {
    /// <summary>Passed normally.</summary>
    Normal,
    /// <summary>Passed by reference.</summary>
    Ref,
    /// <summary>Passed as const.</summary>
    In,
    /// <summary>Passed by reference, and requires setting before returning.</summary>
    Out,
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public readonly unsafe struct ES_FunctionData {
    #region ================== Instance fields

    public readonly void* FunctionPointer;

    public readonly ES_FunctionPrototypeInfo* FunctionType;
    public readonly int OptionalArgsCount;

    #endregion

    public ES_FunctionData (void* funcPtr, ES_FunctionPrototypeInfo* functionType, int optArgCount) {
        Debug.Assert (functionType is not null);

        FunctionPointer = funcPtr;
        FunctionType = functionType;
        OptionalArgsCount = optArgCount;
    }
}
