/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScript.Common;

namespace EchelonScript.Compiler.CompilerCommon.MIR;

public class MIRADTException : Exception {
    internal MIRADTException () : base ("The ADT is not of the requested kind.") { }
}

public unsafe struct MIRString {
    /// <summary>The string's length.</summary>
    public int Length;
    /// <summary>The string's characters as UTF-8. Immutable.</summary>
    public ES_ImmutArray1D<byte> Chars;
}
