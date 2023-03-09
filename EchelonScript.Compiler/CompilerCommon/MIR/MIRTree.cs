﻿/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Common;

namespace EchelonScript.Compiler.CompilerCommon.MIR;

public unsafe struct MIRTree {
    /// <summary>The MIR bodies in the tree.</summary>
    public ES_Array1D<MIRBody> Bodies;
}