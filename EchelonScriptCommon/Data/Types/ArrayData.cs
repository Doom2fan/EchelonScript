/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;

namespace EchelonScriptCommon.Data.Types;

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ES_ArrayData {
    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;

    public readonly ES_TypeInfo* ElementType;
    public readonly int Rank;

    #endregion

    public ES_ArrayData (ES_FullyQualifiedName fullyQualifiedName, [NotNull] ES_TypeInfo* elemType, int rank) {
        TypeInfo = new (ES_TypeTag.Array, ES_AccessModifier.Public, ES_TypeFlag.None, ES_Identifier.Empty, fullyQualifiedName);
        TypeInfo.RuntimeSize = sizeof (void*) + sizeof (int);

        ElementType = elemType;
        Rank = rank;
    }
}
