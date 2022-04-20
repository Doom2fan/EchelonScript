/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Runtime.InteropServices;
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCommon.Data.Types;

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "StructData", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_StructData {
    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;
    public readonly ArrayPointer<Pointer<ES_InterfaceData>> InterfacesList;

    #endregion

    internal ES_StructData (
        ES_FullyQualifiedName fullyQualifiedName, ES_AccessModifier accessMod, ES_Identifier sourceUnit,
        ArrayPointer<Pointer<ES_InterfaceData>> interfaces
    ) {
        TypeInfo = new ES_TypeInfo (ES_TypeTag.Struct, accessMod, ES_TypeFlag.None, sourceUnit, fullyQualifiedName);

        InterfacesList = interfaces;
    }
}
