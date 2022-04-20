/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Runtime.InteropServices;

namespace EchelonScriptCommon.Data.Types;

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "EnumData", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_EnumData {
    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;
    public readonly ES_TypeInfo* BaseType;

    #endregion

    internal ES_EnumData (
        ES_FullyQualifiedName fullyQualifiedName, ES_AccessModifier accessMod, ES_Identifier sourceUnit,
        ES_TypeInfo* baseType
    ) {
        TypeInfo = new ES_TypeInfo (ES_TypeTag.Enum, accessMod, ES_TypeFlag.NoRefs, sourceUnit, fullyQualifiedName);

        BaseType = baseType;
    }
}
