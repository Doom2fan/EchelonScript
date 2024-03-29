﻿/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Runtime.InteropServices;

namespace EchelonScriptCommon.Data.Types;

public enum ES_IntSize : int {
    Int8  = 0,
    Int16 = 1,
    Int32 = 2,
    Int64 = 3,
}

public enum ES_FloatSize : int {
    Single = 0,
    Double = 1,
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "IntData", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_IntData {
    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;
    public readonly ES_IntSize IntSize;
    public readonly bool Unsigned;

    #endregion

    public ES_IntData (
        ES_FullyQualifiedName fullyQualifiedName,
        ES_IntSize size, bool unsigned
    ) {
        TypeInfo = new ES_TypeInfo (ES_TypeTag.Int, ES_AccessModifier.Public, ES_TypeFlag.NoRefs, ES_Identifier.Empty, fullyQualifiedName);

        IntSize = size;
        Unsigned = unsigned;

        TypeInfo.RuntimeSize = size switch {
            ES_IntSize.Int8  => 1,
            ES_IntSize.Int16 => 2,
            ES_IntSize.Int32 => 4,
            ES_IntSize.Int64 => 8,

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FloatData", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_FloatData {
    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;
    public readonly ES_FloatSize FloatSize;

    #endregion

    public ES_FloatData (
        ES_FullyQualifiedName fullyQualifiedName,
        ES_FloatSize size
    ) {
        TypeInfo = new ES_TypeInfo (ES_TypeTag.Float, ES_AccessModifier.Public, ES_TypeFlag.NoRefs, ES_Identifier.Empty, fullyQualifiedName);

        FloatSize = size;

        TypeInfo.RuntimeSize = size switch {
            ES_FloatSize.Single => 4,
            ES_FloatSize.Double => 8,

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "ReferenceData", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_ReferenceData {
    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;
    public ES_TypeInfo* PointedType;

    #endregion

    public ES_ReferenceData (ES_FullyQualifiedName fullyQualifiedName, ES_TypeInfo* pointedType) {
        TypeInfo = new (ES_TypeTag.Reference, ES_AccessModifier.Public, ES_TypeFlag.None, ES_Identifier.Empty, fullyQualifiedName) {
            RuntimeSize = sizeof (void*)
        };

        PointedType = pointedType;
    }
}
[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "ConstTypeData", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_ConstData {
    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;
    public ES_TypeInfo* InnerType;

    #endregion

    public ES_ConstData (ES_FullyQualifiedName fullyQualifiedName, ES_TypeInfo* innerType, bool immutable) {
        var tag = immutable ? ES_TypeTag.Immutable : ES_TypeTag.Const;
        TypeInfo = new ES_TypeInfo (tag, ES_AccessModifier.Public, ES_TypeFlag.None, ES_Identifier.Empty, fullyQualifiedName);

        InnerType = innerType;
    }
}
