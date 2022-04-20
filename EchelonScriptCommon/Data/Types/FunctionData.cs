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
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCommon.Data.Types;

[Flags]
public enum ES_FunctionFlags : int {
    /// <summary>The function is a member function of a type, and has an implicit self pointer.</summary>
    Method = 1,
    /// <summary>Function is virtual and may be overridden in derived classes. (May only be used in classes)</summary>
    Virtual = 1 << 1,
    /// <summary>Function has no function body and must be overridden in derived classes. (May only be used in classes)</summary>
    Abstract = 1 << 2,
}

public enum ES_ArgumentType {
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
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionData", ES_ExportAttributeBase.AggregateType.Struct)]
public readonly unsafe struct ES_FunctionData {
    #region ================== Instance fields

    public readonly ES_FullyQualifiedName Name;

    public readonly ES_AccessModifier AccessModifier;
    public readonly ES_Identifier SourceUnit;

    public readonly ES_FunctionPrototypeData* FunctionType;
    public readonly ArrayPointer<ES_FunctionArg> Arguments;
    public readonly int OptionalArgsCount;

    #endregion

    #region ================== Instance properties

    public readonly string SourceUnitString => SourceUnit.GetCharsSpan ().GetPooledString ();

    #endregion

    public ES_FunctionData (
        ES_FullyQualifiedName fqn, ES_AccessModifier accessMod, ES_Identifier sourceUnit,
        ES_FunctionPrototypeData* functionType, ArrayPointer<ES_FunctionArg> args, int optArgCount
    ) {
        Debug.Assert (functionType is not null);

        Name = fqn;

        AccessModifier = accessMod;
        SourceUnit = sourceUnit;

        FunctionType = functionType;
        Arguments = args;
        OptionalArgsCount = optArgCount;
    }
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionArg", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_FunctionArg {
    #region ================== Instance fields

    public readonly ES_Identifier Name;

    #endregion

    public ES_FunctionArg (ES_Identifier name) => Name = name;
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionTypeData", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_FunctionPrototypeData {
    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;
    private ES_TypeInfo* returnType;
    private ArrayPointer<ES_FunctionPrototypeArg> argumentsList;

    #endregion

    public ES_FunctionPrototypeData (
        ES_FullyQualifiedName fullyQualifiedName,
        ES_TypeInfo* retType, ArrayPointer<ES_FunctionPrototypeArg> argsList
    ) {
        TypeInfo = new (ES_TypeTag.FuncPrototype, ES_AccessModifier.Public, ES_TypeFlag.NoNew, ES_Identifier.Empty, fullyQualifiedName);
        TypeInfo.RuntimeSize = IntPtr.Size;

        returnType = retType;
        argumentsList = argsList;
    }

    #region ================== Instance properties

    public ES_TypeInfo* ReturnType => returnType;
    public ArrayPointer<ES_FunctionPrototypeArg> ArgumentsList => argumentsList;

    #endregion
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionTypeArg", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_FunctionPrototypeArg {
    #region ================== Instance fields

    public readonly ES_ArgumentType ArgType;
    public readonly ES_TypeInfo* ValueType;

    #endregion

    public ES_FunctionPrototypeArg (ES_ArgumentType argType, ES_TypeInfo* valueType) {
        ArgType = argType;
        ValueType = valueType;
    }
}
