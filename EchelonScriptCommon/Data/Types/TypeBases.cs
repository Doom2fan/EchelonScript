﻿/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using CommunityToolkit.HighPerformance.Buffers;
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCommon.Data.Types;

public enum ES_TypeTag : int {
    UNKNOWN   = 0,
    Null,

    Void,
    Bool,
    Int,
    Float,
    Enum,

    Struct,
    Class,
    Interface,

    Function,

    Reference,
    Array,

    Const,
    Immutable,
}

public enum ES_TypeFlag : int {
    None = 0,

    Analyzed = 1 << 0,
    NoRefs = 1 << 1,
    NoNew = 1 << 2,
}

public unsafe struct ES_TypeInfo {
    #region ================== Instance fields

    /// <summary>What type of type this is.</summary>
    public readonly ES_TypeTag TypeTag;

    /// <summary>The size of the type at runtime, in bytes.</summary>
    public int RuntimeSize;

    /// <summary>The type's access modifier.</summary>
    public readonly ES_AccessModifier AccessModifier;

    /// <summary>The type's flags.</summary>
    public ES_TypeFlag Flags;

    /// <summary>The fully qualified name of the type.</summary>
    public readonly ES_FullyQualifiedName Name;

    /// <summary>The source translation unit of the type.</summary>
    public readonly ArrayPointer<byte> SourceUnit;

    /// <summary>A list of all the references in the type.</summary>
    public ArrayPointer<nint> RefsList;

    /// <summary>The members list of the type.</summary>
    public ES_TypeMembers MembersList;

    #endregion

    #region ================== Constructors

    public ES_TypeInfo (
        ES_TypeTag typeTag, ES_AccessModifier accessMod, ES_TypeFlag flags,
        ArrayPointer<byte> sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName
    ) {
        TypeTag = typeTag;
        RuntimeSize = -1;

        AccessModifier = accessMod;
        Flags = flags;

        Name = fullyQualifiedName;
        SourceUnit = sourceUnit;

        RefsList = ArrayPointer<nint>.Null;
        MembersList = new ES_TypeMembers ();
    }

    #endregion

    #region ================== Instance properties

    #region String utilities

    /// <summary>The type's source unit's name as a string.</summary>
    public string SourceUnitString => StringPool.Shared.GetOrAdd (SourceUnit.Span, ES_Encodings.Identifier);

    #endregion

    public bool IsReferenceType () {
        return TypeTag switch {
            ES_TypeTag.Reference => true,
            ES_TypeTag.Array => true,
            ES_TypeTag.Interface => true,

            _ => false,
        };
    }

    #endregion

    #region ================== Instance methods

    public bool IsConstant () => TypeTag == ES_TypeTag.Const || TypeTag == ES_TypeTag.Immutable;

    public bool IsWritable () => TypeTag != ES_TypeTag.Const && TypeTag != ES_TypeTag.Immutable;

    #endregion
}
