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
using System.Diagnostics.CodeAnalysis;
using ChronosLib.Pooled;
using EchelonScript.Common.Utilities;
using ExhaustiveMatching;

namespace EchelonScript.Common.Data.Types;

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

    FuncPrototype,

    Reference,
    Array,
}

public enum ES_Constness {
    Mutable,
    Const,
    Immutable,
}

[Flags]
public enum ES_TypeFlag : ushort {
    None = 0,

    /// <summary>Type is a native type, ExtraData points to a <see cref="ES_NativeTypeInfo"/>.</summary>
    NativeType = 1 << 0,
    /// <summary>Type has no GC refs and can be special-cased.</summary>
    NoRefs     = 1 << 1,
    /// <summary>Type is a value type.</summary>
    ValueType  = 1 << 2,
    /// <summary>The type's environment is being unloaded and any objects of this type must be destroyed.</summary>
    Unloading  = 1 << 3,
}

public readonly unsafe struct ES_TypeInfo {
    #region ================== Instance fields

    /// <summary>The type's method table. Contains frequently-accessed data.</summary>
    public readonly ES_MethodTable* MethodTable;

    /// <summary>What type of type this is.</summary>
    public readonly ES_TypeTag TypeTag;

    /// <summary>Kind-dependant extra data.</summary>
    public readonly void* ExtraData;

    /// <summary>The fully qualified name of the type.</summary>
    public readonly ES_FullyQualifiedName Name;

    /// <summary>The source translation unit of the type.</summary>
    public readonly ES_Utf8String SourceUnit;

    #endregion

    #region ================== Instance fields

    public ES_MethodTable* MethodTableInit { init => MethodTable = value; }

    public ES_TypeTag TypeTagInit {init => TypeTag = value; }

    public void* ExtraDataInit {init => ExtraData = value; }

    public ES_FullyQualifiedName NameInit {init => Name = value; }

    public ES_Utf8String SourceUnitInit {init => SourceUnit = value; }

    #endregion

    private ES_TypeInfo (
        ES_TypeTag typeTag, [NotNull] ES_MethodTable* methodTable,
        ES_Utf8String sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName,
        void* extraData
    ) {
        TypeTag = typeTag;
        MethodTable = methodTable;

        Name = fullyQualifiedName;
        SourceUnit = sourceUnit;
        ExtraData = extraData;
    }

    internal ES_TypeInfo (
        ES_TypeTag typeTag, [NotNull] ES_MethodTable* methodTable,
        ES_Utf8String sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName
    ) {
        TypeTag = typeTag;
        MethodTable = methodTable;

        Name = fullyQualifiedName;
        SourceUnit = sourceUnit;
        ExtraData = null;
    }

    internal ES_TypeInfo (
        [NotNull] ES_MethodTable* methodTable,
        ES_Utf8String sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName,
        ES_ClassInfo* classData
    ) : this (ES_TypeTag.Class, methodTable, sourceUnit, fullyQualifiedName, classData) { }

    internal ES_TypeInfo (
        [NotNull] ES_MethodTable* methodTable,
        ES_Utf8String sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName,
        ES_StructInfo* structData
    ) : this (ES_TypeTag.Struct, methodTable, sourceUnit, fullyQualifiedName, structData) { }

    internal ES_TypeInfo (
        [NotNull] ES_MethodTable* methodTable,
        ES_Utf8String sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName,
        ES_IntInfo intInfo
    ) : this (ES_TypeTag.Int, methodTable, sourceUnit, fullyQualifiedName, (void*) intInfo) { }

    internal ES_TypeInfo (
        [NotNull] ES_MethodTable* methodTable,
        ES_Utf8String sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName,
        ES_FloatInfo floatInfo
    ) : this (ES_TypeTag.Float, methodTable, sourceUnit, fullyQualifiedName, (void*) floatInfo) { }

    internal ES_TypeInfo (
        [NotNull] ES_MethodTable* methodTable,
        ES_Utf8String sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName,
        [NotNull] ES_ArrayInfo* arrayInfo
    ) : this (ES_TypeTag.Array, methodTable, sourceUnit, fullyQualifiedName, arrayInfo) { }

    internal ES_TypeInfo (
        [NotNull] ES_MethodTable* methodTable,
        ES_Utf8String sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName,
        ES_ReferenceInfo* refInfo
    ) : this (ES_TypeTag.Reference, methodTable, sourceUnit, fullyQualifiedName, (void*) refInfo) { }

    #region ================== Instance properties

    #region String utilities

    /// <summary>The type's source unit's name as a string.</summary>
    public string SourceUnitString => SourceUnit.GetPooledString ();

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

    #region ================== Static methods

    public static void GetNiceName (
        ref StructPooledList<char> charsList,
        ES_FullyQualifiedName name,
        bool fullyQualified,
        ES_Utf8String globalsTypesNS,
        ES_Utf8String generatedTypesNS
    ) {
        if (
            fullyQualified &&
            name.NamespaceName != globalsTypesNS &&
            name.NamespaceName != generatedTypesNS
        ) {
            charsList.AddRange (name.NamespaceName);
            charsList.AddRange ("::");
        }

        charsList.AddRange (name.TypeName);
    }

    public static void GetNiceTypeName (
        ref StructPooledList<char> charsList,
        ES_TypeInfo* type,
        bool fullyQualified,
        ES_Utf8String globalTypesNS,
        ES_Utf8String generatedTypesNS
    ) {
        if (type is null) {
            charsList.AddRange ("[NULL]");
            return;
        }

        switch (type->TypeTag) {
            case ES_TypeTag.UNKNOWN:
            case ES_TypeTag.Null:

            case ES_TypeTag.Void:
            case ES_TypeTag.Bool:
            case ES_TypeTag.Int:
            case ES_TypeTag.Float:
            case ES_TypeTag.Enum:

            case ES_TypeTag.Struct:
            case ES_TypeTag.Class:
            case ES_TypeTag.Interface:
                GetNiceName (ref charsList, type->Name, fullyQualified, globalTypesNS, generatedTypesNS);
                break;

            case ES_TypeTag.FuncPrototype: {
                var protoData = (ES_FunctionPrototypeInfo*) type;

                charsList.AddRange ("func ");
                GetNiceTypeName (ref charsList, protoData->ReturnType->TypeInfo, fullyQualified, globalTypesNS, generatedTypesNS);
                charsList.Add ('(');

                var firstArg = true;
                foreach (var arg in protoData->ArgumentsList.Span) {
                    if (!firstArg)
                        charsList.AddRange (", ");
                    firstArg = false;

                    switch (arg.ArgKind) {
                        case ES_ArgumentKind.Normal:
                            break;

                        case ES_ArgumentKind.In:
                            charsList.AddRange ("in ");
                            break;

                        case ES_ArgumentKind.Out:
                            charsList.AddRange ("out ");
                            break;

                        case ES_ArgumentKind.Ref:
                            charsList.AddRange ("ref ");
                            break;

                        default:
                            throw ExhaustiveMatch.Failed (arg.ArgKind);
                    }

                    GetNiceTypeName (ref charsList, arg.ArgType->TypeInfo, fullyQualified, globalTypesNS, generatedTypesNS);
                }

                charsList.Add (')');

                break;
            }

            case ES_TypeTag.Reference: {
                Debug.Assert (type->ExtraData != null);
                var refInfo = (ES_ReferenceInfo*) type->ExtraData;

                GetNiceTypeName (ref charsList, refInfo->PointedType->TypeInfo, fullyQualified, globalTypesNS, generatedTypesNS);
                charsList.Add ('&');

                break;
            }

            case ES_TypeTag.Array: {
                Debug.Assert (type->ExtraData != null);
                var arrayData = (ES_ArrayInfo*) type->ExtraData;

                GetNiceTypeName (ref charsList, arrayData->ElementType->TypeInfo, fullyQualified, globalTypesNS, generatedTypesNS);
                charsList.Add ('[');
                charsList.Add (',', arrayData->Rank - 1);
                charsList.Add (']');

                break;
            }

            default:
                throw new NotImplementedException ("Type not implemented yet.");
        }
    }

    #endregion
}
