/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using ChronosLib.Pooled;
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

    FuncPrototype,

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

    /// <summary>A list of all the references in the type.</summary>
    public ArrayPointer<nint> RefsList;

    /// <summary>The type's flags.</summary>
    public ES_TypeFlag Flags;

    /// <summary>The fully qualified name of the type.</summary>
    public readonly ES_FullyQualifiedName Name;

    /// <summary>The type's access modifier.</summary>
    public readonly ES_AccessModifier AccessModifier;

    /// <summary>The source translation unit of the type.</summary>
    public readonly ES_Identifier SourceUnit;

    #endregion

    public ES_TypeInfo (
        ES_TypeTag typeTag, ES_AccessModifier accessMod, ES_TypeFlag flags,
        ES_Identifier sourceUnit,
        ES_FullyQualifiedName fullyQualifiedName
    ) {
        TypeTag = typeTag;
        RuntimeSize = -1;

        RefsList = ArrayPointer<nint>.Null;
        Flags = flags;

        Name = fullyQualifiedName;
        AccessModifier = accessMod;
        SourceUnit = sourceUnit;
    }

    #region ================== Instance properties

    #region String utilities

    /// <summary>The type's source unit's name as a string.</summary>
    public string SourceUnitString => SourceUnit.GetCharsSpan ().GetPooledString ();

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

    #region ================== Static methods

    public static void GetNiceName (
        ref StructPooledList<char> charsList,
        ES_FullyQualifiedName name,
        bool fullyQualified,
        ES_Identifier globalsTypesNS,
        ES_Identifier generatedTypesNS
    ) {
        if (
            fullyQualified &&
            name.NamespaceName != globalsTypesNS &&
            name.NamespaceName != generatedTypesNS
        ) {
            charsList.AddRange (name.NamespaceName.GetCharsSpan ());
            charsList.AddRange ("::");
        }

        charsList.AddRange (name.TypeName.GetCharsSpan ());
    }

    public static void GetNiceTypeName (
        ref StructPooledList<char> charsList,
        ES_TypeInfo* type,
        bool fullyQualified,
        ES_Identifier globalTypesNS,
        ES_Identifier generatedTypesNS
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
                var protoData = (ES_FunctionPrototypeData*) type;

                charsList.AddRange ("func ");
                GetNiceTypeName (ref charsList, protoData->ReturnType, fullyQualified, globalTypesNS, generatedTypesNS);
                charsList.Add ('(');

                var firstArg = true;
                foreach (var arg in protoData->ArgumentsList.Span) {
                    if (!firstArg)
                        charsList.AddRange (", ");
                    firstArg = false;

                    switch (arg.ArgType) {
                        case ES_ArgumentType.Normal:
                            break;

                        case ES_ArgumentType.In:
                            charsList.AddRange ("in ");
                            break;

                        case ES_ArgumentType.Out:
                            charsList.AddRange ("out ");
                            break;

                        case ES_ArgumentType.Ref:
                            charsList.AddRange ("ref ");
                            break;

                        default:
                            throw new NotImplementedException ("Arg type not implemented yet.");
                    }

                    GetNiceTypeName (ref charsList, arg.ValueType, fullyQualified, globalTypesNS, generatedTypesNS);
                }

                charsList.Add (')');

                break;
            }

            case ES_TypeTag.Reference: {
                var refData = (ES_ReferenceData*) type;

                GetNiceTypeName (ref charsList, refData->PointedType, fullyQualified, globalTypesNS, generatedTypesNS);
                charsList.Add ('&');

                break;
            }

            case ES_TypeTag.Array: {
                var arrayData = (ES_ArrayData*) type;

                GetNiceTypeName (ref charsList, arrayData->ElementType, fullyQualified, globalTypesNS, generatedTypesNS);
                charsList.Add ('[');
                charsList.Add (',', arrayData->Rank - 1);
                charsList.Add (']');

                break;
            }

            case ES_TypeTag.Const:
            case ES_TypeTag.Immutable: {
                var constData = (ES_ConstData*) type;

                var constLabel = type->TypeTag == ES_TypeTag.Const ? "const" : "immutable";
                charsList.AddRange (constLabel);

                charsList.Add ('(');
                GetNiceTypeName (ref charsList, constData->InnerType, fullyQualified, globalTypesNS, generatedTypesNS);
                charsList.Add (')');

                break;
            }

            default:
                throw new NotImplementedException ("Type not implemented yet.");
        }
    }

    #endregion
}
