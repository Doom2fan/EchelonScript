﻿/*
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
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Frontend;

namespace EchelonScriptCompiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
    internal const string DefaultStaticConsName = "_defaultStaticCons";
    internal const string GlobalStaticConsName = "_globalStaticCons";
    internal const string GlobalStorageTypeName = "_globalFunctionsStorageType";
    internal const string StaticVarsMemName = "$_staticVarsMem";

    internal const string ArrayAllocFuncName = "$Allocate";
    internal const string ArrayIndexFuncName = "$Index";
    internal const string ArrayConcatFuncName = "$Concat";

    internal const string DefaultValueFuncName = "$DefaultValue";

    private static string MangleDefaultConstructorName (ES_TypeInfo* typeName, bool isStatic) {
        // Sample name: "struct.System.Numerics__Vector2"
        using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

        // The prefix.
        if (!isStatic)
            mangleChars.AddRange ("defaultConstructor!");
        else
            mangleChars.AddRange ("defaultStaticConstructor!");

        // The namespace.
        mangleChars.AddRange (typeName->Name.NamespaceName.GetCharsSpan ());

        // The mangled namespace separator.
        mangleChars.Add ('_', 2);

        // The function name.
        mangleChars.AddRange (typeName->Name.TypeName.GetCharsSpan ());

        return mangleChars.Span.GetPooledString ();
    }

    internal static string MangleGlobalFunctionName ([DisallowNull] ES_FunctionData* func) {
        // Sample name: "System.Math__FMath.Sin"
        using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

        // The namespace.
        mangleChars.AddRange (func->Name.NamespaceName.GetCharsSpan ());

        // The mangled namespace separator.
        mangleChars.AddRange ("__");

        // The function name.
        mangleChars.AddRange (func->Name.TypeName.GetCharsSpan ());

        return mangleChars.Span.GetPooledString ();
    }

    internal static string MangleMemberFunctionName ([DisallowNull] ES_TypeInfo* owner, [DisallowNull] ES_FunctionData* func) {
        using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

        // The type name.
        mangleChars.AddRange (owner->Name.TypeName.GetCharsSpan ());

        // The mangled namespace separator.
        mangleChars.AddRange ("__");

        // The function name.
        mangleChars.AddRange (func->Name.TypeName.GetCharsSpan ());

        return mangleChars.Span.GetPooledString ();
    }

    internal static string MangleStaticVariable ([DisallowNull] ESIR_StaticVariable staticVar) {
        using var charsList = new StructPooledList<char> (CL_ClearMode.Auto);

        charsList.AddRange ("StaticVar_");
        charsList.AddRange (staticVar.Name.GetCharsSpan ());

        return charsList.Span.GetPooledString ();
    }

    #region Type names

    private static string MangleStructName ([DisallowNull] ES_StructData* structData) {
        // Sample name: "struct.System.Numerics::Vector2"
        using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

        // The prefix.
        mangleChars.AddRange ("struct!");

        // The namespace.
        mangleChars.AddRange (structData->TypeInfo.Name.NamespaceName.GetCharsSpan ());

        // The mangled namespace separator.
        mangleChars.Add (':', 2);

        // The function name.
        mangleChars.AddRange (structData->TypeInfo.Name.TypeName.GetCharsSpan ());

        return mangleChars.Span.GetPooledString ();
    }

    private static string MangleFunctionType ([DisallowNull] ES_FunctionPrototypeData* type) {
        using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

        // Add the return type.
        mangleChars.AddRange ("Ret");
        mangleChars.AddRange (type->ReturnType->Name.NamespaceName.GetCharsSpan ());
        mangleChars.AddRange ("__");
        mangleChars.AddRange (type->ReturnType->Name.TypeName.GetCharsSpan ());

        // Add the arg types.
        foreach (var arg in type->ArgumentsList.Span) {
            mangleChars.AddRange ("_");

            switch (arg.ArgType) {
                case ES_ArgumentType.Normal: break;
                case ES_ArgumentType.In: break;
                case ES_ArgumentType.Out: mangleChars.AddRange ("out"); break;
                case ES_ArgumentType.Ref: mangleChars.AddRange ("ref"); break;
            }

            mangleChars.AddRange (arg.ValueType->Name.NamespaceName.GetCharsSpan ());
            mangleChars.AddRange ("__");
            mangleChars.AddRange (arg.ValueType->Name.TypeName.GetCharsSpan ());
        }

        return mangleChars.Span.GetPooledString ();
    }

    private static string MangleArrayType ([DisallowNull] ES_ArrayData* arrayData) {
        // Sample name: "array.@generated::global::int32$2D"
        using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

        // The prefix.
        mangleChars.AddRange ("array!");

        // The namespace.
        mangleChars.AddRange (arrayData->TypeInfo.Name.NamespaceName.GetCharsSpan ());

        // The mangled namespace separator.
        mangleChars.Add (':', 2);

        // The element type name.
        mangleChars.AddRange (MangleTypeNameAny (arrayData->ElementType));

        // The dimensions suffix.
        Debug.Assert (arrayData->Rank <= byte.MaxValue);

        var dimSuffixSpan = mangleChars.AddSpan (5);
        dimSuffixSpan [0] = '$';

        if (!arrayData->Rank.TryFormat (dimSuffixSpan [1..], out var charsWritten))
            Debug.Fail ("Failed to format dimensions count.");

        dimSuffixSpan [charsWritten + 1] = 'D';

        mangleChars.RemoveEnd (3 - charsWritten);

        return mangleChars.Span.GetPooledString ();
    }

    internal static string MangleTypeName ([DisallowNull] ES_TypeInfo* type) {
        switch (type->TypeTag) {
            case ES_TypeTag.Struct:
                return MangleStructName ((ES_StructData*) type);

            case ES_TypeTag.FuncPrototype:
                return MangleFunctionType ((ES_FunctionPrototypeData*) type);

            case ES_TypeTag.Array:
                return MangleArrayType ((ES_ArrayData*) type);

            case ES_TypeTag.Void:
            case ES_TypeTag.Bool:
            case ES_TypeTag.Int:
            case ES_TypeTag.Float:
                throw new NotSupportedException ("Type cannot be name-mangled.");

            case ES_TypeTag.Class:
            case ES_TypeTag.Enum:
            case ES_TypeTag.Interface:
            case ES_TypeTag.Reference:
                throw new NotImplementedException ("[TODO] Not implemented yet.");

            case ES_TypeTag.Const:
            case ES_TypeTag.Immutable: {
                var constData = (ES_ConstData*) type;
                var constStr = type->TypeTag == ES_TypeTag.Immutable ? "immutable" : "const" ;

                return $"{constStr}({MangleTypeNameAny (constData->InnerType)})";
            }


            default:
                throw new NotImplementedException ("Type tag not implemented.");
        }
    }

    internal static string MangleTypeNameAny ([DisallowNull] ES_TypeInfo* type) {
        switch (type->TypeTag) {
            case ES_TypeTag.Bool:
                return ES_PrimitiveTypes.Bool;

            case ES_TypeTag.Int: {
                var intType = (ES_IntData*) type;
                return ES_PrimitiveTypes.GetIntName (intType->IntSize, intType->Unsigned);
            }

            case ES_TypeTag.Float: {
                var floatType = (ES_FloatData*) type;
                return ES_PrimitiveTypes.GetFloatName (floatType->FloatSize);
            }

            default:
                return MangleTypeName (type);
        }
    }

    #endregion
}
