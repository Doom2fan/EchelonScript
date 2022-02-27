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
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCompiler.Frontend;

internal unsafe static partial class Compiler_TypeChecking {
    private static void AddSplit<T> (ref this Span<T> span, ReadOnlySpan<T> data) {
        data.CopyTo (span);
        span = span [data.Length..];
    }

    private static void AddSplit<T> (ref this Span<T> span, T data, int count) {
        span [..count].Fill (data);
        span = span [count..];
    }

    private static ArrayPointer<byte> MangleTypeName (ref PassData passData, ES_FullyQualifiedName fqn) {
        if (fqn.NamespaceName.Equals (passData.Env.GlobalTypesNamespace))
            return fqn.TypeName;

        var totalLen = fqn.NamespaceName.Length + 2 + fqn.TypeName.Length;
        using var bytesArr = PooledArray<byte>.GetArray (totalLen);

        var workSpan = bytesArr.Span;
        workSpan.AddSplit (fqn.NamespaceName.Span);
        workSpan.AddSplit ((byte) ':', 2);
        workSpan.AddSplit (fqn.TypeName.Span);

        return passData.Env.IdPool.GetIdentifier (bytesArr.Span);
    }

    private static ArrayPointer<byte> MangleTypeName (ref PassData passData, ES_TypeInfo* type)
        => MangleTypeName (ref passData, type->Name);

    private static void MangleTypeName (ref PassData passData, ES_TypeInfo* type, ref StructPooledList<byte> list) {
        var fqn = type->Name;

        if (!fqn.NamespaceName.Equals (passData.Env.GlobalTypesNamespace)) {
            list.AddRange (fqn.NamespaceName.Span);
            list.Add ((byte) ':', 2);
        }
        list.AddRange (fqn.TypeName.Span);
    }

    private static ArrayPointer<byte> MangleFunctionName (ref PassData passData, ES_FunctionData* func) {
        var bytesList = new StructPooledList<byte> (CL_ClearMode.Auto);

        try {
            bytesList.AddRange (func->Name.NamespaceName.Span);
            bytesList.Add ((byte) ':', 2);
            bytesList.AddRange (func->Name.TypeName.Span);

            var protoArgs = func->FunctionType->ArgumentsList.Span;
            if (protoArgs.Length > 0) {
                bytesList.Add ((byte) '$');

                var firstArg = true;
                foreach (var arg in protoArgs) {
                    if (firstArg)
                        firstArg = false;
                    else
                        bytesList.Add ((byte) '_');

                    switch (arg.ArgType) {
                        case ES_ArgumentType.Out:
                            bytesList.Add ((byte) '@');
                            break;

                        case ES_ArgumentType.Ref:
                            bytesList.Add ((byte) '&');
                            break;
                    }

                    MangleTypeName (ref passData, arg.ValueType, ref bytesList);
                }
            }

            return passData.Env.IdPool.GetIdentifier (bytesList.Span);
        } finally {
            bytesList.Dispose ();
        }
    }

    private static ArrayPointer<byte> MangleStaticVar (
        ref PassData passData, ES_FullyQualifiedName typeName, ArrayPointer<byte> varName
    ) {
        var idPool = passData.Env.IdPool;

        var totalLen = typeName.NamespaceName.Length + 2 + typeName.TypeName.Length + 1 + varName.Length;
        using var bytesArr = PooledArray<byte>.GetArray (totalLen);

        var workSpan = bytesArr.Span;
        workSpan.AddSplit (typeName.NamespaceName.Span);
        workSpan.AddSplit ((byte) ':', 2);
        workSpan.AddSplit (typeName.TypeName.Span);
        workSpan.AddSplit ((byte) '.', 1);
        workSpan.AddSplit (varName.Span);

        return idPool.GetIdentifier (bytesArr.Span);
    }

    private static ArrayPointer<byte> MangleDefStaticConstructor (ref PassData passData, ES_FullyQualifiedName typeFQN) {
        const string defConsName = ES_Constants.DefaultStaticConstructorName;

        var fqnMangle = MangleTypeName (ref passData, typeFQN);

        var fqnMangleLen = fqnMangle.Length;
        var suffixLen = defConsName.Length;

        using var bytes = PooledArray<byte>.GetArray (fqnMangleLen + 2 + suffixLen);

        fqnMangle.Span.CopyTo (bytes);
        bytes.Span.Slice (fqnMangleLen, 2).Fill ((byte) '$');
        ES_Encodings.Identifier.GetBytes (defConsName, bytes.Span [(fqnMangleLen + 2)..]);

        return passData.Env.IdPool.GetIdentifier (bytes);
    }
}
