/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.Frontend;

internal unsafe static partial class Compiler_TypeChecking {
    private static ES_Identifier MangleTypeName (ref PassData passData, ES_FullyQualifiedName fqn) {
        if (fqn.NamespaceName.Equals (passData.Env.GlobalTypesNamespace))
            return fqn.TypeName;

        using var charsArray = new StructPooledList<char> (CL_ClearMode.Auto);
        charsArray.AddRange (fqn.NamespaceName.GetCharsSpan ());
        charsArray.AddRange ("::");
        charsArray.AddRange (fqn.TypeName.GetCharsSpan ());

        return passData.Env.IdPool.GetIdentifier (charsArray.Span);
    }

    private static ES_Identifier MangleTypeName (ref PassData passData, ES_TypeInfo* type)
        => MangleTypeName (ref passData, type->Name);

    private static void MangleTypeName (ref PassData passData, ES_TypeInfo* type, ref StructPooledList<char> list) {
        var fqn = type->Name;

        if (!fqn.NamespaceName.Equals (passData.Env.GlobalTypesNamespace)) {
            list.AddRange (fqn.NamespaceName.GetCharsSpan ());
            list.AddRange ("::");
        }
        list.AddRange (fqn.TypeName.GetCharsSpan ());
    }

    private static ES_Identifier MangleFunctionName (ref PassData passData, ES_FunctionData* func) {
        var charsList = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            charsList.AddRange (func->Name.NamespaceName.GetCharsSpan ());
            charsList.AddRange ("::");
            charsList.AddRange (func->Name.TypeName.GetCharsSpan ());

            var protoArgs = func->FunctionType->ArgumentsList.Span;
            if (protoArgs.Length > 0) {
                charsList.Add ('$');

                var firstArg = true;
                foreach (var arg in protoArgs) {
                    if (firstArg)
                        firstArg = false;
                    else
                        charsList.Add ('_');

                    switch (arg.ArgType) {
                        case ES_ArgumentType.Out:
                            charsList.Add ('@');
                            break;

                        case ES_ArgumentType.Ref:
                            charsList.Add ('&');
                            break;
                    }

                    MangleTypeName (ref passData, arg.ValueType, ref charsList);
                }
            }

            return passData.Env.IdPool.GetIdentifier (charsList.Span);
        } finally {
            charsList.Dispose ();
        }
    }

    private static ES_Identifier MangleStaticVar (
        ref PassData passData, ES_FullyQualifiedName typeName, ES_Identifier varName
    ) {
        using var charsArr = new StructPooledList<char> (CL_ClearMode.Auto);

        charsArr.AddRange (typeName.NamespaceName.GetCharsSpan ());
        charsArr.AddRange ("::");
        charsArr.AddRange (typeName.TypeName.GetCharsSpan ());
        charsArr.Add ('.');
        charsArr.AddRange (varName.GetCharsSpan ());

        return passData.IdPool.GetIdentifier (charsArr.Span);
    }

    private static ES_Identifier MangleDefStaticConstructor (ref PassData passData, ES_FullyQualifiedName typeFQN) {
        const string defConsName = ES_Constants.DefaultStaticConstructorName;

        var fqnMangle = MangleTypeName (ref passData, typeFQN);
        using var chars = new StructPooledList<char> (CL_ClearMode.Auto);

        chars.AddRange (fqnMangle.GetCharsSpan ());
        chars.AddRange ("$$");
        chars.AddRange (defConsName);

        return passData.IdPool.GetIdentifier (chars.Span);
    }
}
