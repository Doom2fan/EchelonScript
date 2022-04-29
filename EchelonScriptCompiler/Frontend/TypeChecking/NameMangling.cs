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
using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.Frontend.Data;

namespace EchelonScriptCompiler.Frontend;

internal unsafe static partial class Compiler_TypeChecking {
    private static ES_Identifier MangleTypeName (ref CompileData compileData, ES_FullyQualifiedName fqn) {
        if (fqn.NamespaceName.Equals (compileData.Env.GlobalsNamespace))
            return fqn.TypeName;

        using var charsArray = new StructPooledList<char> (CL_ClearMode.Auto);
        charsArray.AddRange (fqn.NamespaceName.GetCharsSpan ());
        charsArray.AddRange ("::");
        charsArray.AddRange (fqn.TypeName.GetCharsSpan ());

        return compileData.IdPool.GetIdentifier (charsArray.Span);
    }

    private static ES_Identifier MangleTypeName (ref CompileData compileData, ESC_TypeData type)
        => MangleTypeName (ref compileData, type.Name);

    private static void MangleTypeName (ref CompileData compileData, ESC_TypeRef type, ref StructPooledList<char> list) {
        Debug.Assert (type.Type is not null);
        var fqn = type.Type.Name;

        bool closingParens;
        switch (type.Constness) {
            case ESC_Constness.Const:
                list.AddRange ("const(");
                closingParens = true;
                break;

            case ESC_Constness.Immutable:
                list.AddRange ("immutable(");
                closingParens = true;
                break;

            case ESC_Constness.Mutable:
                closingParens = false;
                break;

            default:
                throw new NotImplementedException ("Constness not implemented.");
        }

        if (!fqn.NamespaceName.Equals (compileData.Env.GlobalsNamespace)) {
            list.AddRange (fqn.NamespaceName.GetCharsSpan ());
            list.AddRange ("::");
        }
        list.AddRange (fqn.TypeName.GetCharsSpan ());

        if (closingParens)
            list.Add (')');
    }

    private static ES_Identifier MangleFunctionName (ref CompileData compileData, ESC_Function func) {
        var charsList = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            charsList.AddRange ((func.Parent.Type?.Name.NamespaceName ?? func.Parent.NamespaceName).GetCharsSpan ());
            charsList.AddRange ("::");
            if (func.Parent.Type is not null) {
                charsList.AddRange (func.Parent.Type.Name.TypeName.GetCharsSpan ());
                charsList.Add ('.');
            }
            charsList.AddRange (func.Name.GetCharsSpan ());

            var protoArgs = func.Prototype.Arguments;
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

                    MangleTypeName (ref compileData, arg.ValueType, ref charsList);
                }
            }

            return compileData.IdPool.GetIdentifier (charsList.Span);
        } finally {
            charsList.Dispose ();
        }
    }

    private static ES_Identifier MangleStaticVar (
        ref CompileData compileData, ES_FullyQualifiedName typeName, ES_Identifier varName
    ) {
        using var charsArr = new StructPooledList<char> (CL_ClearMode.Auto);

        charsArr.AddRange (typeName.NamespaceName.GetCharsSpan ());
        charsArr.AddRange ("::");
        charsArr.AddRange (typeName.TypeName.GetCharsSpan ());
        charsArr.Add ('.');
        charsArr.AddRange (varName.GetCharsSpan ());

        return compileData.IdPool.GetIdentifier (charsArr.Span);
    }

    private static ES_Identifier MangleDefStaticConstructor (ref CompileData compileData, ES_FullyQualifiedName typeFQN) {
        const string defConsName = ES_Constants.DefaultStaticConstructorName;

        var fqnMangle = MangleTypeName (ref compileData, typeFQN);
        using var chars = new StructPooledList<char> (CL_ClearMode.Auto);

        chars.AddRange (fqnMangle.GetCharsSpan ());
        chars.AddRange ("$$");
        chars.AddRange (defConsName);

        return compileData.IdPool.GetIdentifier (chars.Span);
    }
}
