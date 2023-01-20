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
using EchelonScript.Common.Data.Types;

namespace EchelonScript.Compiler.Frontend;

internal ref partial struct CompileData {
#if false
    public void GetNiceName (ref StructPooledList<char> charsList, ES_FullyQualifiedName name, bool fullyQualified) {
        if (
            fullyQualified &&
            name.NamespaceName != Env.GlobalsNamespace &&
            name.NamespaceName != Env.GeneratedNamespace
        ) {
            charsList.AddRange (name.NamespaceName.GetCharsSpan ());
            charsList.AddRange ("::");
        }

        charsList.AddRange (name.TypeName.GetCharsSpan ());
    }

    public void GetNiceName (ref StructPooledList<char> charsList, ESC_TypeRef type, bool fullyQualified) {
        bool closingParens;

        switch (type.Constness) {
            case ESC_Constness.Mutable:
                closingParens = false;
                break;

            case ESC_Constness.Const:
                charsList.AddRange ("const(");
                closingParens = true;
                break;

            case ESC_Constness.Immutable:
                charsList.AddRange ("immutable(");
                closingParens = true;
                break;

            default:
                throw new NotImplementedException ("Constness not implemented.");
        }

        switch (type.Type) {
            case null:
                charsList.AddRange ("[NULL]");
                break;

            case ESC_TypeUnknown:
            case ESC_TypeNull:

            case ESC_TypeVoid:
            case ESC_TypeBool:
            case ESC_TypeInt:
            case ESC_TypeFloat:
            case ESC_TypeEnum:

            case ESC_TypeStruct:
            case ESC_TypeClass:
            case ESC_TypeInterface:
                GetNiceName (ref charsList, type.Type.Name, fullyQualified);
                break;

            case ESC_TypePrototype typeProto: {
                charsList.AddRange ("func ");
                GetNiceName (ref charsList, typeProto.ReturnType, fullyQualified);
                charsList.Add ('(');

                var firstArg = true;
                foreach (var arg in typeProto.Arguments) {
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

                    GetNiceName (ref charsList, arg.ValueType, fullyQualified);
                }

                charsList.Add (')');

                break;
            }

            case ESC_TypeReference typeRef: {
                GetNiceName (ref charsList, typeRef.PointedType, fullyQualified);
                charsList.Add ('&');

                break;
            }

            case ESC_TypeArray typeArray: {
                GetNiceName (ref charsList, typeArray.ElementType, fullyQualified);
                charsList.Add ('[');
                charsList.Add (',', typeArray.Rank - 1);
                charsList.Add (']');

                break;
            }

            default:
                throw new NotImplementedException ("Type not implemented yet.");
        }

        if (closingParens)
            charsList.AddRange (")");
    }

    public string GetNiceNameString (ESC_TypeRef type, bool fullyQualified) {
        var charList = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            GetNiceName (ref charList, type, fullyQualified);
            return charList.Span.GetPooledString ();
        } finally {
            charList.Dispose ();
        }
    }

    public void GetFunctionSignature (ref StructPooledList<char> charsList, ReadOnlySpan<ESC_PrototypeArg> argsList) {
        charsList.EnsureCapacity (2);

        charsList.Add ('(');

        var firstArg = true;
        foreach (var arg in argsList) {
            if (!firstArg)
                charsList.AddRange (", ");
            else
                firstArg = false;

            switch (arg.ArgType) {
                case ES_ArgumentType.Normal: break;
                case ES_ArgumentType.In: charsList.AddRange ("in "); break;
                case ES_ArgumentType.Out: charsList.AddRange ("out "); break;
                case ES_ArgumentType.Ref: charsList.AddRange ("ref "); break;
            }

            GetNiceName (ref charsList, arg.ValueType, true);
        }

        charsList.Add (')');
    }

    public string GetFunctionSignatureString (ReadOnlySpan<ESC_PrototypeArg> argsList) {
        var charList = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            GetFunctionSignature (ref charList, argsList);
            return charList.Span.GetPooledString ();
        } finally {
            charList.Dispose ();
        }
    }
#endif
}
