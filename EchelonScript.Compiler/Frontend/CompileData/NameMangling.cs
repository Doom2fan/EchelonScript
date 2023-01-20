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
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;

namespace EchelonScript.Compiler.Frontend;

internal ref partial struct CompileData {
#if false
    private unsafe void GetGeneratedFQN (ref StructPooledList<char> charsList, ES_FullyQualifiedName name) {
        // Format sample: "NamespaceName__TypeName"
        var noNamespace = (
            name.NamespaceName.Equals (Env.GeneratedNamespace) ||
            name.NamespaceName.Equals (Env.GlobalsNamespace)
        );

        if (!noNamespace) {
            charsList.AddRange (name.NamespaceName.GetCharsSpan ());
            charsList.AddRange ("__");
        }

        charsList.AddRange (name.TypeName.GetCharsSpan ());
    }

    private StructPooledList<char> GetGeneratedFQN (ES_FullyQualifiedName name) => GetGeneratedFQN (name, "", "");

    private StructPooledList<char> GetGeneratedFQN (ES_FullyQualifiedName name, ReadOnlySpan<char> prefix, ReadOnlySpan<char> suffix) {
        var charsList = new StructPooledList<char> (CL_ClearMode.Auto);
        try {
            charsList.AddRange (prefix);
            GetGeneratedFQN (ref charsList, name);
            charsList.AddRange (suffix);

            return charsList.Move ();
        } finally {
            charsList.Dispose ();
        }
    }

    private void GetGeneratedTypeName (ref StructPooledList<char> charsList, ESC_TypeRef type) {
        Debug.Assert (type.Type is not null);

        var constnessPrefix = type.Constness switch {
            ESC_Constness.Mutable => null,
            ESC_Constness.Const => "const",
            ESC_Constness.Immutable => "immutable",

            _ => throw new NotImplementedException ("Constness not implemented."),
        };

        if (constnessPrefix is not null) {
            charsList.AddRange (constnessPrefix);
            charsList.Add ('(');
        }

        GetGeneratedFQN (ref charsList, type.Type.Name);

        if (constnessPrefix is not null)
            charsList.Add (')');
    }

    private StructPooledList<char> GetGeneratedTypeName (ESC_TypeRef name, ReadOnlySpan<char> prefix, ReadOnlySpan<char> suffix) {
        var charsList = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            charsList.AddRange (prefix);
            GetGeneratedTypeName (ref charsList, name);
            charsList.AddRange (suffix);

            return charsList.Move ();
        } finally {
            charsList.Dispose ();
        }
    }

    private ES_Identifier GeneratePrototypeName (ESC_TypeRef returnType, ReadOnlySpan<ESC_PrototypeArg> args) {
        Debug.Assert (returnType.Type is not null);

        var charList = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            charList.AddRange ("@FuncType<");

            // Add the return type.
            charList.AddRange ("ret@");
            GetGeneratedTypeName (ref charList, returnType);

            charList.AddRange (", ");

            // Add the arguments.
            var firstArg = true;
            foreach (var arg in args) {
                Debug.Assert (arg.ValueType.Type is not null);

                if (!firstArg)
                    charList.AddRange (", ");
                else
                    firstArg = false;

                charList.AddRange ("arg ");
                switch (arg.ArgType) {
                    case ES_ArgumentType.Normal: charList.AddRange ("normal@"); break;
                    case ES_ArgumentType.In: charList.AddRange ("in@"); break;
                    case ES_ArgumentType.Out: charList.AddRange ("out@"); break;
                    case ES_ArgumentType.Ref: charList.AddRange ("ref@"); break;
                }

                GetGeneratedTypeName (ref charList, arg.ValueType);
            }

            charList.AddRange (">");

            return IdPool.GetIdentifier (charList.Span);
        } finally {
            charList.Dispose ();
        }
    }
#endif
}
