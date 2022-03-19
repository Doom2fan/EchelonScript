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
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCommon.Data.Types;

public enum ES_AccessModifier {
    /// <summary>The symbol is only accessible from the context it was defined in.</summary>
    Private,
    /// <summary>The symbol is accessible from the context it was defined in and anything inheriting from it.</summary>
    Protected,
    /// <summary>The symbol is accessible from the context it was defined in, anything inheriting from it and from
    /// anything defined in the same program/library as the context it was defined in.</summary>
    ProtectedInternal,
    /// <summary>The symbol is accessible from the context it was defined in and from anything defined in the same
    /// program/library as the context it was defined in.</summary>
    Internal,
    /// <summary>The symbol is accessible from any context.</summary>
    Public,
}

public enum ES_VirtualnessModifier {
    None,
    Virtual,
    Abstract,
    Override,
}

public readonly struct ES_FullyQualifiedName {
    public readonly ES_Identifier NamespaceName;
    public readonly ES_Identifier TypeName;

    public ES_FullyQualifiedName (ES_Identifier namespaceName, ES_Identifier typeName) {
        NamespaceName = namespaceName;
        TypeName = typeName;
    }

    public string GetNameAsTypeString () {
        var chars = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            NamespaceName.GetBytesSpan ().GetChars (ref chars, ES_Encodings.Identifier);
            chars.AddRange ("::");
            TypeName.GetBytesSpan ().GetChars (ref chars, ES_Encodings.Identifier);

            return chars.Span.GetPooledString ();
        } finally {
            chars.Dispose ();
        }
    }
}
