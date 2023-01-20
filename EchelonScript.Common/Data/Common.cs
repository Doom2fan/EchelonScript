/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Text;
using ChronosLib.Pooled;
using EchelonScript.Common.Utilities;

namespace EchelonScript.Common.Data.Types;

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
    public readonly ES_Utf8String NamespaceName;
    public readonly ES_Utf8String TypeName;

    public ES_FullyQualifiedName (ES_Utf8String namespaceName, ES_Utf8String typeName) {
        NamespaceName = namespaceName;
        TypeName = typeName;
    }

    public void GetNameAsTypeChars (ref StructPooledList<char> chars) {
        NamespaceName.Span.GetChars (ref chars, Encoding.UTF8);
        chars.AddRange ("::");
        TypeName.Span.GetChars (ref chars, Encoding.UTF8);
    }

    public string GetNameAsTypeString () {
        var chars = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            GetNameAsTypeChars (ref chars);
            return chars.Span.GetPooledString ();
        } finally {
            chars.Dispose ();
        }
    }
}
