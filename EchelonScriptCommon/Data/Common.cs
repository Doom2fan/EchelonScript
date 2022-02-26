/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCommon.Data.Types {
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
        public readonly ArrayPointer<byte> NamespaceName;
        public readonly ArrayPointer<byte> TypeName;

        public string NamespaceNameString {
            get => StringPool.Shared.GetOrAdd (NamespaceName.Span, ES_Encodings.Identifier);
        }
        public string TypeNameString {
            get => StringPool.Shared.GetOrAdd (TypeName.Span, ES_Encodings.Identifier);
        }

        public ES_FullyQualifiedName (ArrayPointer<byte> namespaceName, ArrayPointer<byte> typeName) {
            NamespaceName = namespaceName;
            TypeName = typeName;
        }

        public string GetNameAsTypeString () {
            Span<byte> bytes = stackalloc byte [NamespaceName.Length + TypeName.Length + 2];

            NamespaceName.Span.CopyTo (bytes);
            bytes.Slice (NamespaceName.Length, 2).Fill ((byte) ':');
            TypeName.Span.CopyTo (bytes.Slice (NamespaceName.Length + 2));

            return StringPool.Shared.GetOrAdd (bytes, ES_Encodings.Identifier);
        }
    }
}
