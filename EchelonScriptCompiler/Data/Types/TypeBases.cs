﻿/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Text;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Data.Types {
    public enum ES_TypeTag : int {
        UNKNOWN   = 0,
        Void      = 1,
        Bool      = 2,
        Int       = 3,
        Float     = 4,
        Function  = 5,
        Struct    = 6,
        Class     = 7,
        Enum      = 8,
        Interface = 9,
        Pointer   = 10,
        Const     = 11,
        Immutable = 12,
        Array     = 13,
    }

    public unsafe struct ES_TypeInfo {
        #region ================== Instance fields

        /// <summary>What type of type this is.</summary>
        public readonly ES_TypeTag TypeTag;

        /// <summary>The size of the type at runtime, in bytes.</summary>
        public int RuntimeSize;

        /// <summary>The type's access modifier.</summary>
        public readonly ES_AccessModifier AccessModifier;

        /// <summary>The name of the type.</summary>
        public readonly ArrayPointer<byte> TypeName;

        /// <summary>The fully qualified name of the type.</summary>
        public readonly ArrayPointer<byte> FullyQualifiedName;

        /// <summary>The source translation unit of the type.</summary>
        public readonly ArrayPointer<byte> SourceUnit;

        /// <summary>The members list of the type.</summary>
        public ES_TypeMembers MembersList;

        #endregion

        #region ================== Constructors

        public ES_TypeInfo (
            ES_TypeTag typeTag, ES_AccessModifier accessMod,
            ArrayPointer<byte> sourceUnit,
            ArrayPointer<byte> name, ArrayPointer<byte> fullyQualifiedName
        ) {
            TypeTag = typeTag;
            RuntimeSize = -1;

            AccessModifier = accessMod;

            TypeName = name;
            FullyQualifiedName = fullyQualifiedName;
            SourceUnit = sourceUnit;

            MembersList = new ES_TypeMembers ();
        }

        #endregion

        #region ================== Instance properties

        #region String utilities

        /// <summary>The type's name as a string.</summary>
        public string TypeNameString {
            get {
                return StringPool.Shared.GetOrAdd (TypeName.Span, Encoding.ASCII);
            }
        }

        /// <summary>The type's fully qualified name as a string.</summary>
        public string FullyQualifiedNameString {
            get {
                return StringPool.Shared.GetOrAdd (FullyQualifiedName.Span, Encoding.ASCII);
            }
        }

        /// <summary>The type's source unit's name as a string.</summary>
        public string SourceUnitString {
            get {
                return StringPool.Shared.GetOrAdd (SourceUnit.Span, Encoding.ASCII);
            }
        }

        #endregion

        #endregion
    }
}
