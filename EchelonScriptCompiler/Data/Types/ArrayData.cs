/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Runtime.InteropServices;

namespace EchelonScriptCompiler.Data.Types {
    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public unsafe struct ES_ArrayTypeData {
        #region ================== Instance fields

        public ES_TypeInfo TypeInfo;

        private ES_TypeInfo* elementType;
        private int dimCount;

        #endregion

        #region ================== Constructors

        public ES_ArrayTypeData (ES_FullyQualifiedName fullyQualifiedName, ES_TypeInfo* elemType, int dims) {
            TypeInfo = new ES_TypeInfo (ES_TypeTag.Array, ES_AccessModifier.Public, ArrayPointer<byte>.Null, fullyQualifiedName);

            elementType = elemType;
            dimCount = dims;
        }

        #endregion

        #region ================== Instance properties

        /// <summary>The element type of this array.</summary>
        public ES_TypeInfo* ElementType { get => elementType; }

        /// <summary>The number of dimensions in this array.</summary>
        public int DimensionsCount { get => dimCount; }

        #endregion
    }
}
