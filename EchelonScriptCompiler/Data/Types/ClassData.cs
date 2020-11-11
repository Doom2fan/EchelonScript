/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;
using EchelonScriptCompiler.Frontend.Parser;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Data {
    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public unsafe struct ES_ClassData : ES_TypeData {
        public unsafe sealed class Builder : ES_TypeData {
            #region ================== Instance fields

            private ES_ClassData* classData;

            #endregion

            #region ================== Instance properties

            /// <inheritdoc/>
            public string TypeName => classData->TypeName;

            /// <inheritdoc/>
            public bool Valid {
                get => classData->valid;
                set => classData->valid = value;
            }

            /// <inheritdoc/>
            public int Size {
                get => classData->sizeInBytes;
                set => classData->sizeInBytes = value;
            }

            /// <summary>The base class of this class.</summary>
            public ES_ClassData* BaseClass {
                get => classData->baseClass;
                set => classData->baseClass = value;
            }

            /// <summary>The interfaces list of this class.</summary>
            private ArrayPointer<ES_InterfaceData> InterfacesList {
                get => classData->interfacesList;
                set => classData->interfacesList = value;
            }

            #endregion

            #region ================== Constructors

            internal Builder ([DisallowNull] ES_ClassData* data, ArrayPointer<byte> typeName) {
                classData = data;
                data->typeName = typeName;
            }

            #endregion
        }

        #region ================== Instance fields

        private ArrayPointer<byte> typeName;
        private ES_ClassData* baseClass;
        private ArrayPointer<ES_InterfaceData> interfacesList;
        private bool valid;
        private int sizeInBytes;

        #endregion

        #region ================== Instance properties

        /// <inheritdoc/>
        public string TypeName {
            get {
                return StringPool.Shared.GetOrAdd (typeName.Span, Encoding.ASCII);
            }
        }

        /// <inheritdoc/>
        public bool Valid { get => valid; }

        /// <inheritdoc/>
        public int Size { get => sizeInBytes; }

        /// <summary>The base class of this class.</summary>
        public ES_ClassData* BaseClass { get => baseClass; }

        /// <summary>The interfaces list of this class.</summary>
        private ArrayPointer<ES_InterfaceData> InterfacesList => interfacesList;

        #endregion
    }
}
