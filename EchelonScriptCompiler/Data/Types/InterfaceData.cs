/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using System.Text;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Data {
    public unsafe struct ES_InterfaceData : ES_TypeData {
        public unsafe sealed class Builder : ES_TypeData {
            #region ================== Instance fields

            private ES_InterfaceData* interfaceData;

            #endregion

            #region ================== Instance properties

            /// <inheritdoc/>
            public string TypeName => interfaceData->TypeName;

            /// <inheritdoc/>
            public bool Valid {
                get => interfaceData->valid;
                set => interfaceData->valid = value;
            }

            /// <inheritdoc/>
            public int Size {
                get => interfaceData->sizeInBytes;
                set => interfaceData->sizeInBytes = value;
            }

            /// <summary>The interfaces list of this interface.</summary>
            public ArrayPointer<ES_InterfaceData> InterfacesList {
                get => interfaceData->interfacesList;
                set => interfaceData->interfacesList = value;
            }

            #endregion

            #region ================== Constructors

            internal Builder ([DisallowNull] ES_InterfaceData* data) {
                interfaceData = data;
            }

            #endregion
        }

        #region ================== Instance fields

        private ArrayPointer<byte> typeName;
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

        /// <summary>The interfaces list of this interface.</summary>
        public ArrayPointer<ES_InterfaceData> InterfacesList => interfacesList;

        #endregion
    }
}
