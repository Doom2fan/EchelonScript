/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Data.Types {
    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "StructData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_StructData : ES_TypeData {
        public unsafe sealed class Builder : ES_TypeData {
            #region ================== Instance fields

            private ES_StructData* structData;

            #endregion

            #region ================== Instance properties

            /// <inheritdoc/>
            public string TypeName => structData->TypeName;

            /// <inheritdoc/>
            public bool Valid {
                get => structData->valid;
                set => structData->valid = value;
            }

            /// <inheritdoc/>
            public int Size {
                get => structData->sizeInBytes;
                set => structData->sizeInBytes = value;
            }

            /// <summary>The interfaces list of this struct.</summary>
            private ArrayPointer<ES_InterfaceData> InterfacesList {
                get => structData->interfacesList;
                set => structData->interfacesList = value;
            }

            #endregion

            #region ================== Constructors

            internal Builder ([DisallowNull] ES_StructData* data, ArrayPointer<byte> typeName) {
                structData = data;
                data->typeName = typeName;
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

        /// <summary>The interfaces list of this struct.</summary>
        private ArrayPointer<ES_InterfaceData> InterfacesList => interfacesList;

        #endregion
    }
}
