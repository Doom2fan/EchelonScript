/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Data.Types {
    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public unsafe struct ES_ClassData {
        public unsafe sealed class Builder {
            #region ================== Instance fields

            private ES_ClassData* classData;

            #endregion

            #region ================== Instance properties

            /// <summary>The pointer to the class this builder is for.</summary>
            public ES_ClassData* ClassData => classData;

            /// <summary>The base class of this class.</summary>
            public ES_ClassData* BaseClass {
                get => classData->baseClass;
                set => classData->baseClass = value;
            }

            /// <summary>The interfaces list of this class.</summary>
            public ArrayPointer<Pointer<ES_InterfaceData>> InterfacesList {
                get => classData->interfacesList;
                set => classData->interfacesList = value;
            }

            #endregion

            #region ================== Constructors

            internal Builder ([DisallowNull] ES_ClassData* data, ES_AccessModifier accessMod,
                ES_FullyQualifiedName fullyQualifiedName, ArrayPointer<byte> sourceUnit
            ) {
                classData = data;
                data->TypeInfo = new ES_TypeInfo (ES_TypeTag.Class, accessMod, sourceUnit, fullyQualifiedName);
            }

            #endregion
        }

        #region ================== Instance fields

        public ES_TypeInfo TypeInfo;
        private ES_ClassData* baseClass;
        private ArrayPointer<Pointer<ES_InterfaceData>> interfacesList;

        #endregion

        #region ================== Instance properties

        /// <summary>The base class of this class.</summary>
        public ES_ClassData* BaseClass { get => baseClass; }

        /// <summary>The interfaces list of this class.</summary>
        private ArrayPointer<Pointer<ES_InterfaceData>> InterfacesList => interfacesList;

        #endregion
    }
}
