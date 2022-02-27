/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCommon.Data.Types;

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ES_ClassData {
    public unsafe sealed class Builder {
        #region ================== Instance properties

        /// <summary>The pointer to the class this builder is for.</summary>
        public ES_ClassData* ClassData { get; }

        /// <summary>The base class of this class.</summary>
        public ES_ClassData* BaseClass {
            get => ClassData->baseClass;
            set => ClassData->baseClass = value;
        }

        /// <summary>The interfaces list of this class.</summary>
        public ArrayPointer<Pointer<ES_InterfaceData>> InterfacesList {
            get => ClassData->interfacesList;
            set => ClassData->interfacesList = value;
        }

        #endregion

        #region ================== Constructors

        internal Builder ([DisallowNull] ES_ClassData* data, ES_AccessModifier accessMod,
            ES_FullyQualifiedName fullyQualifiedName, ArrayPointer<byte> sourceUnit
        ) {
            ClassData = data;
            data->TypeInfo = new ES_TypeInfo (ES_TypeTag.Class, accessMod, ES_TypeFlag.NoNew, sourceUnit, fullyQualifiedName);
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
    public ES_ClassData* BaseClass => baseClass;

    /// <summary>The interfaces list of this class.</summary>
    private ArrayPointer<Pointer<ES_InterfaceData>> InterfacesList => interfacesList;

    #endregion
}
