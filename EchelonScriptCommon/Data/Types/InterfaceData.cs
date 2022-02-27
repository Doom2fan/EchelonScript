﻿/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCommon.Data.Types;

public unsafe struct ES_InterfaceData {
    public unsafe sealed class Builder {
        #region ================== Instance properties

        /// <summary>The pointer to the interface this builder is for.</summary>
        public ES_InterfaceData* InterfaceData { get; }

        #endregion

        #region ================== Constructors

        internal Builder ([DisallowNull] ES_InterfaceData* data, ES_AccessModifier accessMod,
            ES_FullyQualifiedName fullyQualifiedName, ArrayPointer<byte> sourceUnit
        ) {
            InterfaceData = data;
            data->TypeInfo = new ES_TypeInfo (ES_TypeTag.Interface, accessMod, ES_TypeFlag.NoNew, sourceUnit, fullyQualifiedName);
        }

        #endregion
    }

    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;

    #endregion

    #region ================== Instance properties

    #endregion
}
