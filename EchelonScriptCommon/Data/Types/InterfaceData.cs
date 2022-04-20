/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScriptCommon.Data.Types;

public unsafe struct ES_InterfaceData {
    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;

    #endregion

    internal ES_InterfaceData (
        ES_FullyQualifiedName fullyQualifiedName, ES_AccessModifier accessMod, ES_Identifier sourceUnit
    ) {
        TypeInfo = new ES_TypeInfo (ES_TypeTag.Interface, accessMod, ES_TypeFlag.NoNew, sourceUnit, fullyQualifiedName);
    }
}
