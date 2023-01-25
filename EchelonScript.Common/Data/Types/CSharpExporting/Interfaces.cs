/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Common.Data;

namespace EchelonScript.Common.Exporting;

public unsafe interface IES_ExportedType {
    protected internal ES_TypeTable.BasicTypeInfo GetBasicData (ES_TypeTable.TypeLoader typeLoader);
    protected internal void InitializeType (ES_TypeTable.TypeLoader typeLoader, ref ES_TypeTable.TypeLoadToken typeToken);
}

internal interface IES_ReferenceType {
    protected internal void InitializeType (ES_TypeTable.TypeLoader typeLoader, ref ES_TypeTable.TypeLoadToken typeToken);
}

internal interface IES_ArrayType : IES_ReferenceType { }
