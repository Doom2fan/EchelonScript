/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;

namespace EchelonScript.Common.Exporting;

public unsafe interface IES_ExportedType {
    public static ES_MethodTable* MethodTable { get; internal set; }

    public abstract static void InitializeType (ES_NativeTypeTable.NativeTypeLoader typeLoader, ref ES_NativeTypeTable.TypeLoadToken typeToken);
}
