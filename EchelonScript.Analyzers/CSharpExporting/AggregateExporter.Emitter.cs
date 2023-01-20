/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;

namespace EchelonScript.Analyzers.CSharpExporting.Internal;

internal sealed class AggregateExporter_Emitter {
    public const string TypeInterfaceName = "IES_ExportedType";
    public const string TypeInterfaceFullName = $"{ES_ExportGenerator.ExportInterfaceNamespace}.{TypeInterfaceName}";

    public string ExportStruct (IReadOnlyList<ExportedStruct> exportedStructs) {
        throw new NotImplementedException ();
    }
}
