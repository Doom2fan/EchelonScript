/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScript.Common.Data.Types;

namespace EchelonScript.Common.Exporting;

[AttributeUsage (AttributeTargets.Struct)]
public sealed class ES_ExportStructAttribute : Attribute {
    public string? ExportName { get; init; }
    public string []? ExportNamespace { get; init; }
}

[AttributeUsage (AttributeTargets.Struct)]
public sealed class ES_ExportClassAttribute : Attribute {
    public string []? ExportNamespace { get; init; }
    public string? ExportName { get; init; }
    public Type? ParentClass { get; init; }
}

[AttributeUsage (AttributeTargets.Property)]
public sealed class ES_ExportFieldAttribute : Attribute {
    public string? ExportName { get; init; }
    public required ES_AccessModifier AccessModifier { get; init; }
    public required ES_Constness Constness { get; init; }
}
