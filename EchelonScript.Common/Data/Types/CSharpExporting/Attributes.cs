/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;

namespace EchelonScript.Common.Exporting;

[AttributeUsage (AttributeTargets.Struct)]
public sealed class ES_ExportStructAttribute : Attribute {
    public string? Namespace { get; init; }
    public string? Name { get; init; }
}

[AttributeUsage (AttributeTargets.Struct)]
public sealed class ES_ExportClassAttribute : Attribute {
    public string? Namespace { get; init; }
    public string? Name { get; init; }
    public Type? ParentClass { get; init; }
}

[AttributeUsage (AttributeTargets.Field)]
public sealed class ES_ExportFieldAttribute : Attribute {
    public string? Name { get; init; }
    public required ES_AccessModifier AccessModifier { get; init; }
    public required ES_Constness Constness { get; init; }
}
