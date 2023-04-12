/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using Microsoft.CodeAnalysis;

namespace EchelonScript.Analyzers.CSharpExporting.Internal;

internal enum ExportedFieldSpecialType {
    None,
    Reference,
    Array,
}

internal struct ExportedField {
    public ExportedFieldSpecialType SpecialType;

    public bool NoExport;
    public string ExportName;
    public string AccessModifier;
    public string Constness;

    public string SourceType;
    public string FieldType;
    public string PropertyName;
    public string PropertyAccessibility;
}

internal struct ExportedMethod {

}

internal struct ExportedFunction {

}

internal struct ExportedStruct {
    public bool Error;

    public string NativeName;
    public string NativeNamespace;
    public string [] NativeParents;

    public bool IsClass;
    public bool IsSealed;
    public INamedTypeSymbol? BaseClass;

    public string? ExportNamespace;
    public string ExportName;

    public ExportedField [] Fields;
}
