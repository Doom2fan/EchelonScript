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

internal static class DiagnosticDescriptors {
    public static DiagnosticDescriptor MissingRequiredType => new (
        id: "ESAE1001",
        title: "Could not find a required type definition",
        messageFormat: "Could not find definition for type {0}",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor StructNotPartial => new (
        id: "ESAE1002",
        title: "Exported type must be marked partial",
        messageFormat: "Exported type '{0}' must be marked with the partial keyword",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor ExportedTypeNestedInGeneric => new (
        id: "ESAE1003",
        title: "Exported type nested within generic type",
        messageFormat: "Exported type '{0}' cannot be nested within a generic type at any level of nesting",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor InstanceFieldInAggregate => new (
        id: "ESAE1004",
        title: "Instance field declared in exported aggregate",
        messageFormat: "Exported aggregates may not have instance fields, field '{0}' must be declared as a partial property",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor ExportedPropertiesMustBePartial => new (
        id: "ESAE1005",
        title: "Field property is not partial",
        messageFormat: "Field property '{0}' must be marked partial",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );
}
