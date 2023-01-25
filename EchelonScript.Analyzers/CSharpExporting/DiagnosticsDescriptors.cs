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

internal static partial class DiagnosticDescriptors {
    public static DiagnosticDescriptor MissingRequiredType => new (
        id: GetExportErrorId (DiagnosticId.MissingRequiredType),
        title: "Could not find a required type definition",
        messageFormat: "Could not find definition for type {0}",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor StructNotPartial => new (
        id: GetExportErrorId (DiagnosticId.StructNotPartial),
        title: "Exported type must be marked partial",
        messageFormat: "Exported type '{0}' must be marked with the partial keyword",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor DefinitionStructMissing => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructMissing),
        title: "Missing definition struct in exported aggregate",
        messageFormat: $"Exported aggregate must have a struct named '{AggregateExporter_Parser.DefinitionStructName}' containing its members",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor DefinitionTypeMustBeAStruct => new (
        id: GetExportErrorId (DiagnosticId.DefinitionTypeMustBeAStruct),
        title: "Definition type is not a struct",
        messageFormat: "Export definition type for type '{0}' must be a struct",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor DefinitionStructMustBePartial => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructMustBePartial),
        title: "Definition struct must be partial",
        messageFormat: "Export definition struct for type '{0}' must be partial",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor DefinitionStructCannotBeGeneric => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructCannotBeGeneric),
        title: "Definition struct is a generic type",
        messageFormat: "Export definition struct for type '{0}' cannot have type parameters",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor DefinitionStructMustBeUnmanaged => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructMustBeUnmanaged),
        title: "Definition struct is not unmanaged",
        messageFormat: "Export definition struct for type '{0}' must be unmanaged",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor DefinitionStructMustBeDeclaredOnce => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructMustBeUnmanaged),
        title: "Definition struct cannot be declared in multiple places",
        messageFormat: "Export definition struct for type '{0}' cannot be declared in multiple places",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor ExportedTypeNestedInGeneric => new (
        id: GetExportErrorId (DiagnosticId.ExportedTypeNestedInGeneric),
        title: "Exported type nested within generic type",
        messageFormat: "Exported type '{0}' cannot be nested within a generic type at any level of nesting",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor InstanceMemberInAggregate => new (
        id: GetExportErrorId (DiagnosticId.InstanceMemberInAggregate),
        title: "Instance member declared outside definition struct",
        messageFormat: "Exported aggregates may not directly contain instance methods, fields or properties, member '{0}' must be defined in the definition struct",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor StaticMemberInDefStruct => new (
        id: GetExportErrorId (DiagnosticId.StaticMemberInDefStruct),
        title: "Static member declared in definition struct",
        messageFormat: "Definition structs may not contain static methods, fields or properties, member '{0}' must be defined in the exported aggregate",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor ExportedMembersCannotBeReadonly => new (
        id: GetExportErrorId (DiagnosticId.ExportedMembersCannotBeReadonly),
        title: "Member is readonly",
        messageFormat: "Member '{0}' cannot be readonly",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor RefMembersNotAllowed => new (
        id: GetExportErrorId (DiagnosticId.RefMembersNotAllowed),
        title: "Exported members cannot be 'ref'",
        messageFormat: "Exported members '{0}' cannot be ref",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor RefReturnNotAllowed => new (
        id: GetExportErrorId (DiagnosticId.RefReturnNotAllowed),
        title: "Return values of exported functions cannot be 'ref'",
        messageFormat: "Return value for exported function '{0}' cannot be ref",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor ReferenceTypesNotAllowed => new (
        id: GetExportErrorId (DiagnosticId.ReferenceTypesNotAllowed),
        title: "Reference types are not allowed in exports",
        messageFormat: "Reference type '{0}' cannot be exported",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );

    public static DiagnosticDescriptor DisallowedTypeInField => new (
        id: GetExportErrorId (DiagnosticId.DisallowedTypeInField),
        title: "Disallowed type in exports",
        messageFormat: "Type '{0}' cannot be exported",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: new [] { WellKnownDiagnosticTags.NotConfigurable }
    );
}
