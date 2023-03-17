﻿/*
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
    private static string [] errorTags = new [] { WellKnownDiagnosticTags.NotConfigurable };

    /*
     *
     * Export source generator
     *
     */
    public static DiagnosticDescriptor MissingRequiredType => new (
        id: GetExportErrorId (DiagnosticId.MissingRequiredType),
        title: "Could not find a required type definition",
        messageFormat: "Could not find definition for type {0}",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor StructNotPartial => new (
        id: GetExportErrorId (DiagnosticId.StructNotPartial),
        title: "Exported type must be partial",
        messageFormat: "Exported type '{0}' must have the partial keyword",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor StructIsRef => new (
        id: GetExportErrorId (DiagnosticId.StructIsRef),
        title: "Exported type cannot be ref",
        messageFormat: "Exported type '{0}' cannot have the ref keyword",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor StructIsGeneric => new (
        id: GetExportErrorId (DiagnosticId.StructIsGeneric),
        title: "Exported type is a generic type",
        messageFormat: "Exported type '{0}' cannot be have type parameters",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor StructNotUnmanaged => new (
        id: GetExportErrorId (DiagnosticId.StructNotUnmanaged),
        title: "Exported type must be partial",
        messageFormat: "Exported type '{0}' must be unmanaged",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DefinitionStructMissing => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructMissing),
        title: "Missing definition struct in exported aggregate",
        messageFormat: $"Exported aggregate must have a struct named '{AggregateExporter_Parser.DefinitionStructName}' containing its members",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DefinitionTypeNotAStruct => new (
        id: GetExportErrorId (DiagnosticId.DefinitionTypeNotAStruct),
        title: "Definition type is not a struct",
        messageFormat: "Export definition type for type '{0}' must be a struct",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DefinitionStructNotPartial => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructNotPartial),
        title: "Definition struct must be partial",
        messageFormat: "Export definition struct for type '{0}' must have the partial keyword",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DefinitionStructIsGeneric => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructIsGeneric),
        title: "Definition struct is a generic type",
        messageFormat: "Export definition struct for type '{0}' cannot have type parameters",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DefinitionStructNotUnmanaged => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructNotUnmanaged),
        title: "Definition struct is not unmanaged",
        messageFormat: "Export definition struct for type '{0}' must be unmanaged",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DefinitionStructNotPrivate => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructNotPrivate),
        title: "Definition struct is not private",
        messageFormat: "Export definition struct for type '{0}' must be private",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DefinitionStructDeclaredInMultiplePlaces => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructDeclaredInMultiplePlaces),
        title: "Definition struct cannot be declared in multiple places",
        messageFormat: "Export definition struct for type '{0}' cannot be declared in multiple places",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DefinitionStructIsRef => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructIsRef),
        title: "Definition struct cannot be ref",
        messageFormat: "Export definition struct for type '{0}' cannot have the ref keyword",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor ExportedTypeNestedInGeneric => new (
        id: GetExportErrorId (DiagnosticId.ExportedTypeNestedInGeneric),
        title: "Exported type nested within generic type",
        messageFormat: "Exported type '{0}' cannot be nested within a generic type at any level of nesting",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor InstanceMemberInAggregate => new (
        id: GetExportErrorId (DiagnosticId.InstanceMemberInAggregate),
        title: "Instance member declared outside definition struct",
        messageFormat: "Exported aggregates may not directly contain instance methods, fields or properties, member '{0}' must be defined in the definition struct",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor StaticMemberInDefStruct => new (
        id: GetExportErrorId (DiagnosticId.StaticMemberInDefStruct),
        title: "Static member declared in definition struct",
        messageFormat: "Definition structs may not contain static methods, fields or properties, member '{0}' must be defined in the exported aggregate",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor ExportedMemberIsReadonly => new (
        id: GetExportErrorId (DiagnosticId.ExportedMemberIsReadonly),
        title: "Member is readonly",
        messageFormat: "Member '{0}' cannot be readonly",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor RefMembersNotAllowed => new (
        id: GetExportErrorId (DiagnosticId.RefMembersNotAllowed),
        title: "Exported members cannot be 'ref'",
        messageFormat: "Exported members '{0}' cannot be ref",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor RefReturnNotAllowed => new (
        id: GetExportErrorId (DiagnosticId.RefReturnNotAllowed),
        title: "Return values of exported functions cannot be 'ref'",
        messageFormat: "Return value for exported function '{0}' cannot be ref",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor ReferenceTypesNotAllowed => new (
        id: GetExportErrorId (DiagnosticId.ReferenceTypesNotAllowed),
        title: "Reference types are not allowed in exports",
        messageFormat: "Reference type '{0}' cannot be exported",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DisallowedTypeInField => new (
        id: GetExportErrorId (DiagnosticId.DisallowedTypeInField),
        title: "Disallowed type in export",
        messageFormat: "Type '{0}' cannot be exported",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    private const string ValidIdentifierChars = "Identifiers may only contain letters (A-Z), numbers (0-9) and underscores (_)";
    public static DiagnosticDescriptor InvalidNamespace => new (
        id: GetExportErrorId (DiagnosticId.InvalidNamespace),
        title: "Invalid export namespace",
        messageFormat: "The export namespace for type '{0}' is invalid. Namespaces may only contain letters (A-Z), numbers (0-9) and underscores (_), with different segments separated by dots (.)",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor InvalidTypeName => new (
        id: GetExportErrorId (DiagnosticId.InvalidTypeName),
        title: "Invalid export name for field",
        messageFormat: $"The export name for type '{{0}}' is invalid. {ValidIdentifierChars}",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor InvalidAutoTypeName => new (
        id: GetExportErrorId (DiagnosticId.InvalidAutoTypeName),
        title: "Native name cannot be used for auto-named exported type",
        messageFormat: $"The native name for type '{{0}}' is not a valid identifier. {ValidIdentifierChars}",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor InvalidFieldName => new (
        id: GetExportErrorId (DiagnosticId.InvalidFieldName),
        title: "Invalid export name for field",
        messageFormat: $"The export name for field '{{0}}' is invalid. {ValidIdentifierChars}",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor InvalidAutoFieldName => new (
        id: GetExportErrorId (DiagnosticId.InvalidAutoFieldName),
        title: "Native name cannot be used for auto-named exported field",
        messageFormat: $"The native name for field '{{0}}' is not a valid identifier. {ValidIdentifierChars}",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    /*
     *
     * Error analyzer
     *
     */
    public static DiagnosticDescriptor NonStructExported => new (
        id: GetExportErrorId (DiagnosticId.NonStructExported),
        title: "Non-struct type cannot be exported",
        messageFormat: "Type '{0}' is not an unmanaged struct and cannot be exported",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor ReferenceUsedOutsideExport => new (
        id: GetExportErrorId (DiagnosticId.ReferenceUsedOutsideExport),
        title: "GC reference in non-exported type",
        messageFormat: "Member '{0}' in type non-exported '{1}' cannot be a GC reference",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );
}
