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

    public static DiagnosticDescriptor ExportedTypeNestedInNonPartial => new (
        id: GetExportErrorId (DiagnosticId.ExportedTypeNestedInNonPartial),
        title: "Exported type nested within a type that is not partial",
        messageFormat: "Exported type '{0}' cannot be nested within a type that is not partial",
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

    public static DiagnosticDescriptor ManagedTypesNotAllowed => new (
        id: GetExportErrorId (DiagnosticId.ManagedTypesNotAllowed),
        title: "Managed types are not allowed in exports",
        messageFormat: "Managed type '{0}' cannot be exported",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DisallowedTypeInField_NotExportOrPrimitive => new (
        id: GetExportErrorId (DiagnosticId.DisallowedTypeInField_NotExportOrPrimitive),
        title: "Disallowed type in export",
        messageFormat: "Type '{0}' cannot be exported: Type is not a primitive or an exported type",
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

    public static DiagnosticDescriptor DuplicateFieldExportName => new (
        id: GetExportErrorId (DiagnosticId.DuplicateFieldExportName),
        title: "Duplicate field export name",
        messageFormat: $"The type '{{0}}' already contains a field with the export name '{{1}}'",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor FieldNameUsedInBase => new (
        id: GetExportErrorId (DiagnosticId.FieldNameUsedInBase),
        title: "Field name already defined in base type",
        messageFormat: $"The native field '{{1}}' is already defined in the base type '{{0}}'",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor FieldExportNameUsedInBase => new (
        id: GetExportErrorId (DiagnosticId.FieldExportNameUsedInBase),
        title: "Field export name already defined in base type",
        messageFormat: $"The field export name '{{1}}' is already defined in the base type '{{0}}'",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor ProtectedMemberInStruct => new (
        id: GetExportErrorId (DiagnosticId.ProtectedMemberInStruct),
        title: "New protected member declared in struct",
        messageFormat: $"New protected member '{{0}}' declared in struct '{{1}}'",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor ProtectedMemberInSealedClass => new (
        id: GetExportErrorId (DiagnosticId.ProtectedMemberInSealedClass),
        title: "New protected member declared in sealed class",
        messageFormat: $"New protected member '{{0}}' declared in sealed class '{{1}}'",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor InheritsFromSealedType => new (
        id: GetExportErrorId (DiagnosticId.InheritsFromSealedType),
        title: "Type inherits a sealed type",
        messageFormat: $"Type '{{0}}' cannot inherit from sealed type '{{1}}'",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor InheritanceCycle => new (
        id: GetExportErrorId (DiagnosticId.InheritanceCycle),
        title: "Type creates inheritance cycle",
        messageFormat: $"Type '{{0}}' inheriting from '{{1}}' creates an inheritance cycle",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor ClassInheritsNonClass => new (
        id: GetExportErrorId (DiagnosticId.ClassInheritsNonClass),
        title: "Class cannot inherit from a non-class type",
        messageFormat: $"Class '{{0}}' cannot inherit from non-class type '{{1}}'",
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
    public static DiagnosticDescriptor ReferenceUsedOutsideExport => new (
        id: GetExportErrorId (DiagnosticId.ReferenceUsedOutsideExport),
        title: "GC reference in non-exported type",
        messageFormat: "Member '{0}' in non-exported type '{1}' cannot be a GC reference",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor ExportUsedAsValueTypeOutsideExport => new (
        id: GetExportErrorId (DiagnosticId.ExportUsedAsValueTypeOutsideExport),
        title: "Exported type used by-value in non-exported type",
        messageFormat: "Member '{0}' in non-exported type '{1}' cannot be of exported type '{2}'",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor DefinitionStructReferenced => new (
        id: GetExportErrorId (DiagnosticId.DefinitionStructReferenced),
        title: "Reference to export definition struct",
        messageFormat: "Export definition structs cannot be used in code",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );

    public static DiagnosticDescriptor ClassUsedAsAValueType => new (
        id: GetExportErrorId (DiagnosticId.ClassUsedAsAValueType),
        title: "Exported class used as a value type",
        messageFormat: "Exported class '{0}' cannot be used as a value type",
        category: Constants.Category.Exports,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true,
        customTags: errorTags
    );
}
