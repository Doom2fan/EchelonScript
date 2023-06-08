/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;

namespace EchelonScript.Analyzers.CSharpExporting.Internal;

static partial class DiagnosticDescriptors {
    private enum DiagnosticId : ushort {
        MissingRequiredType = 1,
        StructNotPartial,
        DefinitionStructMissing,
        DefinitionTypeNotAStruct,
        DefinitionStructNotPartial,
        DefinitionStructIsGeneric,
        DefinitionStructNotUnmanaged,
        DefinitionStructNotPrivate,
        DefinitionStructDeclaredInMultiplePlaces,
        ExportedTypeNestedInGeneric,
        InstanceMemberInAggregate,
        StaticMemberInDefStruct,
        ExportedMemberIsReadonly,
        RefReturnNotAllowed,
        ReferenceTypesNotAllowed,
        DisallowedTypeInField_NotExportOrPrimitive,
        ReferenceUsedOutsideExport,
        InvalidNamespace,
        InvalidTypeName,
        InvalidAutoTypeName,
        InvalidFieldName,
        InvalidAutoFieldName,
        StructNotUnmanaged,
        StructIsRef,
        StructIsGeneric,
        DefinitionStructIsRef,
        ExportedTypeNestedInNonPartial,
        ExportUsedAsValueTypeOutsideExport,
        ProtectedMemberInStruct,
        ProtectedMemberInSealedClass,
        DuplicateFieldExportName,
        FieldNameUsedInBase,
        FieldExportNameUsedInBase,
        InheritsFromSealedType,
        InheritanceCycle,
        ClassInheritsNonClass,
        ManagedTypesNotAllowed,
        DefinitionStructReferenced,
        ClassUsedAsAValueType,
    }

    private static string GetExportErrorId (DiagnosticId id) {
        Debug.Assert ((ushort) id <= 999);
        return $"ESAE1{(int) id:D3}";
    }
}
