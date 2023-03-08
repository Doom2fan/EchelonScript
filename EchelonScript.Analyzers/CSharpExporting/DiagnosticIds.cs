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
        DefinitionTypeMustBeAStruct,
        DefinitionStructMustBePartial,
        DefinitionStructCannotBeGeneric,
        DefinitionStructMustBeUnmanaged,
        DefinitionStructMustBePrivate,
        DefinitionStructMustBeDeclaredOnce,
        ExportedTypeNestedInGeneric,
        InstanceMemberInAggregate,
        StaticMemberInDefStruct,
        ExportedMembersCannotBeReadonly,
        RefMembersNotAllowed,
        RefReturnNotAllowed,
        ReferenceTypesNotAllowed,
        DisallowedTypeInField,
        NonStructExported,
        NoReferencesOutsideExports,
        InvalidNamespace,
        InvalidTypeName,
        InvalidAutoTypeName,
        InvalidFieldName,
        InvalidAutoFieldName,
    }

    private static string GetExportErrorId (DiagnosticId id) {
        Debug.Assert ((ushort) id <= 999);
        return $"ESAE1{(int) id:D3}";
    }
}
