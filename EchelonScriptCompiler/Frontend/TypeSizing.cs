﻿/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Frontend {
    public unsafe partial class CompilerFrontend {
        private void TypeSizing () {
            Debug.Assert (Environment is not null);
            Debug.Assert (EnvironmentBuilder is not null);

            var refListRefType = EnvironmentBuilder.ReferenceTypeRefList;
            foreach (var nmData in Environment.Namespaces.Values) {
                foreach (var type in nmData.Types) {
                    if (TypeSizing_AnalyzeCycles (type))
                        continue;

                    TypeSizing_SizeType (type, refListRefType);
                }
            }
        }

        private void TypeSizing_SizeType ([NotNull] ES_TypeInfo* type, ArrayPointer<nint> refListRefType) {
            Debug.Assert (Environment is not null);
            Debug.Assert (EnvironmentBuilder is not null);

            if (type->Flags.HasFlag (ES_TypeFlag.Analyzed))
                return;

            var hasRefs = !type->Flags.HasFlag (ES_TypeFlag.NoRefs);

            switch (type->TypeTag) {
                case ES_TypeTag.Array:
                case ES_TypeTag.Interface:
                case ES_TypeTag.Reference: {
                    type->RuntimeSize = sizeof (void*);
                    type->RefsList = refListRefType;
                    type->Flags &= ~ES_TypeFlag.NoRefs;
                    type->Flags |= ES_TypeFlag.Analyzed;
                    return;
                }

                case ES_TypeTag.Void:
                case ES_TypeTag.Bool:
                case ES_TypeTag.Int:
                case ES_TypeTag.Float:
                case ES_TypeTag.Function:
                case ES_TypeTag.Enum:
                    type->Flags |= ES_TypeFlag.Analyzed;
                    return;

                case ES_TypeTag.Const:
                case ES_TypeTag.Immutable: {
                    var constData = (ES_ConstData*) type;

                    TypeSizing_SizeType (constData->InnerType, refListRefType);

                    type->RuntimeSize = constData->InnerType->RuntimeSize;
                    type->RefsList = constData->InnerType->RefsList;

                    type->Flags = constData->InnerType->Flags | ES_TypeFlag.Analyzed;

                    return;
                }

                case ES_TypeTag.Struct:
                case ES_TypeTag.Class:
                    break;

                default:
                    throw new NotImplementedException ("Type not implemented yet.");
            }

            using var refsList = new StructPooledList<nint> (CL_ClearMode.Auto);
            var curOffs = 0;
            foreach (var memberAddr in type->MembersList.MembersList.Span) {
                var member = memberAddr.Address;

                if (member->MemberType != ES_MemberType.Field)
                    continue;

                if (member->Flags.HasFlag (ES_MemberFlags.Static))
                    continue;

                var field = (ES_MemberData_Variable*) member;

                field->Offset = curOffs;

                TypeSizing_SizeType (field->Type, refListRefType);

                curOffs += field->Type->RuntimeSize;

                foreach (var refOffs in field->Type->RefsList.Span)
                    refsList.Add (field->Offset + refOffs);

                hasRefs |= !field->Type->Flags.HasFlag (ES_TypeFlag.NoRefs);
            }

            type->RuntimeSize = curOffs;
            type->RefsList = ArrayPointer<nint>.Null;
            if (refsList.Count > 0) {
                type->RefsList = EnvironmentBuilder.MemoryManager.GetArrayAligned<nint> (refsList.Count, sizeof (nint));
                refsList.Span.CopyTo (type->RefsList.Span);
            }

            type->Flags &= ~ES_TypeFlag.NoRefs;
            if (!hasRefs)
                type->Flags |= ES_TypeFlag.NoRefs;

            type->Flags |= ES_TypeFlag.Analyzed;
        }

        private bool TypeSizing_AnalyzeCycles ([NotNull] ES_TypeInfo* type) {
            switch (type->TypeTag) {
                case ES_TypeTag.Struct:
                case ES_TypeTag.Class:
                    break;

                case ES_TypeTag.Void:
                case ES_TypeTag.Bool:
                case ES_TypeTag.Int:
                case ES_TypeTag.Float:
                case ES_TypeTag.Function:
                case ES_TypeTag.Enum:
                case ES_TypeTag.Interface:
                case ES_TypeTag.Reference:
                case ES_TypeTag.Const:
                case ES_TypeTag.Immutable:
                case ES_TypeTag.Array:
                    return false;

                default:
                    throw new NotImplementedException ("Type not implemented yet.");
            }

            var hasCycles = false;
            foreach (var memberAddr in type->MembersList.MembersList.Span) {
                var member = memberAddr.Address;

                if (member->MemberType != ES_MemberType.Field)
                    continue;

                if (member->Flags.HasFlag (ES_MemberFlags.Static))
                    continue;

                var field = (ES_MemberData_Variable*) member;

                if (TypeSizing_AnalyzeCycles_Traverse (field->Type, type)) {
                    if (!EnvironmentBuilder!.PointerAstMap.TryGetValue ((IntPtr) field, out var astNode))
                        Debug.Fail ("PointerAstMap is missing field mappings.");

                    var fieldAstNode = astNode as ES_AstMemberVarDefinition;
                    Debug.Assert (fieldAstNode is not null);

                    errorList.Add (ES_FrontendErrors.GenFieldCausesCycle (
                        member->Name.GetPooledString (Encoding.ASCII), type->Name.GetNameAsTypeString (), fieldAstNode.Name
                    ));

                    hasCycles = true;
                }
            }

            return hasCycles;
        }

        private bool TypeSizing_AnalyzeCycles_Traverse ([NotNull] ES_TypeInfo* innerType, [NotNull] ES_TypeInfo* containingType) {
            switch (innerType->TypeTag) {
                case ES_TypeTag.Struct:
                case ES_TypeTag.Class:
                    break;

                case ES_TypeTag.Const:
                case ES_TypeTag.Immutable: {
                    var constData = (ES_ConstData*) innerType;
                    return TypeSizing_AnalyzeCycles_Traverse (constData->InnerType, containingType);
                }

                case ES_TypeTag.Array:
                case ES_TypeTag.Void:
                case ES_TypeTag.Bool:
                case ES_TypeTag.Int:
                case ES_TypeTag.Float:
                case ES_TypeTag.Function:
                case ES_TypeTag.Enum:
                case ES_TypeTag.Interface:
                case ES_TypeTag.Reference:
                    return false;

                default:
                    throw new NotImplementedException ("Type not implemented yet.");
            }

            foreach (var memberAddr in innerType->MembersList.MembersList.Span) {
                var member = memberAddr.Address;

                if (member->MemberType != ES_MemberType.Field)
                    continue;

                if (member->Flags.HasFlag (ES_MemberFlags.Static))
                    continue;

                var field = (ES_MemberData_Variable*) member;

                if (field->Type == containingType)
                    return true;

                if (TypeSizing_AnalyzeCycles_Traverse (field->Type, containingType))
                    return true;
            }

            return false;
        }
    }
}
