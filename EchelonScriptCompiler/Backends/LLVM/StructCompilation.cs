/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics.CodeAnalysis;
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Frontend;
using LLVMSharp.Interop;

namespace EchelonScriptCompiler.Backends.LLVMBackend {
    public unsafe sealed partial class LLVMCompilerBackend {
        private PooledArray<char> MangleStructName ([DisallowNull] ES_StructData* structData) {
            // Sample name: "struct.System.Numerics__Vector2"
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // The prefix.
            mangleChars.AddRange ("struct.");

            // The namespace.
            var namespaceName = structData->TypeInfo.Name.NamespaceName.Span;
            var namespaceSpan = mangleChars.AddSpan (namespaceName.Length);
            Encoding.ASCII.GetChars (namespaceName, namespaceSpan);

            // The mangled namespace separator.
            mangleChars.Add ('_', 2);

            // The function name.
            var structName = structData->TypeInfo.Name.TypeName.Span;
            var structSpan = mangleChars.AddSpan (structName.Length);
            Encoding.ASCII.GetChars (structName, structSpan);

            return mangleChars.ToPooledArray ();
        }

        private void GetOrGenerateStruct ([DisallowNull] ES_StructData* structData, out LLVMTypeRef structDef, bool noGenerate = false) {
            using var structChars = MangleStructName (structData);

            structDef = moduleRef.GetTypeByName (structChars);

            if (structDef != null) {
                if (structDef.Kind != LLVMTypeKind.LLVMStructTypeKind)
                    throw new CompilationException ("Found non-struct LLVM type when getting struct.");

                return;
            }

            if (!noGenerate)
                structDef = contextRef.CreateNamedStruct (structChars);
        }

        private void GenerateCode_Struct (
            ref TranslationUnitData transUnit, ref AstUnitData astUnit,
            SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            [DisallowNull] ES_AstStructDefinition structDef, [DisallowNull] ES_StructData* structData
        ) {
            GetOrGenerateStruct (structData, out var structType);

            using var memberTypes = new StructPooledList<LLVMTypeRef> (CL_ClearMode.Auto);

            foreach (var memberAddr in structData->TypeInfo.MembersList.MembersList.Span) {
                // Skip anything that isn't a field.
                if (memberAddr.Address->MemberType != ES_MemberType.Field)
                    continue;

                var memberPtr = (ES_MemberData_Variable*) memberAddr.Address;
                var llvmType = GetLLVMType (memberPtr->Type);

                if (!memberPtr->Info.Flags.HasFlag (ES_MemberFlags.Static))
                    memberTypes.Add (llvmType);
                else
                    throw new NotImplementedException ("[TODO] Static fields not implemented yet.");
            }

            structType.StructSetBody (memberTypes.Span, false);
        }
    }
}
