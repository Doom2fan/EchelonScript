/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using ChronosLib.Pooled;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Frontend;
using LLVMSharp.Interop;

namespace EchelonScriptCompiler.Backends.LLVMBackend {
    public unsafe sealed partial class LLVMCompilerBackend {
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
            Debug.Assert (envBuilder is not null);

            GetOrGenerateStruct (structData, out var structType);

            var staticConsType = LLVMTypeRef.CreateFunction (contextRef.VoidType, Array.Empty<LLVMTypeRef> (), false);
            var staticConsName = MangleDefaultConstructorName (&structData->TypeInfo, true);
            var staticConsFunc = moduleRef.AddFunction (staticConsName, staticConsType);
            // TODO: Add inlining hints

            staticConsFunc.AppendBasicBlock ("entry");
            builderRef.PositionAtEnd (staticConsFunc.EntryBasicBlock);

            using var memberTypes = new StructPooledList<LLVMTypeRef> (CL_ClearMode.Auto);

            foreach (var memberAddr in structData->TypeInfo.MembersList.MembersList.Span) {
                // Skip anything that isn't a field.
                if (memberAddr.Address->MemberType != ES_MemberType.Field)
                    continue;

                var memberPtr = (ES_MemberData_Variable*) memberAddr.Address;
                var llvmType = GetLLVMType (memberPtr->Type);

                if (!memberPtr->Info.Flags.HasFlag (ES_MemberFlags.Static)) {
                    memberTypes.Add (llvmType);
                } else {
                    if (!envBuilder.PointerAstMap.TryGetValue ((IntPtr) memberPtr, out var varDefNode))
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    var varDef = (ES_AstMemberVarDefinition) varDefNode;

                    using var mangledName = MangleStaticVarName (&structData->TypeInfo, memberPtr->Info.Name);
                    var val = moduleRef.AddGlobal (llvmType, mangledName);
                    val.Initializer = GetDefaultValue (memberPtr->Type);

                    if (varDef.InitializationExpression is not null) {
                        var initExpr = GenerateCode_Expression (ref transUnit, symbols, src, varDef.InitializationExpression, memberPtr->Type);

                        if (!initExpr.Constant || !initExpr.Addressable)
                            throw new CompilationException (ES_BackendErrors.FrontendError);

                        GenerateCode_EnsureImplicitCompat (ref initExpr, memberPtr->Type);

                        builderRef.BuildStore (initExpr.Value, val);
                    }
                }
            }

            structType.StructSetBody (memberTypes.Span, false);

            // Emit the return for the default static constructor.
            builderRef.BuildRetVoid ();

            // Add the default static constructor to the global constructor.
            var globalCons = GetGlobalConstructor ();
            Debug.Assert (globalCons != null);

            builderRef.PositionAtEnd (globalCons.EntryBasicBlock);
            builderRef.BuildCall (staticConsFunc, Array.Empty<LLVMValueRef> ());
        }
    }
}
