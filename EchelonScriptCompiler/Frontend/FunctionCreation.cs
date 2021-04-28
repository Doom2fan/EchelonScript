﻿/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using ChronosLib.Pooled;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Frontend {
    public unsafe partial class CompilerFrontend {
        protected void CreateFunctions (ref TranslationUnitData transUnit) {
            var idPool = Environment!.IdPool;

            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                foreach (var nm in astUnit.Ast.Namespaces) {
                    var symbols = astUnit.Symbols;
                    var unitSrc = astUnit.Ast.Source.Span;

                    ArrayPointer<byte> namespaceName;
                    using (var nameArr = nm.NamespaceName.ToPooledChars ())
                        namespaceName = idPool.GetIdentifier (nameArr);

                    var namespaceBuilder = EnvironmentBuilder!.GetOrCreateNamespace (namespaceName);

                    symbols.Push ();
                    ImportNamespaceSymbols (astUnit.Symbols, namespaceBuilder.NamespaceData);

                    foreach (var type in nm.Contents) {
                        switch (type) {
                            case ES_AstFunctionDefinition funcDef: {
                                CreateFunctions_Function (
                                    ref transUnit, namespaceBuilder,
                                    symbols, unitSrc,
                                    null, funcDef
                                );
                                break;
                            }

                            // Once we've got everything else working with aggregates, we need to traverse
                            // these to create their functions.
                            case ES_AstClassDefinition classDef:
                            case ES_AstStructDefinition structDef:
                                throw new NotImplementedException ();

                            // These are ignored.
                            case ES_AstEnumDefinition enumDef:
                                break;

                            default:
                                throw new NotImplementedException ();
                        }
                    }

                    symbols.Pop ();
                }
            }
        }

        protected void CreateFunctions_Function (
            ref TranslationUnitData transUnit, ES_NamespaceData.Builder namespaceBuilder,
            SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> unitSrc,
            ES_TypeInfo* parentType, ES_AstFunctionDefinition funcDef
        ) {
            Debug.Assert (Environment is not null);
            Debug.Assert (EnvironmentBuilder is not null);

            var sourceUnit = transUnit.Name;
            var idPool = Environment.IdPool;

            // Get the namespace and function names.
            var namespaceName = namespaceBuilder.NamespaceData.NamespaceName;
            var funcName = Environment.IdPool.GetIdentifier (funcDef.Name.Text.Span);

            // Get the fully-qualified name.
            ArrayPointer<byte> fullyQualifiedName;
            if (parentType == null)
                fullyQualifiedName = Environment.GetFullyQualifiedName (namespaceName, funcName);
            else {
                Span<ArrayPointer<byte>> parts = stackalloc ArrayPointer<byte> [2];
                parts [0] = parentType->TypeName;
                parts [1] = funcName;
                fullyQualifiedName = Environment.GetFullyQualifiedName (namespaceName, funcName);
            }

            // Handle the return type.
            Debug.Assert (funcDef.ReturnType is not null);
            funcDef.ReturnType = GenerateASTTypeRef (ref transUnit, symbols, unitSrc, funcDef.ReturnType);

            // Handle arguments.
            using var argData = new StructPooledList<ES_FunctionArgData> (CL_ClearMode.Auto);
            using var argTypes = new StructPooledList<ES_FunctionPrototypeArgData> (CL_ClearMode.Auto);

            int optArgNum = 0;
            int argNum = 0;
            var argNamesList = CL_PooledListPool<ArrayPointer<byte>>.Shared.Rent ();
            foreach (var arg in funcDef.ArgumentsList) {
                var argName = idPool.GetIdentifier (arg.Name.Text.Span);
                arg.ValueType = GenerateASTTypeRef (ref transUnit, symbols, unitSrc, arg.ValueType!);

                var idx = argNamesList.BinarySearch (argName, UnmanagedIdentifierComparer.Instance);
                if (idx >= 0) {
                    errorList.Add (ES_FrontendErrors.GenArgAlreadyDefined (
                        arg.Name.Text.Span.GetPooledString (), arg.Name
                    ));
                } else
                    argNamesList.Insert (~idx, argName);

                argData.Add (new ES_FunctionArgData (argName, null));
                argTypes.Add (new ES_FunctionPrototypeArgData (arg.ArgType, GetTypeRef (arg.ValueType)));

                if (arg.DefaultExpression is not null)
                    optArgNum++;
                argNum++;
            }
            CL_PooledListPool<ArrayPointer<byte>>.Shared.Return (argNamesList);

            var argDataMem = ArrayPointer<ES_FunctionArgData>.Null;
            if (argData.Count > 0) {
                argDataMem = EnvironmentBuilder.MemoryManager.GetArray<ES_FunctionArgData> (argData.Count);
                argData.Span.CopyTo (argDataMem.Span);
            }
            argData.Dispose ();

            // Get the function type.
            var funcType = EnvironmentBuilder.GetOrAddFunctionType (GetTypeRef (funcDef.ReturnType), argTypes.Span, true);

            // Add the function variant to the namespace or parent type.
            if (parentType != null) {
                throw new NotImplementedException ();
            } else {
                if (namespaceBuilder.CheckTypeExists (funcName, null) != null) {
                    errorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                        namespaceBuilder.NamespaceData.NamespaceNameString,
                        funcDef.Name.Text.Span.GetPooledString (),
                        funcDef.Name
                    ));
                    return;
                }

                var funcData = EnvironmentBuilder.MemoryManager.GetMemory<ES_FunctionData> ();

                *funcData = new ES_FunctionData (
                    funcName, fullyQualifiedName,
                    funcDef.AccessModifier, sourceUnit,
                    funcType, argDataMem, optArgNum
                );

                EnvironmentBuilder!.PointerAstMap.Add ((IntPtr) funcData, funcDef);

                namespaceBuilder.Functions.Add (funcName, funcData);
            }

            //funcBuilder.ParentType = parentType;
            //funcBuilder.Flags = 0 | (parentType != null ? ES_FunctionFlags.Method : 0);
        }
    }
}