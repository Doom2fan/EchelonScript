﻿/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using ChronosLib.Pooled;
using ChronosLib.Unmanaged;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Frontend;

public unsafe partial class CompilerFrontend {
    private void CreateFunctions (ref TranslationUnitData transUnit) {
        var idPool = Environment!.IdPool;

        foreach (ref var astUnit in transUnit.AstUnits.Span) {
            foreach (var nm in astUnit.Ast.Namespaces) {
                var symbols = astUnit.Symbols;
                var unitSrc = astUnit.SourceData;

                ES_Identifier namespaceName;
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

                        // [TODO] Once we've got everything else working with aggregates, we need to traverse
                        // these to create their functions.
                        case ES_AstClassDefinition classDef:
                            throw new NotImplementedException ("[TODO] Classes not implemented yet.");
                        case ES_AstStructDefinition structDef:
                            break;

                        // These are ignored.
                        case ES_AstEnumDefinition enumDef:
                            break;

                        default:
                            throw new NotImplementedException ("Node type not implemented.");
                    }
                }

                symbols.Pop ();
            }
        }
    }

    private void CreateFunctions_Function (
        ref TranslationUnitData transUnit, ES_NamespaceData.Builder namespaceBuilder,
        SymbolStack<FrontendSymbol> symbols, SourceData unitSrc,
        ES_TypeInfo* parentType, ES_AstFunctionDefinition funcDef
    ) {
        Debug.Assert (Environment is not null);
        Debug.Assert (EnvironmentBuilder is not null);

        var sourceUnit = transUnit.Name;
        var idPool = Environment.IdPool;

        // Get the namespace and function names.
        var funcName = Environment.IdPool.GetIdentifier (funcDef.Name.Text.Span);

        // Get the fully-qualified name.
        ES_FullyQualifiedName fullyQualifiedName;
        if (parentType == null) {
            var namespaceName = namespaceBuilder.NamespaceData.NamespaceName;
            fullyQualifiedName = new ES_FullyQualifiedName (namespaceName, funcName);
        } else {
            using var namespaceChars = new StructPooledList<char> (CL_ClearMode.Auto);

            namespaceChars.AddRange (parentType->Name.NamespaceName.GetCharsSpan ());

            namespaceChars.AddRange ("::");

            namespaceChars.AddRange (parentType->Name.TypeName.GetCharsSpan ());

            var namespaceName = idPool.GetIdentifier (namespaceChars.Span);
            fullyQualifiedName = new ES_FullyQualifiedName (namespaceName, funcName);
        }

        // Handle the return type.
        Debug.Assert (funcDef.ReturnType is not null);
        funcDef.ReturnType = GenerateASTTypeRef (transUnit.Name, symbols, unitSrc, funcDef.ReturnType);

        // Handle arguments.
        using var argData = new StructPooledList<ES_FunctionArgData> (CL_ClearMode.Auto);
        using var argTypes = new StructPooledList<ES_FunctionPrototypeArgData> (CL_ClearMode.Auto);

        var optArgNum = 0;
        var argNum = 0;
        var reqAfterOptReported = false;
        foreach (var arg in funcDef.ArgumentsList) {
            var argName = idPool.GetIdentifier (arg.Name.Text.Span);
            arg.ValueType = GenerateASTTypeRef (transUnit.Name, symbols, unitSrc, arg.ValueType!);

            foreach (var otherArg in argData) {
                if (otherArg.Name != argName)
                    continue;

                errorList.Add (ES_FrontendErrors.GenArgAlreadyDefined (
                    arg.Name.Text.Span.GetPooledString (), arg.Name
                ));
            }

            argData.Add (new ES_FunctionArgData (argName, null));
            argTypes.Add (new ES_FunctionPrototypeArgData (arg.ArgType, GetTypeRef (arg.ValueType)));

            if (arg.DefaultExpression is not null) {
                if (arg.ArgType == ES_ArgumentType.Out || arg.ArgType == ES_ArgumentType.Ref) {
                    errorList.Add (ES_FrontendErrors.GenArgTypeCantUseDefExpr (
                        arg.Name.Text.Span.GetPooledString (), arg.ArgType.ToString (), arg.Name
                    ));
                }

                optArgNum++;
            } else if (optArgNum > 0 && !reqAfterOptReported) {
                errorList.Add (new EchelonScriptErrorMessage (arg.Name, ES_FrontendErrors.ReqArgAfterOptional));
                reqAfterOptReported = true;
            }
            argNum++;
        }

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
            throw new NotImplementedException ("[TODO] Member functions not implemented yet.");
        } else {
            if (namespaceBuilder.CheckTypeExists (funcName, null) != null) {
                errorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                    namespaceBuilder.NamespaceData.NamespaceName.GetCharsSpan ().GetPooledString (),
                    funcDef.Name.Text.Span.GetPooledString (),
                    funcDef.Name
                ));
                return;
            }

            var funcData = EnvironmentBuilder.MemoryManager.GetMemory<ES_FunctionData> ();

            *funcData = new ES_FunctionData (
                fullyQualifiedName,
                funcDef.AccessModifier, sourceUnit,
                funcType, argDataMem, optArgNum
            );

            EnvironmentBuilder!.PointerAstMap.Add ((IntPtr) funcData, funcDef);

            namespaceBuilder.Functions.Add (funcName, funcData);
        }

        // TODO:
        //funcBuilder.ParentType = parentType;
        //funcBuilder.Flags = 0 | (parentType != null ? ES_FunctionFlags.Method : 0);
    }
}
