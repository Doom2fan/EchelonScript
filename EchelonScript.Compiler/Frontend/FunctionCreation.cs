/*
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
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.Utilities;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;

namespace EchelonScript.Compiler.Frontend;

internal static class Compiler_FunctionCreation {
#if false
    private ref struct PassData {
        public ES_Identifier TransUnitName;
        public SourceData Source;
    }

    private static ESC_TypeRef GetTypeRef (ES_AstTypeDeclaration typeDecl) {
        var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;

        Debug.Assert (typeRef is not null);

        return default; // TODO: FIXME
    }

    public static void CreateFunctions (ref CompileData compileData) {
        var idPool = compileData.IdPool;

        Debug.Assert (compileData.Symbols.ScopesCount == 0);
        compileData.Symbols.Push ();

        compileData.GatherGlobalImports ();

        foreach (ref var transUnit in compileData.TranslationUnits) {
            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                var passData = new PassData {
                    TransUnitName = transUnit.Name,
                    Source = astUnit.SourceData,
                };

                foreach (var nm in astUnit.Ast.Namespaces) {
                    var nsName = nm.NamespaceName.ToIdentifier (idPool);
                    var nsData = compileData.GetOrCreateNamespace (nsName);

                    compileData.Symbols.Push ();
                    compileData.ImportNamespaceSymbols (nsData);

                    foreach (var type in nm.Contents) {
                        switch (type) {
                            case ES_AstFunctionDefinition funcDef: {
                                CreateFunctions_Function (
                                    ref compileData, ref passData,
                                    funcDef, nsData, null
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

                    compileData.Symbols.Pop ();
                }
            }
        }

        compileData.Symbols.Pop ();
        Debug.Assert (compileData.Symbols.ScopesCount == 0);
    }

    private static void CreateFunctions_Function (
        ref CompileData compileData, ref PassData passData,
        ES_AstFunctionDefinition funcDef, ESC_Namespace nsData, ESC_TypeData? parentType
    ) {
        // Get the namespace and function names.
        var funcName = compileData.IdPool.GetIdentifier (funcDef.Name.Text.Span);

        // Handle the return type.
        Debug.Assert (funcDef.ReturnType is not null);

        // Handle arguments.
        using var argsList = new StructPooledList<ESC_FunctionArg> (CL_ClearMode.Auto);
        using var argsTypes = new StructPooledList<ESC_PrototypeArg> (CL_ClearMode.Auto);

        var optArgNum = 0;
        var argNum = 0;
        var reqAfterOptReported = false;
        foreach (var arg in funcDef.ArgumentsList) {
            var argName = compileData.IdPool.GetIdentifier (arg.Name.Text.Span);

            foreach (var otherArg in argsList) {
                if (otherArg.Name != argName)
                    continue;

                compileData.ErrorList.Add (ES_FrontendErrors.GenArgAlreadyDefined (
                    arg.Name.Text.Span.GetPooledString (), arg.Name
                ));
            }

            Debug.Assert (arg.ValueType is not null);

            argsList.Add (new (argName, null));
            argsTypes.Add (new (arg.ArgType, GetTypeRef (arg.ValueType)));

            if (arg.DefaultExpression is not null) {
                if (arg.ArgType == ES_ArgumentType.Out || arg.ArgType == ES_ArgumentType.Ref) {
                    compileData.ErrorList.Add (ES_FrontendErrors.GenArgTypeCantUseDefExpr (
                        arg.Name.Text.Span.GetPooledString (), arg.ArgType.ToString (), arg.Name
                    ));
                }

                optArgNum++;
            } else if (optArgNum > 0 && !reqAfterOptReported) {
                compileData.ErrorList.Add (new (arg.Name, ES_FrontendErrors.ReqArgAfterOptional));
                reqAfterOptReported = true;
            }
            argNum++;
        }

        // Get the function type.
        var funcType = compileData.GetPrototype (GetTypeRef (funcDef.ReturnType), argsTypes.Span, true)!;

        // Add the function variant to the namespace or parent type.
        if (parentType != null) {
            throw new NotImplementedException ("[TODO] Member functions not implemented yet.");
        } else {
            if (nsData.CheckTypeExists (funcName, null, out _) != null) {
                compileData.ErrorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                    nsData.NamespaceName.GetCharsSpan ().GetPooledString (),
                    funcDef.Name.Text.Span.GetPooledString (),
                    funcDef.Name
                ));
                return;
            }

            var funcData = new ESC_Function (
                new (nsData.NamespaceName), funcName,
                funcDef.AccessModifier, passData.TransUnitName,
                funcType, argsList.ToArray (), optArgNum
            );

            nsData.Functions.Add (funcName, funcData);
        }

        // TODO:
        //funcBuilder.ParentType = parentType;
        //funcBuilder.Flags = 0 | (parentType != null ? ES_FunctionFlags.Method : 0);
    }
#endif
}
