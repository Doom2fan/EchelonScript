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

namespace EchelonScript.Compiler.Frontend;

internal static partial class Compiler_TypeGathering {
    public static void GatherTypes_Functions (ref CompileData compileData) { }
#if false
    public static void GatherTypes_Functions (ref CompileData compileData) {
        Debug.Assert (compileData.Symbols.ScopesCount == 0);
        compileData.Symbols.Push ();

        compileData.GatherGlobalImports ();

        foreach (ref var transUnit in compileData.TranslationUnits) {
            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                var passData = new PassData {
                    TransUnitName = transUnit.Name,
                    Source = astUnit.SourceData,
                };

                GatherAstUnit_Functions (ref compileData, ref passData, astUnit.Ast);
            }
        }

        compileData.Symbols.Pop ();
        Debug.Assert (compileData.Symbols.ScopesCount == 0);
    }

    private static void GatherAstUnit_Functions (ref CompileData compileData, ref PassData passData, ES_AbstractSyntaxTree ast) {
        GatherASTImports (ref compileData, ref passData, ast);

        foreach (var nsDef in ast.Namespaces) {
            var namespaceName = nsDef.NamespaceName.ToIdentifier (compileData.IdPool);
            var namespaceData = compileData.GetOrCreateNamespace (namespaceName);
            Debug.Assert (namespaceData is not null);

            compileData.Symbols.Push ();
            ImportNamespaceSymbols (ref compileData, namespaceData);

            foreach (var type in nsDef.Contents) {
                switch (type) {
                    case ES_AstAggregateDefinition aggregateDef: {
                        foreach (var member in aggregateDef.Contents) {
                            if (member is not ES_AstFunctionDefinition funcDef)
                                continue;

                            GatherTypes_FunctionSignature (ref compileData, ref passData, funcDef);
                        }

                        break;
                    }

                    case ES_AstFunctionDefinition funcDef:
                        GatherTypes_FunctionSignature (ref compileData, ref passData, funcDef);
                        break;

                    default:
                        throw new NotImplementedException ("Node type not implemented.");
                }
            }

            compileData.Symbols.Pop ();
        }
    }

    private static void GatherTypes_FunctionSignature (ref CompileData compileData, ref PassData passData, ES_AstFunctionDefinition funcDef) {
        // Handle the return type.
        Debug.Assert (funcDef.ReturnType is not null);
        funcDef.ReturnType = GenerateASTTypeRef (ref compileData, ref passData, funcDef.ReturnType);

        // Handle arguments.
        foreach (var arg in funcDef.ArgumentsList)
            arg.ValueType = GenerateASTTypeRef (ref compileData, ref passData, arg.ValueType!);
    }
#endif
}
