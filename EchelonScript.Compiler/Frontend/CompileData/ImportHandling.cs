/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using EchelonScript.Common.Data;
using EchelonScript.Compiler.Frontend.AST;

namespace EchelonScript.Compiler.Frontend;

internal ref partial struct CompileData {
#if false
    public void GatherGlobalImports () {
        var globalsNS = GetOrCreateNamespace (Env.GlobalsNamespace);
        var globalsList = globalsNS.Types;

        // Add built-in symbols.
        foreach (var type in globalsList)
            Symbols.AddSymbol (type.Value.Name.TypeName, FrontendSymbol.NewType (new (ESC_Constness.Mutable, type.Value)));
    }

    public void GatherAstImports (ref AstUnitData astUnit) {
        // Add imported symbols.
        foreach (var import in astUnit.Ast.ImportStatements)
            HandleImport (import);

        foreach (var alias in astUnit.Ast.TypeAliases)
            HandleAlias (alias);
    }

    public void HandleImport (ES_AstImportStatement import) {
        var namespaceData = GetNamespace (import.NamespaceName.ToIdentifier (IdPool));

        if (namespaceData is null)
            throw new CompilationException ("Non-existing namespace in import.");

        if (import.ImportedNames is null || import.ImportedNames.Length == 0) {
            foreach (var typeKVP in namespaceData.Types) {
                if (!Symbols.AddSymbol (typeKVP.Key, FrontendSymbol.NewType (new (ESC_Constness.Mutable, typeKVP.Value))))
                    Debug.Fail ("This shouldn't be reachable.");
            }

            foreach (var funcKVP in namespaceData.Functions) {
                if (!Symbols.AddSymbol (funcKVP.Key, FrontendSymbol.NewFunction (funcKVP.Value)))
                    Debug.Fail ("This shouldn't be reachable.");
            }

            return;
        }

        foreach (var importTk in import.ImportedNames) {
            var name = IdPool.GetIdentifier (importTk.Text.Span);

            var symbolFound = false;
            var isDuplicate = false;

            foreach (var typeKVP in namespaceData.Types) {
                if (!typeKVP.Key.Equals (name))
                    continue;

                isDuplicate = !Symbols.AddSymbol (name, FrontendSymbol.NewType (new (ESC_Constness.Mutable, typeKVP.Value)));
                symbolFound = true;
                break;
            }

            if (!symbolFound) {
                foreach (var funcKVP in namespaceData.Functions) {
                    if (!funcKVP.Key.Equals (name))
                        continue;

                    isDuplicate = !Symbols.AddSymbol (name, FrontendSymbol.NewFunction (funcKVP.Value));
                    symbolFound = true;
                    break;
                }
            }

            if (!symbolFound)
                Debug.Fail ("This shouldn't be reachable.");

            if (isDuplicate)
                Debug.Fail ("This shouldn't be reachable.");
        }
    }

    public void HandleAlias (ES_AstTypeAlias alias) {
        // TODO: FIXME
        /*var aliasId = IdPool.GetIdentifier (alias.AliasName.Text.Span);

        Debug.Assert (alias.OriginalName is ES_AstTypeDeclaration_TypeReference or ES_AstTypeDeclaration_FunctionReference);
        var aliasSymbol = alias.OriginalName switch {
            ES_AstTypeDeclaration_TypeReference typeRef => FrontendSymbol.NewType (typeRef.Reference),
            ES_AstTypeDeclaration_FunctionReference funcRef => FrontendSymbol.NewFunction (funcRef.Reference),

            _ => throw new CompilationException ("Unrecognized reference type in alias."),
        };

        if (!Symbols.AddSymbol (aliasId, aliasSymbol))
            Debug.Fail ("This shouldn't be reachable.");*/
    }

    public void ImportNamespaceSymbols (ESC_Namespace nsData) {
        foreach (var type in nsData.Types)
            Symbols.AddSymbol (type.Key, FrontendSymbol.NewType (new (ESC_Constness.Mutable, type.Value)));
        foreach (var funcKVP in nsData.Functions)
            Symbols.AddSymbol (funcKVP.Key, FrontendSymbol.NewFunction (funcKVP.Value));
    }

    public void ImportNamespaceSymbols (ES_Identifier name) => ImportNamespaceSymbols (GetOrCreateNamespace (name));
#endif
}
