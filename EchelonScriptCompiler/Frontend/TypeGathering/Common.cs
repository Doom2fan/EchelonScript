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
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend.Data;

namespace EchelonScriptCompiler.Frontend;

internal static partial class Compiler_TypeGathering {
    private ref struct PassData {
        public ES_Identifier TransUnitName { get; set; }
        public SourceData Source { get; set; }
    }

    private static ESC_TypeRef GetType (
        ref CompileData compileData, ref PassData passData,
        ES_AstTypeDeclaration_TypeName typeName
    ) {
        var idPool = compileData.IdPool;

        Debug.Assert (typeName.TypeName.Parts.Length > 0);

        if (typeName.Namespace is not null) {
            var namespaceId = typeName.Namespace.ToIdentifier (idPool);
            var nameId = typeName.TypeName.ToIdentifier (idPool);

            var type = compileData.GetFullyQualifiedType (namespaceId, nameId);

            if (type.Type is null) {
                compileData.ErrorList.Add (ES_FrontendErrors.GenCantFindSymbol (
                    typeName.ToString (), passData.Source, typeName.NodeBounds
                ));

                return compileData.GetUnknownType (ESC_Constness.Mutable);
            }

            return type;
        } else {
            var typeParts = typeName.TypeName.Parts;
            var typeId = idPool.GetIdentifier (typeParts [0].Text.Span);

            var symbol = compileData.Symbols.GetSymbol (typeId);

            if (symbol.Tag == FrontendSymbolType.None) {
                compileData.ErrorList.Add (ES_FrontendErrors.GenCantFindSymbol (
                    typeParts [0].Text.Span.GetPooledString (), typeParts [0]
                ));

                return compileData.GetUnknownType (ESC_Constness.Mutable);
            } else if (symbol.Tag == FrontendSymbolType.Variable) {
                compileData.ErrorList.Add (ES_FrontendErrors.GenVarUsedAsType (
                    typeParts [0].Text.Span.GetPooledString (), typeParts [0]
                ));

                return compileData.GetUnknownType (ESC_Constness.Mutable);
            } else if (symbol.Tag == FrontendSymbolType.Function) {
                compileData.ErrorList.Add (ES_FrontendErrors.GenFuncUsedAsType (
                    typeParts [0].Text.Span.GetPooledString (), typeParts [0]
                ));

                return compileData.GetUnknownType (ESC_Constness.Mutable);
            } else if (symbol.Tag != FrontendSymbolType.Type)
                throw new NotImplementedException ("Not implemented?");

            if (typeParts.Length > 1)
                throw new NotImplementedException ("[TODO] Nested types not implemented yet.");

            return symbol.MatchType ();
        }
    }

    private static ESC_TypeRef ResolveTypeDeclaration (
        ref CompileData compileData, ref PassData passData,
        ES_AstTypeDeclaration typeDecl
    ) {
        switch (typeDecl) {
            case ES_AstTypeDeclaration_TypeName typeName: {
                var type = GetType (ref compileData, ref passData, typeName);
                Debug.Assert (type.Type is not null);

                switch (type.Type.AccessModifier) {
                    case ES_AccessModifier.Public: break;

                    case ES_AccessModifier.Internal:
                        if (!type.Type.SourceUnit.Equals (passData.TransUnitName)) {
                            using var symbolName = PooledArray<char>.GetArray (typeName.GetStringLength ());
                            typeName.ToString (symbolName);

                            compileData.ErrorList.Add (ES_FrontendErrors.GenInaccessibleProtectionLevel (
                                symbolName.Span.GetPooledString (), passData.Source, typeName.NodeBounds
                            ));
                        }
                        break;

                    default:
                        throw new NotImplementedException ("Access modifier not implemented yet.");
                }

                return type;
            }

            case ES_AstTypeDeclaration_TypeReference typeRef:
                return typeRef.Reference;

            case ES_AstTypeDeclaration_Array arrayDecl: {
                var elemType = ResolveTypeDeclaration (ref compileData, ref passData, arrayDecl.ElementType);
                return compileData.GetArrayType (elemType, arrayDecl.Dimensions, ESC_Constness.Mutable);
            }

            case ES_AstTypeDeclaration_Basic basicDecl: {
                var innerType = ResolveTypeDeclaration (ref compileData, ref passData, basicDecl.Inner!);

                switch (basicDecl.Type) {
                    case ES_AstTypeDeclaration_Basic.DeclType.Const:
                        if (innerType.Constness != ESC_Constness.Mutable) {
                            compileData.ErrorList.Add (ES_FrontendErrors.GenTypeAlreadyConst (
                                false, innerType.Constness == ESC_Constness.Immutable,
                                passData.Source, basicDecl.NodeBounds
                            ));
                            return innerType;
                        }
                        return innerType.WithConst (ESC_Constness.Const);

                    case ES_AstTypeDeclaration_Basic.DeclType.Immutable:
                        if (innerType.Constness != ESC_Constness.Mutable) {
                            compileData.ErrorList.Add (ES_FrontendErrors.GenTypeAlreadyConst (
                                true, innerType.Constness == ESC_Constness.Immutable,
                                passData.Source, basicDecl.NodeBounds
                            ));
                            return innerType;
                        }
                        return innerType.WithConst (ESC_Constness.Immutable);

                    case ES_AstTypeDeclaration_Basic.DeclType.Nullable:
                        return compileData.GetNullableType (innerType, ESC_Constness.Mutable);

                    case ES_AstTypeDeclaration_Basic.DeclType.Reference:
                        return compileData.GetReferenceType (innerType, ESC_Constness.Mutable);

                    default:
                        throw new NotImplementedException ("Basic declaration type not implemented.");
                }
            }

            default:
                throw new NotImplementedException ("Declaration type not implemented.");
        }
    }

    private static ES_AstTypeDeclaration_TypeReference GenerateASTTypeRef (
        ref CompileData compileData, ref PassData passData,
        ES_AstTypeDeclaration typeDecl
    ) {
        Debug.Assert (typeDecl is not null);

        if (typeDecl is ES_AstTypeDeclaration_TypeReference)
            return (typeDecl as ES_AstTypeDeclaration_TypeReference)!;

        var type = ResolveTypeDeclaration (ref compileData, ref passData, typeDecl);

        if (type.Type is null)
            type = compileData.GetUnknownType (ESC_Constness.Mutable);

        return new ES_AstTypeDeclaration_TypeReference (typeDecl, type);
    }

    private static ESC_TypeRef GetTypeRef (ES_AstTypeDeclaration typeDecl) {
        var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;
        Debug.Assert (typeRef is not null);

        return typeRef.Reference;
    }

    private static ESC_Namespace? GetNamespace (
        ref CompileData compileData, ref PassData passData,
        ES_AstNodeBounds nodeBounds,
        ES_Identifier namespaceName
    ) {
        if (!compileData.Namespaces.TryGetValue (namespaceName, out var namespaceData)) {
            var err = ES_FrontendErrors.GenNamespaceDoesntExist (
                namespaceName.GetCharsSpan ().GetPooledString (),
                passData.Source,
                nodeBounds
            );
            compileData.ErrorList.Add (err);
            return null;
        }

        return namespaceData;
    }

    private static void ImportNamespaceSymbols (ref CompileData compileData, ESC_Namespace namespaceData) {
        foreach (var typeKVP in namespaceData.Types) {
            var symbol = FrontendSymbol.NewType (new (ESC_Constness.Mutable, typeKVP.Value));
            if (!compileData.Symbols.AddSymbol (typeKVP.Key, symbol))
                throw new CompilationException ("Missing namespace scope push.");
        }

        foreach (var funcKVP in namespaceData.Functions) {
            if (!compileData.Symbols.AddSymbol (funcKVP.Key, FrontendSymbol.NewFunction (funcKVP.Value)))
                throw new CompilationException ("Missing namespace scope push.");
        }
    }

    private static void AST_HandleImport (ref CompileData compileData, ref PassData passData, ES_AstImportStatement import) {
        var symbols = compileData.Symbols;

        var namespaceName = import.NamespaceName.ToIdentifier (compileData.IdPool);
        var namespaceData = GetNamespace (ref compileData, ref passData, import.NamespaceName.NodeBounds, namespaceName);

        if (namespaceData is null)
            return;

        if (import.ImportedNames is null || import.ImportedNames.Length == 0) {
            foreach (var type in namespaceData.Types) {
                if (!symbols.AddSymbol (type.Key, FrontendSymbol.NewType (new (ESC_Constness.Mutable, type.Value)))) {
                    compileData.ErrorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (
                        type.Key.GetCharsSpan ().GetPooledString (),
                        passData.Source, import.NamespaceName.NodeBounds
                    ));
                }
            }
            foreach (var funcKVP in namespaceData.Functions) {
                if (!symbols.AddSymbol (funcKVP.Key, FrontendSymbol.NewFunction (funcKVP.Value))) {
                    compileData.ErrorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (
                        funcKVP.Key.GetCharsSpan ().GetPooledString (),
                        passData.Source, import.NamespaceName.NodeBounds
                    ));
                }
            }
        } else {
            foreach (var importTk in import.ImportedNames) {
                var name = compileData.IdPool.GetIdentifier (importTk.Text.Span);

                var symbolFound = false;
                var isDuplicate = false;

                if (!symbolFound) {
                    foreach (var typeKVP in namespaceData.Types) {
                        if (!typeKVP.Key.Equals (name))
                            continue;

                        isDuplicate = !symbols.AddSymbol (name, FrontendSymbol.NewType (new (ESC_Constness.Mutable, typeKVP.Value)));
                        symbolFound = true;
                    }
                }

                if (!symbolFound) {
                    foreach (var funcKVP in namespaceData.Functions) {
                        if (!funcKVP.Key.Equals (name))
                            break;

                        isDuplicate = !symbols.AddSymbol (name, FrontendSymbol.NewFunction (funcKVP.Value));
                        symbolFound = true;
                    }
                }

                if (!symbolFound)
                    compileData.ErrorList.Add (ES_FrontendErrors.GenCantFindSymbol (
                        importTk.Text.Span.GetPooledString (), importTk
                    ));

                if (isDuplicate) {
                    compileData.ErrorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (
                        importTk.Text.Span.GetPooledString (), importTk
                    ));
                }
            }
        }
    }

    private static void AST_HandleAlias (ref CompileData compileData, ref PassData passData, ES_AstTypeAlias alias) {
        var errorList = compileData.ErrorList;

        var aliasName = alias.AliasName.Text.Span;
        var aliasId = compileData.IdPool.GetIdentifier (aliasName);

        if (alias.OriginalName is ES_AstTypeDeclaration_TypeName typeName) {
            ESC_Function? func = null;

            var origName = typeName.TypeName.ToIdentifier (compileData.IdPool);

            if (typeName.Namespace is not null) {
                var namespaceName = typeName.Namespace!.ToIdentifier (compileData.IdPool);
                var namespaceData = GetNamespace (ref compileData, ref passData, typeName.Namespace.NodeBounds, namespaceName);

                if (namespaceData is not null && namespaceData.Functions.TryGetValue (origName, out var newFunc))
                    func = newFunc;
            } else {
                var symbol = compileData.Symbols.GetSymbol (origName);

                if (symbol.Tag == FrontendSymbolType.Function)
                    func = symbol.MatchFunction ();
            }

            if (func != null) {
                if (!compileData.Symbols.AddSymbol (aliasId, FrontendSymbol.NewFunction (func)))
                    errorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (aliasName.GetPooledString (), alias.AliasName));

                return;
            } else
                alias.OriginalName = GenerateASTTypeRef (ref compileData, ref passData, alias.OriginalName!);
        } else
            alias.OriginalName = GenerateASTTypeRef (ref compileData, ref passData, alias.OriginalName!);

        var aliasSymbol = alias.OriginalName switch {
            ES_AstTypeDeclaration_TypeReference typeRef => FrontendSymbol.NewType (typeRef.Reference),
            ES_AstTypeDeclaration_FunctionReference funcRef => FrontendSymbol.NewFunction (funcRef.Reference),

            _ => throw new NotImplementedException ("AST type ref type not implemented."),
        };

        if (!compileData.Symbols.AddSymbol (aliasId, aliasSymbol))
            errorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (aliasName.GetPooledString (), alias.AliasName));
    }

    private static void GatherASTImports (ref CompileData compileData, ref PassData passData, ES_AbstractSyntaxTree ast) {
        // Add imported symbols
        foreach (var import in ast.ImportStatements)
            AST_HandleImport (ref compileData, ref passData, import);

        foreach (var alias in ast.TypeAliases)
            AST_HandleAlias (ref compileData, ref passData, alias);
    }
}
