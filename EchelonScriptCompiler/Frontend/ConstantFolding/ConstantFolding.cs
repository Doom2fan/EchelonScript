/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Frontend;

internal unsafe static partial class Compiler_ConstantFolding {
    private ref struct PassData {
        public EchelonScriptEnvironment Env { get; init; }
        public EchelonScriptEnvironment.Builder EnvBuilder { get; init; }

        public List<EchelonScriptErrorMessage> ErrorList { get; init; }
        public List<EchelonScriptErrorMessage> WarnList { get; init; }
        public List<EchelonScriptErrorMessage> InfoList { get; init; }

        public ArrayPointer<byte> TransUnitName { get; init; }
        public SourceData Source { get; set; }

        public SymbolStack<FrontendSymbol> Symbols { get; set; }
    }

    private struct ExpressionData {
        public ES_AstExpression Expr;
        public ES_TypeInfo* Type;
        public ES_TypeInfo* TypeInfo;
        public ES_FunctionData* Function;

        public static ExpressionData NewValue (ES_AstExpression expr, ES_TypeInfo* type) {
            return new ExpressionData {
                Expr = expr,
                Type = type,

                TypeInfo = null,
                Function = null
            };
        }

        public static ExpressionData NewType (ES_AstExpression expr, ES_TypeInfo* typeInfo) {
            return new ExpressionData {
                Expr = expr,
                TypeInfo = typeInfo,

                Type = null,
                Function = null
            };
        }

        public static ExpressionData NewFunction (ES_AstExpression expr, ES_FunctionData* func, ES_TypeInfo* funcType) {
            return new ExpressionData {
                Expr = expr,
                Function = func,
                TypeInfo = funcType,

                Type = null
            };
        }
    }

    private static ES_TypeInfo* GetTypeRef (ES_AstTypeDeclaration? typeDecl) {
        Debug.Assert (typeDecl is not null);

        var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;
        Debug.Assert (typeRef is not null);

        return typeRef.Reference;
    }

    public static void FoldConstants (
        EchelonScriptEnvironment env,
        EchelonScriptEnvironment.Builder envBuilder,

        List<EchelonScriptErrorMessage> errList,
        List<EchelonScriptErrorMessage> warnList,
        List<EchelonScriptErrorMessage> infoList,

        ref TranslationUnitData transUnit
    ) {
        var passData = new PassData {
            Env = env,
            EnvBuilder = envBuilder,

            ErrorList = errList,
            WarnList = warnList,
            InfoList = infoList,

            TransUnitName = transUnit.Name,
        };

        FoldConstants (ref passData, ref transUnit);
    }

    private static void FoldConstants (ref PassData passData, ref TranslationUnitData transUnit) {
        var idPool = passData.Env.IdPool;

        foreach (ref var astUnit in transUnit.AstUnits.Span) {
            foreach (var nm in astUnit.Ast.Namespaces) {
                ArrayPointer<byte> namespaceName;
                using (var nameArr = nm.NamespaceName.ToPooledChars ())
                    namespaceName = idPool.GetIdentifier (nameArr);

                var namespaceBuilder = passData.EnvBuilder.GetOrCreateNamespace (namespaceName);
                var namespaceData = namespaceBuilder.NamespaceData;

                passData.Source = astUnit.SourceData;
                passData.Symbols = astUnit.Symbols;

                passData.Symbols.Push ();
                ImportNamespaceSymbols (ref passData, namespaceData);

                foreach (var type in nm.Contents) {
                    switch (type) {
                        case ES_AstClassDefinition classDef: {
                            var typeName = idPool.GetIdentifier (classDef.Name.Text.Span);
                            var classBuilder = namespaceBuilder.GetClass (typeName);
                            Debug.Assert (classBuilder is not null);

                            // TODO: FoldConstants_Class (ref transUnit, ref astUnit, classDef, classBuilder);
                            throw new NotImplementedException ("[TODO] Classes not implemented yet.");
                        }

                        case ES_AstStructDefinition structDef: {
                            var typeName = idPool.GetIdentifier (structDef.Name.Text.Span);
                            var structBuilder = namespaceBuilder.GetStruct (typeName);
                            Debug.Assert (structBuilder is not null);

                            FoldStruct (ref passData, structDef);
                            break;
                        }

                        case ES_AstEnumDefinition enumDef: {
                            var typeName = idPool.GetIdentifier (enumDef.Name.Text.Span);
                            var enumBuilder = namespaceBuilder.GetEnum (typeName);
                            Debug.Assert (enumBuilder is not null);

                            // TODO: FoldConstants_Enum (ref transUnit, ref astUnit, enumDef, enumBuilder);
                            throw new NotImplementedException ("[TODO] Enums not implemented yet.");
                        }

                        case ES_AstFunctionDefinition funcDef:
                            FoldFunction (ref passData, funcDef);
                            break;

                        default:
                            throw new NotImplementedException ("Node type not implemented.");
                    }
                }

                passData.Symbols.Pop ();
            }
        }
    }

    private static ES_NamespaceData? GetNamespace (ref PassData passData, ReadOnlySpan<char> namespaceStr) {
        var namespaceName = passData.Env.IdPool.GetIdentifier (namespaceStr);

        if (!passData.Env.Namespaces.TryGetValue (namespaceName, out var namespaceData)) {
            Debug.Fail ("We shouldn't get here.");
            return null;
        }

        return namespaceData;
    }

    private static void ImportNamespaceSymbols (
        ref PassData passData,
        ES_NamespaceData namespaceData
    ) {
        foreach (var type in namespaceData.Types)
            passData.Symbols.AddSymbol (type.Address->Name.TypeName, FrontendSymbol.NewType (type));
        foreach (var funcKVP in namespaceData.Functions)
            passData.Symbols.AddSymbol (funcKVP.Key, FrontendSymbol.NewFunction (funcKVP.Value));
    }

    private static void HandleImport (
        ref PassData passData,
        ES_AstImportStatement import
    ) {
        var symbols = passData.Symbols;

        using var nmNameString = import.NamespaceName.ToPooledChars ();
        var namespaceData = GetNamespace (ref passData, nmNameString);

        if (namespaceData is null)
            return;

        if (import.ImportedNames is null || import.ImportedNames.Length == 0) {
            foreach (var type in namespaceData.Types) {
                if (!symbols.AddSymbol (type.Address->Name.TypeName, FrontendSymbol.NewType (type)))
                    Debug.Fail ("This shouldn't be reachable.");
            }

            foreach (var funcKVP in namespaceData.Functions) {
                if (!symbols.AddSymbol (funcKVP.Key, FrontendSymbol.NewFunction (funcKVP.Value)))
                    Debug.Fail ("This shouldn't be reachable.");
            }

            return;
        }

        foreach (var importTk in import.ImportedNames) {
            var name = passData.Env.IdPool.GetIdentifier (importTk.Text.Span);

            var symbolFound = false;
            var isDuplicate = false;

            if (!symbolFound) {
                foreach (var typeData in namespaceData.Types) {
                    if (!typeData.Address->Name.TypeName.Equals (name))
                        continue;

                    isDuplicate = !symbols.AddSymbol (name, FrontendSymbol.NewType (typeData));
                    symbolFound = true;
                    break;
                }
            }

            if (!symbolFound) {
                foreach (var funcKVP in namespaceData.Functions) {
                    if (!funcKVP.Key.Equals (name))
                        continue;

                    isDuplicate = !symbols.AddSymbol (name, FrontendSymbol.NewFunction (funcKVP.Value.Address));
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

    private static void HandleAlias (
        ref PassData passData,
        ES_AstTypeAlias alias
    ) {
        var idPool = passData.Env.IdPool;

        var aliasName = alias.AliasName.Text.Span;
        var aliasId = idPool.GetIdentifier (aliasName);

        Debug.Assert (alias.OriginalName is ES_AstTypeDeclaration_TypeReference);
        var origType = GetTypeRef (alias.OriginalName);

        if (!passData.Symbols.AddSymbol (aliasId, FrontendSymbol.NewType (origType)))
            Debug.Fail ("This shouldn't be reachable.");
    }

    private static void FoldStruct (ref PassData passData, ES_AstStructDefinition structDef) {
        foreach (var member in structDef.Contents) {
            switch (member) {
                case ES_AstMemberVarDefinition varDef: {
                    var varType = GetTypeRef (varDef.ValueType);

                    if (varDef.InitializationExpression is not null)
                        FoldExpression (ref passData, ref varDef.InitializationExpression, varType);

                    break;
                }

                case ES_AstFunctionDefinition funcDef:
                    throw new NotImplementedException ("[TODO] Member functions not implemented yet.");

                default:
                    throw new NotImplementedException ("Node type not implemented.");
            }
        }
    }

    private static void FoldFunction (ref PassData passData, ES_AstFunctionDefinition funcDef) {
        var idPool = passData.Env.IdPool;

        passData.Symbols.Push ();

        var retType = GetTypeRef (funcDef.ReturnType);

        foreach (var arg in funcDef.ArgumentsList) {
            var argName = idPool.GetIdentifier (arg.Name.Text.Span);
            var argValType = GetTypeRef (arg.ValueType);

            var flags = (FrontendSymbolFlags) 0;

            switch (arg.ArgType) {
                case ES_ArgumentType.Normal:
                    break;

                case ES_ArgumentType.Ref:
                    flags |= FrontendSymbolFlags.RefVar;
                    break;

                case ES_ArgumentType.In:
                    argValType = passData.EnvBuilder.CreateConstType (argValType);
                    break;

                case ES_ArgumentType.Out:
                    throw new NotImplementedException ("[TODO] Argument type not implemented yet.");

                default:
                    throw new NotImplementedException ("Argument type not implemented yet.");
            }

            passData.Symbols.AddSymbol (argName, FrontendSymbol.NewVariable (argValType, flags));

            if (arg.DefaultExpression is not null)
                FoldExpression (ref passData, ref arg.DefaultExpression, argValType);
        }

        var curStatement = funcDef.Statement;
        while (curStatement is not null) {
            FoldStatement (ref passData, retType, curStatement);

            curStatement = curStatement.Endpoint;
        }

        passData.Symbols.Pop ();
    }

    private static void FoldStatement (ref PassData passData, ES_TypeInfo* retType, ES_AstStatement stmt) {
        var idPool = passData.Env.IdPool;

        var typeUnkn = passData.Env.TypeUnknownValue;
        var typeBool = passData.Env.TypeBool;

        switch (stmt) {
            case ES_AstEmptyStatement:
                break;

            case ES_AstLabeledStatement:
                throw new NotImplementedException ("[TODO] Labels not implemented yet.");

            case ES_AstBlockStatement blockStmt: {
                passData.Symbols.Push ();

                var subStmt = blockStmt.Statement;
                while (subStmt is not null) {
                    Debug.Assert (subStmt is not null);
                    FoldStatement (ref passData, retType, subStmt);

                    subStmt = subStmt.Endpoint;
                }

                passData.Symbols.Pop ();

                break;
            }

            #region Symbol definition

            case ES_AstImportStatement importStmt:
                HandleImport (ref passData, importStmt);
                break;

            case ES_AstTypeAlias aliasStmt:
                HandleAlias (ref passData, aliasStmt);
                break;

            case ES_AstLocalVarDefinition varDef: {
                var implicitType = varDef.ValueType is null;
                var varType = !implicitType ? GetTypeRef (varDef.ValueType) : null;
                var symbolFlags = (FrontendSymbolFlags) 0;

                if (varDef.UsingVar)
                    symbolFlags |= FrontendSymbolFlags.UsingVar;

                foreach (ref var variable in varDef.Variables.AsSpan ()) {
                    var varName = variable.Name.Text.Span;
                    var varNameId = idPool.GetIdentifier (varName);

                    if (!implicitType) {
                        passData.Symbols.AddSymbol (varNameId, FrontendSymbol.NewVariable (varType, symbolFlags));

                        if (variable.InitializationExpression is not null)
                            FoldExpression (ref passData, ref variable.InitializationExpression, varType);
                    } else {
                        Debug.Assert (variable.InitializationExpression is not null);
                        var exprData = FoldExpression (ref passData, ref variable.InitializationExpression, typeUnkn);

                        passData.Symbols.AddSymbol (varNameId, FrontendSymbol.NewVariable (exprData.Type, symbolFlags));
                    }
                }

                break;
            }

            #endregion

            #region Jumps

            case ES_AstConditionalStatement condStmt: {
                FoldExpression (ref passData, ref condStmt.ConditionExpression, typeBool);

                FoldStatement (ref passData, retType, condStmt.ThenStatement);
                if (condStmt.ElseStatement is not null)
                    FoldStatement (ref passData, retType, condStmt.ElseStatement);

                break;
            }

            case ES_AstSwitchStatement switchStmt: {
                var exprTypeData = FoldExpression (ref passData, ref switchStmt.ValueExpression, typeUnkn);

                foreach (var section in switchStmt.Sections) {
                    foreach (ref var expr in section.Expressions.AsSpan ()) {
                        if (expr is not null)
                            FoldExpression (ref passData, ref expr, exprTypeData.Type);
                    }

                    var subStmt = section.StatementsBlock;
                    while (subStmt is not null) {
                        FoldStatement (ref passData, retType, subStmt);

                        subStmt = subStmt.Endpoint;
                    }
                }

                break;
            }

            case ES_AstBreakStatement:
                break;

            case ES_AstContinueStatement:
                break;

            case ES_AstGotoCaseStatement:
                throw new NotImplementedException ("[TODO] 'goto case' not implemented yet.");

            case ES_AstReturnStatement retStmt: {
                if (retStmt.ReturnExpression is not null)
                    FoldExpression (ref passData, ref retStmt.ReturnExpression, retType);

                break;
            }

            #endregion

            #region Loops

            case ES_AstLoopStatement loopStmt: {
                passData.Symbols.Push ();

                if (loopStmt.InitializationStatement is not null)
                    FoldStatement (ref passData, retType, loopStmt.InitializationStatement);

                if (loopStmt.ConditionExpression is not null)
                    FoldExpression (ref passData, ref loopStmt.ConditionExpression, typeBool);

                if (loopStmt.IterationExpressions is not null) {
                    foreach (ref var expr in loopStmt.IterationExpressions.AsSpan ()) {
                        if (expr is not null)
                            FoldExpression (ref passData, ref expr, typeUnkn);
                    }
                }

                Debug.Assert (loopStmt.LoopBody is not null);
                Debug.Assert (loopStmt.LoopBody.Endpoint is null);

                FoldStatement (ref passData, retType, loopStmt.LoopBody);

                passData.Symbols.Pop ();

                break;
            }

            #endregion

            case ES_AstExpressionStatement exprStmt:
                FoldExpression (ref passData, ref exprStmt.Expression, typeUnkn);
                break;

            case ES_AstExpressionListStatement exprListStmt: {
                foreach (ref var expr in exprListStmt.Expressions.AsSpan ())
                    FoldExpression (ref passData, ref expr, typeUnkn);

                break;
            }

            default:
                throw new NotImplementedException ("Statement type not implemented.");
        }
    }
}
