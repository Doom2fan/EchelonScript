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
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.Data;

namespace EchelonScript.Compiler.Frontend;

internal unsafe static partial class Compiler_ConstantFolding {
#if false
    private ref struct PassData {
        public ES_Identifier TransUnitName;
        public SourceData Source;
    }

    private struct ExpressionData {
        public ES_AstExpression Expr;
        public ESC_TypeRef Type;
        public ESC_TypeRef TypeInfo;
        public ESC_Function? Function;

        public static ExpressionData NewValue (ES_AstExpression expr, ESC_TypeRef type) {
            return new ExpressionData {
                Expr = expr,
                Type = type,

                TypeInfo = ESC_TypeRef.Null (),
                Function = null
            };
        }

        public static ExpressionData NewType (ES_AstExpression expr, ESC_TypeRef typeInfo) {
            return new ExpressionData {
                Expr = expr,
                TypeInfo = typeInfo,

                Type = ESC_TypeRef.Null (),
                Function = null
            };
        }

        public static ExpressionData NewFunction (ES_AstExpression expr, ESC_Function? func, ESC_TypeRef funcType) {
            return new ExpressionData {
                Expr = expr,
                Function = func,
                TypeInfo = funcType,

                Type = ESC_TypeRef.Null ()
            };
        }
    }

    private static ESC_TypeRef GetTypeRef (ES_AstTypeDeclaration? typeDecl) {
        Debug.Assert (typeDecl is not null);

        var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;
        Debug.Assert (typeRef is not null);

        return typeRef.Reference;
    }

    public static void FoldConstants (ref CompileData compileData) {
        Debug.Assert (compileData.Symbols.ScopesCount == 0);
        compileData.Symbols.Push ();

        compileData.GatherGlobalImports ();

        foreach (ref var transUnit in compileData.TranslationUnits) {
            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                var passData = new PassData {
                    TransUnitName = transUnit.Name,
                    Source = astUnit.SourceData,
                };

                FoldConstants (ref compileData, ref passData, astUnit.Ast);
            }
        }

        compileData.Symbols.Pop ();
        Debug.Assert (compileData.Symbols.ScopesCount == 0);
    }

    private static void FoldConstants (ref CompileData compileData, ref PassData passData, ES_AbstractSyntaxTree ast) {
        var idPool = compileData.IdPool;

        foreach (var nm in ast.Namespaces) {
            var namespaceName = nm.NamespaceName.ToIdentifier (idPool);
            var namespaceData = compileData.GetOrCreateNamespace (namespaceName);

            compileData.Symbols.Push ();
            compileData.ImportNamespaceSymbols (namespaceData);

            foreach (var type in nm.Contents) {
                switch (type) {
                    case ES_AstClassDefinition classDef: {
                        var typeName = idPool.GetIdentifier (classDef.Name.Text.Span);
                        var classData = namespaceData.GetClass (typeName);
                        Debug.Assert (classData is not null);

                        // TODO: FoldConstants_Class (ref compileData, ref passData, classDef, classData);
                        throw new NotImplementedException ("[TODO] Classes not implemented yet.");
                    }

                    case ES_AstStructDefinition structDef: {
                        var typeName = idPool.GetIdentifier (structDef.Name.Text.Span);
                        var structBuilder = namespaceData.GetStruct (typeName);
                        Debug.Assert (structBuilder is not null);

                        FoldStruct (ref compileData, ref passData, structDef);
                        break;
                    }

                    case ES_AstEnumDefinition enumDef: {
                        var typeName = idPool.GetIdentifier (enumDef.Name.Text.Span);
                        var enumData = namespaceData.GetEnum (typeName);
                        Debug.Assert (enumData is not null);

                        // TODO: FoldConstants_Enum (ref compileData, ref passData, enumDef, enumData);
                        throw new NotImplementedException ("[TODO] Enums not implemented yet.");
                    }

                    case ES_AstFunctionDefinition funcDef:
                        var funcName = idPool.GetIdentifier (funcDef.Name.Text.Span);
                        if (!namespaceData.Functions.TryGetValue (funcName, out var funcData))
                            throw new CompilationException ("Function doesn't exist.");

                        FoldFunction (ref compileData, ref passData, funcDef, funcData);
                        break;

                    default:
                        throw new NotImplementedException ("Node type not implemented.");
                }
            }

            compileData.Symbols.Pop ();
        }
    }

    private static void FoldStruct (ref CompileData compileData, ref PassData passData, ES_AstStructDefinition structDef) {
        foreach (var member in structDef.Contents) {
            switch (member) {
                case ES_AstMemberVarDefinition varDef: {
                    var varType = GetTypeRef (varDef.ValueType);

                    if (varDef.InitializationExpression is not null)
                        FoldExpression (ref compileData, ref passData, ref varDef.InitializationExpression, varType);

                    break;
                }

                case ES_AstFunctionDefinition funcDef:
                    throw new NotImplementedException ("[TODO] Member functions not implemented yet.");

                default:
                    throw new NotImplementedException ("Node type not implemented.");
            }
        }
    }

    private static void FoldFunction (ref CompileData compileData, ref PassData passData, ES_AstFunctionDefinition funcDef, ESC_Function func) {
        var idPool = compileData.IdPool;

        compileData.Symbols.Push ();

        var retType = GetTypeRef (funcDef.ReturnType);

        var argIdx = 0;
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
                    argValType = argValType.WithConst (ESC_Constness.Const);
                    break;

                case ES_ArgumentType.Out:
                    throw new NotImplementedException ("[TODO] Argument type not implemented yet.");

                default:
                    throw new NotImplementedException ("Argument type not implemented yet.");
            }

            compileData.Symbols.AddSymbol (argName, FrontendSymbol.NewVariable (new (argValType, null!), flags));

            if (arg.DefaultExpression is not null) {
                FoldExpression (ref compileData, ref passData, ref arg.DefaultExpression, argValType);

                ref var funcArg = ref func.Arguments [argIdx++];
                funcArg = new (funcArg.Name, arg.DefaultExpression);
            }
        }

        var curStatement = funcDef.Statement;
        while (curStatement is not null) {
            FoldStatement (ref compileData, ref passData, curStatement, retType);

            curStatement = curStatement.Endpoint;
        }

        compileData.Symbols.Pop ();
    }

    private static void FoldStatement (
        ref CompileData compileData, ref PassData passData,
        ES_AstStatement stmt, ESC_TypeRef retType
    ) {
        var idPool = compileData.IdPool;

        var typeUnkn = compileData.GetUnknownType (ESC_Constness.Mutable);
        var typeBool = compileData.GetBoolType (ESC_Constness.Mutable);

        switch (stmt) {
            case ES_AstEmptyStatement:
                break;

            case ES_AstLabeledStatement:
                throw new NotImplementedException ("[TODO] Labels not implemented yet.");

            case ES_AstBlockStatement blockStmt: {
                compileData.Symbols.Push ();

                var subStmt = blockStmt.Statement;
                while (subStmt is not null) {
                    Debug.Assert (subStmt is not null);
                    FoldStatement (ref compileData, ref passData, subStmt, retType);

                    subStmt = subStmt.Endpoint;
                }

                compileData.Symbols.Pop ();

                break;
            }

#region Symbol definition

            case ES_AstImportStatement importStmt:
                compileData.HandleImport (importStmt);
                break;

            case ES_AstTypeAlias aliasStmt:
                compileData.HandleAlias (aliasStmt);
                break;

            case ES_AstLocalVarDefinition varDef: {
                var implicitType = varDef.ValueType is null;
                var varType = !implicitType ? GetTypeRef (varDef.ValueType) : ESC_TypeRef.Null ();
                var symbolFlags = (FrontendSymbolFlags) 0;

                if (varDef.UsingVar)
                    symbolFlags |= FrontendSymbolFlags.UsingVar;

                foreach (ref var variable in varDef.Variables.AsSpan ()) {
                    var varName = variable.Name.Text.Span;
                    var varNameId = idPool.GetIdentifier (varName);

                    if (!implicitType) {
                        compileData.Symbols.AddSymbol (varNameId, FrontendSymbol.NewVariable (new (varType, null!), symbolFlags));

                        if (variable.InitializationExpression is not null)
                            FoldExpression (ref compileData, ref passData, ref variable.InitializationExpression, varType);
                    } else {
                        Debug.Assert (variable.InitializationExpression is not null);
                        var exprData = FoldExpression (ref compileData, ref passData, ref variable.InitializationExpression, typeUnkn);

                        compileData.Symbols.AddSymbol (varNameId, FrontendSymbol.NewVariable (new (exprData.Type, null!), symbolFlags));
                    }
                }

                break;
            }

#endregion

#region Jumps

            case ES_AstConditionalStatement condStmt: {
                FoldExpression (ref compileData, ref passData, ref condStmt.ConditionExpression, typeBool);

                FoldStatement (ref compileData, ref passData, condStmt.ThenStatement, retType);
                if (condStmt.ElseStatement is not null)
                    FoldStatement (ref compileData, ref passData, condStmt.ElseStatement, retType);

                break;
            }

            case ES_AstSwitchStatement switchStmt: {
                var exprTypeData = FoldExpression (ref compileData, ref passData, ref switchStmt.ValueExpression, typeUnkn);

                foreach (var section in switchStmt.Sections) {
                    foreach (ref var expr in section.Expressions.AsSpan ()) {
                        if (expr is not null)
                            FoldExpression (ref compileData, ref passData, ref expr, exprTypeData.Type);
                    }

                    var subStmt = section.StatementsBlock;
                    while (subStmt is not null) {
                        FoldStatement (ref compileData, ref passData, subStmt, retType);

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
                    FoldExpression (ref compileData, ref passData, ref retStmt.ReturnExpression, retType);

                break;
            }

#endregion

#region Loops

            case ES_AstLoopStatement loopStmt: {
                compileData.Symbols.Push ();

                if (loopStmt.InitializationStatement is not null)
                    FoldStatement (ref compileData, ref passData, loopStmt.InitializationStatement, retType);

                if (loopStmt.ConditionExpression is not null)
                    FoldExpression (ref compileData, ref passData, ref loopStmt.ConditionExpression, typeBool);

                if (loopStmt.IterationExpressions is not null) {
                    foreach (ref var expr in loopStmt.IterationExpressions.AsSpan ()) {
                        if (expr is not null)
                            FoldExpression (ref compileData, ref passData, ref expr, typeUnkn);
                    }
                }

                Debug.Assert (loopStmt.LoopBody is not null);
                Debug.Assert (loopStmt.LoopBody.Endpoint is null);

                FoldStatement (ref compileData, ref passData, loopStmt.LoopBody, retType);

                compileData.Symbols.Pop ();

                break;
            }

#endregion

            case ES_AstExpressionStatement exprStmt:
                FoldExpression (ref compileData, ref passData, ref exprStmt.Expression, typeUnkn);
                break;

            case ES_AstExpressionListStatement exprListStmt: {
                foreach (ref var expr in exprListStmt.Expressions.AsSpan ())
                    FoldExpression (ref compileData, ref passData, ref expr, typeUnkn);

                break;
            }

            default:
                throw new NotImplementedException ("Statement type not implemented.");
        }
    }
#endif
}
