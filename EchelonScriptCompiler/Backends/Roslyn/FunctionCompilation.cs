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
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Utilities;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        public ref struct FunctionInfo {
            public ES_FunctionData* Data;
            public ES_FunctionPrototypeData* Type;

            public MethodDeclarationSyntax Definition;
        }

        public struct StatementData : IDisposable {
            public bool AlwaysReturns;

            public PooledArray<StatementSyntax> Statements;

            public StatementData (StatementSyntax statement, bool alwaysReturns = false) {
                AlwaysReturns = alwaysReturns;

                Statements = PooledArray<StatementSyntax>.GetArray (1);
                Statements.Span [0] = statement;
            }

            public StatementData (ReadOnlySpan<StatementSyntax> statements, bool alwaysReturns = false) {
                AlwaysReturns = alwaysReturns;

                Statements = PooledArray<StatementSyntax>.GetArray (statements.Length);
                statements.CopyTo (Statements.Span);
            }

            public StatementData (bool alwaysReturns) {
                AlwaysReturns = alwaysReturns;
                Statements = PooledArray<StatementSyntax>.Empty ();
            }

            public void Dispose () {
                Statements.Dispose ();
            }
        }

        private ExpressionSyntax GetDefaultValue (ES_TypeInfo* varType) {
            switch (varType->TypeTag) {
                case ES_TypeTag.Bool:
                    return LiteralExpression (SyntaxKind.FalseLiteralExpression);
                case ES_TypeTag.Int:
                    return LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (0));

                case ES_TypeTag.Float:
                    return LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (0));

                case ES_TypeTag.Reference:
                    return LiteralExpression (SyntaxKind.NullLiteralExpression);

                case ES_TypeTag.Struct: {
                    using var initExprNodes = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);

                    foreach (var memberAddr in varType->MembersList.MembersList.Span) {
                        var memberPtr = memberAddr.Address;

                        if (memberPtr->MemberType != ES_MemberType.Field)
                            continue;

                        if (memberPtr->Flags.HasFlag (ES_MemberFlags.Static))
                            continue;

                        var memberVarPtr = (ES_MemberData_Variable*) memberPtr;
                        var memberVarType = memberVarPtr->Type;

                        var varValue = GetDefaultValue (memberVarType);
                        initExprNodes.Add (AssignmentExpression (
                            SyntaxKind.SimpleAssignmentExpression,
                            IdentifierName (memberPtr->Name.GetPooledString (Encoding.ASCII)),
                            varValue
                        ));
                        initExprNodes.Add (Token (SyntaxKind.CommaToken));
                    }

                    using var structNameChars = MangleTypeName (varType);
                    var structName = IdentifierName (structNameChars.Span.GetPooledString ());
                    var initExpr = InitializerExpression (
                        SyntaxKind.ObjectInitializerExpression,
                        SeparatedList<ExpressionSyntax> (initExprNodes.ToArray ())
                    );
                    return ObjectCreationExpression (structName).WithInitializer (initExpr);
                }

                default:
                    throw new NotImplementedException ("Variable type not implemented.");
            }
        }

        private MethodDeclarationSyntax GenerateFunction (
            ES_NamespaceData? namespaceData, ES_TypeInfo* parentType, ArrayPointer<byte> funcId,
            out ES_FunctionData* funcData
        ) {
            if (namespaceData is not null) {
                if (!namespaceData.Functions.TryGetValue (funcId, out var ptr))
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                funcData = ptr;
            } else if (parentType is not null) {
                throw new NotImplementedException ("[TODO] Member functions not implemented yet.");
            } else
                throw new CompilationException (ES_BackendErrors.FrontendError);

            Debug.Assert (funcData is not null);
            var funcType = funcData->FunctionType;

            var protoArgsList = funcType->ArgumentsList.Span;
            var funcArgsList = funcData->Arguments.Span;
            Debug.Assert (protoArgsList.Length == funcArgsList.Length);

            using var argsArr = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
            argsArr.EnsureCapacity (funcType->ArgumentsList.Length);
            for (int argNum = 0; argNum < funcArgsList.Length; argNum++) {
                var protoArg = protoArgsList [argNum];
                var funcArg = funcArgsList [argNum];

                if (protoArg.ArgType != ES_ArgumentType.Normal)
                    throw new NotImplementedException ("[TODO] Argument type not implemented yet.");

                var roslynType = GetRoslynType (protoArg.ValueType);
                var argIdent = Identifier (funcArg.Name.GetPooledString (Encoding.ASCII));

                var parameterData = Parameter (argIdent).WithType (roslynType);

                if (argNum > 0)
                    argsArr.Add (Token (SyntaxKind.CommaToken));
                argsArr.Add (parameterData);
            }

            var retType = funcData->FunctionType->ReturnType;
            var paramsList = ParameterList (SeparatedList<ParameterSyntax> (argsArr));

            if (namespaceData is not null) {
                using var funcName = MangleGlobalFunctionName (funcData);

                return (MethodDeclaration (GetRoslynType (retType), funcName.Span.GetPooledString ())
                    .WithParameterList (paramsList)
                    .WithModifiers (TokenList (
                        Token (SyntaxKind.PublicKeyword),
                        Token (SyntaxKind.StaticKeyword)
                    ))
                );
            } else if (parentType is not null) {
                throw new NotImplementedException ("[TODO] Member functions not implemented yet.");
            } else
                throw new CompilationException (ES_BackendErrors.FrontendError);
        }

        private MethodDeclarationSyntax GenerateCode_Function (
            ref TranslationUnitData transUnit, ES_NamespaceData? namespaceData,
            SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_TypeInfo* parentType, ES_AstFunctionDefinition funcDef
        ) {
            Debug.Assert (env is not null);
            Debug.Assert (namespaceData is not null || parentType is not null);

            symbols.Push ();

            var funcInfo = new FunctionInfo ();

            funcInfo.Definition = GenerateFunction (
                namespaceData, parentType, env.IdPool.GetIdentifier (funcDef.Name.Text.Span),
                out funcInfo.Data
            );

            funcInfo.Type = funcInfo.Data->FunctionType;
            var retType = funcInfo.Type->ReturnType;

            var protoArgsList = funcInfo.Type->ArgumentsList.Span;
            var funcArgsList = funcInfo.Data->Arguments.Span;
            Debug.Assert (protoArgsList.Length == funcArgsList.Length);

            for (uint i = 0; i < funcArgsList.Length; i++) {
                ref var argTypeInfo = ref protoArgsList [(int) i];
                ref var argData = ref funcArgsList [(int) i];

                var argFlags = (VariableFlags) 0;

                switch (argTypeInfo.ArgType) {
                    case ES_ArgumentType.Normal:
                        break;

                    default:
                        throw new NotImplementedException ("Argument type not implemented.");
                }

                symbols.AddSymbol (argData.Name, new Symbol (new VariableData (
                    argTypeInfo.ValueType,
                    argFlags,
                    argData.Name.GetPooledString (Encoding.ASCII)
                )));
            }

            Debug.Assert (funcDef.Statement is not null);
            Debug.Assert (funcDef.Statement.Endpoint is null);
            if (funcDef.ExpressionBody) {
                Debug.Assert (funcDef.Statement is ES_AstExpressionStatement);

                var exprExpType = retType->TypeTag != ES_TypeTag.Void ? retType : env.TypeUnknownValue;
                var exprStmt = (funcDef.Statement as ES_AstExpressionStatement)!;
                var exprData = GenerateCode_Expression (ref transUnit, symbols, src, exprStmt.Expression, exprExpType);

                if (exprData.TypeInfo is not null || exprData.Function is not null)
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                if (retType->TypeTag != ES_TypeTag.Void)
                    GenerateCode_EnsureImplicitCompat (ref exprData, retType);

                Debug.Assert (exprData.Value is not null);

                funcInfo.Definition = funcInfo.Definition.WithExpressionBody (ArrowExpressionClause (exprData.Value));
            } else {
                using var stmtData = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, funcDef.Statement);

                if (!stmtData.AlwaysReturns && retType->TypeTag != ES_TypeTag.Void)
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                Debug.Assert (stmtData.Statements.RealLength == 1);
                Debug.Assert (stmtData.Statements.Span [0] is BlockSyntax);

                funcInfo.Definition = funcInfo.Definition.WithBody (stmtData.Statements.Span [0] as BlockSyntax);
            }

            symbols.Pop ();

            return funcInfo.Definition;
        }

        private StatementData GenerateCode_Statement (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            FunctionInfo funcInfo, ES_TypeInfo* retType, ES_AstStatement stmt
        ) {
            Debug.Assert (stmt is not null);
            var idPool = env!.IdPool;
            var typeUnkn = env.TypeUnknownValue;

            switch (stmt) {
                case ES_AstEmptyStatement:
                    return new StatementData (EmptyStatement (), false);

                case ES_AstLabeledStatement labelStmt: {
                    throw new NotImplementedException ("[TODO] Labels not implemented yet.");
                    //return GenerateCode_Statement (ref transUnit, symbols, src, retType, labelStmt.Statement);
                }

                case ES_AstBlockStatement blockStmt: {
                    symbols.Push ();

                    bool alwaysReturns = false;

                    using var stmtsList = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);
                    ES_AstStatement? subStmt = blockStmt.Statement;
                    while (subStmt is not null) {
                        using var subStmtData = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, subStmt);

                        stmtsList.AddRange (subStmtData.Statements);

                        if (subStmtData.AlwaysReturns) {
                            alwaysReturns = true;
                            break;
                        }

                        subStmt = subStmt.Endpoint;
                    }

                    symbols.Pop ();

                    return new StatementData (Block (stmtsList), alwaysReturns);
                }

                #region Symbol definition

                case ES_AstImportStatement importStmt:
                    AST_HandleImport (symbols, importStmt);
                    return new StatementData (false);

                case ES_AstTypeAlias aliasStmt:
                    AST_HandleAlias (symbols, aliasStmt);
                    return new StatementData (false);

                case ES_AstLocalVarDefinition varDef: {
                    bool implicitType = varDef.ValueType is null;

                    var baseVarFlags = (VariableFlags) 0;
                    if (varDef.UsingVar)
                        baseVarFlags |= VariableFlags.Using;

                    using var declStatements = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);
                    foreach (var variable in varDef.Variables) {
                        var varName = variable.Name.Text.Span;
                        var varNameId = idPool.GetIdentifier (varName);

                        var varFlags = baseVarFlags;
                        var varData = new VariableData {
                            Flags = varFlags,
                            RoslynName = varName.GetPooledString (),
                        };

                        ExpressionSyntax initVal;
                        if (!implicitType) {
                            varData.Type = GetTypeRef (varDef.ValueType);

                            if (variable.InitializationExpression is not null) {
                                var initExprData = GenerateCode_Expression (ref transUnit, symbols, src, variable.InitializationExpression, varData.Type);
                                GenerateCode_EnsureImplicitCompat (ref initExprData, varData.Type);

                                Debug.Assert (initExprData.Value is not null);

                                initVal = initExprData.Value;
                            } else
                                initVal = GetDefaultValue (varData.Type);
                        } else {
                            Debug.Assert (variable.InitializationExpression is not null);
                            var initExprData = GenerateCode_Expression (ref transUnit, symbols, src, variable.InitializationExpression, typeUnkn);

                            Debug.Assert (initExprData.Value is not null);

                            varData.Type = initExprData.Type;
                            initVal = initExprData.Value;
                        }

                        if (!symbols.AddSymbol (varNameId, new Symbol (varData)))
                            throw new CompilationException (ES_BackendErrors.FrontendError);

                        var varDeclarator = VariableDeclarator (varData.RoslynName).WithInitializer (EqualsValueClause (initVal));
                        var declStmt = LocalDeclarationStatement (
                            VariableDeclaration (GetRoslynType (varData.Type))
                                .WithVariables (SingletonSeparatedList (varDeclarator))
                        );
                        declStatements.Add (declStmt);
                    }

                    return new StatementData (declStatements.Span, false);
                }

                #endregion

                #region Jumps

                case ES_AstConditionalStatement condStmt: {
                    var boolType = env!.TypeBool;

                    bool hasElse = condStmt.ElseStatement is not null;

                    // Generate the condition.
                    var condExpr = GenerateCode_Expression (ref transUnit, symbols, src, condStmt.ConditionExpression, boolType);
                    GenerateCode_EnsureImplicitCompat (ref condExpr, boolType);

                    Debug.Assert (condExpr.Value is not null);

                    // Generate the then block.
                    using var thenStmtData = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, condStmt.ThenStatement);
                    StatementSyntax thenStmt;
                    if (thenStmtData.Statements.RealLength != 1) {
                        Debug.Assert (thenStmtData.Statements.RealLength > 1);
                        thenStmt = Block (thenStmtData.Statements.Span.ToArray ());
                    } else
                        thenStmt = thenStmtData.Statements.Span [0];

                    // Generate the else block (if any) and the Roslyn statement.
                    if (hasElse) {
                        var elseStmtData = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, condStmt.ElseStatement!);
                        StatementSyntax elseStmt;
                        if (elseStmtData.Statements.RealLength != 1) {
                            Debug.Assert (elseStmtData.Statements.RealLength > 1);
                            elseStmt = Block (elseStmtData.Statements.Span.ToArray ());
                        } else
                            elseStmt = elseStmtData.Statements.Span [0];

                        var ifStatement = IfStatement (condExpr.Value, thenStmt, ElseClause (elseStmt));
                        var alwaysReturns = thenStmtData.AlwaysReturns && elseStmtData.AlwaysReturns;
                        return new StatementData (ifStatement, alwaysReturns);
                    } else {
                        var ifStatement = IfStatement (condExpr.Value, thenStmt);
                        return new StatementData (ifStatement);
                    }
                }

                case ES_AstSwitchStatement switchStmt: {
                    throw new NotImplementedException ("[TODO] Switches not implemented yet.");
                    /*var exprTypeData = GenerateCode_Expression (ref transUnit, symbols, src, switchStmt.ValueExpression, null);

                    foreach (var section in switchStmt.Sections) {
                        foreach (var expr in section.Expressions) {
                            if (expr is not null) {
                                var sectionTypeData = GenerateCode_Expression (ref transUnit, symbols, src, expr, exprTypeData.Type);

                                if (!sectionTypeData.Constant) {
                                    errorList.Add (new EchelonScriptErrorMessage (
                                        src, expr.NodeBounds, ES_FrontendErrors.ConstantExprExpected
                                    ));
                                }

                                if (!CheckTypes_MustBeCompat (exprTypeData.Type, sectionTypeData.Type)) {
                                    errorList.Add (ES_FrontendErrors.GenNoCast (
                                        exprTypeData.Type->FullyQualifiedNameString, sectionTypeData.Type->FullyQualifiedNameString,
                                        src, sectionTypeData.Expr.NodeBounds
                                    ));
                                }
                            }
                        }

                        foreach (var subStmt in section.StatementsBlock)
                            GenerateCode_Statement (ref transUnit, symbols, src, retType, subStmt);
                    }

                    // TODO: Change Switch statements to check if they always return.
                    return new StatementData (false);*/
                }

                case ES_AstGotoCaseStatement gotoCaseStmt:
                    throw new NotImplementedException ("[TODO] 'goto case' not implemented yet.");

                case ES_AstReturnStatement retStmt: {
                    if (retType->TypeTag == ES_TypeTag.Void) {
                        if (retStmt.ReturnExpression is not null)
                            throw new CompilationException (ES_BackendErrors.FrontendError);

                        return new StatementData (ReturnStatement (), true);
                    } else if (retStmt.ReturnExpression is not null) {
                        var exprData = GenerateCode_Expression (ref transUnit, symbols, src, retStmt.ReturnExpression, retType);
                        GenerateCode_EnsureImplicitCompat (ref exprData, retType);

                        Debug.Assert (exprData.Value is not null);

                        return new StatementData (ReturnStatement (exprData.Value), true);
                    } else // if (retType->TypeTag != ES_TypeTag.Void)
                        throw new CompilationException (ES_BackendErrors.FrontendError);
                }

                #endregion

                #region Loops

                case ES_AstLoopStatement loopStmt: {
                    var boolType = env!.TypeBool;

                    symbols.Push ();

                    using var loopBlock = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);

                    // Emit the init statements, if any.
                    if (loopStmt.InitializationStatement is not null) {
                        using var initStmt = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, loopStmt.InitializationStatement);
                        loopBlock.AddRange (initStmt.Statements);
                    }

                    // Emit the body.
                    Debug.Assert (loopStmt.LoopBody is not null);
                    Debug.Assert (loopStmt.LoopBody.Endpoint is null);

                    using var loopBodyStmt = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, loopStmt.LoopBody);
                    StatementSyntax loopBody;
                    if (loopBodyStmt.Statements.RealLength == 1)
                        loopBody = loopBodyStmt.Statements.Span [0];
                    else {
                        Debug.Assert (loopBodyStmt.Statements.RealLength > 1);
                        loopBody = Block (loopBodyStmt.Statements.Span.ToArray ());
                    }

                    // Emit the loop.
                    var statement = ForStatement (loopBody);

                    // Emit the condition, if any.
                    if (loopStmt.ConditionExpression is not null) {
                        var condExprData = GenerateCode_Expression (ref transUnit, symbols, src, loopStmt.ConditionExpression, boolType);
                        GenerateCode_EnsureImplicitCompat (ref condExprData, boolType);

                        Debug.Assert (condExprData.Value is not null);
                        statement = statement.WithCondition (condExprData.Value);
                    }

                    // Attach the increment expressions, if any.
                    if (loopStmt.IterationExpressions is not null) {
                        using var iterExprs = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
                        foreach (var expr in loopStmt.IterationExpressions) {
                            Debug.Assert (expr is not null);
                            var exprData = GenerateCode_Expression (ref transUnit, symbols, src, expr, null);

                            Debug.Assert (exprData.Value is not null);
                            iterExprs.Add (exprData.Value);
                        }

                        statement = statement.WithIncrementors (SeparatedList<ExpressionSyntax> (iterExprs));
                    }

                    loopBlock.Add (statement);

                    symbols.Pop ();

                    return new StatementData (Block (loopBlock), false);
                }

                #endregion

                case ES_AstExpressionStatement exprStmt: {
                    var exprData = GenerateCode_Expression (ref transUnit, symbols, src, exprStmt.Expression, typeUnkn);
                    Debug.Assert (exprData.Value is not null);

                    return new StatementData (ExpressionStatement (exprData.Value), false);
                }

                case ES_AstExpressionListStatement exprListStmt: {
                    using var expressions = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);

                    foreach (var expr in exprListStmt.Expressions) {
                        var exprData = GenerateCode_Expression (ref transUnit, symbols, src, expr, typeUnkn);
                        Debug.Assert (exprData.Value is not null);

                        expressions.Add (ExpressionStatement (exprData.Value));
                    }

                    return new StatementData (expressions.Span, false);
                }

                default:
                    throw new NotImplementedException ("Expression type not implemented.");
            }
        }
    }
}
