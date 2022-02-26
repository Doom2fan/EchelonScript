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
using ChronosLib.Pooled;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Utilities;

using static EchelonScriptCompiler.CompilerCommon.IR.ESIR_Factory;

namespace EchelonScriptCompiler.Frontend {
    internal unsafe static partial class Compiler_TypeChecking {
        private struct StatementData {
            public bool AlwaysReturns;
        }

        private static StatementData CheckStatement (
            ref PassData passData, TypeData retType,
            ES_AstStatement stmt
        ) {
            var idPool = passData.Env.IdPool;
            var typeUnknMut = passData.GetUnknownType (Constness.Mutable);

            switch (stmt) {
                case ES_AstEmptyStatement:
                    return new StatementData { AlwaysReturns = false };

                case ES_AstLabeledStatement labelStmt:
                    throw new NotImplementedException ("[TODO] Labels not implemented yet.");

                case ES_AstBlockStatement blockStmt:
                    return CheckStatement_Block (ref passData, retType, blockStmt);

                #region Symbol definition

                case ES_AstImportStatement importStmt:
                    HandleImport (ref passData, importStmt);
                    return new StatementData { AlwaysReturns = false };

                case ES_AstTypeAlias aliasStmt:
                    HandleAlias (ref passData, aliasStmt);
                    return new StatementData { AlwaysReturns = false };

                case ES_AstLocalVarDefinition varDef:
                    return CheckStatement_LocalVarDef (ref passData, retType, varDef);

                #endregion

                #region Jumps

                case ES_AstConditionalStatement condStmt:
                    return CheckStatement_Conditional (ref passData, retType, condStmt);

                case ES_AstSwitchStatement switchStmt: {
                    throw new NotImplementedException ("[TODO] Switches not implemented yet.");
                    /*var exprTypeData = CheckTypes_Expression (ref transUnit, symbols, src, switchStmt.ValueExpression, typeUnkn);

                    foreach (var section in switchStmt.Sections) {
                        foreach (var expr in section.Expressions) {
                            if (expr is not null) {
                                var sectionTypeData = CheckTypes_Expression (ref transUnit, symbols, src, expr, exprTypeData.Type);

                                if (!sectionTypeData.Constant) {
                                    errorList.Add (new (
                                        src, expr.NodeBounds, ES_FrontendErrors.ConstantExprExpected
                                    ));
                                }

                                if (!CheckTypes_MustBeCompat (exprTypeData.Type, sectionTypeData.Type, out var sectionConst)) {
                                    errorList.Add (ES_FrontendErrors.GenNoCast (
                                        exprTypeData.Type->Name.GetNameAsTypeString (), sectionTypeData.Type->Name.GetNameAsTypeString (),
                                        src, sectionTypeData.Expr.NodeBounds
                                    ));
                                }

                                if (!sectionTypeData.Constant || !sectionConst) {
                                    errorList.Add (new (
                                        src, expr.NodeBounds, ES_FrontendErrors.ConstantExprExpected
                                    ));
                                }
                            }
                        }

                        ES_AstStatement? subStmt = section.StatementsBlock;
                        while (subStmt is not null) {
                            CheckTypes_Statement (ref transUnit, symbols, src, retType, subStmt);

                            subStmt = subStmt.Endpoint;
                        }
                    }

                    // TODO: Change Switch statements to check if they always return.
                    return new StatementData { AlwaysReturns = false };*/
                }

                case ES_AstBreakStatement breakStmt: {
                    var irStmt = breakStmt.LabelName is not null
                        ? BreakStatement (idPool.GetIdentifier (breakStmt.LabelName.Value.Text.Span))
                        : BreakStatement ();

                    passData.IRWriter.AddStatement (irStmt);

                    return new StatementData { AlwaysReturns = false, };
                }

                case ES_AstContinueStatement continueStmt: {
                    var irStmt = continueStmt.LabelName is not null
                        ? ContinueStatement (idPool.GetIdentifier (continueStmt.LabelName.Value.Text.Span))
                        : ContinueStatement ();

                    passData.IRWriter.AddStatement (irStmt);

                    return new StatementData { AlwaysReturns = false, };
                }

                case ES_AstGotoCaseStatement gotoCaseStmt:
                    throw new NotImplementedException ("[TODO] 'goto case' not implemented yet.");

                case ES_AstReturnStatement retStmt:
                    return CheckStatement_Return (ref passData, retType, retStmt);

                #endregion

                case ES_AstLoopStatement loopStmt:
                    return CheckStatement_Loop (ref passData, retType, loopStmt);

                case ES_AstExpressionStatement exprStmt: {
                    var exprData = CheckExpression (ref passData, exprStmt.Expression, typeUnknMut);
                    FinishExpression (ref passData, ref exprData);

                    return new StatementData { AlwaysReturns = false };
                }

                case ES_AstExpressionListStatement exprListStmt: {
                    foreach (var expr in exprListStmt.Expressions) {
                        var exprData = CheckExpression (ref passData, expr, passData.GetUnknownType (Constness.Mutable));
                        FinishExpression (ref passData, ref exprData);
                    }

                    return new StatementData { AlwaysReturns = false };
                }

                default:
                    throw new NotImplementedException ("Statement type not implemented.");
            }
        }

        private static StatementData CheckStatement_Block (
            ref PassData passData, TypeData retType,
            ES_AstBlockStatement blockStmt
        ) {
            var symbols = passData.Symbols;
            var irWriter = passData.IRWriter;

            symbols.Push ();
            irWriter.PushScope ();

            var alwaysReturns = false;
            var reportedUnreachable = false;

            var subStmt = blockStmt.Statement;
            while (subStmt is not null) {
                Debug.Assert (subStmt is not null);

                if (alwaysReturns && !reportedUnreachable) {
                    passData.WarnList.Add (new (
                        passData.Source, subStmt.NodeBounds, ES_FrontendWarnings.UnreachableCode
                    ));

                    reportedUnreachable = true;
                }

                var subStmtData = CheckStatement (ref passData, retType, subStmt);

                alwaysReturns |= subStmtData.AlwaysReturns;

                subStmt = subStmt.Endpoint;
            }

            using var blockStmts = irWriter.PopScope ();
            irWriter.AddStatements (blockStmts.Span);

            symbols.Pop ();
            return new StatementData { AlwaysReturns = alwaysReturns };
        }

        private static StatementData CheckStatement_LocalVarDef (
            ref PassData passData, TypeData retType,
            ES_AstLocalVarDefinition varDef
        ) {
            var idPool = passData.Env.IdPool;
            var symbols = passData.Symbols;
            var irWriter = passData.IRWriter;

            var typeUnknMut = passData.GetUnknownType (Constness.Mutable);

            var implicitType = varDef.ValueType is null;

            var varType = !implicitType ? UnpackFirstConst (GetTypeRef (varDef.ValueType)) : TypeData.Null;
            var allVarsFlags = (SymbolFlags) 0;

            if (varDef.UsingVar)
                allVarsFlags |= SymbolFlags.UsingVar;

            foreach (var variable in varDef.Variables) {
                var varName = variable.Name.Text.Span;
                var varNameId = idPool.GetIdentifier (varName);

                using var exprList = new ExpressionList (null, null);
                ExpressionData varExprData;
                ESIR_Expression? valueExpr;
                int? valueReg;

                var varFlags = allVarsFlags;

                if (!implicitType) {
                    if (variable.InitializationExpression is not null) {
                        varExprData = CheckExpression (ref passData, variable.InitializationExpression, varType);
                        EnsureCompat (ref varExprData, varType, ref passData, varExprData.Expr.NodeBounds);

                        exprList.Merge (ref varExprData.Expressions);

                        valueExpr = varExprData.Value;
                        valueReg = varExprData.ValueRegister;
                    } else {
                        varExprData = default;
                        valueExpr = null;
                        valueReg = null;
                    }
                } else {
                    Debug.Assert (variable.InitializationExpression is not null);
                    varExprData = CheckExpression (ref passData, variable.InitializationExpression, typeUnknMut);

                    exprList.Merge (ref varExprData.Expressions);

                    varType = varExprData.Type;
                    valueExpr = varExprData.Value;
                    valueReg = varExprData.ValueRegister;

                    if (varType.Type is not null && varType.Type->TypeTag == ES_TypeTag.Null) {
                        passData.ErrorList.Add (new (passData.Source, varExprData.Expr.NodeBounds, ES_FrontendErrors.NoImplicitNullVars));

                        varType = typeUnknMut;
                    } else if (varExprData.Type.Type is null) {
                        passData.ErrorList.Add (new (passData.Source, varExprData.Expr.NodeBounds, ES_FrontendErrors.NotValueExpression));

                        varType = typeUnknMut;
                        varExprData = default;
                        valueExpr = null;
                        valueReg = null;
                    }
                }

                // TODO: Handle constants.
                if (varType.IsWritable)
                    varFlags |= SymbolFlags.Writable;
                if (varType.Constness != Constness.Mutable && varExprData.CompileTimeConst)
                    varFlags |= SymbolFlags.CompileTimeConstant;

                foreach (var expr in exprList.Expressions)
                    irWriter.AddStatement (ExpressionStatement (expr));

                var varReg = irWriter.RentRegister (TypeNode (ref passData, varType));
                irWriter.AddScopeRegister (varReg);
                var value = LocalValueExpression (varReg);
                if (valueExpr is not null)
                    irWriter.AddStatement (ExpressionStatement (AssignmentExpression (value, valueExpr)));

                exprList.ReturnRegisters (irWriter);
                irWriter.ReturnRegister (valueReg);

                if (!symbols.AddSymbol (varNameId, TCSymbol.NewVariable (varType, value, varFlags))) {
                    passData.ErrorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (
                        varName.GetPooledString (), variable.Name
                    ));
                }
            }

            return new StatementData { AlwaysReturns = false };
        }

        private static StatementData CheckStatement_Conditional (
            ref PassData passData, TypeData retType,
            ES_AstConditionalStatement condStmt
        ) {
            var irWriter = passData.IRWriter;

            var typeBoolConst = passData.GetBoolType (Constness.Const);

            // Emit the condition expression.
            var condExprData = CheckExpression (ref passData, condStmt.ConditionExpression, typeBoolConst);
            EnsureCompat (ref condExprData, typeBoolConst, ref passData, condExprData.Expr.NodeBounds);

            foreach (var expr in condExprData.Expressions.Expressions)
                irWriter.AddStatement (ExpressionStatement (expr));
            condExprData.Expressions.ReturnRegisters (irWriter);
            condExprData.Expressions.Dispose ();

            irWriter.ReturnRegister (condExprData.ValueRegister);

            var alwaysReturns = false;

            // Emit the "then" part.
            irWriter.PushScope ();
            var thenStmtData = CheckStatement (ref passData, retType, condStmt.ThenStatement);
            using var thenStmtsList = irWriter.PopScope ();
            var thenStmt = OptionalBlockStatement (thenStmtsList.Span);

            // Emit the "else" part, if any.
            ESIR_Statement? elseStmt = null;
            if (condStmt.ElseStatement is not null) {
                irWriter.PushScope ();

                var elseStmtData = CheckStatement (ref passData, retType, condStmt.ElseStatement);

                using var elseStmtsList = irWriter.PopScope ();
                elseStmt = OptionalBlockStatement (elseStmtsList.Span);

                if (thenStmtData.AlwaysReturns && elseStmtData.AlwaysReturns)
                    alwaysReturns = true;
            }

            irWriter.AddStatement (ConditionalStatement (condExprData.Value, thenStmt, elseStmt));

            return new StatementData { AlwaysReturns = alwaysReturns };
        }

        private static StatementData CheckStatement_Return (
            ref PassData passData, TypeData retType,
            ES_AstReturnStatement retStmt
        ) {
            var irWriter = passData.IRWriter;

            if (retStmt.ReturnExpression is not null) {
                var exprData = CheckExpression (ref passData, retStmt.ReturnExpression, retType);
                EnsureCompat (ref exprData, retType, ref passData, exprData.Expr.NodeBounds);

                using var exprList = exprData.Expressions;

                foreach (var expr in exprList.Expressions)
                    irWriter.AddStatement (ExpressionStatement (expr));

                exprList.ReturnRegisters (irWriter);
                irWriter.ReturnRegister (exprData.ValueRegister);

                if (exprData.Type.Type->TypeTag != ES_TypeTag.Void)
                    irWriter.AddStatement (ReturnStatement (exprData.Value));
                else {
                    irWriter.AddStatement (ExpressionStatement (exprData.Value));
                    irWriter.AddStatement (ReturnStatement ());
                }
            } else if (retType.Type->TypeTag != ES_TypeTag.Void) {
                passData.ErrorList.Add (ES_FrontendErrors.GenMissingReturnValue (
                    retType.ToType (ref passData)->Name.GetNameAsTypeString (),
                    passData.Source, retStmt.NodeBounds
                ));
            } else
                irWriter.AddStatement (ReturnStatement ());

            return new StatementData { AlwaysReturns = true };
        }

        private static StatementData CheckStatement_Loop (
            ref PassData passData, TypeData retType,
            ES_AstLoopStatement loopStmt
        ) {
            var symbols = passData.Symbols;
            var irWriter = passData.IRWriter;

            var typeUnknMut = passData.GetUnknownType (Constness.Mutable);
            var typeBoolConst = passData.GetBoolType (Constness.Const);

            symbols.Push ();
            irWriter.PushScope ();

            // Emit the initialization statements.
            if (loopStmt.InitializationStatement is not null)
                CheckStatement (ref passData, retType, loopStmt.InitializationStatement);

            // Emit the condition expression.
            ESIR_List<ESIR_Expression> condExprs;
            if (loopStmt.ConditionExpression is not null) {
                var condExprData = CheckExpression (ref passData, loopStmt.ConditionExpression, typeBoolConst);
                EnsureCompat (ref condExprData, typeBoolConst, ref passData, condExprData.Expr.NodeBounds);

                using var condExprList = condExprData.Expressions;

                irWriter.AddScopeRegisters (condExprList.Registers);
                irWriter.AddScopeRegister (condExprData.ValueRegister);

                condExprList.AddExpression (condExprData.Value);

                condExprs = List (condExprList.Expressions);
            } else
                condExprs = List<ESIR_Expression> (LiteralTrueExpression ());

            // Emit the iteration expressions.
            ESIR_List<ESIR_Expression>? iterExprs;
            if (loopStmt.IterationExpressions is not null) {
                using var exprList = new ExpressionList (null, null);

                foreach (var expr in loopStmt.IterationExpressions) {
                    Debug.Assert (expr is not null);

                    var iterExprData = CheckExpression (ref passData, expr, typeUnknMut);

                    exprList.Merge (ref iterExprData.Expressions);
                    exprList.AddExpression (iterExprData.Value);
                    exprList.AddRegister (iterExprData.ValueRegister);
                }

                iterExprs = List (exprList.Expressions);
                irWriter.AddScopeRegisters (exprList.Registers);
            } else
                iterExprs = null;

            // Emit the loop body.
            Debug.Assert (loopStmt.LoopBody is not null);
            Debug.Assert (loopStmt.LoopBody.Endpoint is null);

            var loopBody = loopStmt.LoopBody;

            if (loopBody is ES_AstBlockStatement loopBodyBlock)
                loopBody = loopBodyBlock.Statement;

            irWriter.PushScope ();

            for (var expr = loopBody; expr is not null; expr = expr.Endpoint)
                CheckStatement (ref passData, retType, expr);

            // Pop the scopes and get the statements.
            using var bodyStmts = irWriter.PopScope ();
            using var initStmts = irWriter.PopScope ();

            // Emit the IR.
            if (loopStmt.PostLoop)
                irWriter.AddStatements (bodyStmts.Span);

            irWriter.AddStatements (initStmts.Span);
            irWriter.AddStatement (LoopStatement (
                condExprs,
                iterExprs,
                OptionalBlockStatement (bodyStmts.Span)
            ));

            symbols.Pop ();

            return new StatementData { AlwaysReturns = false };
        }
    }
}
