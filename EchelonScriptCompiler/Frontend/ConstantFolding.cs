/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Frontend {
    public unsafe partial class CompilerFrontend {
        protected void FoldConstants (ref TranslationUnitData transUnit) {
            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                foreach (var nm in astUnit.Ast.Namespaces) {
                    ArrayPointer<byte> namespaceName;
                    using (var nameArr = nm.NamespaceName.ToPooledChars ())
                        namespaceName = Environment!.IdPool.GetIdentifier (nameArr);

                    var namespaceBuilder = EnvironmentBuilder!.GetOrCreateNamespace (namespaceName);
                    var namespaceData = namespaceBuilder.NamespaceData;

                    astUnit.Symbols.Push ();
                    ImportNamespaceSymbols (astUnit.Symbols, namespaceData);

                    foreach (var type in nm.Contents) {
                        switch (type) {
                            case ES_AstClassDefinition classDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (classDef.Name.Text.Span);
                                var classBuilder = namespaceBuilder.GetClass (typeName);
                                Debug.Assert (classBuilder is not null);

                                //FoldConstants_Class (ref transUnit, ref astUnit, classDef, classBuilder);
                                throw new NotImplementedException ("[TODO] Classes not implemented yet.");
                            }

                            case ES_AstStructDefinition structDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (structDef.Name.Text.Span);
                                var structBuilder = namespaceBuilder.GetStruct (typeName);
                                Debug.Assert (structBuilder is not null);

                                FoldConstants_Struct (ref transUnit, ref astUnit, structDef);
                                break;
                            }

                            case ES_AstEnumDefinition enumDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (enumDef.Name.Text.Span);
                                var enumBuilder = namespaceBuilder.GetEnum (typeName);
                                Debug.Assert (enumBuilder is not null);

                                //FoldConstants_Enum (ref transUnit, ref astUnit, enumDef, enumBuilder);
                                throw new NotImplementedException ("[TODO] Enums not implemented yet.");
                            }

                            case ES_AstFunctionDefinition funcDef:
                                FoldConstants_Function (ref transUnit, ref astUnit, funcDef);
                                break;

                            default:
                                throw new NotImplementedException ("Node type not implemented.");
                        }
                    }

                    astUnit.Symbols.Pop ();
                }
            }
        }

        protected void FoldConstants_Struct (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstStructDefinition structDef) {
            var srcCode = astUnit.Ast.Source.Span;
            var symbols = astUnit.Symbols;

            foreach (var member in structDef.Contents) {
                switch (member) {
                    case ES_AstMemberVarDefinition varDef: {
                        var varType = GetTypeRef (varDef.ValueType);

                        if (varDef.InitializationExpression is not null)
                            FoldConstants_Expression (ref transUnit, symbols, srcCode, ref varDef.InitializationExpression, varType);

                        break;
                    }

                    case ES_AstFunctionDefinition funcDef:
                        throw new NotImplementedException ("[TODO] Member functions not implemented yet.");

                    default:
                        throw new NotImplementedException ("Node type not implemented.");
                }
            }
        }

        protected void FoldConstants_Function (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstFunctionDefinition funcDef) {
            var idPool = Environment!.IdPool;
            var unitSrc = astUnit.Ast.Source.Span;
            var symbols = astUnit.Symbols;

            symbols.Push ();

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
                    case ES_ArgumentType.Out:
                        throw new NotImplementedException ("[TODO] Argument type not implemented yet.");

                    default:
                        throw new NotImplementedException ("Argument type not implemented yet.");
                }

                symbols.AddSymbol (argName, NewSymbolVariable (argValType, flags));

                if (arg.DefaultExpression is not null)
                    FoldConstants_Expression (ref transUnit, symbols, unitSrc, ref arg.DefaultExpression, argValType);
            }

            ES_AstStatement? curStatement = funcDef.Statement;
            while (curStatement is not null) {
                FoldConstants_Statement (ref transUnit, symbols, unitSrc, retType, curStatement);

                curStatement = curStatement.Endpoint;
            }

            symbols.Pop ();
        }

        protected void FoldConstants_Statement (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src,
            ES_TypeInfo* retType, ES_AstStatement stmt
        ) {
            Debug.Assert (stmt is not null);
            var idPool = Environment!.IdPool!;
            var typeUnkn = Environment.TypeUnknownValue;

            switch (stmt) {
                case ES_AstEmptyStatement:
                    break;

                case ES_AstLabeledStatement labelStmt:
                    throw new NotImplementedException ("[TODO] Labels not implemented yet.");

                case ES_AstBlockStatement blockStmt: {
                    symbols.Push ();

                    ES_AstStatement? subStmt = blockStmt.Statement;
                    while (subStmt is not null) {
                        Debug.Assert (subStmt is not null);
                        FoldConstants_Statement (ref transUnit, symbols, src, retType, subStmt);

                        subStmt = subStmt.Endpoint;
                    }

                    symbols.Pop ();

                    break;
                }

                #region Symbol definition

                case ES_AstImportStatement importStmt:
                    AST_HandleImport (symbols, src, importStmt);
                    break;

                case ES_AstTypeAlias aliasStmt:
                    AST_HandleAlias (ref transUnit, symbols, src, aliasStmt);
                    break;

                case ES_AstLocalVarDefinition varDef: {
                    bool implicitType = varDef.ValueType is null;
                    var varType = !implicitType ? GetTypeRef (varDef.ValueType) : null;
                    var symbolFlags = (FrontendSymbolFlags) 0;

                    if (varDef.UsingVar)
                        symbolFlags |= FrontendSymbolFlags.UsingVar;

                    foreach (ref var variable in varDef.Variables.AsSpan ()) {
                        var varName = variable.Name.Text.Span;
                        var varNameId = idPool.GetIdentifier (varName);

                        if (!implicitType) {
                            symbols.AddSymbol (varNameId, NewSymbolVariable (varType, symbolFlags));

                            if (variable.InitializationExpression is not null)
                                FoldConstants_Expression (ref transUnit, symbols, src, ref variable.InitializationExpression, varType);
                        } else {
                            Debug.Assert (variable.InitializationExpression is not null);
                            var exprData = FoldConstants_Expression (ref transUnit, symbols, src, ref variable.InitializationExpression, typeUnkn);

                            symbols.AddSymbol (varNameId, NewSymbolVariable (exprData.Type, symbolFlags));
                        }
                    }

                    break;
                }

                #endregion

                #region Jumps

                case ES_AstConditionalStatement condStmt: {
                    var boolType = Environment.TypeBool;

                    FoldConstants_Expression (ref transUnit, symbols, src, ref condStmt.ConditionExpression, boolType);

                    FoldConstants_Statement (ref transUnit, symbols, src, retType, condStmt.ThenStatement);
                    if (condStmt.ElseStatement is not null)
                        FoldConstants_Statement (ref transUnit, symbols, src, retType, condStmt.ElseStatement);

                    break;
                }

                case ES_AstSwitchStatement switchStmt: {
                    var exprTypeData = FoldConstants_Expression (ref transUnit, symbols, src, ref switchStmt.ValueExpression, typeUnkn);

                    foreach (var section in switchStmt.Sections) {
                        foreach (ref var expr in section.Expressions.AsSpan ()) {
                            if (expr is not null)
                                FoldConstants_Expression (ref transUnit, symbols, src, ref expr, exprTypeData.Type);
                        }

                        ES_AstStatement? subStmt = section.StatementsBlock;
                        while (subStmt is not null) {
                            FoldConstants_Statement (ref transUnit, symbols, src, retType, subStmt);

                            subStmt = subStmt.Endpoint;
                        }
                    }

                    break;
                }

                case ES_AstGotoCaseStatement gotoCaseStmt:
                    throw new NotImplementedException ("[TODO] 'goto case' not implemented yet.");

                case ES_AstReturnStatement retStmt: {
                    if (retStmt.ReturnExpression is not null)
                        FoldConstants_Expression (ref transUnit, symbols, src, ref retStmt.ReturnExpression, retType);

                    break;
                }

                #endregion

                #region Loops

                case ES_AstLoopStatement loopStmt: {
                    var boolType = Environment.TypeBool;

                    symbols.Push ();

                    if (loopStmt.InitializationStatement is not null)
                        FoldConstants_Statement (ref transUnit, symbols, src, retType, loopStmt.InitializationStatement);

                    if (loopStmt.ConditionExpression is not null)
                        FoldConstants_Expression (ref transUnit, symbols, src, ref loopStmt.ConditionExpression, boolType);

                    if (loopStmt.IterationExpressions is not null) {
                        foreach (ref var expr in loopStmt.IterationExpressions.AsSpan ()) {
                            if (expr is not null)
                                FoldConstants_Expression (ref transUnit, symbols, src, ref expr, typeUnkn);
                        }
                    }

                    Debug.Assert (loopStmt.LoopBody is not null);
                    Debug.Assert (loopStmt.LoopBody.Endpoint is null);

                    FoldConstants_Statement (ref transUnit, symbols, src, retType, loopStmt.LoopBody);

                    symbols.Pop ();

                    break;
                }

                #endregion

                case ES_AstExpressionStatement exprStmt:
                    FoldConstants_Expression (ref transUnit, symbols, src, ref exprStmt.Expression, typeUnkn);
                    break;

                case ES_AstExpressionListStatement exprListStmt: {
                    foreach (ref var expr in exprListStmt.Expressions.AsSpan ())
                        FoldConstants_Expression (ref transUnit, symbols, src, ref expr, typeUnkn);

                    break;
                }

                default:
                    throw new NotImplementedException ("Statement type not implemented.");
            }
        }

        protected bool FoldConstants_EnsureCompat (ES_TypeInfo* dstType, ref ES_AstExpression expr) {
            if (dstType is null)
                return false;

            switch (expr) {
                case ES_AstFloat32ConstantExpression exprFlt64: {
                    if (dstType->TypeTag != ES_TypeTag.Float)
                        return false;

                    var dstTypeFlt = (ES_FloatTypeData*) dstType;
                    if (dstTypeFlt->FloatSize < ES_FloatSize.Single)
                        return false;

                    switch (dstTypeFlt->FloatSize) {
                        case ES_FloatSize.Single:
                            return true;

                        case ES_FloatSize.Double:
                            expr = new ES_AstFloat64ConstantExpression (exprFlt64.Value, expr);
                            return true;

                        default:
                            throw new NotImplementedException ("Size not implemented.");
                    }
                }

                case ES_AstFloat64ConstantExpression exprFlt64: {
                    if (dstType->TypeTag != ES_TypeTag.Float)
                        return false;

                    var dstTypeFlt = (ES_FloatTypeData*) dstType;
                    if (dstTypeFlt->FloatSize < ES_FloatSize.Double)
                        return false;

                    switch (dstTypeFlt->FloatSize) {
                        case ES_FloatSize.Double:
                            return true;

                        default:
                            throw new NotImplementedException ("Size not implemented.");
                    }
                }

                case ES_AstIntegerConstantExpression intExpr: {
                    if (dstType->TypeTag != ES_TypeTag.Int)
                        return false;

                    var intSrcType = (ES_IntTypeData*) intExpr.IntType;
                    var intDstType = (ES_IntTypeData*) dstType;

                    if (intSrcType->Unsigned != intDstType->Unsigned)
                        return false;

                    var srcSize = intSrcType->IntSize;
                    var dstSize = intDstType->IntSize;

                    if (srcSize > dstSize)
                        return false;

                    if (srcSize == dstSize)
                        return true;

                    ulong val = intExpr.SignExtend ();
                    expr = new ES_AstIntegerConstantExpression (dstType, val, expr);
                    return true;
                }

                case ES_AstBooleanConstantExpression exprBool:
                    return dstType->TypeTag == ES_TypeTag.Bool;

                default:
                    return false;
            }
        }

        protected void FoldConstants_ExplicitCast (ES_TypeInfo* dstType, ref ES_AstExpression expr, out bool isRedundant) {
            Debug.Assert (dstType is not null);

            var castExpr = expr as ES_AstCastExpression;
            Debug.Assert (castExpr is not null);

            if (FoldConstants_EnsureCompat (dstType, ref expr)) {
                isRedundant = true;
                return;
            }

            isRedundant = false;
            switch (dstType->TypeTag) {
                case ES_TypeTag.Int: {
                    FoldConstants_ExplicitCast_ToInt (dstType, castExpr.InnerExpression, ref expr, out isRedundant);
                    break;
                }

                case ES_TypeTag.Float: {
                    FoldConstants_ExplicitCast_ToFloat (dstType, castExpr.InnerExpression, ref expr, out isRedundant);
                    break;
                }
            }
        }

        protected void FoldConstants_BinaryExpression (ref ES_AstExpression expr) {
            var binExpr = expr as ES_AstSimpleBinaryExpression;
            Debug.Assert (binExpr is not null);

            var op = binExpr.ExpressionType;

            if (op.IsAssignment ())
                return;

            if (binExpr.Left is ES_AstBooleanConstantExpression lhsBool && binExpr.Right is ES_AstBooleanConstantExpression rhsBool) {
                if (op.IsComparison ())
                    FoldConstants_BinaryExpression_BoolBool_Comp (ref expr, op, lhsBool, rhsBool);
                else
                    FoldConstants_BinaryExpression_BoolBool_Arithmetic (ref expr, op, lhsBool, rhsBool);
            }

            if (binExpr.Left is ES_AstIntegerConstantExpression lhsInt && binExpr.Right is ES_AstIntegerConstantExpression rhsInt) {
                if (op.IsComparison ())
                    FoldConstants_BinaryExpression_IntInt_Comp (ref expr, op, lhsInt, rhsInt);
                else if (op.IsBitShift ())
                    FoldConstants_BinaryExpression_IntInt_BitShifting (ref expr, op, lhsInt, rhsInt);
                else
                    FoldConstants_BinaryExpression_IntInt_Arithmetic (ref expr, op, lhsInt, rhsInt);
            }

            if (binExpr.Left is ES_AstFloat32ConstantExpression lhsF32) {
                if (binExpr.Right is ES_AstFloat32ConstantExpression rhsF32) {
                    if (op.IsComparison ())
                        FoldConstants_BinaryExpression_Float32Float32_Comp (ref expr, op, lhsF32, rhsF32);
                    else
                        FoldConstants_BinaryExpression_Float32Float32_Arithmetic (ref expr, op, lhsF32, rhsF32);
                } else if (binExpr.Right is ES_AstIntegerConstantExpression) {
                    FoldConstants_BinaryExpression_Float32Int_Arithmetic (
                        ref expr, op, lhsF32, (binExpr.Right as ES_AstIntegerConstantExpression)!
                    );
                }
            }

            if (binExpr.Left is ES_AstFloat64ConstantExpression lhsF64) {
                if (binExpr.Right is ES_AstFloat64ConstantExpression rhsF64) {
                    if (op.IsComparison ())
                        FoldConstants_BinaryExpression_Float64Float64_Comp (ref expr, op, lhsF64, rhsF64);
                    else
                        FoldConstants_BinaryExpression_Float64Float64_Arithmetic (ref expr, op, lhsF64, rhsF64);
                } else if (binExpr.Right is ES_AstIntegerConstantExpression) {
                    FoldConstants_BinaryExpression_Float64Int_Arithmetic (
                        ref expr, op, lhsF64, (binExpr.Right as ES_AstIntegerConstantExpression)!
                    );
                }
            }
        }

        protected void FoldConstants_UnaryExpression (ref ES_AstExpression expr) {
            Debug.Assert (expr is ES_AstSimpleUnaryExpression);

            var unaryExpr = (expr as ES_AstSimpleUnaryExpression)!;

            switch (unaryExpr.Inner) {
                case ES_AstBooleanConstantExpression boolConstExpr: {
                    if (unaryExpr.ExpressionType == SimpleUnaryExprType.LogicalNot)
                        expr = new ES_AstBooleanConstantExpression (!boolConstExpr.Value, expr);

                    break;
                }

                case ES_AstIntegerConstantExpression intConstExpr: {
                    var innerIntType = (ES_IntTypeData*) intConstExpr.IntType;

                    if (unaryExpr.ExpressionType == SimpleUnaryExprType.Positive)
                        expr = new ES_AstIntegerConstantExpression (intConstExpr.IntType, intConstExpr.Value, expr);
                    else if (unaryExpr.ExpressionType == SimpleUnaryExprType.Negative && !innerIntType->Unsigned) {
                        var val = -(long) intConstExpr.Value;
                        expr = new ES_AstIntegerConstantExpression (intConstExpr.IntType, (ulong) -val, expr);
                    } else if (unaryExpr.ExpressionType == SimpleUnaryExprType.BitNot)
                        expr = new ES_AstIntegerConstantExpression (intConstExpr.IntType, ~intConstExpr.Value, expr);

                    break;
                }

                case ES_AstFloat32ConstantExpression floatConstExpr: {
                    if (unaryExpr.ExpressionType == SimpleUnaryExprType.Positive)
                        expr = new ES_AstFloat32ConstantExpression (floatConstExpr.Value, expr);
                    else if (unaryExpr.ExpressionType == SimpleUnaryExprType.Negative)
                        expr = new ES_AstFloat32ConstantExpression (-floatConstExpr.Value, expr);

                    break;
                }

                case ES_AstFloat64ConstantExpression doubleConstExpr: {
                    if (unaryExpr.ExpressionType == SimpleUnaryExprType.Positive)
                        expr = new ES_AstFloat64ConstantExpression (doubleConstExpr.Value, expr);
                    else if (unaryExpr.ExpressionType == SimpleUnaryExprType.Negative)
                        expr = new ES_AstFloat64ConstantExpression (-doubleConstExpr.Value, expr);

                    break;
                }
            }
        }

        protected ExpressionData FoldConstants_Expression (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src,
            ref ES_AstExpression expr, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (expr is not null);

            var typeUnkn = Environment!.TypeUnknownValue;
            var idPool = Environment.IdPool;

            switch (expr) {
                case ES_AstParenthesisExpression parenExpr: {
                    var innerExpr = FoldConstants_Expression (ref transUnit, symbols, src, ref parenExpr.Inner, expectedType);

                    switch (innerExpr.Expr) {
                        case ES_AstBooleanConstantExpression boolExpr:
                            expr = new ES_AstBooleanConstantExpression (boolExpr.Value, expr);
                            break;
                        case ES_AstIntegerConstantExpression intExpr:
                            expr = new ES_AstIntegerConstantExpression (intExpr.IntType, intExpr.Value, expr);
                            break;
                        case ES_AstFloat32ConstantExpression float32Expr:
                            expr = new ES_AstFloat32ConstantExpression (float32Expr.Value, expr);
                            break;
                        case ES_AstFloat64ConstantExpression float64Expr:
                            expr = new ES_AstFloat64ConstantExpression (float64Expr.Value, expr);
                            break;
                    }

                    return new ExpressionData { Expr = expr, Type = typeUnkn };
                }

                #region Primary expressions

                case ES_AstFunctionCallExpression funcCallExpr:
                    return FoldConstants_Expression_FunctionCall (ref transUnit, symbols, src, funcCallExpr, expectedType);

                case ES_AstIndexingExpression indexExpr: {
                    var typeIndex = Environment.GetArrayIndexType ();

                    var indexedExpr = FoldConstants_Expression (ref transUnit, symbols, src, ref indexExpr.IndexedExpression, typeUnkn);
                    var returnType = typeUnkn;

                    if (indexedExpr.Type is not null) {
                        var indexedType = indexedExpr.Type;
                        var indexedTypeTag = indexedType->TypeTag;

                        if (indexedTypeTag == ES_TypeTag.Array) {
                            var arrayData = (ES_ArrayTypeData*) indexedExpr.Type;
                            returnType = arrayData->ElementType;
                        }
                    }

                    foreach (ref var rank in indexExpr.RankExpressions.AsSpan ()) {
                        Debug.Assert (rank is not null);
                        FoldConstants_Expression (ref transUnit, symbols, src, ref rank, typeIndex);
                        FoldConstants_EnsureCompat (typeIndex, ref rank);
                    }

                    return new ExpressionData { Expr = expr, Type = returnType, };
                }

                case ES_AstNewObjectExpression newObjExpr: {
                    var type = GetTypeRef (newObjExpr.TypeDeclaration);

                    foreach (var arg in newObjExpr.Arguments)
                        FoldConstants_Expression (ref transUnit, symbols, src, ref arg.ValueExpression, typeUnkn);

                    return new ExpressionData { Expr = expr, Type = type, };
                }

                case ES_AstNewArrayExpression newArrayExpr: {
                    var indexType = Environment!.GetArrayIndexType ();

                    foreach (ref var rank in newArrayExpr.Ranks.AsSpan ()) {
                        Debug.Assert (rank is not null);
                        FoldConstants_Expression (ref transUnit, symbols, src, ref rank, indexType);
                    }

                    var elemType = GetTypeRef (newArrayExpr.ElementType);
                    var arrType = EnvironmentBuilder!.CreateArrayType (elemType, newArrayExpr.Ranks.Length);

                    return new ExpressionData { Expr = expr, Type = arrType, Constant = false, Addressable = false };
                }

                #region Literals

                case ES_AstIntegerLiteralExpression intLitExpr:
                    FoldConstants_IntLiteral (ref expr, expectedType, false);
                    return new ExpressionData { Expr = expr, Type = (expr as ES_AstIntegerConstantExpression)!.IntType };

                case ES_AstBooleanLiteralExpression boolLitExpr:
                    expr = new ES_AstBooleanConstantExpression (boolLitExpr.Value, boolLitExpr);
                    return new ExpressionData { Expr = expr, Type = Environment.TypeBool };

                case ES_AstFloatLiteralExpression floatLitExpr: {
                    ES_TypeInfo* type;

                    if (floatLitExpr.IsFloat) {
                        expr = new ES_AstFloat32ConstantExpression (floatLitExpr.ValueFloat, floatLitExpr);
                        type = Environment.TypeFloat32;
                    } else {
                        expr = new ES_AstFloat64ConstantExpression (floatLitExpr.ValueDouble, floatLitExpr);
                        type = Environment.TypeFloat64;
                    }

                    return new ExpressionData { Expr = expr, Type = type };
                }

                case ES_AstStringLiteralExpression:
                    throw new NotImplementedException ("[TODO] String literals not implemented yet.");

                case ES_AstCharLiteralExpression:
                    throw new NotImplementedException ("[TODO] Char literals not implemented yet.");

                #endregion

                #region Constants

                case ES_AstIntegerConstantExpression intConstExpr:
                    Debug.Assert (intConstExpr.IntType->TypeTag == ES_TypeTag.Int);
                    return new ExpressionData { Expr = expr, Type = intConstExpr.IntType, Constant = true, Addressable = false };

                case ES_AstBooleanConstantExpression:
                    return new ExpressionData { Expr = expr, Type = Environment.TypeBool, Constant = true, Addressable = false };

                case ES_AstFloat32ConstantExpression:
                    return new ExpressionData { Expr = expr, Type = Environment.TypeFloat32, Constant = true, Addressable = false };

                case ES_AstFloat64ConstantExpression:
                    return new ExpressionData { Expr = expr, Type = Environment.TypeFloat64, Constant = true, Addressable = false };

                #endregion

                case ES_AstNameExpression nameExpr: {
                    var id = idPool.GetIdentifier (nameExpr.Value.Text.Span);
                    var symbol = symbols.GetSymbol (id);

                    switch (symbol.Tag) {
                        case FrontendSymbolType.None: {
                            return new ExpressionData { Expr = expr, Type = typeUnkn };
                        }

                        case FrontendSymbolType.Variable:
                            return new ExpressionData { Expr = expr, Type = symbol.MatchVar () };

                        case FrontendSymbolType.Type: {
                            if (expectedType is not null)
                                return new ExpressionData { Expr = expr, Type = typeUnkn };

                            return new ExpressionData { Expr = expr, TypeInfo = symbol.MatchType () };
                        }

                        case FrontendSymbolType.Function: {
                            var func = symbol.MatchFunction ();
                            var type = (ES_TypeInfo*) func->FunctionType;
                            return new ExpressionData { Expr = expr, Type = type, Function = func };
                        }

                        default:
                            throw new NotImplementedException ("Symbol type not implemented.");
                    }
                }

                case ES_AstMemberAccessExpression memberAccessExpr: {
                    Debug.Assert (memberAccessExpr.Member is not null);

                    var parenExpr = FoldConstants_Expression (ref transUnit, symbols, src, ref memberAccessExpr.Parent, null);

                    // [TODO]: Allow constant references to be folded.

                    return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = false, };
                }

                #endregion

                case ES_AstIncDecExpression incDecExpr: {
                    var exprData = FoldConstants_Expression (ref transUnit, symbols, src, ref incDecExpr.Inner, expectedType);

                    return new ExpressionData { Expr = expr, Type = exprData.Type };
                }

                #region Unary expressions

                case ES_AstSimpleUnaryExpression unaryExpr: {
                    if (unaryExpr.ExpressionType == SimpleUnaryExprType.Negative &&
                        unaryExpr.Inner is ES_AstIntegerLiteralExpression innerIntLit) {
                        var newExpr = unaryExpr.Inner;

                        if (FoldConstants_IntLiteral (ref newExpr, expectedType, true))
                            expr = newExpr;
                        else
                            unaryExpr.Inner = newExpr;

                        return new ExpressionData { Expr = expr, Type = (newExpr as ES_AstIntegerConstantExpression)!.IntType };
                    }

                    var innerExpr = FoldConstants_Expression (ref transUnit, symbols, src, ref unaryExpr.Inner, expectedType);

                    if (EnvironmentBuilder!.UnaryOpCompat (innerExpr.Type, unaryExpr.ExpressionType, out _, out _))
                        return new ExpressionData { Expr = expr, Type = typeUnkn };

                    FoldConstants_UnaryExpression (ref expr);

                    return new ExpressionData { Expr = expr, Type = innerExpr.Type };
                }

                case ES_AstCastExpression castExpr: {
                    var destType = GetTypeRef (castExpr.DestinationType);
                    var exprType = FoldConstants_Expression (ref transUnit, symbols, src, ref castExpr.InnerExpression, null);

                    FoldConstants_ExplicitCast (destType, ref expr, out var isRedundant);

                    if (isRedundant) {
                        infoList.Add (new EchelonScriptErrorMessage (
                            src, castExpr.CastBounds, ES_FrontendInfoMsg.RedundantCast
                        ));
                    }

                    return new ExpressionData { Expr = expr, Type = destType };
                }

                #endregion

                case ES_AstSimpleBinaryExpression simpleBinaryExpr: {
                    var expectedRightType = expectedType;

                    var leftType = FoldConstants_Expression (ref transUnit, symbols, src, ref simpleBinaryExpr.Left, expectedType);

                    if (simpleBinaryExpr.ExpressionType.IsBitShift () && leftType.Type->TypeTag == ES_TypeTag.Int) {
                        var intName = ES_PrimitiveTypes.GetIntName (((ES_IntTypeData*) expectedType)->IntSize, true);
                        expectedRightType = Environment.GetFullyQualifiedType (Environment.GlobalTypesNamespace, idPool.GetIdentifier (intName));
                    }

                    var rightType = FoldConstants_Expression (ref transUnit, symbols, src, ref simpleBinaryExpr.Right, expectedRightType);

                    if (leftType.Type is null || rightType.Type is null)
                        return new ExpressionData { Expr = expr, Type = typeUnkn };

                    if (!EnvironmentBuilder!.BinaryOpCompat (leftType.Type, rightType.Type, simpleBinaryExpr.ExpressionType, out var finalType, out _))
                        return new ExpressionData { Expr = expr, Type = typeUnkn };

                    FoldConstants_BinaryExpression (ref expr);

                    return new ExpressionData { Expr = expr, Type = finalType };
                }

                case ES_AstConditionalExpression condExpr: {
                    var condType = FoldConstants_Expression (ref transUnit, symbols, src, ref condExpr.Condition, Environment.TypeBool);

                    FoldConstants_EnsureCompat (Environment.TypeBool, ref condExpr.Condition);

                    var leftExpr = FoldConstants_Expression (ref transUnit, symbols, src, ref condExpr.Then, expectedType);
                    var rightExpr = FoldConstants_Expression (ref transUnit, symbols, src, ref condExpr.Else, expectedType);

                    bool isCompat = (
                        FoldConstants_EnsureCompat (expectedType, ref condExpr.Then) &
                        FoldConstants_EnsureCompat (expectedType, ref condExpr.Else)
                    );

                    var finalType = isCompat ? expectedType : typeUnkn;

                    return new ExpressionData { Expr = expr, Type = finalType };
                }

                default:
                    throw new NotImplementedException ("Expression type not implemented.");
            }
        }

        protected ExpressionData FoldConstants_Expression_FunctionCall (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src,
            ES_AstFunctionCallExpression funcCallExpr, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (Environment is not null);
            var typeUnkn = Environment.TypeUnknownValue;

            var funcExpr = FoldConstants_Expression (ref transUnit, symbols, src, ref funcCallExpr.FunctionExpression, typeUnkn);
            ES_FunctionData* func = null;
            ES_FunctionPrototypeData* funcType = null;

            if (funcExpr.Function is not null) {
                func = funcExpr.Function;
                funcType = func->FunctionType;
            } else {
                if (funcExpr.TypeInfo is not null)
                    return new ExpressionData { Expr = funcCallExpr, Type = typeUnkn };
                else if (funcExpr.Type is not null)
                    return new ExpressionData { Expr = funcCallExpr, Type = typeUnkn };
                else
                    Debug.Fail ("???");
            }

            int funcArgCount = funcType->ArgumentsList.Length;
            int callArgCount = funcCallExpr.Arguments.Length;

            for (int argIdx = 0; argIdx < callArgCount; argIdx++) {
                var arg = funcCallExpr.Arguments [argIdx];
                ES_FunctionArgData* argData = null;
                ES_FunctionPrototypeArgData* argTypeData = null;

                if (argIdx < funcArgCount) {
                    argData = func->Arguments.Elements + argIdx;
                    argTypeData = funcType->ArgumentsList.Elements + argIdx;
                }

                ES_TypeInfo* argValType = typeUnkn;
                if (argTypeData is not null)
                    argValType = argTypeData->ValueType;

                FoldConstants_Expression (ref transUnit, symbols, src, ref arg.ValueExpression, argValType);
            }

            return new ExpressionData { Expr = funcCallExpr, Type = funcType->ReturnType };
        }
    }
}