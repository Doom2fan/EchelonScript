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
using EchelonScriptCompiler.CompilerCommon;

namespace EchelonScriptCompiler.Frontend {
    internal unsafe static partial class Compiler_ConstantFolding {
        private static bool FoldConstants_EnsureCompat (ES_TypeInfo* dstType, ref ES_AstExpression expr) {
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

        private static void FoldExpression_ExplicitCast (ES_TypeInfo* dstType, ref ES_AstExpression expr, out bool isRedundant) {
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
                    FoldExpression_ExplicitCast_ToInt (dstType, castExpr.InnerExpression, ref expr, out isRedundant);
                    break;
                }

                case ES_TypeTag.Float: {
                    FoldConstants_ExplicitCast_ToFloat (dstType, castExpr.InnerExpression, ref expr, out isRedundant);
                    break;
                }
            }
        }

        private static void FoldExpression_Binary (ref ES_AstExpression expr) {
            var binExpr = expr as ES_AstSimpleBinaryExpression;
            Debug.Assert (binExpr is not null);

            var op = binExpr.ExpressionType;

            if (op.IsAssignment ())
                return;

            if (binExpr.Left is ES_AstBooleanConstantExpression lhsBool && binExpr.Right is ES_AstBooleanConstantExpression rhsBool) {
                if (op.IsComparison ())
                    FoldExpression_Binary_BoolBool_Comp (ref expr, op, lhsBool, rhsBool);
                else
                    FoldExpression_Binary_BoolBool_Arithmetic (ref expr, op, lhsBool, rhsBool);
            }

            if (binExpr.Left is ES_AstIntegerConstantExpression lhsInt && binExpr.Right is ES_AstIntegerConstantExpression rhsInt) {
                if (op.IsComparison ())
                    FoldExpression_Binary_IntInt_Comp (ref expr, op, lhsInt, rhsInt);
                else if (op.IsBitShift ())
                    FoldExpression_Binary_IntInt_BitShifting (ref expr, op, lhsInt, rhsInt);
                else
                    FoldExpression_Binary_IntInt_Arithmetic (ref expr, op, lhsInt, rhsInt);
            }

            if (binExpr.Left is ES_AstFloat32ConstantExpression lhsF32) {
                if (binExpr.Right is ES_AstFloat32ConstantExpression rhsF32) {
                    if (op.IsComparison ())
                        FoldExpression_Binary_Float32Float32_Comp (ref expr, op, lhsF32, rhsF32);
                    else
                        FoldExpression_Binary_Float32Float32_Arithmetic (ref expr, op, lhsF32, rhsF32);
                } else if (binExpr.Right is ES_AstIntegerConstantExpression) {
                    FoldExpression_Binary_Float32Int_Arithmetic (
                        ref expr, op, lhsF32, (binExpr.Right as ES_AstIntegerConstantExpression)!
                    );
                }
            }

            if (binExpr.Left is ES_AstFloat64ConstantExpression lhsF64) {
                if (binExpr.Right is ES_AstFloat64ConstantExpression rhsF64) {
                    if (op.IsComparison ())
                        FoldExpression_Binary_Float64Float64_Comp (ref expr, op, lhsF64, rhsF64);
                    else
                        FoldExpression_Binary_Float64Float64_Arithmetic (ref expr, op, lhsF64, rhsF64);
                } else if (binExpr.Right is ES_AstIntegerConstantExpression) {
                    FoldExpression_Binary_Float64Int_Arithmetic (
                        ref expr, op, lhsF64, (binExpr.Right as ES_AstIntegerConstantExpression)!
                    );
                }
            }

            if (binExpr.Left is ES_AstNullLiteralExpression) {
                if (binExpr.Right is ES_AstNullLiteralExpression) {
                    if (op == SimpleBinaryExprType.Equals || op == SimpleBinaryExprType.NotEquals)
                        expr = new ES_AstBooleanConstantExpression (op != SimpleBinaryExprType.NotEquals, expr);
                }
            }
        }

        private static void FoldExpression_Unary (ref ES_AstExpression expr) {
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

        private static ExpressionData FoldExpression (
            ref PassData passData,
            ref ES_AstExpression expr, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (expr is not null);

            var typeUnkn = passData.Env.TypeUnknownValue;
            var idPool = passData.Env.IdPool;

            switch (expr) {
                case ES_AstParenthesisExpression parenExpr: {
                    var innerExpr = FoldExpression (ref passData, ref parenExpr.Inner, expectedType);

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

                    innerExpr.Expr = expr;
                    return innerExpr;
                }

                #region Primary expressions

                case ES_AstFunctionCallExpression funcCallExpr:
                    return FoldExpression_FunctionCall (ref passData, funcCallExpr, expectedType);

                case ES_AstIndexingExpression indexExpr: {
                    var typeIndex = passData.Env.GetArrayIndexType ();

                    var indexedExpr = FoldExpression (ref passData, ref indexExpr.IndexedExpression, typeUnkn);
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
                        FoldExpression (ref passData, ref rank, typeIndex);
                        FoldConstants_EnsureCompat (typeIndex, ref rank);
                    }

                    return ExpressionData.NewValue (expr, returnType);
                }

                case ES_AstNewObjectExpression newObjExpr: {
                    var type = GetTypeRef (newObjExpr.TypeDeclaration);

                    foreach (var arg in newObjExpr.Arguments)
                        FoldExpression (ref passData, ref arg.ValueExpression, typeUnkn);

                    return ExpressionData.NewValue (expr, type);
                }

                case ES_AstNewArrayExpression newArrayExpr: {
                    var indexType = passData.Env.GetArrayIndexType ();

                    foreach (ref var rank in newArrayExpr.Ranks.AsSpan ()) {
                        Debug.Assert (rank is not null);
                        FoldExpression (ref passData, ref rank, indexType);
                    }

                    var elemType = GetTypeRef (newArrayExpr.ElementType);
                    var arrType = passData.EnvBuilder.CreateArrayType (elemType, newArrayExpr.Ranks.Length);

                    return ExpressionData.NewValue (expr, arrType);
                }

                #region Literals

                case ES_AstIntegerLiteralExpression intLitExpr:
                    FoldExpression_IntLiteral (ref passData, ref expr, expectedType, false);
                    return ExpressionData.NewValue (expr, (expr as ES_AstIntegerConstantExpression)!.IntType);

                case ES_AstBooleanLiteralExpression boolLitExpr:
                    expr = new ES_AstBooleanConstantExpression (boolLitExpr.Value, boolLitExpr);
                    return ExpressionData.NewValue (expr, passData.Env.TypeBool);

                case ES_AstFloatLiteralExpression floatLitExpr: {
                    ES_TypeInfo* type;

                    if (floatLitExpr.IsFloat) {
                        expr = new ES_AstFloat32ConstantExpression (floatLitExpr.ValueFloat, floatLitExpr);
                        type = passData.Env.TypeFloat32;
                    } else {
                        expr = new ES_AstFloat64ConstantExpression (floatLitExpr.ValueDouble, floatLitExpr);
                        type = passData.Env.TypeFloat64;
                    }

                    return ExpressionData.NewValue (expr, type);
                }

                case ES_AstStringLiteralExpression:
                    throw new NotImplementedException ("[TODO] String literals not implemented yet.");

                case ES_AstCharLiteralExpression:
                    throw new NotImplementedException ("[TODO] Char literals not implemented yet.");

                case ES_AstNullLiteralExpression:
                    return ExpressionData.NewValue (expr, passData.Env.TypeNull);

                #endregion

                #region Constants

                case ES_AstIntegerConstantExpression intConstExpr:
                    Debug.Assert (intConstExpr.IntType->TypeTag == ES_TypeTag.Int);
                    return ExpressionData.NewValue (expr, intConstExpr.IntType);

                case ES_AstBooleanConstantExpression:
                    return ExpressionData.NewValue (expr, passData.Env.TypeBool);

                case ES_AstFloat32ConstantExpression:
                    return ExpressionData.NewValue (expr, passData.Env.TypeFloat32);

                case ES_AstFloat64ConstantExpression:
                    return ExpressionData.NewValue (expr, passData.Env.TypeFloat64);

                #endregion

                case ES_AstNameExpression nameExpr: {
                    var id = idPool.GetIdentifier (nameExpr.Value.Text.Span);
                    var symbol = passData.Symbols.GetSymbol (id);

                    switch (symbol.Tag) {
                        case FrontendSymbolType.None:
                            return ExpressionData.NewValue (expr, typeUnkn);

                        case FrontendSymbolType.Variable:
                            return ExpressionData.NewValue (expr, symbol.MatchVar ());

                        case FrontendSymbolType.Type: {
                            if (expectedType is not null)
                                return ExpressionData.NewValue (expr, typeUnkn);

                            return ExpressionData.NewType (expr, symbol.MatchType ());
                        }

                        case FrontendSymbolType.Function: {
                            if (expectedType is not null)
                                return ExpressionData.NewValue (expr, typeUnkn);

                            var func = symbol.MatchFunction ();
                            var type = (ES_TypeInfo*) func->FunctionType;
                            return ExpressionData.NewFunction (expr, func, type);
                        }

                        default:
                            throw new NotImplementedException ("Symbol type not implemented.");
                    }
                }

                case ES_AstMemberAccessExpression memberAccessExpr: {
                    Debug.Assert (memberAccessExpr.Member is not null);

                    var parenExpr = FoldExpression (ref passData, ref memberAccessExpr.Parent, null);

                    // [TODO]: Allow constant references to be folded.

                    return ExpressionData.NewValue (expr, typeUnkn);
                }

                #endregion

                case ES_AstIncDecExpression incDecExpr: {
                    var exprData = FoldExpression (ref passData, ref incDecExpr.Inner, expectedType);
                    return ExpressionData.NewValue (expr, exprData.Type);
                }

                #region Unary expressions

                case ES_AstSimpleUnaryExpression unaryExpr: {
                    if (unaryExpr.ExpressionType == SimpleUnaryExprType.Negative &&
                        unaryExpr.Inner is ES_AstIntegerLiteralExpression innerIntLit) {
                        var newExpr = unaryExpr.Inner;

                        if (FoldExpression_IntLiteral (ref passData, ref newExpr, expectedType, true))
                            expr = newExpr;
                        else
                            unaryExpr.Inner = newExpr;

                        return ExpressionData.NewValue (expr, (newExpr as ES_AstIntegerConstantExpression)!.IntType);
                    }

                    var innerExpr = FoldExpression (ref passData, ref unaryExpr.Inner, expectedType);

                    if (CompilerFrontend.UnaryOpCompat (passData.Env, innerExpr.Type, unaryExpr.ExpressionType, out _, out _))
                        return ExpressionData.NewValue (expr, typeUnkn);

                    FoldExpression_Unary (ref expr);

                    return ExpressionData.NewValue (expr, innerExpr.Type);
                }

                case ES_AstCastExpression castExpr: {
                    var destType = GetTypeRef (castExpr.DestinationType);
                    var exprType = FoldExpression (ref passData, ref castExpr.InnerExpression, null);

                    FoldExpression_ExplicitCast (destType, ref expr, out var isRedundant);

                    if (isRedundant) {
                        passData.InfoList.Add (new (
                            passData.Source, castExpr.CastBounds, ES_FrontendInfoMsg.RedundantCast
                        ));
                    }

                    return ExpressionData.NewValue (expr, destType);
                }

                #endregion

                case ES_AstSimpleBinaryExpression simpleBinaryExpr: {
                    var expectedRightType = expectedType;

                    var leftType = FoldExpression (ref passData, ref simpleBinaryExpr.Left, expectedType);

                    if (simpleBinaryExpr.ExpressionType.IsBitShift () && leftType.Type->TypeTag == ES_TypeTag.Int)
                        expectedRightType = passData.Env.GetIntType (((ES_IntTypeData*) expectedType)->IntSize, true);

                    var rightType = FoldExpression (ref passData, ref simpleBinaryExpr.Right, expectedRightType);

                    if (leftType.Type is null || rightType.Type is null)
                        return ExpressionData.NewValue (expr, typeUnkn);

                    if (!CompilerFrontend.BinaryOpCompat (passData.Env, leftType.Type, rightType.Type, simpleBinaryExpr.ExpressionType, out var finalType, out _))
                        return ExpressionData.NewValue (expr, typeUnkn);

                    FoldExpression_Binary (ref expr);

                    return ExpressionData.NewValue (expr, finalType);
                }

                case ES_AstConditionalExpression condExpr: {
                    var typeBool = passData.Env.TypeBool;
                    var condType = FoldExpression (ref passData, ref condExpr.Condition, typeBool);

                    FoldConstants_EnsureCompat (passData.Env.TypeBool, ref condExpr.Condition);

                    var leftExpr = FoldExpression (ref passData, ref condExpr.Then, expectedType);
                    var rightExpr = FoldExpression (ref passData, ref condExpr.Else, expectedType);

                    bool isCompat = (
                        FoldConstants_EnsureCompat (expectedType, ref condExpr.Then) &
                        FoldConstants_EnsureCompat (expectedType, ref condExpr.Else)
                    );

                    var finalType = isCompat ? expectedType : typeUnkn;

                    return ExpressionData.NewValue (expr, finalType);
                }

                default:
                    throw new NotImplementedException ("Expression type not implemented.");
            }
        }

        private static ExpressionData FoldExpression_FunctionCall (
            ref PassData passData,
            ES_AstFunctionCallExpression funcCallExpr, ES_TypeInfo* expectedType
        ) {
            var typeUnkn = passData.Env.TypeUnknownValue;

            var funcExpr = FoldExpression (ref passData, ref funcCallExpr.FunctionExpression, typeUnkn);
            ES_FunctionData* func = null;
            ES_FunctionPrototypeData* funcType = null;

            if (funcExpr.Function is not null) {
                func = funcExpr.Function;
                funcType = func->FunctionType;
            } else {
                if (funcExpr.TypeInfo is not null)
                    return ExpressionData.NewValue (funcCallExpr, typeUnkn);
                else if (funcExpr.Type is not null)
                    return ExpressionData.NewValue (funcCallExpr, typeUnkn);
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

                var argValType = typeUnkn;
                if (argTypeData is not null)
                    argValType = argTypeData->ValueType;

                FoldExpression (ref passData, ref arg.ValueExpression, argValType);
            }

            return ExpressionData.NewValue (funcCallExpr, funcType->ReturnType);
        }
    }
}
