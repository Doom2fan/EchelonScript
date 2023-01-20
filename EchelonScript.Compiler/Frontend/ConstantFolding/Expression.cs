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
using EchelonScript.Common.Data.Types;

namespace EchelonScript.Compiler.Frontend;

internal unsafe static partial class Compiler_ConstantFolding {
#if false
    private static bool FoldConstants_EnsureCompat (ESC_TypeRef dstType, ref ES_AstExpression expr) {
        if (dstType.Type is null)
            return false;

        switch (expr) {
            case ES_AstFloat32ConstantExpression exprFlt32: {
                if (dstType.Type is not ESC_TypeFloat)
                    return false;

                var dstTypeFlt = (dstType.Type as ESC_TypeFloat)!;
                if (dstTypeFlt.Size < ES_FloatSize.Single)
                    return false;

                switch (dstTypeFlt.Size) {
                    case ES_FloatSize.Single:
                        return true;

                    case ES_FloatSize.Double:
                        expr = new ES_AstFloat64ConstantExpression (exprFlt32.Value, expr);
                        return true;

                    default:
                        throw new NotImplementedException ("Size not implemented.");
                }
            }

            case ES_AstFloat64ConstantExpression: {
                if (dstType.Type is not ESC_TypeFloat)
                    return false;

                var dstTypeFlt = (dstType.Type as ESC_TypeFloat)!;
                if (dstTypeFlt.Size < ES_FloatSize.Double)
                    return false;

                return dstTypeFlt.Size switch {
                    ES_FloatSize.Double => true,

                    _ => throw new NotImplementedException ("Size not implemented."),
                };
            }

            case ES_AstIntegerConstantExpression intExpr: {
                if (dstType.Type is not ESC_TypeInt)
                    return false;

                var intSrcType = (intExpr.IntType.Type as ESC_TypeInt)!;
                var intDstType = (dstType.Type as ESC_TypeInt)!;

                if (intSrcType.Unsigned != intDstType.Unsigned)
                    return false;

                var srcSize = intSrcType.Size;
                var dstSize = intDstType.Size;

                if (srcSize > dstSize)
                    return false;

                if (srcSize == dstSize)
                    return true;

                var val = intExpr.SignExtend ();
                expr = new ES_AstIntegerConstantExpression (dstType, val, expr);
                return true;
            }

            case ES_AstBooleanConstantExpression:
                return dstType.Type is ESC_TypeBool;

            default:
                return false;
        }
    }

    private static void FoldExpression_ExplicitCast (ESC_TypeRef dstType, ref ES_AstExpression expr, out bool isRedundant) {
        Debug.Assert (dstType.Type is not null);

        var castExpr = expr as ES_AstCastExpression;
        Debug.Assert (castExpr is not null);

        if (FoldConstants_EnsureCompat (dstType, ref expr)) {
            isRedundant = true;
            return;
        }

        isRedundant = false;
        switch (dstType.Type) {
            case ESC_TypeInt: {
                FoldExpression_ExplicitCast_ToInt (ref expr, dstType, castExpr.InnerExpression, out isRedundant);
                break;
            }

            case ESC_TypeFloat: {
                FoldConstants_ExplicitCast_ToFloat (ref expr, dstType, castExpr.InnerExpression, out isRedundant);
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
                var innerIntType = (ESC_TypeInt?) intConstExpr.IntType.Type;
                Debug.Assert (innerIntType is not null);

                if (unaryExpr.ExpressionType == SimpleUnaryExprType.Positive)
                    expr = new ES_AstIntegerConstantExpression (intConstExpr.IntType, intConstExpr.Value, expr);
                else if (unaryExpr.ExpressionType == SimpleUnaryExprType.Negative && !innerIntType.Unsigned) {
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
        ref CompileData compileData, ref PassData passData,
        ref ES_AstExpression expr, ESC_TypeRef expectedType
    ) {
        Debug.Assert (expr is not null);

        var typeUnkn = compileData.GetUnknownType (ESC_Constness.Mutable);
        var idPool = compileData.IdPool;

        switch (expr) {
            case ES_AstParenthesisExpression parenExpr: {
                var innerExpr = FoldExpression (ref compileData, ref passData, ref parenExpr.Inner, expectedType);

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
                return FoldExpression_FunctionCall (ref compileData, ref passData, funcCallExpr, expectedType);

            case ES_AstIndexingExpression indexExpr: {
                var typeIndex = compileData.GetArrayIndexType ();

                var indexedExpr = FoldExpression (ref compileData, ref passData, ref indexExpr.IndexedExpression, typeUnkn);
                var returnType = typeUnkn;

                if (indexedExpr.Type.Type is not null) {
                    var indexedType = indexedExpr.Type;

                    if (indexedType.Type is ESC_TypeArray typeArray)
                        returnType = typeArray.ElementType.WithInheritedConst (indexedType.Constness);
                }

                foreach (ref var dim in indexExpr.DimensionExpressions.AsSpan ()) {
                    Debug.Assert (dim is not null);
                    FoldExpression (ref compileData, ref passData, ref dim, typeIndex);
                    FoldConstants_EnsureCompat (typeIndex, ref dim);
                }

                return ExpressionData.NewValue (expr, returnType);
            }

            case ES_AstNewObjectExpression newObjExpr: {
                var type = GetTypeRef (newObjExpr.TypeDeclaration);

                foreach (var arg in newObjExpr.Arguments)
                    FoldExpression (ref compileData, ref passData, ref arg.ValueExpression, typeUnkn);

                return ExpressionData.NewValue (expr, type);
            }

            case ES_AstNewArrayExpression newArrayExpr: {
                var indexType = compileData.GetArrayIndexType ();

                foreach (ref var dim in newArrayExpr.Dimensions.AsSpan ()) {
                    Debug.Assert (dim is not null);
                    FoldExpression (ref compileData, ref passData, ref dim, indexType);
                }

                var elemType = GetTypeRef (newArrayExpr.ElementType);
                var arrType = compileData.GetArrayType (elemType, newArrayExpr.Dimensions.Length, ESC_Constness.Mutable);

                return ExpressionData.NewValue (expr, arrType);
            }

#region Literals

            case ES_AstIntegerLiteralExpression intLitExpr:
                FoldExpression_IntLiteral (ref compileData, ref expr, expectedType, false);
                return ExpressionData.NewValue (expr, (expr as ES_AstIntegerConstantExpression)!.IntType);

            case ES_AstBooleanLiteralExpression boolLitExpr:
                expr = new ES_AstBooleanConstantExpression (boolLitExpr.Value, boolLitExpr);
                return ExpressionData.NewValue (expr, compileData.GetBoolType (ESC_Constness.Mutable));

            case ES_AstFloatLiteralExpression floatLitExpr: {
                ESC_TypeRef type;

                if (floatLitExpr.IsFloat) {
                    expr = new ES_AstFloat32ConstantExpression (floatLitExpr.ValueFloat, floatLitExpr);
                    type = compileData.GetFloat32Type (ESC_Constness.Mutable);
                } else {
                    expr = new ES_AstFloat64ConstantExpression (floatLitExpr.ValueDouble, floatLitExpr);
                    type = compileData.GetFloat64Type (ESC_Constness.Mutable);
                }

                return ExpressionData.NewValue (expr, type);
            }

            case ES_AstStringLiteralExpression:
                throw new NotImplementedException ("[TODO] String literals not implemented yet.");

            case ES_AstCharLiteralExpression:
                throw new NotImplementedException ("[TODO] Char literals not implemented yet.");

            case ES_AstNullLiteralExpression:
                return ExpressionData.NewValue (expr, compileData.GetNullType (ESC_Constness.Mutable));

#endregion

#region Constants

            case ES_AstIntegerConstantExpression intConstExpr:
                Debug.Assert (intConstExpr.IntType.Type is ESC_TypeInt);
                return ExpressionData.NewValue (expr, intConstExpr.IntType);

            case ES_AstBooleanConstantExpression:
                return ExpressionData.NewValue (expr, compileData.GetBoolType (ESC_Constness.Mutable));

            case ES_AstFloat32ConstantExpression:
                return ExpressionData.NewValue (expr, compileData.GetFloat32Type (ESC_Constness.Mutable));

            case ES_AstFloat64ConstantExpression:
                return ExpressionData.NewValue (expr, compileData.GetFloat64Type (ESC_Constness.Mutable));

#endregion

            case ES_AstNameExpression nameExpr: {
                var id = idPool.GetIdentifier (nameExpr.Value.Text.Span);
                var symbol = compileData.Symbols.GetSymbol (id);

                switch (symbol.Tag) {
                    case FrontendSymbolType.None:
                        return ExpressionData.NewValue (expr, typeUnkn);

                    case FrontendSymbolType.Variable:
                        return ExpressionData.NewValue (expr, symbol.MatchVar ().Type);

                    case FrontendSymbolType.Type: {
                        if (expectedType.Type is not null)
                            return ExpressionData.NewValue (expr, typeUnkn);

                        return ExpressionData.NewType (expr, symbol.MatchType ());
                    }

                    case FrontendSymbolType.Function: {
                        if (expectedType.Type is not null)
                            return ExpressionData.NewValue (expr, typeUnkn);

                        var func = symbol.MatchFunction ();
                        var type = func.Prototype;
                        return ExpressionData.NewFunction (expr, func, new (ESC_Constness.Const, type));
                    }

                    default:
                        throw new NotImplementedException ("Symbol type not implemented.");
                }
            }

            case ES_AstMemberAccessExpression memberAccessExpr: {
                Debug.Assert (memberAccessExpr.Member is not null);

                var parenExpr = FoldExpression (ref compileData, ref passData, ref memberAccessExpr.Parent, ESC_TypeRef.Null ());

                // [TODO]: Allow constant references to be folded.

                return ExpressionData.NewValue (expr, typeUnkn);
            }

#endregion

            case ES_AstIncDecExpression incDecExpr: {
                var exprData = FoldExpression (ref compileData, ref passData, ref incDecExpr.Inner, expectedType);
                return ExpressionData.NewValue (expr, exprData.Type);
            }

#region Unary expressions

            case ES_AstSimpleUnaryExpression unaryExpr: {
                if (unaryExpr.ExpressionType == SimpleUnaryExprType.Negative &&
                    unaryExpr.Inner is ES_AstIntegerLiteralExpression) {
                    var newExpr = unaryExpr.Inner;

                    if (FoldExpression_IntLiteral (ref compileData, ref newExpr, expectedType, true))
                        expr = newExpr;
                    else
                        unaryExpr.Inner = newExpr;

                    return ExpressionData.NewValue (expr, (newExpr as ES_AstIntegerConstantExpression)!.IntType);
                }

                var innerExpr = FoldExpression (ref compileData, ref passData, ref unaryExpr.Inner, expectedType);

                if (compileData.UnaryOpCompat (innerExpr.Type, unaryExpr.ExpressionType, out _, out _))
                    return ExpressionData.NewValue (expr, typeUnkn);

                FoldExpression_Unary (ref expr);

                return ExpressionData.NewValue (expr, innerExpr.Type);
            }

            case ES_AstCastExpression castExpr: {
                var destType = GetTypeRef (castExpr.DestinationType);
                var exprType = FoldExpression (ref compileData, ref passData, ref castExpr.InnerExpression, ESC_TypeRef.Null ());

                FoldExpression_ExplicitCast (destType, ref expr, out var isRedundant);

                if (isRedundant) {
                    compileData.InfoList.Add (new (
                        passData.Source, castExpr.CastBounds, ES_FrontendInfoMsg.RedundantCast
                    ));
                }

                return ExpressionData.NewValue (expr, destType);
            }

#endregion

            case ES_AstSimpleBinaryExpression simpleBinaryExpr: {
                var expectedRightType = expectedType;

                var leftExpr = FoldExpression (ref compileData, ref passData, ref simpleBinaryExpr.Left, expectedType);
                if (simpleBinaryExpr.ExpressionType.IsBitShift () && leftExpr.Type.Type is ESC_TypeInt)
                    expectedRightType = compileData.GetIntType (ES_IntSize.Int32, false, ESC_Constness.Const);
                else if (simpleBinaryExpr.ExpressionType.IsAssignment ())
                    expectedRightType = leftExpr.Type.Type is not null ? leftExpr.Type : typeUnkn;

                var rightExpr = FoldExpression (ref compileData, ref passData, ref simpleBinaryExpr.Right, expectedRightType);

                if (leftExpr.Type.Type is null || rightExpr.Type.Type is null)
                    return ExpressionData.NewValue (expr, typeUnkn);

                if (!compileData.BinaryOpCompat (leftExpr.Type, rightExpr.Type, simpleBinaryExpr.ExpressionType, out var finalType, out _))
                    return ExpressionData.NewValue (expr, typeUnkn);

                FoldExpression_Binary (ref expr);

                return ExpressionData.NewValue (expr, finalType);
            }

            case ES_AstConditionalExpression condExpr: {
                var typeBool = compileData.GetBoolType (ESC_Constness.Const);
                var condType = FoldExpression (ref compileData, ref passData, ref condExpr.Condition, typeBool);

                FoldConstants_EnsureCompat (typeBool, ref condExpr.Condition);

                var leftExpr = FoldExpression (ref compileData, ref passData, ref condExpr.Then, expectedType);
                var rightExpr = FoldExpression (ref compileData, ref passData, ref condExpr.Else, expectedType);

                var isCompat = (
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
        ref CompileData compileData, ref PassData passData,
        ES_AstFunctionCallExpression funcCallExpr, ESC_TypeRef expectedType
    ) {
        var typeUnkn = compileData.GetUnknownType (ESC_Constness.Mutable);

        var funcExpr = FoldExpression (ref compileData, ref passData, ref funcCallExpr.FunctionExpression, typeUnkn);
        ESC_TypePrototype? funcType = null;

        if (funcExpr.Function is not null) {
            var func = funcExpr.Function;
            funcType = func.Prototype;
        } else {
            if (funcExpr.TypeInfo.Type is not null)
                return ExpressionData.NewValue (funcCallExpr, typeUnkn);
            else if (funcExpr.Type.Type is not null)
                return ExpressionData.NewValue (funcCallExpr, typeUnkn);
            else
                Debug.Fail ("???");
        }

        var funcArgCount = funcType.Arguments.Length;
        var callArgCount = funcCallExpr.Arguments.Length;

        for (var argIdx = 0; argIdx < callArgCount; argIdx++) {
            var arg = funcCallExpr.Arguments [argIdx];
            ESC_PrototypeArg? argTypeData = null;

            if (argIdx < funcArgCount)
                argTypeData = funcType.Arguments [argIdx];

            var argValType = typeUnkn;
            if (argTypeData is not null)
                argValType = argTypeData.Value.ValueType;

            FoldExpression (ref compileData, ref passData, ref arg.ValueExpression, argValType);
        }

        return ExpressionData.NewValue (funcCallExpr, funcType.ReturnType);
    }
#endif
}
