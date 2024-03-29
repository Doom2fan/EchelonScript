﻿/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.Frontend.Data;

namespace EchelonScriptCompiler.Frontend;

internal unsafe static partial class Compiler_ConstantFolding {
    private static void FoldConstants_ExplicitCast_ToFloat (ref ES_AstExpression expr, ESC_TypeRef dstType, in ES_AstExpression innerExpr, out bool isRedundant) {
        var dstFloatType = dstType.Type as ESC_TypeFloat;
        Debug.Assert (dstFloatType is not null);

        if (innerExpr is ES_AstIntegerConstantExpression intExpr) {
            var srcIntType = intExpr.IntType.Type as ESC_TypeInt;
            Debug.Assert (srcIntType is not null);

            isRedundant = false;
            if (srcIntType.Unsigned) {
                switch (dstFloatType.Size) {
                    case ES_FloatSize.Single:
                        expr = new ES_AstFloat32ConstantExpression ((long) intExpr.SignExtend (), expr);
                        break;

                    case ES_FloatSize.Double:
                        expr = new ES_AstFloat64ConstantExpression ((long) intExpr.SignExtend (), expr);
                        break;
                }
            } else {
                switch (dstFloatType.Size) {
                    case ES_FloatSize.Single:
                        expr = new ES_AstFloat32ConstantExpression (intExpr.Value, expr);
                        break;

                    case ES_FloatSize.Double:
                        expr = new ES_AstFloat64ConstantExpression (intExpr.Value, expr);
                        break;
                }
            }
        } else if (innerExpr is ES_AstFloat32ConstantExpression f32Expr) {
            isRedundant = dstFloatType.Size == ES_FloatSize.Single;

            switch (dstFloatType.Size) {
                case ES_FloatSize.Single:
                    expr = new ES_AstFloat32ConstantExpression (f32Expr.Value, expr);
                    break;

                case ES_FloatSize.Double:
                    expr = new ES_AstFloat64ConstantExpression (f32Expr.Value, expr);
                    break;
            }
        } else if (innerExpr is ES_AstFloat64ConstantExpression f64Expr) {
            isRedundant = dstFloatType.Size == ES_FloatSize.Double;

            switch (dstFloatType.Size) {
                case ES_FloatSize.Single:
                    expr = new ES_AstFloat32ConstantExpression ((float) f64Expr.Value, expr);
                    break;

                case ES_FloatSize.Double:
                    expr = new ES_AstFloat64ConstantExpression (f64Expr.Value, expr);
                    break;
            }
        } else
            isRedundant = false;
    }

    /*
     *
     * Single precision ops.
     *
     */
    private static void FoldExpression_Binary_Float32Float32_Comp (
        ref ES_AstExpression expr, SimpleBinaryExprType op,
        ES_AstFloat32ConstantExpression lhs, ES_AstFloat32ConstantExpression rhs
    ) {
        Debug.Assert (op.IsComparison ());

        bool finalValue;
        switch (op) {
            case SimpleBinaryExprType.Equals:
                finalValue = lhs.Value == rhs.Value;
                break;
            case SimpleBinaryExprType.NotEquals:
                finalValue = lhs.Value != rhs.Value;
                break;

            case SimpleBinaryExprType.LesserThan:
                finalValue = lhs.Value > rhs.Value;
                break;
            case SimpleBinaryExprType.LesserThanEqual:
                finalValue = lhs.Value >= rhs.Value;
                break;

            case SimpleBinaryExprType.GreaterThan:
                finalValue = lhs.Value < rhs.Value;
                break;
            case SimpleBinaryExprType.GreaterThanEqual:
                finalValue = lhs.Value <= rhs.Value;
                break;

            default:
                return;
        }

        expr = new ES_AstBooleanConstantExpression (finalValue, expr);
    }

    private static void FoldExpression_Binary_Float32Float32_Arithmetic (
        ref ES_AstExpression expr, SimpleBinaryExprType op,
        ES_AstFloat32ConstantExpression lhs, ES_AstFloat32ConstantExpression rhs
    ) {
        Debug.Assert (!op.IsComparison () && !op.IsBitShift ());

        float finalValue;
        switch (op) {
            // Add/sub
            case SimpleBinaryExprType.Add:
                finalValue = lhs.Value + rhs.Value;
                break;
            case SimpleBinaryExprType.Subtract:
                finalValue = lhs.Value - rhs.Value;
                break;

            // Mul/Div/Mod
            case SimpleBinaryExprType.Multiply:
                finalValue = lhs.Value * rhs.Value;
                break;
            case SimpleBinaryExprType.Divide:
                    finalValue = lhs.Value / rhs.Value;
                break;
            case SimpleBinaryExprType.Modulo:
                    finalValue = lhs.Value % rhs.Value;
                break;

            case SimpleBinaryExprType.Power:
                throw new NotImplementedException ("[TODO] ** is not implemented yet.");

            default:
                return;
        }

        expr = new ES_AstFloat32ConstantExpression (finalValue, expr);
    }

    private static void FoldExpression_Binary_Float32Int_Arithmetic (
        ref ES_AstExpression expr, SimpleBinaryExprType op,
        ES_AstFloat32ConstantExpression lhs, ES_AstIntegerConstantExpression rhs
    ) {
        Debug.Assert (!op.IsComparison () && !op.IsBitShift ());

        //float finalValue;
        switch (op) {
            case SimpleBinaryExprType.Power:
                throw new NotImplementedException ("[TODO] ** is not implemented yet.");

            default:
                return;
        }

        //expr = new ES_AstFloat32ConstantExpression (finalValue, expr);
    }

    /*
     *
     * Double precision ops.
     *
     */
    private static void FoldExpression_Binary_Float64Float64_Comp (
        ref ES_AstExpression expr, SimpleBinaryExprType op,
        ES_AstFloat64ConstantExpression lhs, ES_AstFloat64ConstantExpression rhs
    ) {
        Debug.Assert (op.IsComparison ());

        bool finalValue;
        switch (op) {
            case SimpleBinaryExprType.Equals:
                finalValue = lhs.Value == rhs.Value;
                break;
            case SimpleBinaryExprType.NotEquals:
                finalValue = lhs.Value != rhs.Value;
                break;

            case SimpleBinaryExprType.LesserThan:
                finalValue = lhs.Value > rhs.Value;
                break;
            case SimpleBinaryExprType.LesserThanEqual:
                finalValue = lhs.Value >= rhs.Value;
                break;

            case SimpleBinaryExprType.GreaterThan:
                finalValue = lhs.Value < rhs.Value;
                break;
            case SimpleBinaryExprType.GreaterThanEqual:
                finalValue = lhs.Value <= rhs.Value;
                break;

            default:
                return;
        }

        expr = new ES_AstBooleanConstantExpression (finalValue, expr);
    }

    private static void FoldExpression_Binary_Float64Float64_Arithmetic (
        ref ES_AstExpression expr, SimpleBinaryExprType op,
        ES_AstFloat64ConstantExpression lhs, ES_AstFloat64ConstantExpression rhs
    ) {
        Debug.Assert (!op.IsComparison () && !op.IsBitShift ());

        double finalValue;
        switch (op) {
            // Add/sub
            case SimpleBinaryExprType.Add:
                finalValue = lhs.Value + rhs.Value;
                break;
            case SimpleBinaryExprType.Subtract:
                finalValue = lhs.Value - rhs.Value;
                break;

            // Mul/Div/Mod
            case SimpleBinaryExprType.Multiply:
                finalValue = lhs.Value * rhs.Value;
                break;
            case SimpleBinaryExprType.Divide:
                finalValue = lhs.Value / rhs.Value;
                break;
            case SimpleBinaryExprType.Modulo:
                finalValue = lhs.Value % rhs.Value;
                break;

            case SimpleBinaryExprType.Power:
                throw new NotImplementedException ("[TODO] ** is not implemented yet.");

            default:
                return;
        }

        expr = new ES_AstFloat64ConstantExpression (finalValue, expr);
    }

    private static void FoldExpression_Binary_Float64Int_Arithmetic (
        ref ES_AstExpression expr, SimpleBinaryExprType op,
        ES_AstFloat64ConstantExpression lhs, ES_AstIntegerConstantExpression rhs
    ) {
        Debug.Assert (!op.IsComparison () && !op.IsBitShift ());

        //double finalValue;
        switch (op) {
            case SimpleBinaryExprType.Power:
                throw new NotImplementedException ("[TODO] ** is not implemented yet.");

            default:
                return;
        }

        //expr = new ES_AstFloat64ConstantExpression (finalValue, expr);
    }
}
