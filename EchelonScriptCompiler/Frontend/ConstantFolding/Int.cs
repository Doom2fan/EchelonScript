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
    private static ESC_TypeRef DetermineIntLiteralType (
        ref CompileData compileData,
        ES_AstIntegerLiteralExpression intLitExpr, ESC_TypeRef expectedType, bool negated
    ) {
        ESC_TypeInt? expectedIntType = null;

        bool? unsigned = null;
        var isSigned = intLitExpr.Signed;
        var chosenSize = intLitExpr.Size;
        var constness = ESC_Constness.Mutable;

        if (expectedType.Type != null && expectedType.Type is ESC_TypeInt) {
            expectedIntType = (expectedType.Type as ESC_TypeInt)!;
            constness = expectedType.Constness;

            if (isSigned == null)
                isSigned = !expectedIntType.Unsigned;
        }

        ES_IntSize size;
        ES_IntSize minSize;

        var tooBig = false;

        var value = intLitExpr.Value;
        if (isSigned == false || intLitExpr.HexBin) {
            if (value <= byte.MaxValue)
                minSize = ES_IntSize.Int8;
            else if (value <= ushort.MaxValue)
                minSize = ES_IntSize.Int16;
            else if (value <= uint.MaxValue)
                minSize = ES_IntSize.Int32;
            else
                minSize = ES_IntSize.Int64;
        } else if (!negated) {
            if (value <= (ulong) sbyte.MaxValue)
                minSize = ES_IntSize.Int8;
            else if (value <= (ulong) short.MaxValue)
                minSize = ES_IntSize.Int16;
            else if (value <= int.MaxValue)
                minSize = ES_IntSize.Int32;
            else if (value <= long.MaxValue)
                minSize = ES_IntSize.Int64;
            else {
                minSize = ES_IntSize.Int64;
                unsigned = true;

                if (isSigned == true)
                    tooBig = true;
            }
        } else {
            unsigned = false;

            if (value <= ((ulong) sbyte.MaxValue) + 1)
                minSize = ES_IntSize.Int8;
            else if (value <= ((ulong) short.MaxValue) + 1)
                minSize = ES_IntSize.Int16;
            else if (value <= ((ulong) int.MaxValue) + 1)
                minSize = ES_IntSize.Int32;
            else if (value <= ((ulong) long.MaxValue) + 1)
                minSize = ES_IntSize.Int64;
            else {
                minSize = ES_IntSize.Int64;
                unsigned = true;

                if (isSigned == true)
                    tooBig = true;
            }
        }

        tooBig |= (
            (chosenSize is not null && chosenSize.Value < minSize) ||
            (unsigned == true && isSigned == true)
        );

        if (tooBig) {
            var errSize = minSize;
            if (chosenSize is not null)
                errSize = chosenSize.Value;
            compileData.ErrorList.Add (ES_FrontendErrors.GenIntLitTooBig (isSigned!.Value, errSize, intLitExpr.Token));

            if (unsigned is null && isSigned == false)
                unsigned = true;
        } else {
            if (chosenSize is not null)
                minSize = chosenSize.Value;

            if (unsigned is null && isSigned is not null)
                unsigned = !isSigned.Value;
        }

        size = minSize;
        if (expectedIntType is not null) {
            var isCompat = false;

            var expectsUnsign = expectedIntType.Unsigned;
            var expectedSize = expectedIntType.Size;

            if (intLitExpr.HexBin && (isSigned is null || isSigned == !expectsUnsign) && minSize <= expectedSize)
                isCompat = true;
            else if (unsigned == expectsUnsign && minSize <= expectedSize)
                isCompat = true;

            if (isCompat) {
                size = expectedSize;
                unsigned = expectsUnsign;
            }
        } else if (chosenSize is null && size < ES_IntSize.Int32)
            size = ES_IntSize.Int32;

        // This means we got here without encountering any sign. Assume signed.
        if (unsigned is null)
            unsigned = false;

        var intType = compileData.GetIntType (size, unsigned.Value, constness);

        Debug.Assert (intType.Type is ESC_TypeInt);

        return intType;
    }

    private static bool FoldExpression_IntLiteral (
        ref CompileData compileData,
        ref ES_AstExpression expr, ESC_TypeRef expectedType, bool negated
    ) {
        var intLitExpr = expr as ES_AstIntegerLiteralExpression;
        Debug.Assert (intLitExpr is not null);

        var intType = DetermineIntLiteralType (ref compileData, intLitExpr, expectedType, negated);
        var intTypeData = (ESC_TypeInt?) intType.Type;
        Debug.Assert (intTypeData is not null);

        if (!negated) {
            expr = new ES_AstIntegerConstantExpression (intType, intLitExpr.Value, intLitExpr);
            return true;
        } else {
            if (!intTypeData.Unsigned) {
                var val = -(long) intLitExpr.Value;

                if (intLitExpr.Value == (ulong) (long.MaxValue) + 1)
                    val = long.MinValue;

                expr = new ES_AstIntegerConstantExpression (intType, (ulong) val, intLitExpr);
                return true;
            } else {
                expr = new ES_AstIntegerConstantExpression (intType, intLitExpr.Value, intLitExpr);
                return false;
            }
        }
    }

    private static void FoldExpression_ExplicitCast_ToInt (
        ref ES_AstExpression expr, ESC_TypeRef dstType, in ES_AstExpression innerExpr, out bool isRedundant
    ) {
        var dstIntType = dstType.Type as ESC_TypeInt;
        Debug.Assert (dstIntType is not null);

        if (innerExpr is ES_AstIntegerConstantExpression intExpr) {
            var constIntType = intExpr.IntType.Type as ESC_TypeInt;
            Debug.Assert (constIntType is not null);

            ulong val;
            if (!constIntType.Unsigned)
                val = intExpr.SignExtend ();
            else
                val = intExpr.Value;

            isRedundant = (
                constIntType.Unsigned == dstIntType.Unsigned &&
                constIntType.Size == dstIntType.Size
            );

            expr = new ES_AstIntegerConstantExpression (dstType, val, expr);
        } else if (innerExpr is ES_AstFloat32ConstantExpression f32Expr) {
            isRedundant = false;

            var unsigned = dstIntType.Unsigned;
            ulong val;
            if (!unsigned) {
                val = dstIntType.Size switch {
                    ES_IntSize.Int8 => (ulong) (sbyte) f32Expr.Value,
                    ES_IntSize.Int16 => (ulong) (short) f32Expr.Value,
                    ES_IntSize.Int32 => (ulong) (int) f32Expr.Value,
                    ES_IntSize.Int64 => (ulong) (long) f32Expr.Value,

                    _ => throw new NotImplementedException ("Int size not implemented yet."),
                };
            } else {
                val = dstIntType.Size switch {
                    ES_IntSize.Int8 => (byte) f32Expr.Value,
                    ES_IntSize.Int16 => (ushort) f32Expr.Value,
                    ES_IntSize.Int32 => (uint) f32Expr.Value,
                    ES_IntSize.Int64 => (ulong) f32Expr.Value,

                    _ => throw new NotImplementedException ("Int size not implemented yet."),
                };
            }

            expr = new ES_AstIntegerConstantExpression (dstType, val, expr);
        } else if (innerExpr is ES_AstFloat64ConstantExpression f64Expr) {
            isRedundant = false;

            var unsigned = dstIntType.Unsigned;
            ulong val;
            if (!unsigned) {
                val = dstIntType.Size switch {
                    ES_IntSize.Int8 => (ulong) (sbyte) f64Expr.Value,
                    ES_IntSize.Int16 => (ulong) (short) f64Expr.Value,
                    ES_IntSize.Int32 => (ulong) (int) f64Expr.Value,
                    ES_IntSize.Int64 => (ulong) (long) f64Expr.Value,

                    _ => throw new NotImplementedException ("Int size not implemented yet."),
                };
            } else {
                val = dstIntType.Size switch {
                    ES_IntSize.Int8 => (byte) f64Expr.Value,
                    ES_IntSize.Int16 => (ushort) f64Expr.Value,
                    ES_IntSize.Int32 => (uint) f64Expr.Value,
                    ES_IntSize.Int64 => (ulong) f64Expr.Value,

                    _ => throw new NotImplementedException ("Int size not implemented yet."),
                };
            }

            expr = new ES_AstIntegerConstantExpression (dstType, val, expr);
        } else
            isRedundant = false;
    }

    private static void FoldExpression_Binary_IntInt_Comp (
        ref ES_AstExpression expr, SimpleBinaryExprType op,
        ES_AstIntegerConstantExpression lhs, ES_AstIntegerConstantExpression rhs
    ) {
        Debug.Assert (lhs.IntType.Type is ESC_TypeInt);
        Debug.Assert (rhs.IntType.Type is ESC_TypeInt);
        Debug.Assert (op.IsComparison ());

        var lhsType = (ESC_TypeInt) lhs.IntType.Type;
        var rhsType = (ESC_TypeInt) rhs.IntType.Type;

        if (op == SimpleBinaryExprType.Equals)
            expr = new ES_AstBooleanConstantExpression (lhs.Value == rhs.Value, expr);
        else if (op == SimpleBinaryExprType.NotEquals)
            expr = new ES_AstBooleanConstantExpression (lhs.Value != rhs.Value, expr);

        bool finalValue;

        if (!lhsType.Unsigned) {
            var lhsVal = (long) lhs.SignExtend ();
            var rhsVal = (long) rhs.SignExtend ();

            switch (op) {
                case SimpleBinaryExprType.LesserThan:
                    finalValue = lhsVal > rhsVal;
                    break;
                case SimpleBinaryExprType.LesserThanEqual:
                    finalValue = lhsVal >= rhsVal;
                    break;

                case SimpleBinaryExprType.GreaterThan:
                    finalValue = lhsVal < rhsVal;
                    break;
                case SimpleBinaryExprType.GreaterThanEqual:
                    finalValue = lhsVal <= rhsVal;
                    break;

                default:
                    return;
            }
        } else {
            switch (op) {
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
        }

        expr = new ES_AstBooleanConstantExpression (finalValue, expr);
    }

    private static void FoldExpression_Binary_IntInt_BitShifting (
        ref ES_AstExpression expr, SimpleBinaryExprType op,
        ES_AstIntegerConstantExpression lhs, ES_AstIntegerConstantExpression rhs
    ) {
        Debug.Assert (lhs.IntType.Type is ESC_TypeInt);
        Debug.Assert (rhs.IntType.Type is ESC_TypeInt);
        Debug.Assert (op.IsBitShift ());

        var lhsType = (ESC_TypeInt) lhs.IntType.Type;
        var rhsType = (ESC_TypeInt) rhs.IntType.Type;

        var unsigned = lhsType.Unsigned;

        ulong finalValue;

        switch (lhsType.Size) {
            case ES_IntSize.Int8: {
                var lhsValU = (byte) lhs.Value;
                var rhsValU = (byte) rhs.Value;

                var lhsValS = (sbyte) lhs.Value;
                var rhsValS = (sbyte) rhs.Value;

                switch (op) {
                    case SimpleBinaryExprType.ShiftLeft:
                        if (!unsigned)
                            finalValue = (ulong) (lhsValS << rhsValS);
                        else
                            finalValue = (ulong) (lhsValU << rhsValU);
                        break;

                    case SimpleBinaryExprType.ShiftRight:
                        if (!unsigned)
                            finalValue = (ulong) (lhsValS >> rhsValS);
                        else
                            finalValue = (ulong) (lhsValU >> rhsValU);
                        break;

                    case SimpleBinaryExprType.ShiftRightUnsigned:
                        finalValue = (ulong) (lhsValU >> rhsValU);
                        break;

                    default:
                        return;
                }

                break;
            }

            case ES_IntSize.Int16: {
                var lhsValU = (ushort) lhs.Value;
                var rhsValU = (ushort) rhs.Value;

                var lhsValS = (short) lhs.Value;
                var rhsValS = (short) rhs.Value;

                switch (op) {
                    case SimpleBinaryExprType.ShiftLeft:
                        if (!unsigned)
                            finalValue = (ulong) (lhsValS << rhsValS);
                        else
                            finalValue = (ulong) (lhsValU << rhsValU);
                        break;

                    case SimpleBinaryExprType.ShiftRight:
                        if (!unsigned)
                            finalValue = (ulong) (lhsValS >> rhsValS);
                        else
                            finalValue = (ulong) (lhsValU >> rhsValU);
                        break;

                    case SimpleBinaryExprType.ShiftRightUnsigned:
                        finalValue = (ulong) (lhsValU >> rhsValU);
                        break;

                    default:
                        return;
                }

                break;
            }

            case ES_IntSize.Int32: {
                var lhsValU = (uint) lhs.Value;
                var rhsValU = (uint) rhs.Value;

                var lhsValS = (int) lhs.Value;
                var rhsValS = (int) rhs.Value;

                switch (op) {
                    case SimpleBinaryExprType.ShiftLeft:
                        if (!unsigned)
                            finalValue = (ulong) (lhsValS << rhsValS);
                        else
                            finalValue = lhsValU << (int) rhsValU;
                        break;

                    case SimpleBinaryExprType.ShiftRight:
                        if (!unsigned)
                            finalValue = (ulong) (lhsValS >> rhsValS);
                        else
                            finalValue = lhsValU >> (int) rhsValU;
                        break;

                    case SimpleBinaryExprType.ShiftRightUnsigned:
                        finalValue = lhsValU >> (int) rhsValU;
                        break;

                    default:
                        return;
                }

                break;
            }

            case ES_IntSize.Int64: {
                var lhsValU = lhs.Value;
                var rhsValU = rhs.Value;

                var lhsValS = (long) lhs.Value;
                var rhsValS = (long) rhs.Value;

                switch (op) {
                    case SimpleBinaryExprType.ShiftLeft:
                        if (!unsigned)
                            finalValue = (ulong) (lhsValS << (int) rhsValS);
                        else
                            finalValue = lhsValU << (int) rhsValU;
                        break;

                    case SimpleBinaryExprType.ShiftRight:
                        if (!unsigned)
                            finalValue = (ulong) (lhsValS >> (int) rhsValS);
                        else
                            finalValue = lhsValU >> (int) rhsValU;
                        break;

                    case SimpleBinaryExprType.ShiftRightUnsigned:
                        finalValue = lhsValU >> (int) rhsValU;
                        break;

                    default:
                        return;
                }

                break;
            }

            default:
                throw new NotImplementedException ("Size not implemented.");
        }

        expr = new ES_AstIntegerConstantExpression (new (ESC_Constness.Mutable, lhsType), finalValue, expr);
    }

    private static void FoldExpression_Binary_IntInt_Arithmetic (
        ref ES_AstExpression expr, SimpleBinaryExprType op,
        ES_AstIntegerConstantExpression lhs, ES_AstIntegerConstantExpression rhs
    ) {
        Debug.Assert (lhs.IntType.Type is ESC_TypeInt);
        Debug.Assert (rhs.IntType.Type is ESC_TypeInt);
        Debug.Assert (!op.IsComparison () && !op.IsBitShift ());

        var lhsType = (ESC_TypeInt) lhs.IntType.Type;
        var rhsType = (ESC_TypeInt) rhs.IntType.Type;

        if (lhsType != rhsType)
            return;

        ESC_TypeRef finalType;
        ulong finalValue;
        switch (op) {
            // Add/sub
            case SimpleBinaryExprType.Add:
                finalValue = lhs.Value + rhs.Value;
                finalType = lhs.IntType;
                break;
            case SimpleBinaryExprType.Subtract:
                finalValue = lhs.Value - rhs.Value;
                finalType = lhs.IntType;
                break;

            // Mul/Div/Mod
            case SimpleBinaryExprType.Multiply:
                finalValue = lhs.Value * rhs.Value;
                finalType = lhs.IntType;
                break;
            case SimpleBinaryExprType.Divide:
                if (!lhsType.Unsigned)
                    finalValue = (ulong) ((long) lhs.Value / (long) rhs.Value);
                else
                    finalValue = lhs.Value / lhs.Value;
                finalType = lhs.IntType;
                break;
            case SimpleBinaryExprType.Modulo:
                if (!lhsType.Unsigned)
                    finalValue = (ulong) ((long) lhs.Value % (long) rhs.Value);
                else
                    finalValue = lhs.Value % rhs.Value;
                finalType = lhs.IntType;
                break;

            case SimpleBinaryExprType.Power:
                throw new NotImplementedException ("[TODO] ** is not implemented yet.");

            // Bit ops
            case SimpleBinaryExprType.BitAnd:
                finalValue = lhs.Value & rhs.Value;
                finalType = lhs.IntType;
                break;
            case SimpleBinaryExprType.BitOr:
                finalValue = lhs.Value | rhs.Value;
                finalType = lhs.IntType;
                break;
            case SimpleBinaryExprType.BitXor:
                finalValue = lhs.Value ^ rhs.Value;
                finalType = lhs.IntType;
                break;

            default:
                return;
        }

        expr = new ES_AstIntegerConstantExpression (finalType, finalValue, expr);
    }
}
