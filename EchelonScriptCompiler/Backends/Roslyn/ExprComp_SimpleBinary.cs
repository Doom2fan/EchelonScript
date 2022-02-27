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
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon.IR;
using Microsoft.CodeAnalysis.CSharp;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
    private static ExpressionData CompileExpression_SimpleBinary (
        ref PassData passData,
        ref FunctionData funcData,
        ESIR_SimpleBinaryExpression expr
    ) {
        var lhsExpr = CompileExpression (ref passData, ref funcData, expr.ExprLeft);
        var rhsExpr = CompileExpression (ref passData, ref funcData, expr.ExprRight);

        StripFirstConst (ref lhsExpr);
        StripFirstConst (ref rhsExpr);

        return (lhsExpr.Type->TypeTag, rhsExpr.Type->TypeTag) switch {
            (ES_TypeTag.Bool, ES_TypeTag.Bool) => CompileExpression_BinaryBoolBool (ref passData, expr, ref lhsExpr, ref rhsExpr),

            (ES_TypeTag.Int, ES_TypeTag.Int) => CompileExpression_BinaryIntInt (ref passData, expr, ref lhsExpr, ref rhsExpr),

            (ES_TypeTag.Float, ES_TypeTag.Float) => CompileExpression_BinaryFloatFloat (ref passData, expr, ref lhsExpr, ref rhsExpr),
            (ES_TypeTag.Float, ES_TypeTag.Int) => CompileExpression_BinaryFloatInt (ref passData, expr, ref lhsExpr, ref rhsExpr),

            (ES_TypeTag.Reference, ES_TypeTag.Reference) => CompileExpression_BinaryRefRef (ref passData, expr, ref lhsExpr, ref rhsExpr),

            (ES_TypeTag.Array, ES_TypeTag.Array) => CompileExpression_BinaryArrayArray (ref passData, expr, ref lhsExpr, ref rhsExpr),

            _ => throw new CompilationException ("Binary expression not supported."),
        };
    }

    private static ExpressionData CompileExpression_BinarySimple (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        var op = expr.Kind switch {
            ESIR_NodeKind.BinaryExprAdd => SyntaxKind.AddExpression,
            ESIR_NodeKind.BinaryExprSubtract => SyntaxKind.SubtractExpression,

            ESIR_NodeKind.BinaryExprMultiply => SyntaxKind.MultiplyExpression,
            ESIR_NodeKind.BinaryExprDivide => SyntaxKind.DivideExpression,
            ESIR_NodeKind.BinaryExprModulo => SyntaxKind.ModuloExpression,

            ESIR_NodeKind.BinaryExprShiftLeft => SyntaxKind.LeftShiftExpression,
            ESIR_NodeKind.BinaryExprShiftRight => SyntaxKind.RightShiftExpression,

            ESIR_NodeKind.BinaryExprLesserThan => SyntaxKind.LessThanExpression,
            ESIR_NodeKind.BinaryExprGreaterThan => SyntaxKind.GreaterThanExpression,
            ESIR_NodeKind.BinaryExprLesserThanEqual => SyntaxKind.LessThanOrEqualExpression,
            ESIR_NodeKind.BinaryExprGreaterThanEqual => SyntaxKind.GreaterThanOrEqualExpression,

            ESIR_NodeKind.BinaryExprEquals => SyntaxKind.EqualsExpression,
            ESIR_NodeKind.BinaryExprNotEquals => SyntaxKind.NotEqualsExpression,

            ESIR_NodeKind.BinaryExprBitAnd => SyntaxKind.BitwiseAndExpression,
            ESIR_NodeKind.BinaryExprBitOr => SyntaxKind.BitwiseOrExpression,
            ESIR_NodeKind.BinaryExprBitXor => SyntaxKind.ExclusiveOrExpression,

            ESIR_NodeKind.BinaryExprLogicalAnd => SyntaxKind.LogicalAndExpression,
            ESIR_NodeKind.BinaryExprLogicalOr => SyntaxKind.LogicalOrExpression,

            _ => throw new CompilationException ("Not a simple binary operation."),
        };
        var type = lhsExpr.Type;

        if (expr.IsComparison ())
            type = passData.Env.TypeBool;

        var value = BinaryExpression (op, lhsExpr.Value!, rhsExpr.Value!);

        return new ExpressionData { Type = type, Value = value, };
    }

    private static ExpressionData CompileExpression_BinaryBoolBool (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        Debug.Assert (lhsExpr.Type->TypeTag is ES_TypeTag.Bool);
        Debug.Assert (rhsExpr.Type->TypeTag is ES_TypeTag.Bool);

        return expr.Kind switch {
            ESIR_NodeKind.BinaryExprEquals or
            ESIR_NodeKind.BinaryExprNotEquals or

            ESIR_NodeKind.BinaryExprBitAnd or
            ESIR_NodeKind.BinaryExprBitOr or
            ESIR_NodeKind.BinaryExprBitXor or

            ESIR_NodeKind.BinaryExprLogicalAnd or
            ESIR_NodeKind.BinaryExprLogicalOr => CompileExpression_BinarySimple (ref passData, expr, ref lhsExpr, ref rhsExpr),

            _ => throw new CompilationException ("Invalid binary op for bool/bool."),
        };
    }

    private static ExpressionData CompileExpression_BinaryIntInt (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        Debug.Assert (lhsExpr.Type->TypeTag is ES_TypeTag.Int);
        Debug.Assert (rhsExpr.Type->TypeTag is ES_TypeTag.Int);

        return expr.Kind switch {
            ESIR_NodeKind.BinaryExprAdd or
            ESIR_NodeKind.BinaryExprSubtract or

            ESIR_NodeKind.BinaryExprMultiply or

            ESIR_NodeKind.BinaryExprLesserThan or
            ESIR_NodeKind.BinaryExprGreaterThan or
            ESIR_NodeKind.BinaryExprLesserThanEqual or
            ESIR_NodeKind.BinaryExprGreaterThanEqual or

            ESIR_NodeKind.BinaryExprEquals or
            ESIR_NodeKind.BinaryExprNotEquals or

            ESIR_NodeKind.BinaryExprBitAnd or
            ESIR_NodeKind.BinaryExprBitOr or
            ESIR_NodeKind.BinaryExprBitXor => CompileExpression_BinaryIntInt_Simple (ref passData, expr, ref lhsExpr, ref rhsExpr),

            ESIR_NodeKind.BinaryExprShiftLeft or
            ESIR_NodeKind.BinaryExprShiftRight or
            ESIR_NodeKind.BinaryExprShiftRightUnsigned => CompileExpression_BinaryIntInt_Shift (ref passData, expr, ref lhsExpr, ref rhsExpr),

            ESIR_NodeKind.BinaryExprDivide or
            ESIR_NodeKind.BinaryExprModulo => CompileExpression_BinaryIntInt_Division (ref passData, expr, ref lhsExpr, ref rhsExpr),

            ESIR_NodeKind.BinaryExprPower => throw new NotImplementedException ("[TODO] ** op not implemented yet."),

            _ => throw new CompilationException ("Invalid binary op for int/int."),
        };
    }

    private static ExpressionData CompileExpression_BinaryIntInt_Simple (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        var lhsInt = (ES_IntTypeData*) lhsExpr.Type;
        var rhsInt = (ES_IntTypeData*) rhsExpr.Type;

        Debug.Assert (lhsInt->IntSize == rhsInt->IntSize);
        Debug.Assert (lhsInt->Unsigned == rhsInt->Unsigned);

        return CompileExpression_BinarySimple (ref passData, expr, ref lhsExpr, ref rhsExpr);
    }

    private static ExpressionData CompileExpression_BinaryIntInt_Shift (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        var lhsInt = (ES_IntTypeData*) lhsExpr.Type;
        var rhsInt = (ES_IntTypeData*) rhsExpr.Type;

        Debug.Assert (rhsInt->IntSize <= ES_IntSize.Int32);
        Debug.Assert (!rhsInt->Unsigned);

        if (expr.Kind == ESIR_NodeKind.BinaryExprShiftRightUnsigned) {
            var unsignedLhsType = GetIntType (lhsInt->IntSize, true);

            var value = CastExpression (
                GetRoslynType (lhsExpr.Type),
                BinaryExpression (
                    SyntaxKind.RightShiftExpression,
                    CastExpression (unsignedLhsType, lhsExpr.Value!),
                    rhsExpr.Value!
                )
            );

            return new ExpressionData { Type = lhsExpr.Type, Value = value, };
        }

        return CompileExpression_BinarySimple (ref passData, expr, ref lhsExpr, ref rhsExpr);
    }

    private static ExpressionData CompileExpression_BinaryIntInt_Division (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        var lhsInt = (ES_IntTypeData*) lhsExpr.Type;
        var rhsInt = (ES_IntTypeData*) rhsExpr.Type;

        Debug.Assert (lhsInt->IntSize == rhsInt->IntSize);
        Debug.Assert (lhsInt->Unsigned == rhsInt->Unsigned);

        var divFunc = expr.Kind switch {
            ESIR_NodeKind.BinaryExprDivide => nameof (ES_DotNetIntrinsicsImpl.IntegerDivision),
            ESIR_NodeKind.BinaryExprModulo => nameof (ES_DotNetIntrinsicsImpl.IntegerModulo),

            _ => throw new CompilationException ("Not an int/int division expression."),
        };

        var value = CastExpression (
            GetRoslynType (lhsExpr.Type),
            InvocationExpression (
                SimpleMemberAccess (
                    nameof (ES_DotNetIntrinsicsImpl),
                    divFunc
                )
            ).WithArgumentList (ArgumentList (
                SimpleSeparatedList (
                    Token (SyntaxKind.CommaToken),
                    Argument (lhsExpr.Value!),
                    Argument (rhsExpr.Value!)
                )
            ))
        );

        return new ExpressionData { Type = lhsExpr.Type, Value = value, };
    }

    private static ExpressionData CompileExpression_BinaryFloatFloat (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        var lhsFloat = (ES_FloatTypeData*) lhsExpr.Type;
        var rhsFloat = (ES_FloatTypeData*) rhsExpr.Type;

        Debug.Assert (lhsFloat->FloatSize == rhsFloat->FloatSize);

        return expr.Kind switch {
            ESIR_NodeKind.BinaryExprAdd or
            ESIR_NodeKind.BinaryExprSubtract or

            ESIR_NodeKind.BinaryExprMultiply or
            ESIR_NodeKind.BinaryExprDivide or
            ESIR_NodeKind.BinaryExprModulo or

            ESIR_NodeKind.BinaryExprLesserThan or
            ESIR_NodeKind.BinaryExprGreaterThan or
            ESIR_NodeKind.BinaryExprLesserThanEqual or
            ESIR_NodeKind.BinaryExprGreaterThanEqual or

            ESIR_NodeKind.BinaryExprEquals or
            ESIR_NodeKind.BinaryExprNotEquals => CompileExpression_BinarySimple (ref passData, expr, ref lhsExpr, ref rhsExpr),

            ESIR_NodeKind.BinaryExprPower => throw new NotImplementedException ("[TODO] ** op not implemented yet."),

            _ => throw new CompilationException ("Invalid binary op for float/float."),
        };
    }

    private static ExpressionData CompileExpression_BinaryFloatInt (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        throw expr.Kind switch {
            ESIR_NodeKind.BinaryExprPower => new NotImplementedException ("[TODO] ** op not implemented yet."),

            _ => new CompilationException ("Invalid binary op for float/int."),
        };
    }

    private static ExpressionData CompileExpression_BinaryRefRef (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        switch (expr.Kind) {
            case ESIR_NodeKind.BinaryExprEquals:
            case ESIR_NodeKind.BinaryExprNotEquals: {
                var roslynVoidPtr = PointerType (PredefinedType (Token (SyntaxKind.VoidKeyword)));

                var value = BinaryExpression (
                    expr.Kind == ESIR_NodeKind.BinaryExprEquals ? SyntaxKind.EqualsExpression : SyntaxKind.NotEqualsExpression,
                    CastExpression (roslynVoidPtr, lhsExpr.Value!),
                    CastExpression (roslynVoidPtr, rhsExpr.Value!)
                );

                return new ExpressionData { Type = passData.Env.TypeBool, Value = value, };
            }

            default:
                throw new CompilationException ("Invalid binary op for ref/ref.");
        }
    }

    private static ExpressionData CompileExpression_BinaryArrayArray (
        ref PassData passData,
        ESIR_SimpleBinaryExpression expr,
        ref ExpressionData lhsExpr,
        ref ExpressionData rhsExpr
    ) {
        var lhsArray = (ES_ArrayTypeData*) lhsExpr.Type;
        var rhsArray = (ES_ArrayTypeData*) rhsExpr.Type;

        switch (expr.Kind) {
            case ESIR_NodeKind.BinaryExprEquals:
            case ESIR_NodeKind.BinaryExprNotEquals: {
                var roslynVoidPtr = PointerType (PredefinedType (Token (SyntaxKind.VoidKeyword)));

                var value = BinaryExpression (
                    expr.Kind == ESIR_NodeKind.BinaryExprEquals ? SyntaxKind.EqualsExpression : SyntaxKind.NotEqualsExpression,
                    CastExpression (roslynVoidPtr, lhsExpr.Value!),
                    CastExpression (roslynVoidPtr, rhsExpr.Value!)
                );

                return new ExpressionData { Type = passData.Env.TypeBool, Value = value, };
            }

            case ESIR_NodeKind.BinaryExprConcat: {
                if (lhsArray->DimensionsCount != rhsArray->DimensionsCount)
                    throw new CompilationException ("Both arrays must have the same rank.");
                else if (lhsArray->DimensionsCount != 1)
                    throw new CompilationException ("The arrays must have rank 1.");

                Debug.Assert (lhsExpr.Value is not null);
                Debug.Assert (rhsExpr.Value is not null);

                var value = InvocationExpression (MemberAccessExpression (
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName (MangleTypeName (&lhsArray->TypeInfo)),
                    IdentifierName (ArrayConcatFuncName)
                )).WithArgumentList (ArgumentList (
                    SimpleSeparatedList (
                        Token (SyntaxKind.CommaToken),
                        Argument (lhsExpr.Value),
                        Argument (rhsExpr.Value)
                    )
                ));

                return new ExpressionData { Type = lhsExpr.Type, Value = value, };
            }

            default:
                throw new CompilationException ("Invalid binary op for array/array.");
        }
    }
}
