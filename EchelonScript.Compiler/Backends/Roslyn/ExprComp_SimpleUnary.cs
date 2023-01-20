/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Common.Data.Types;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScript.Compiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
#if false
    private static ExpressionData CompileExpression_Unary (
        ref PassData passData,
        ref FunctionData funcData,
        ESIR_UnaryExpression expr
    ) {
        var innerExpr = CompileExpression (ref passData, ref funcData, expr.ExprInner);

        StripFirstConst (ref innerExpr);

        return innerExpr.Type->TypeTag switch {
            ES_TypeTag.Bool => CompileExpression_UnaryBool (ref passData, expr, ref innerExpr),
            ES_TypeTag.Int => CompileExpression_UnaryInt (ref passData, expr, ref innerExpr),
            ES_TypeTag.Float => CompileExpression_UnaryFloat (ref passData, expr, ref innerExpr),

            ES_TypeTag.Reference => CompileExpression_UnaryRef (ref passData, expr, ref innerExpr),

            _ => throw new CompilationException ("Binary expression not supported."),
        };
    }

    private static ExpressionData CompileExpression_UnarySimple (
        ref PassData passData,
        ESIR_UnaryExpression expr,
        ref ExpressionData innerExpr
    ) {
        var op = expr.Kind switch {
            ESIR_NodeKind.UnaryNegative => SyntaxKind.UnaryMinusExpression,
            ESIR_NodeKind.UnaryLogicalNot => SyntaxKind.LogicalNotExpression,
            ESIR_NodeKind.UnaryBitNot => SyntaxKind.BitwiseNotExpression,

            ESIR_NodeKind.UnaryPreIncrement => SyntaxKind.PreIncrementExpression,
            ESIR_NodeKind.UnaryPreDecrement => SyntaxKind.PreDecrementExpression,

            ESIR_NodeKind.UnaryPostIncrement => SyntaxKind.PostIncrementExpression,
            ESIR_NodeKind.UnaryPostDecrement => SyntaxKind.PostDecrementExpression,

            _ => throw new CompilationException ("Not a simple unary operation."),
        };
        var postfix = expr.Kind switch {
            ESIR_NodeKind.UnaryPostIncrement => true,
            ESIR_NodeKind.UnaryPostDecrement => true,

            _ => false,
        };

        ExpressionSyntax value = !postfix
            ? PrefixUnaryExpression (op, innerExpr.Value!)
            : PostfixUnaryExpression (op, innerExpr.Value!);

        return new ExpressionData { Type = innerExpr.Type, Value = value, };
    }

    private static ExpressionData CompileExpression_UnaryBool (
        ref PassData passData,
        ESIR_UnaryExpression expr,
        ref ExpressionData innerExpr
    ) {
        return expr.Kind switch {
            ESIR_NodeKind.UnaryLogicalNot => CompileExpression_UnarySimple (ref passData, expr, ref innerExpr),

            _ => throw new CompilationException ("Invalid unary op for bool."),
        };
    }

    private static ExpressionData CompileExpression_UnaryInt (
        ref PassData passData,
        ESIR_UnaryExpression expr,
        ref ExpressionData innerExpr
    ) {
        return expr.Kind switch {
            ESIR_NodeKind.UnaryNegative or
            ESIR_NodeKind.UnaryBitNot or

            ESIR_NodeKind.UnaryPreIncrement or
            ESIR_NodeKind.UnaryPreDecrement or

            ESIR_NodeKind.UnaryPostIncrement or
            ESIR_NodeKind.UnaryPostDecrement => CompileExpression_UnarySimple (ref passData, expr, ref innerExpr),

            _ => throw new CompilationException ("Invalid unary op for int."),
        };
    }

    private static ExpressionData CompileExpression_UnaryFloat (
        ref PassData passData,
        ESIR_UnaryExpression expr,
        ref ExpressionData innerExpr
    ) {
        return expr.Kind switch {
            ESIR_NodeKind.UnaryNegative or

            ESIR_NodeKind.UnaryPreIncrement or
            ESIR_NodeKind.UnaryPreDecrement or

            ESIR_NodeKind.UnaryPostIncrement or
            ESIR_NodeKind.UnaryPostDecrement => CompileExpression_UnarySimple (ref passData, expr, ref innerExpr),

            _ => throw new CompilationException ("Invalid unary op for float."),
        };
    }

    private static ExpressionData CompileExpression_UnaryRef (
        ref PassData passData,
        ESIR_UnaryExpression expr,
        ref ExpressionData innerExpr
    ) {
    var refType = (ES_ReferenceData*) innerExpr.Type;

        switch (expr.Kind) {
            case ESIR_NodeKind.UnaryDereference: {
                var innerValue = CompileCode_NullCheck (innerExpr.Value!, refType->PointedType);
                var value = PrefixUnaryExpression (SyntaxKind.PointerIndirectionExpression, innerValue);
                return new ExpressionData { Type = refType->PointedType, Value = value, };
            }

            default:
                throw new CompilationException ("Invalid unary op for bool.");
        }
    }
#endif
}
