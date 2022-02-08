/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon.IR;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private static ExpressionData CompileExpression_Unary (
            ref PassData passData,
            ref FunctionData funcData,
            ESIR_UnaryExpression expr
        ) {
            var innerExpr = CompileExpression (ref passData, ref funcData, expr.ExprInner);

            if (innerExpr.Type->TypeTag == ES_TypeTag.Bool)
                return CompileExpression_UnaryBool (ref passData, expr, ref innerExpr);
            else if (innerExpr.Type->TypeTag == ES_TypeTag.Int)
                return CompileExpression_UnaryInt (ref passData, expr, ref innerExpr);
            else if (innerExpr.Type->TypeTag == ES_TypeTag.Float)
                return CompileExpression_UnaryFloat (ref passData, expr, ref innerExpr);
            else if (innerExpr.Type->TypeTag == ES_TypeTag.Reference)
                return CompileExpression_UnaryRef (ref passData, expr, ref innerExpr);
            else
                throw new CompilationException ("Binary expression not supported.");
        }

        private static ExpressionData CompileExpression_UnarySimple (
            ref PassData passData,
            ESIR_UnaryExpression expr,
            ref ExpressionData innerExpr
        ) {
            var postfix = false;
            SyntaxKind op;

            switch (expr.Kind) {
                case ESIR_NodeKind.UnaryNegative: op = SyntaxKind.UnaryMinusExpression; break;
                case ESIR_NodeKind.UnaryLogicalNot: op = SyntaxKind.LogicalNotExpression; break;
                case ESIR_NodeKind.UnaryBitNot: op = SyntaxKind.BitwiseNotExpression; break;

                case ESIR_NodeKind.UnaryPreIncrement: op = SyntaxKind.PreIncrementExpression; break;
                case ESIR_NodeKind.UnaryPreDecrement: op = SyntaxKind.PreDecrementExpression; break;
                case ESIR_NodeKind.UnaryPostIncrement:
                    op = SyntaxKind.PostIncrementExpression;
                    postfix = true;
                    break;
                case ESIR_NodeKind.UnaryPostDecrement:
                    op = SyntaxKind.PostDecrementExpression;
                    postfix = true;
                    break;

                default:
                    throw new CompilationException ("Not a simple binary operation.");
            }

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
            switch (expr.Kind) {
                case ESIR_NodeKind.UnaryLogicalNot:
                    return CompileExpression_UnarySimple (ref passData, expr, ref innerExpr);

                default:
                    throw new CompilationException ("Invalid unary op for bool.");
            }
        }

        private static ExpressionData CompileExpression_UnaryInt (
            ref PassData passData,
            ESIR_UnaryExpression expr,
            ref ExpressionData innerExpr
        ) {
            switch (expr.Kind) {
                case ESIR_NodeKind.UnaryNegative:
                case ESIR_NodeKind.UnaryBitNot:

                case ESIR_NodeKind.UnaryPreIncrement:
                case ESIR_NodeKind.UnaryPreDecrement:
                case ESIR_NodeKind.UnaryPostIncrement:
                case ESIR_NodeKind.UnaryPostDecrement:
                    return CompileExpression_UnarySimple (ref passData, expr, ref innerExpr);

                default:
                    throw new CompilationException ("Invalid unary op for int.");
            }
        }

        private static ExpressionData CompileExpression_UnaryFloat (
            ref PassData passData,
            ESIR_UnaryExpression expr,
            ref ExpressionData innerExpr
        ) {
            switch (expr.Kind) {
                case ESIR_NodeKind.UnaryNegative:

                case ESIR_NodeKind.UnaryPreIncrement:
                case ESIR_NodeKind.UnaryPreDecrement:
                case ESIR_NodeKind.UnaryPostIncrement:
                case ESIR_NodeKind.UnaryPostDecrement:
                    return CompileExpression_UnarySimple (ref passData, expr, ref innerExpr);

                default:
                    throw new CompilationException ("Invalid unary op for float.");
            }
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
    }
}
