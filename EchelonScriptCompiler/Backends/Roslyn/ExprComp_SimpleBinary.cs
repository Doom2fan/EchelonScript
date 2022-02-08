/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon.IR;
using Microsoft.CodeAnalysis.CSharp;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private static ExpressionData CompileExpression_SimpleBinary (
            ref PassData passData,
            ref FunctionData funcData,
            ESIR_SimpleBinaryExpression expr
        ) {
            var lhsExpr = CompileExpression (ref passData, ref funcData, expr.ExprLeft);
            var rhsExpr = CompileExpression (ref passData, ref funcData, expr.ExprRight);

            if (lhsExpr.Type->TypeTag == ES_TypeTag.Bool && rhsExpr.Type->TypeTag == ES_TypeTag.Bool)
                return CompileExpression_BinaryBoolBool (ref passData, expr, ref lhsExpr, ref rhsExpr);
            else if (lhsExpr.Type->TypeTag == ES_TypeTag.Int && rhsExpr.Type->TypeTag == ES_TypeTag.Int)
                return CompileExpression_BinaryIntInt (ref passData, expr, ref lhsExpr, ref rhsExpr);
            else if (lhsExpr.Type->TypeTag == ES_TypeTag.Float && rhsExpr.Type->TypeTag == ES_TypeTag.Float)
                return CompileExpression_BinaryFloatFloat (ref passData, expr, ref lhsExpr, ref rhsExpr);
            else if (lhsExpr.Type->TypeTag == ES_TypeTag.Float && rhsExpr.Type->TypeTag == ES_TypeTag.Int)
                return CompileExpression_BinaryFloatInt (ref passData, expr, ref lhsExpr, ref rhsExpr);
            else if (lhsExpr.Type->TypeTag == ES_TypeTag.Reference && rhsExpr.Type->TypeTag == ES_TypeTag.Reference)
                return CompileExpression_BinaryRefRef (ref passData, expr, ref lhsExpr, ref rhsExpr);
            else if (lhsExpr.Type->TypeTag == ES_TypeTag.Array && rhsExpr.Type->TypeTag == ES_TypeTag.Array)
                return CompileExpression_BinaryArrayArray (ref passData, expr, ref lhsExpr, ref rhsExpr);
            else
                throw new CompilationException ("Binary expression not supported.");
        }

        private static ExpressionData CompileExpression_BinarySimple (
            ref PassData passData,
            ESIR_SimpleBinaryExpression expr,
            ref ExpressionData lhsExpr,
            ref ExpressionData rhsExpr
        ) {
            SyntaxKind op;

            switch (expr.Kind) {
                case ESIR_NodeKind.BinaryExprAdd: op = SyntaxKind.AddExpression; break;
                case ESIR_NodeKind.BinaryExprSubtract: op = SyntaxKind.SubtractExpression; break;

                case ESIR_NodeKind.BinaryExprMultiply: op = SyntaxKind.MultiplyExpression; break;
                case ESIR_NodeKind.BinaryExprDivide: op = SyntaxKind.DivideExpression; break;
                case ESIR_NodeKind.BinaryExprModulo: op = SyntaxKind.ModuloExpression; break;

                case ESIR_NodeKind.BinaryExprShiftLeft: op = SyntaxKind.LeftShiftExpression; break;
                case ESIR_NodeKind.BinaryExprShiftRight: op = SyntaxKind.RightShiftExpression; break;
                //case ESIR_NodeKind.BinaryExprShiftRightUnsigned: op = SyntaxKind.RightShiftExpression; break;

                case ESIR_NodeKind.BinaryExprLesserThan: op = SyntaxKind.LessThanExpression; break;
                case ESIR_NodeKind.BinaryExprGreaterThan: op = SyntaxKind.GreaterThanExpression; break;
                case ESIR_NodeKind.BinaryExprLesserThanEqual: op = SyntaxKind.LessThanOrEqualExpression; break;
                case ESIR_NodeKind.BinaryExprGreaterThanEqual: op = SyntaxKind.GreaterThanOrEqualExpression; break;

                case ESIR_NodeKind.BinaryExprEquals: op = SyntaxKind.EqualsExpression; break;
                case ESIR_NodeKind.BinaryExprNotEquals: op = SyntaxKind.NotEqualsExpression; break;

                case ESIR_NodeKind.BinaryExprBitAnd: op = SyntaxKind.BitwiseAndExpression; break;
                case ESIR_NodeKind.BinaryExprBitOr: op = SyntaxKind.BitwiseOrExpression; break;
                case ESIR_NodeKind.BinaryExprBitXor: op = SyntaxKind.ExclusiveOrExpression; break;

                case ESIR_NodeKind.BinaryExprLogicalAnd: op = SyntaxKind.LogicalAndExpression; break;
                case ESIR_NodeKind.BinaryExprLogicalOr: op = SyntaxKind.LogicalOrExpression; break;

                default:
                    throw new CompilationException ("Not a simple binary operation.");
            }

            var value = BinaryExpression (op, lhsExpr.Value!, rhsExpr.Value!);

            return new ExpressionData { Type = lhsExpr.Type, Value = value, };
        }

        private static ExpressionData CompileExpression_BinaryBoolBool (
            ref PassData passData,
            ESIR_SimpleBinaryExpression expr,
            ref ExpressionData lhsExpr,
            ref ExpressionData rhsExpr
        ) {
            switch (expr.Kind) {
                case ESIR_NodeKind.BinaryExprEquals:
                case ESIR_NodeKind.BinaryExprNotEquals:

                case ESIR_NodeKind.BinaryExprBitAnd:
                case ESIR_NodeKind.BinaryExprBitOr:
                case ESIR_NodeKind.BinaryExprBitXor:

                case ESIR_NodeKind.BinaryExprLogicalAnd:
                case ESIR_NodeKind.BinaryExprLogicalOr:
                    break;

                default:
                    throw new CompilationException ("Invalid binary op for bool/bool.");
            }

            return CompileExpression_BinarySimple (ref passData, expr, ref lhsExpr, ref rhsExpr);
        }

        private static ExpressionData CompileExpression_BinaryIntInt (
            ref PassData passData,
            ESIR_SimpleBinaryExpression expr,
            ref ExpressionData lhsExpr,
            ref ExpressionData rhsExpr
        ) {
            var lhsInt = (ES_IntTypeData*) lhsExpr.Type;
            var rhsInt = (ES_IntTypeData*) rhsExpr.Type;

            switch (expr.Kind) {
                case ESIR_NodeKind.BinaryExprAdd:
                case ESIR_NodeKind.BinaryExprSubtract:

                case ESIR_NodeKind.BinaryExprMultiply:

                case ESIR_NodeKind.BinaryExprLesserThan:
                case ESIR_NodeKind.BinaryExprGreaterThan:
                case ESIR_NodeKind.BinaryExprLesserThanEqual:
                case ESIR_NodeKind.BinaryExprGreaterThanEqual:

                case ESIR_NodeKind.BinaryExprEquals:
                case ESIR_NodeKind.BinaryExprNotEquals:

                case ESIR_NodeKind.BinaryExprBitAnd:
                case ESIR_NodeKind.BinaryExprBitOr:
                case ESIR_NodeKind.BinaryExprBitXor:
                    break;

                case ESIR_NodeKind.BinaryExprShiftLeft:
                case ESIR_NodeKind.BinaryExprShiftRight:
                case ESIR_NodeKind.BinaryExprShiftRightUnsigned:
                    Debug.Assert (lhsInt->IntSize == rhsInt->IntSize);
                    Debug.Assert (lhsInt->Unsigned == rhsInt->Unsigned);

                    return CompileExpression_BinaryIntInt_Shift (ref passData, expr, ref lhsExpr, ref rhsExpr);

                case ESIR_NodeKind.BinaryExprDivide:
                case ESIR_NodeKind.BinaryExprModulo:
                    return CompileExpression_BinaryIntInt_Division (ref passData, expr, ref lhsExpr, ref rhsExpr);

                //case ESIR_NodeKind.BinaryExprPower:

                default:
                    throw new CompilationException ("Invalid binary op for int/int.");
            }

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

            string divFunc;
            switch (expr.Kind) {
                case ESIR_NodeKind.BinaryExprDivide:
                    divFunc = nameof (ES_DotNetIntrinsicsImpl.IntegerDivision);
                    break;

                case ESIR_NodeKind.BinaryExprModulo:
                    divFunc = nameof (ES_DotNetIntrinsicsImpl.IntegerModulo);
                    break;

                default:
                    throw new CompilationException ("Not an int/int division expression.");
            }

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

            switch (expr.Kind) {
                case ESIR_NodeKind.BinaryExprAdd:
                case ESIR_NodeKind.BinaryExprSubtract:

                case ESIR_NodeKind.BinaryExprMultiply:
                case ESIR_NodeKind.BinaryExprDivide:
                case ESIR_NodeKind.BinaryExprModulo:

                case ESIR_NodeKind.BinaryExprLesserThan:
                case ESIR_NodeKind.BinaryExprGreaterThan:
                case ESIR_NodeKind.BinaryExprLesserThanEqual:
                case ESIR_NodeKind.BinaryExprGreaterThanEqual:

                case ESIR_NodeKind.BinaryExprEquals:
                case ESIR_NodeKind.BinaryExprNotEquals:
                    break;

                //case ESIR_NodeKind.BinaryExprPower:

                default:
                    throw new CompilationException ("Invalid binary op for float/float.");
            }

            return CompileExpression_BinarySimple (ref passData, expr, ref lhsExpr, ref rhsExpr);
        }

        private static ExpressionData CompileExpression_BinaryFloatInt (
            ref PassData passData,
            ESIR_SimpleBinaryExpression expr,
            ref ExpressionData lhsExpr,
            ref ExpressionData rhsExpr
        ) {
            switch (expr.Kind) {
                //case ESIR_NodeKind.BinaryExprPower:

                default:
                    throw new CompilationException ("Invalid binary op for float/int.");
            }
        }

        private static ExpressionData CompileExpression_BinaryRefRef (
            ref PassData passData,
            ESIR_SimpleBinaryExpression expr,
            ref ExpressionData lhsExpr,
            ref ExpressionData rhsExpr
        ) {
            var lhsRef = (ES_ReferenceData*) lhsExpr.Type;
            var rhsRef = (ES_ReferenceData*) rhsExpr.Type;

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
}
