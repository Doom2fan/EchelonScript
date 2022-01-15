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
using System.Diagnostics.CodeAnalysis;
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Frontend;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private bool GetSimpleBinaryExprSyntaxKind (SimpleBinaryExprType exprType, out SyntaxKind syntaxKind) {
            syntaxKind = exprType switch {
                SimpleBinaryExprType.Add => SyntaxKind.AddExpression,
                SimpleBinaryExprType.Subtract => SyntaxKind.SubtractExpression,

                SimpleBinaryExprType.Multiply => SyntaxKind.MultiplyExpression,
                SimpleBinaryExprType.Divide => SyntaxKind.DivideExpression,
                SimpleBinaryExprType.Modulo => SyntaxKind.ModuloExpression,

                SimpleBinaryExprType.ShiftLeft => SyntaxKind.LeftShiftExpression,
                SimpleBinaryExprType.ShiftRight => SyntaxKind.RightShiftExpression,

                SimpleBinaryExprType.LesserThan => SyntaxKind.LessThanExpression,
                SimpleBinaryExprType.GreaterThan => SyntaxKind.GreaterThanExpression,
                SimpleBinaryExprType.LesserThanEqual => SyntaxKind.LessThanOrEqualExpression,
                SimpleBinaryExprType.GreaterThanEqual => SyntaxKind.GreaterThanOrEqualExpression,

                SimpleBinaryExprType.Equals => SyntaxKind.EqualsExpression,
                SimpleBinaryExprType.NotEquals => SyntaxKind.NotEqualsExpression,

                SimpleBinaryExprType.BitAnd => SyntaxKind.BitwiseAndExpression,
                SimpleBinaryExprType.BitXor => SyntaxKind.ExclusiveOrExpression,
                SimpleBinaryExprType.BitOr => SyntaxKind.BitwiseOrExpression,

                SimpleBinaryExprType.LogicalAnd => SyntaxKind.LogicalAndExpression,
                SimpleBinaryExprType.LogicalOr => SyntaxKind.LogicalOrExpression,

                _ => SyntaxKind.None,
            };

            return syntaxKind != SyntaxKind.None;
        }

        private bool GetSimpleBinaryExprAssignSyntaxKind (SimpleBinaryExprType exprType, out SyntaxKind syntaxKind) {
            syntaxKind = exprType switch {
                SimpleBinaryExprType.Assign => SyntaxKind.SimpleAssignmentExpression,

                SimpleBinaryExprType.AssignAdd => SyntaxKind.AddAssignmentExpression,
                SimpleBinaryExprType.AssignSubtract => SyntaxKind.SubtractAssignmentExpression,

                SimpleBinaryExprType.AssignMultiply => SyntaxKind.MultiplyAssignmentExpression,
                SimpleBinaryExprType.AssignDivide => SyntaxKind.DivideAssignmentExpression,
                SimpleBinaryExprType.AssignModulo => SyntaxKind.ModuloAssignmentExpression,

                SimpleBinaryExprType.AssignBitAnd => SyntaxKind.AndAssignmentExpression,
                SimpleBinaryExprType.AssignXor => SyntaxKind.ExclusiveOrAssignmentExpression,
                SimpleBinaryExprType.AssignBitOr => SyntaxKind.OrAssignmentExpression,

                SimpleBinaryExprType.AssignShiftLeft => SyntaxKind.LeftShiftAssignmentExpression,
                SimpleBinaryExprType.AssignShiftRight => SyntaxKind.RightShiftAssignmentExpression,

                _ => SyntaxKind.None,
            };

            return syntaxKind != SyntaxKind.None;
        }

        private ExpressionData GenerateCode_BinaryExpr (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            if (!envBuilder!.BinaryOpCompat (lhs.Type, rhs.Type, exprOp, out _, out _))
                throw new CompilationException (ES_BackendErrors.FrontendError);

            var lhsNull = lhs.Type->TypeTag == ES_TypeTag.Null;
            var rhsNull = rhs.Type->TypeTag == ES_TypeTag.Null;

            Debug.Assert (lhs.Value != null || lhsNull);
            Debug.Assert (rhs.Value != null || rhsNull);

            if (lhsNull || rhsNull) {
                if (lhsNull && rhsNull)
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                ref var nullSide = ref lhs;
                var refType = rhs.Type;

                if (rhsNull) {
                    nullSide = ref rhs;
                    refType = lhs.Type;
                }

                nullSide.Value = LiteralExpression (SyntaxKind.NullLiteralExpression);
                nullSide.Type = refType;
            }

            if (lhs.Type->TypeTag == ES_TypeTag.Int && rhs.Type->TypeTag == ES_TypeTag.Int)
                return GenerateCode_BinaryExpr_IntInt (lhs, rhs, exprOp);

            else if (lhs.Type->TypeTag == ES_TypeTag.Bool && rhs.Type->TypeTag == ES_TypeTag.Bool)
                return GenerateCode_BinaryExpr_BoolBool (lhs, rhs, exprOp);

            else if (lhs.Type->TypeTag == ES_TypeTag.Float && rhs.Type->TypeTag == ES_TypeTag.Float)
                return GenerateCode_BinaryExpr_FloatFloat (lhs, rhs, exprOp);

            else if (lhs.Type->TypeTag == ES_TypeTag.Float && rhs.Type->TypeTag == ES_TypeTag.Int)
                return GenerateCode_BinaryExpr_FloatInt (lhs, rhs, exprOp);

            else if (lhs.Type->TypeTag == ES_TypeTag.Reference && rhs.Type->TypeTag == ES_TypeTag.Reference)
                return GenerateCode_BinaryExpr_RefRef (lhs, rhs, exprOp);

            throw new CompilationException (ES_BackendErrors.FrontendError);
        }

        private ExpressionSyntax GenerateCode_BinaryExpr_IntIntDiv ([NotNull] ES_TypeInfo* type, ExpressionSyntax lhs, ExpressionSyntax rhs, bool mod) {
            Debug.Assert (type->TypeTag == ES_TypeTag.Int);

            var roslynType = GetRoslynType (type);

            return CastExpression (
                roslynType,
                InvocationExpression (
                    SimpleMemberAccess (
                        nameof (ES_DotNetIntrinsicsImpl),
                        !mod ? nameof (ES_DotNetIntrinsicsImpl.IntegerDivision) : nameof (ES_DotNetIntrinsicsImpl.IntegerModulo)
                    )
                ).WithArgumentList (ArgumentList (
                    SimpleSeparatedList (
                        Token (SyntaxKind.CommaToken),
                        Argument (lhs),
                        Argument (rhs)
                    )
                ))
            );
        }

        private ExpressionData GenerateCode_BinaryExpr_IntInt (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            var boolType = env!.TypeBool;

            // Validate the types and op.
            Debug.Assert (lhs.Type->TypeTag == ES_TypeTag.Int);
            Debug.Assert (rhs.Type->TypeTag == ES_TypeTag.Int);

            var lhsInt = (ES_IntTypeData*) lhs.Type;
            var rhsInt = (ES_IntTypeData*) rhs.Type;

            bool isShift = exprOp.IsBitShift ();

            if (!isShift && lhsInt->Unsigned != rhsInt->Unsigned)
                throw new CompilationException (ES_BackendErrors.FrontendError);
            else if (isShift && !rhsInt->Unsigned)
                throw new CompilationException (ES_BackendErrors.FrontendError);

            bool isAssignment = exprOp.IsAssignment ();

            Debug.Assert (!isAssignment || lhs.Addressable);

            if (!isShift && !isAssignment) {
                if (lhsInt->IntSize > rhsInt->IntSize) {
                    rhs = GenerateCode_Cast (rhs, lhs.Type);
                    rhsInt = lhsInt;
                } else if (rhsInt->IntSize > lhsInt->IntSize) {
                    lhs = GenerateCode_Cast (lhs, rhs.Type);
                    lhsInt = rhsInt;
                }
            } else {
                if (rhsInt->IntSize < lhsInt->IntSize) {
                    rhs = GenerateCode_Cast (rhs, lhs.Type);
                    rhsInt = lhsInt;
                } else if (rhsInt->IntSize > lhsInt->IntSize)
                    throw new CompilationException (ES_BackendErrors.FrontendError);
            }

            bool unsigned = lhsInt->Unsigned;

            Debug.Assert (lhs.Value is not null);
            Debug.Assert (rhs.Value is not null);

            ExpressionSyntax value;
            if (exprOp == SimpleBinaryExprType.Divide || exprOp == SimpleBinaryExprType.AssignDivide ||
                exprOp == SimpleBinaryExprType.Modulo || exprOp == SimpleBinaryExprType.AssignModulo
            ) {
                var mod = exprOp == SimpleBinaryExprType.Modulo || exprOp == SimpleBinaryExprType.AssignModulo;

                value = GenerateCode_BinaryExpr_IntIntDiv (lhs.Type, lhs.Value, rhs.Value, mod);

                if (isAssignment)
                    value = AssignmentExpression (SyntaxKind.SimpleAssignmentExpression, lhs.Value, value);
            } else if (GetSimpleBinaryExprSyntaxKind (exprOp, out var simpleBinarySyntaxKind))
                value = BinaryExpression (simpleBinarySyntaxKind, lhs.Value, rhs.Value);
            else if (GetSimpleBinaryExprAssignSyntaxKind (exprOp, out var simpleBinaryAssignSyntaxKind))
                value = AssignmentExpression (simpleBinaryAssignSyntaxKind, lhs.Value, rhs.Value);
            else if (exprOp == SimpleBinaryExprType.ShiftRightUnsigned || exprOp == SimpleBinaryExprType.AssignShiftRightUnsigned) {
                var leftVal = lhs.Value;
                if (!lhsInt->Unsigned)
                    leftVal = CastExpression (GetIntType (lhsInt->IntSize, true), leftVal);

                value = BinaryExpression (simpleBinarySyntaxKind, leftVal, rhs.Value);

                if (!lhsInt->Unsigned)
                    value = CastExpression (GetRoslynType (lhs.Type), value);
            } else if (exprOp == SimpleBinaryExprType.Power || exprOp == SimpleBinaryExprType.AssignPower)
                throw new NotImplementedException ("[TODO] ** not implemented yet.");
            else
                throw new NotImplementedException ("Operation not implemented yet");

            if (!exprOp.IsComparison ())
                return new ExpressionData { Type = lhs.Type, Value = value, Constant = false, Addressable = false, };
            else
                return new ExpressionData { Type = boolType, Value = value, Constant = false, Addressable = false, };
        }

        private ExpressionData GenerateCode_BinaryExpr_BoolBool (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            var boolType = env!.TypeBool;

            // Validate the types and op.
            Debug.Assert (lhs.Type->TypeTag == ES_TypeTag.Bool);
            Debug.Assert (rhs.Type->TypeTag == ES_TypeTag.Bool);

            Debug.Assert (!exprOp.IsAssignment () || lhs.Addressable);
            Debug.Assert (lhs.Value is not null);
            Debug.Assert (rhs.Value is not null);

            ExpressionSyntax value;
            if (GetSimpleBinaryExprSyntaxKind (exprOp, out var simpleBinarySyntaxKind))
                value = BinaryExpression (simpleBinarySyntaxKind, lhs.Value, rhs.Value);
            else if (GetSimpleBinaryExprAssignSyntaxKind (exprOp, out var simpleBinaryAssignSyntaxKind))
                value = AssignmentExpression (simpleBinaryAssignSyntaxKind, lhs.Value, rhs.Value);
            else
                throw new NotImplementedException ("Operation not implemented yet");

            return new ExpressionData { Type = boolType, Value = value, Constant = false, Addressable = false, };
        }

        private ExpressionData GenerateCode_BinaryExpr_FloatFloat (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            Debug.Assert (env is not null);

            // Validate the types and op.
            Debug.Assert (lhs.Type->TypeTag == ES_TypeTag.Float);
            Debug.Assert (rhs.Type->TypeTag == ES_TypeTag.Float);

            Debug.Assert (!exprOp.IsAssignment () || lhs.Addressable);
            Debug.Assert (lhs.Value is not null);
            Debug.Assert (rhs.Value is not null);

            var lhsFloat = (ES_FloatTypeData*) lhs.Type;
            var rhsFloat = (ES_FloatTypeData*) rhs.Type;
            if (lhsFloat->FloatSize != rhsFloat->FloatSize)
                throw new CompilationException (ES_BackendErrors.FrontendError);

            ExpressionSyntax value;
            if (GetSimpleBinaryExprSyntaxKind (exprOp, out var simpleBinarySyntaxKind))
                value = BinaryExpression (simpleBinarySyntaxKind, lhs.Value, rhs.Value);
            else if (GetSimpleBinaryExprAssignSyntaxKind (exprOp, out var simpleBinaryAssignSyntaxKind))
                value = AssignmentExpression (simpleBinaryAssignSyntaxKind, lhs.Value, rhs.Value);
            else if (exprOp == SimpleBinaryExprType.Power || exprOp == SimpleBinaryExprType.AssignPower)
                throw new NotImplementedException ("[TODO] ** not implemented yet.");
            else
                throw new NotImplementedException ("Operation not implemented yet");

            if (!exprOp.IsComparison ())
                return new ExpressionData { Type = lhs.Type, Value = value, Constant = false, Addressable = false, };
            else
                return new ExpressionData { Type = env!.TypeBool, Value = value, Constant = false, Addressable = false, };
        }

        private ExpressionData GenerateCode_BinaryExpr_FloatInt (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            Debug.Assert (env is not null);

            // Validate the types and op.
            Debug.Assert (lhs.Type->TypeTag == ES_TypeTag.Float);
            Debug.Assert (rhs.Type->TypeTag == ES_TypeTag.Int);

            Debug.Assert (!exprOp.IsAssignment () || lhs.Addressable);
            Debug.Assert (lhs.Value is not null);
            Debug.Assert (rhs.Value is not null);

            var lhsFloat = (ES_FloatTypeData*) lhs.Type;
            var rhsFloat = (ES_IntTypeData*) rhs.Type;

            ExpressionSyntax value;
            if (exprOp == SimpleBinaryExprType.Power || exprOp == SimpleBinaryExprType.AssignPower)
                throw new NotImplementedException ("[TODO] ** not implemented yet.");
            else
                throw new NotImplementedException ("Operation not implemented yet");

            if (!exprOp.IsComparison ())
                return new ExpressionData { Type = lhs.Type, Value = value, Constant = false, Addressable = false, };
            else
                return new ExpressionData { Type = env!.TypeBool, Value = value, Constant = false, Addressable = false, };
        }

        private ExpressionData GenerateCode_BinaryExpr_RefRef (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            Debug.Assert (env is not null);

            // Validate the types and op.
            Debug.Assert (lhs.Type->TypeTag == ES_TypeTag.Reference);
            Debug.Assert (rhs.Type->TypeTag == ES_TypeTag.Reference);

            Debug.Assert (!exprOp.IsAssignment () || lhs.Addressable);
            Debug.Assert (lhs.Value is not null);
            Debug.Assert (rhs.Value is not null);

            var lhsRef = (ES_ReferenceData*) lhs.Type;
            var rhsRef = (ES_ReferenceData*) rhs.Type;
            if (lhsRef != rhsRef)
                throw new CompilationException (ES_BackendErrors.FrontendError);

            ExpressionSyntax value;
            if (GetSimpleBinaryExprSyntaxKind (exprOp, out var simpleBinarySyntaxKind))
                value = BinaryExpression (simpleBinarySyntaxKind, lhs.Value, rhs.Value);
            else if (GetSimpleBinaryExprAssignSyntaxKind (exprOp, out var simpleBinaryAssignSyntaxKind))
                value = AssignmentExpression (simpleBinaryAssignSyntaxKind, lhs.Value, rhs.Value);
            else
                throw new NotImplementedException ("Operation not implemented yet");

            if (!exprOp.IsComparison ())
                return new ExpressionData { Type = lhs.Type, Value = value, Constant = false, Addressable = false, };
            else
                return new ExpressionData { Type = env!.TypeBool, Value = value, Constant = false, Addressable = false, };
        }

        private ExpressionData GenerateCode_LogicalBinaryExpr (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_AstSimpleBinaryExpression binExpr
        ) {
            var boolType = env!.TypeBool;

            var exprOp = binExpr.ExpressionType;
            Debug.Assert (exprOp.IsLogical ());

            bool isAnd = exprOp == SimpleBinaryExprType.LogicalAnd;
            Debug.Assert (isAnd || exprOp == SimpleBinaryExprType.LogicalOr);

            // Generate the left-hand side.
            var lhs = GenerateCode_Expression (ref transUnit, symbols, src, binExpr.Left, boolType);

            GenerateCode_EnsureImplicitCompat (ref lhs, boolType);

            // Generate the right-hand side.
            var rhs = GenerateCode_Expression (ref transUnit, symbols, src, binExpr.Right, boolType);
            GenerateCode_EnsureImplicitCompat (ref rhs, boolType);

            Debug.Assert (lhs.Value is not null);
            Debug.Assert (rhs.Value is not null);

            // Generate the comparison.
            var exprKind = isAnd ? SyntaxKind.LogicalAndExpression : SyntaxKind.LogicalOrExpression;
            var value = BinaryExpression (exprKind, lhs.Value, rhs.Value);

            return new ExpressionData { Type = boolType, Value = value, Constant = false, Addressable = false, };
        }
    }
}
