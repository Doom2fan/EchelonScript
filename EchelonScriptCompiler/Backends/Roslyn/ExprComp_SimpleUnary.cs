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
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data.Types;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private ExpressionData GenerateCode_UnaryExpr (ExpressionData inner, SimpleUnaryExprType exprOp) {
            if (!envBuilder!.UnaryOpCompat (inner.Type, exprOp, out var _, out _))
                throw new CompilationException (ES_BackendErrors.FrontendError);

            Debug.Assert (inner.Value != null);

            if (inner.Type->TypeTag == ES_TypeTag.Int)
                return GenerateCode_UnaryExpr_Int (inner, exprOp);
            else if (inner.Type->TypeTag == ES_TypeTag.Bool)
                return GenerateCode_UnaryExpr_Bool (inner, exprOp);
            else if (inner.Type->TypeTag == ES_TypeTag.Float)
                return GenerateCode_UnaryExpr_Float (inner, exprOp);
            else if (inner.Type->TypeTag == ES_TypeTag.Reference)
                return GenerateCode_UnaryExpr_Ref (inner, exprOp);
            else
                throw new NotImplementedException ("Operation not implemented yet.");

            throw new CompilationException (ES_BackendErrors.FrontendError);
        }

        private ExpressionData GenerateCode_UnaryExpr_Int (ExpressionData inner, SimpleUnaryExprType exprOp) {
            Debug.Assert (inner.Type->TypeTag == ES_TypeTag.Int);
            Debug.Assert (inner.Value is not null);

            var innerInt = (ES_IntTypeData*) inner.Type;

            ExpressionSyntax value;
            switch (exprOp) {
                case SimpleUnaryExprType.BitNot:
                    value = PrefixUnaryExpression (SyntaxKind.BitwiseNotExpression, inner.Value);
                    break;
                case SimpleUnaryExprType.Negative:
                    if (innerInt->Unsigned)
                        throw new CompilationException (ES_BackendErrors.FrontendError);
                    value = PrefixUnaryExpression (SyntaxKind.UnaryMinusExpression, inner.Value);
                    break;
                case SimpleUnaryExprType.Positive:
                    value = inner.Value;
                    break;

                default:
                    throw new NotImplementedException ("Operation not implemented yet.");
            }

            return new ExpressionData { Type = inner.Type, Value = value, Constant = inner.Constant, Addressable = false, };
        }

        private ExpressionData GenerateCode_UnaryExpr_Bool (ExpressionData inner, SimpleUnaryExprType exprOp) {
            Debug.Assert (inner.Type->TypeTag == ES_TypeTag.Bool);
            Debug.Assert (inner.Value is not null);

            ExpressionSyntax value;
            switch (exprOp) {
                case SimpleUnaryExprType.LogicalNot:
                    value = PrefixUnaryExpression (SyntaxKind.LogicalNotExpression, inner.Value);
                    break;

                default:
                    throw new NotImplementedException ("Operation not implemented yet.");
            }

            return new ExpressionData { Type = inner.Type, Value = value, Constant = inner.Constant, Addressable = false, };
        }

        private ExpressionData GenerateCode_UnaryExpr_Float (ExpressionData inner, SimpleUnaryExprType exprOp) {
            Debug.Assert (inner.Type->TypeTag == ES_TypeTag.Float);
            Debug.Assert (inner.Value is not null);

            ExpressionSyntax value;
            switch (exprOp) {
                case SimpleUnaryExprType.Negative:
                    value = PrefixUnaryExpression (SyntaxKind.UnaryMinusExpression, inner.Value);
                    break;
                case SimpleUnaryExprType.Positive:
                    value = inner.Value;
                    break;

                default:
                    throw new NotImplementedException ("Operation not implemented yet.");
            }

            return new ExpressionData { Type = inner.Type, Value = value, Constant = inner.Constant, Addressable = false, };
        }

        private ExpressionData GenerateCode_UnaryExpr_Ref (ExpressionData inner, SimpleUnaryExprType exprOp) {
            Debug.Assert (inner.Type->TypeTag == ES_TypeTag.Reference);
            Debug.Assert (inner.Value is not null);

            var innerRef = (ES_ReferenceData*) inner.Type;

            ES_TypeInfo* finalType;
            bool isAddressable;

            ExpressionSyntax value;
            switch (exprOp) {
                case SimpleUnaryExprType.Dereference:
                    value = PrefixUnaryExpression (SyntaxKind.PointerIndirectionExpression, inner.Value);
                    finalType = innerRef->PointedType;
                    isAddressable = true;
                    break;

                default:
                    throw new NotImplementedException ("Operation not implemented yet.");
            }

            return new ExpressionData { Type = finalType, Value = value, Constant = false, Addressable = isAddressable, };
        }
    }
}
