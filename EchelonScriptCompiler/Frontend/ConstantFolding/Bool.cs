/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using EchelonScriptCompiler.CompilerCommon;

namespace EchelonScriptCompiler.Frontend {
    internal unsafe static partial class Compiler_ConstantFolding {
        private static void FoldExpression_Binary_BoolBool_Comp (
            ref ES_AstExpression expr, SimpleBinaryExprType op,
            ES_AstBooleanConstantExpression lhs, ES_AstBooleanConstantExpression rhs
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

                default:
                    return;
            }

            expr = new ES_AstBooleanConstantExpression (finalValue, expr);
        }

        private static void FoldExpression_Binary_BoolBool_Arithmetic (
            ref ES_AstExpression expr, SimpleBinaryExprType op,
            ES_AstBooleanConstantExpression lhs, ES_AstBooleanConstantExpression rhs
        ) {
            Debug.Assert (!op.IsComparison () && !op.IsBitShift ());

            bool finalValue;
            switch (op) {
                // Bit ops
                case SimpleBinaryExprType.BitAnd:
                    finalValue = lhs.Value & rhs.Value;
                    break;
                case SimpleBinaryExprType.BitOr:
                    finalValue = lhs.Value | rhs.Value;
                    break;
                case SimpleBinaryExprType.BitXor:
                    finalValue = lhs.Value ^ rhs.Value;
                    break;

                default:
                    return;
            }

            expr = new ES_AstBooleanConstantExpression (finalValue, expr);
        }
    }
}
