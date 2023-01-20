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

namespace EchelonScript.Compiler.Frontend;

internal ref partial struct CompileData {
#if false
    public bool UnaryOpCompat (
        ESC_TypeRef exprType,
        SimpleUnaryExprType op,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        finalType = GetUnknownType (ESC_Constness.Mutable);
        isConst = false;

        return exprType.Type switch {
            ESC_TypeInt => UnaryOpCompat_Int (exprType, op, out finalType, out isConst),
            ESC_TypeBool => UnaryOpCompat_Bool (exprType, op, out finalType, out isConst),
            ESC_TypeFloat => UnaryOpCompat_Float (exprType, op, out finalType, out isConst),
            ESC_TypeReference => UnaryOpCompat_Ref (exprType, op, out finalType, out isConst),

            _ => false,
        };
    }

    private bool UnaryOpCompat_Int (
        ESC_TypeRef exprType,
        SimpleUnaryExprType op,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (exprType.Type is ESC_TypeInt);

        switch (op) {
            case SimpleUnaryExprType.Positive:
            case SimpleUnaryExprType.BitNot:
                finalType = exprType;
                isConst = true;
                return true;

            case SimpleUnaryExprType.Negative: {
                var intData = (ESC_TypeInt) exprType.Type;

                if (!intData.Unsigned) {
                    finalType = exprType;
                    isConst = true;
                    return true;
                } else {
                    finalType = GetUnknownType (ESC_Constness.Mutable);
                    isConst = false;
                    return false;
                }
            }

            case SimpleUnaryExprType.Dereference:
            case SimpleUnaryExprType.LogicalNot:
                finalType = GetUnknownType (ESC_Constness.Mutable);
                isConst = false;
                return false;

            default:
                throw new NotImplementedException ("Operation not implemented.");
        }
    }

    private bool UnaryOpCompat_Bool (
        ESC_TypeRef exprType,
        SimpleUnaryExprType op,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (exprType.Type is ESC_TypeBool);

        switch (op) {
            case SimpleUnaryExprType.LogicalNot:
                finalType = exprType;
                isConst = true;
                return true;

            case SimpleUnaryExprType.Positive:
            case SimpleUnaryExprType.Negative:
            case SimpleUnaryExprType.BitNot:
            case SimpleUnaryExprType.Dereference:
                finalType = GetUnknownType (ESC_Constness.Mutable);
                isConst = false;
                return false;

            default:
                throw new NotImplementedException ("Operation not implemented.");
        }
    }

    private bool UnaryOpCompat_Float (
        ESC_TypeRef exprType,
        SimpleUnaryExprType op,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (exprType.Type is ESC_TypeFloat);

        switch (op) {
            case SimpleUnaryExprType.Positive:
            case SimpleUnaryExprType.Negative:
                finalType = exprType;
                isConst = true;
                return true;

            case SimpleUnaryExprType.LogicalNot:
            case SimpleUnaryExprType.BitNot:
            case SimpleUnaryExprType.Dereference:
                finalType = GetUnknownType (ESC_Constness.Mutable);
                isConst = false;
                return false;

            default:
                throw new NotImplementedException ("Operation not implemented.");
        }
    }

    private bool UnaryOpCompat_Ref (
        ESC_TypeRef exprType,
        SimpleUnaryExprType op,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (exprType.Type is ESC_TypeReference);

        var refType = (ESC_TypeReference) exprType.Type;

        switch (op) {
            case SimpleUnaryExprType.Dereference:
                finalType = refType.PointedType;
                isConst = false;
                return true;

            case SimpleUnaryExprType.Positive:
            case SimpleUnaryExprType.Negative:
            case SimpleUnaryExprType.LogicalNot:
            case SimpleUnaryExprType.BitNot:
                finalType = GetUnknownType (ESC_Constness.Mutable);
                isConst = false;
                return false;

            default:
                throw new NotImplementedException ("Operation not implemented.");
        }
    }
#endif
}
