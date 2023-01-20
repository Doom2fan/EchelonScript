/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using EchelonScript.Common.Data.Types;

namespace EchelonScript.Compiler.Frontend;

internal ref partial struct CompileData {
#if false
    public bool BinaryOpCompat (
        ESC_TypeRef lhsType, ESC_TypeRef rhsType,
        SimpleBinaryExprType exprType,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        finalType = GetUnknownType (ESC_Constness.Mutable);

        if (lhsType.Type is ESC_TypeUnknown || rhsType.Type is ESC_TypeUnknown) {
            isConst = false;
            return true;
        }

        if (lhsType.Type is ESC_TypeNull || rhsType.Type is ESC_TypeNull)
            return BinaryOpCompat_Null (lhsType, rhsType, exprType, out finalType, out isConst);

        return (lhsType.Type, rhsType.Type) switch {
            (ESC_TypeInt, ESC_TypeInt) =>
                BinaryOpCompat_IntInt (lhsType, rhsType, exprType, out finalType, out isConst),

            (ESC_TypeBool, ESC_TypeBool) =>
                BinaryOpCompat_BoolBool (lhsType, rhsType, exprType, out finalType, out isConst),

            (ESC_TypeFloat, ESC_TypeFloat) =>
                BinaryOpCompat_FloatFloat (lhsType, rhsType, exprType, out finalType, out isConst),

            (ESC_TypeFloat, ESC_TypeInt) =>
                BinaryOpCompat_FloatInt (lhsType, rhsType, exprType, out finalType, out isConst),

            (ESC_TypeReference, ESC_TypeReference) =>
                BinaryOpCompat_RefRef (lhsType, rhsType, exprType, out finalType, out isConst),

            (ESC_TypeArray, ESC_TypeArray) =>
                BinaryOpCompat_ArrayArray (lhsType, rhsType, exprType, out finalType, out isConst),

            _ => isConst = false,
        };
    }

    private bool BinaryOpCompat_Null (
        ESC_TypeRef lhsType, ESC_TypeRef rhsType,
        SimpleBinaryExprType exprType,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (lhsType.Type is not null);
        Debug.Assert (rhsType.Type is not null);
        Debug.Assert (lhsType.Type is ESC_TypeNull || rhsType.Type is ESC_TypeNull);

        switch (exprType) {
            case SimpleBinaryExprType.Equals:
            case SimpleBinaryExprType.NotEquals:
                if (!lhsType.Type.IsReferenceType ())
                    goto default;

                finalType = GetBoolType (ESC_Constness.Const);
                isConst = false;
                return true;

            case SimpleBinaryExprType.Assign:
                isConst = false;
                if (lhsType.Type.IsReferenceType ())
                    finalType = lhsType;
                else if (rhsType.Type.IsReferenceType ()) {
                    finalType = GetUnknownType (ESC_Constness.Mutable);
                    return false;
                } else
                    goto default;

                return true;

            default:
                finalType = GetUnknownType (ESC_Constness.Mutable);
                isConst = false;
                return false;
        }
    }

    private bool BinaryOpCompat_IntInt (
        ESC_TypeRef lhsType, ESC_TypeRef rhsType,
        SimpleBinaryExprType exprType,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (lhsType.Type is ESC_TypeInt);
        Debug.Assert (rhsType.Type is ESC_TypeInt);

        var lhsIntType = (ESC_TypeInt) lhsType.Type;
        var rhsIntType = (ESC_TypeInt) rhsType.Type;

        finalType = GetUnknownType (ESC_Constness.Mutable);

        switch (exprType) {
            case SimpleBinaryExprType.Power:
            case SimpleBinaryExprType.Multiply:
            case SimpleBinaryExprType.Divide:
            case SimpleBinaryExprType.Modulo:
            case SimpleBinaryExprType.Add:
            case SimpleBinaryExprType.Subtract:
            case SimpleBinaryExprType.BitAnd:
            case SimpleBinaryExprType.BitXor:
            case SimpleBinaryExprType.BitOr:
                isConst = true;
                break;

            case SimpleBinaryExprType.ShiftLeft:
            case SimpleBinaryExprType.ShiftRight:
            case SimpleBinaryExprType.ShiftRightUnsigned:
                isConst = true;
                break;

            case SimpleBinaryExprType.LesserThan:
            case SimpleBinaryExprType.GreaterThan:
            case SimpleBinaryExprType.LesserThanEqual:
            case SimpleBinaryExprType.GreaterThanEqual:
            case SimpleBinaryExprType.Equals:
            case SimpleBinaryExprType.NotEquals:
                isConst = true;
                break;

            case SimpleBinaryExprType.Assign:
            case SimpleBinaryExprType.AssignAdd:
            case SimpleBinaryExprType.AssignSubtract:
            case SimpleBinaryExprType.AssignMultiply:
            case SimpleBinaryExprType.AssignDivide:
            case SimpleBinaryExprType.AssignModulo:
            case SimpleBinaryExprType.AssignPower:
            case SimpleBinaryExprType.AssignBitAnd:
            case SimpleBinaryExprType.AssignBitOr:
            case SimpleBinaryExprType.AssignXor:

            case SimpleBinaryExprType.AssignShiftLeft:
            case SimpleBinaryExprType.AssignShiftRight:
            case SimpleBinaryExprType.AssignShiftRightUnsigned:
                isConst = false;
                break;

            default:
                isConst = false;
                return false;
        }

        bool isCompatible;

        if (exprType.IsBitShift ()) {
            if (rhsIntType.Unsigned)
                return false;

            if (rhsIntType.Size > ES_IntSize.Int32)
                return false;

            isCompatible = true;
        } else if (lhsIntType.Unsigned == rhsIntType.Unsigned)
            isCompatible = true;
        else
            isCompatible = false;

        if (isCompatible) {
            if (exprType.IsAssignment ()) {
                if (lhsIntType.Size < rhsIntType.Size)
                    return false;

                finalType = lhsType;
            } else if (!exprType.IsComparison ()) {
                if (lhsIntType.Size >= rhsIntType.Size)
                    finalType = lhsType;
                else
                    finalType = rhsType;
            } else
                finalType = GetBoolType (ESC_Constness.Const);
        }

        return isCompatible;
    }

    private bool BinaryOpCompat_BoolBool (
        ESC_TypeRef lhsType, ESC_TypeRef rhsType,
        SimpleBinaryExprType exprType,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (lhsType.Type is ESC_TypeBool);
        Debug.Assert (rhsType.Type is ESC_TypeBool);

        switch (exprType) {
            case SimpleBinaryExprType.BitAnd:
            case SimpleBinaryExprType.BitXor:
            case SimpleBinaryExprType.BitOr:
                isConst = true;
                break;

            case SimpleBinaryExprType.Equals:
            case SimpleBinaryExprType.NotEquals:
                isConst = true;
                break;

            case SimpleBinaryExprType.LogicalAnd:
            case SimpleBinaryExprType.LogicalOr:
                isConst = true;
                break;

            case SimpleBinaryExprType.Assign:
            case SimpleBinaryExprType.AssignBitAnd:
            case SimpleBinaryExprType.AssignBitOr:
            case SimpleBinaryExprType.AssignXor:
                isConst = false;
                break;

            default:
                finalType = GetUnknownType (ESC_Constness.Mutable);
                isConst = false;
                return false;
        }

        finalType = GetBoolType (ESC_Constness.Const);
        return true;
    }

    private bool BinaryOpCompat_FloatFloat (
        ESC_TypeRef lhsType, ESC_TypeRef rhsType,
        SimpleBinaryExprType exprType,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (lhsType.Type is ESC_TypeFloat);
        Debug.Assert (rhsType.Type is ESC_TypeFloat);

        var lhsFloatType = (ESC_TypeFloat) lhsType.Type;
        var rhsFloatType = (ESC_TypeFloat) rhsType.Type;

        finalType = GetUnknownType (ESC_Constness.Mutable);

        if (lhsFloatType.Size != rhsFloatType.Size) {
            isConst = false;
            return false;
        }

        switch (exprType) {
            case SimpleBinaryExprType.Power:
            case SimpleBinaryExprType.Multiply:
            case SimpleBinaryExprType.Divide:
            case SimpleBinaryExprType.Modulo:
            case SimpleBinaryExprType.Add:
            case SimpleBinaryExprType.Subtract:
                isConst = true;
                break;

            case SimpleBinaryExprType.LesserThan:
            case SimpleBinaryExprType.GreaterThan:
            case SimpleBinaryExprType.LesserThanEqual:
            case SimpleBinaryExprType.GreaterThanEqual:
            case SimpleBinaryExprType.Equals:
            case SimpleBinaryExprType.NotEquals:
                isConst = true;
                break;

            case SimpleBinaryExprType.Assign:
            case SimpleBinaryExprType.AssignAdd:
            case SimpleBinaryExprType.AssignSubtract:
            case SimpleBinaryExprType.AssignMultiply:
            case SimpleBinaryExprType.AssignDivide:
            case SimpleBinaryExprType.AssignModulo:
            case SimpleBinaryExprType.AssignPower:
                isConst = false;
                break;

            default:
                isConst = false;
                return false;
        }

        finalType = !exprType.IsComparison () ? lhsType : GetBoolType (ESC_Constness.Const);

        return true;
    }

    private bool BinaryOpCompat_FloatInt (
        ESC_TypeRef lhsType, ESC_TypeRef rhsType,
        SimpleBinaryExprType exprType,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (lhsType.Type is ESC_TypeFloat);
        Debug.Assert (rhsType.Type is ESC_TypeInt);

        finalType = GetUnknownType (ESC_Constness.Mutable);

        switch (exprType) {
            case SimpleBinaryExprType.Power:
                isConst = true;
                break;

            case SimpleBinaryExprType.AssignPower:
                isConst = false;
                break;

            default:
                isConst = false;
                return false;
        }

        finalType = lhsType;

        return true;
    }

    private bool BinaryOpCompat_RefRef (
        ESC_TypeRef lhsType, ESC_TypeRef rhsType,
        SimpleBinaryExprType exprType,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (lhsType.Type is ESC_TypeReference);
        Debug.Assert (rhsType.Type is ESC_TypeReference);

        finalType = GetUnknownType (ESC_Constness.Mutable);
        isConst = false;

        if (lhsType != rhsType)
            return false;

        switch (exprType) {
            case SimpleBinaryExprType.Assign:
                finalType = lhsType;
                break;

            case SimpleBinaryExprType.Equals:
            case SimpleBinaryExprType.NotEquals:
                finalType = GetBoolType (ESC_Constness.Const);
                break;

            default:
                return false;
        }

        return true;
    }

    private bool BinaryOpCompat_ArrayArray (
        ESC_TypeRef lhsType, ESC_TypeRef rhsType,
        SimpleBinaryExprType exprType,
        out ESC_TypeRef finalType,
        out bool isConst
    ) {
        Debug.Assert (lhsType.Type is ESC_TypeArray);
        Debug.Assert (rhsType.Type is ESC_TypeArray);

        finalType = GetUnknownType (ESC_Constness.Mutable);
        isConst = false;

        var lhsArr = (ESC_TypeArray) lhsType.Type;
        var rhsArr = (ESC_TypeArray) rhsType.Type;

        if (lhsType != rhsType)
            return false;

        switch (exprType) {
            case SimpleBinaryExprType.Assign:
                finalType = lhsType;
                break;

            case SimpleBinaryExprType.Concatenation:
            case SimpleBinaryExprType.AssignConcatenate:
                if (lhsArr.Rank != 1 || rhsArr.Rank != 1)
                    goto default;

                finalType = lhsType;
                break;

            case SimpleBinaryExprType.Equals:
            case SimpleBinaryExprType.NotEquals:
                finalType = GetBoolType (ESC_Constness.Const);
                break;

            default:
                return false;
        }

        return true;
    }
#endif
}
