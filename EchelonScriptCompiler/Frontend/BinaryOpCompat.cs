/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Frontend {
    public unsafe sealed partial class CompilerFrontend {
        public static bool BinaryOpCompat (
            EchelonScriptEnvironment env,
            ES_TypeInfo* lhsType, ES_TypeInfo* rhsType,
            SimpleBinaryExprType exprType,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            finalType = env.TypeUnknownValue;

            if (lhsType->TypeTag == ES_TypeTag.UNKNOWN || rhsType->TypeTag == ES_TypeTag.UNKNOWN) {
                isConst = false;
                return true;
            }

            if (lhsType->TypeTag == ES_TypeTag.Null || rhsType->TypeTag == ES_TypeTag.Null)
                return BinaryOpCompat_Null (env, lhsType, rhsType, exprType, out finalType, out isConst);

            if (lhsType->TypeTag == ES_TypeTag.Int && rhsType->TypeTag == ES_TypeTag.Int)
                return BinaryOpCompat_IntInt (env, lhsType, rhsType, exprType, out finalType, out isConst);

            if (lhsType->TypeTag == ES_TypeTag.Bool && rhsType->TypeTag == ES_TypeTag.Bool)
                return BinaryOpCompat_BoolBool (env, lhsType, rhsType, exprType, out finalType, out isConst);

            if (lhsType->TypeTag == ES_TypeTag.Float && rhsType->TypeTag == ES_TypeTag.Float)
                return BinaryOpCompat_FloatFloat (env, lhsType, rhsType, exprType, out finalType, out isConst);

            if (lhsType->TypeTag == ES_TypeTag.Float && rhsType->TypeTag == ES_TypeTag.Int)
                return BinaryOpCompat_FloatInt (env, lhsType, rhsType, exprType, out finalType, out isConst);

            if (lhsType->TypeTag == ES_TypeTag.Reference && rhsType->TypeTag == ES_TypeTag.Reference)
                return BinaryOpCompat_RefRef (env, lhsType, rhsType, exprType, out finalType, out isConst);

            if (lhsType->TypeTag == ES_TypeTag.Array && rhsType->TypeTag == ES_TypeTag.Array)
                return BinaryOpCompat_ArrayArray (env, lhsType, rhsType, exprType, out finalType, out isConst);

            isConst = false;
            return false;
        }

        private static bool BinaryOpCompat_Null (
            EchelonScriptEnvironment env,
            ES_TypeInfo* lhsType, ES_TypeInfo* rhsType,
            SimpleBinaryExprType exprType,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (lhsType->TypeTag == ES_TypeTag.Null || rhsType->TypeTag == ES_TypeTag.Null);

            switch (exprType) {
                case SimpleBinaryExprType.Equals:
                case SimpleBinaryExprType.NotEquals:
                    if (!lhsType->IsReferenceType ())
                        goto default;

                    finalType = env.TypeBool;
                    isConst = false;
                    return true;

                case SimpleBinaryExprType.Assign:
                    isConst = false;
                    if (lhsType->IsReferenceType ())
                        finalType = lhsType;
                    else if (rhsType->IsReferenceType ()) {
                        finalType = env.TypeUnknownValue;
                        return false;
                    } else
                        goto default;

                    return true;

                default:
                    finalType = env.TypeUnknownValue;
                    isConst = false;
                    return false;
            }
        }

        private static bool BinaryOpCompat_IntInt (
            EchelonScriptEnvironment env,
            ES_TypeInfo* lhsType, ES_TypeInfo* rhsType,
            SimpleBinaryExprType exprType,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (lhsType->TypeTag == ES_TypeTag.Int);
            Debug.Assert (rhsType->TypeTag == ES_TypeTag.Int);

            var lhsIntType = (ES_IntTypeData*) lhsType;
            var rhsIntType = (ES_IntTypeData*) rhsType;

            finalType = env.TypeUnknownValue;

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
                if (rhsIntType->Unsigned)
                    return false;

                if (rhsIntType->IntSize > ES_IntSize.Int32)
                    return false;

                isCompatible = true;
            } else if (lhsIntType->Unsigned == rhsIntType->Unsigned)
                isCompatible = true;
            else
                isCompatible = false;

            if (isCompatible) {
                if (exprType.IsAssignment ()) {
                    if (lhsIntType->IntSize < rhsIntType->IntSize)
                        return false;

                    finalType = lhsType;
                } else if (!exprType.IsComparison ()) {
                    if (lhsIntType->IntSize >= rhsIntType->IntSize)
                        finalType = lhsType;
                    else
                        finalType = rhsType;
                } else
                    finalType = env.TypeBool;
            }

            return isCompatible;
        }

        private static bool BinaryOpCompat_BoolBool (
            EchelonScriptEnvironment env,
            ES_TypeInfo* lhsType, ES_TypeInfo* rhsType,
            SimpleBinaryExprType exprType,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (lhsType->TypeTag == ES_TypeTag.Bool);
            Debug.Assert (rhsType->TypeTag == ES_TypeTag.Bool);

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
                    finalType = env.TypeUnknownValue;
                    isConst = false;
                    return false;
            }

            finalType = env.TypeBool;
            return true;
        }

        private static bool BinaryOpCompat_FloatFloat (
            EchelonScriptEnvironment env,
            ES_TypeInfo* lhsType, ES_TypeInfo* rhsType,
            SimpleBinaryExprType exprType,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (lhsType->TypeTag == ES_TypeTag.Float);
            Debug.Assert (rhsType->TypeTag == ES_TypeTag.Float);

            var lhsFloatType = (ES_FloatTypeData*) lhsType;
            var rhsFloatType = (ES_FloatTypeData*) rhsType;

            finalType = env.TypeUnknownValue;

            if (lhsFloatType->FloatSize != rhsFloatType->FloatSize) {
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

            finalType = !exprType.IsComparison () ? lhsType : env.TypeBool;

            return true;
        }

        private static bool BinaryOpCompat_FloatInt (
            EchelonScriptEnvironment env,
            ES_TypeInfo* lhsType, ES_TypeInfo* rhsType,
            SimpleBinaryExprType exprType,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (lhsType->TypeTag == ES_TypeTag.Float);
            Debug.Assert (rhsType->TypeTag == ES_TypeTag.Int);

            finalType = env.TypeUnknownValue;

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

        private static bool BinaryOpCompat_RefRef (
            EchelonScriptEnvironment env,
            ES_TypeInfo* lhsType, ES_TypeInfo* rhsType,
            SimpleBinaryExprType exprType,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (lhsType->TypeTag == ES_TypeTag.Reference);
            Debug.Assert (rhsType->TypeTag == ES_TypeTag.Reference);

            finalType = env.TypeUnknownValue;
            isConst = false;

            if (lhsType != rhsType)
                return false;

            switch (exprType) {
                case SimpleBinaryExprType.Assign:
                    finalType = lhsType;
                    break;

                case SimpleBinaryExprType.Equals:
                case SimpleBinaryExprType.NotEquals:
                    finalType = env.TypeBool;
                    break;

                default:
                    return false;
            }

            return true;
        }

        private static bool BinaryOpCompat_ArrayArray (
            EchelonScriptEnvironment env,
            ES_TypeInfo* lhsType, ES_TypeInfo* rhsType,
            SimpleBinaryExprType exprType,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (lhsType->TypeTag == ES_TypeTag.Array);
            Debug.Assert (rhsType->TypeTag == ES_TypeTag.Array);

            finalType = env.TypeUnknownValue;
            isConst = false;

            var lhsArr = (ES_ArrayTypeData*) lhsType;
            var rhsArr = (ES_ArrayTypeData*) rhsType;

            if (lhsType != rhsType)
                return false;

            switch (exprType) {
                case SimpleBinaryExprType.Assign:
                    finalType = lhsType;
                    break;

                case SimpleBinaryExprType.Concatenation:
                case SimpleBinaryExprType.AssignConcatenate:
                    if (lhsArr->DimensionsCount != 1)
                        goto default;

                    finalType = lhsType;
                    break;

                case SimpleBinaryExprType.Equals:
                case SimpleBinaryExprType.NotEquals:
                    finalType = env.TypeBool;
                    break;

                default:
                    return false;
            }

            return true;
        }
    }
}
