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
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Frontend {
    public unsafe sealed partial class CompilerFrontend {
        internal static bool UnaryOpCompat (
            EchelonScriptEnvironment env,
            ES_TypeInfo* exprType,
            SimpleUnaryExprType op,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            switch (exprType->TypeTag) {
                case ES_TypeTag.Int:
                    return UnaryOpCompat_Int (env, exprType, op, out finalType, out isConst);

                case ES_TypeTag.Bool:
                    return UnaryOpCompat_Bool (env, exprType, op, out finalType, out isConst);

                case ES_TypeTag.Float:
                    return UnaryOpCompat_Float (env, exprType, op, out finalType, out isConst);

                case ES_TypeTag.Reference:
                    return UnaryOpCompat_Ref (env, exprType, op, out finalType, out isConst);

                default:
                    finalType = env.TypeUnknownValue;
                    isConst = false;
                    return false;
            }
        }

        private static bool UnaryOpCompat_Int (
            EchelonScriptEnvironment env,
            ES_TypeInfo* exprType,
            SimpleUnaryExprType op,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (exprType->TypeTag == ES_TypeTag.Int);

            switch (op) {
                case SimpleUnaryExprType.Positive:
                case SimpleUnaryExprType.BitNot:
                    finalType = exprType;
                    isConst = true;
                    return true;

                case SimpleUnaryExprType.Negative: {
                    var intData = (ES_IntTypeData*) exprType;

                    if (!intData->Unsigned) {
                        finalType = exprType;
                        isConst = true;
                        return true;
                    } else {
                        finalType = env.TypeUnknownValue;
                        isConst = false;
                        return false;
                    }
                }

                case SimpleUnaryExprType.Dereference:
                case SimpleUnaryExprType.LogicalNot:
                    finalType = env.TypeUnknownValue;
                    isConst = false;
                    return false;

                default:
                    throw new NotImplementedException ("Operation not implemented.");
            }
        }

        private static bool UnaryOpCompat_Bool (
            EchelonScriptEnvironment env,
            ES_TypeInfo* exprType,
            SimpleUnaryExprType op,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (exprType->TypeTag == ES_TypeTag.Bool);

            switch (op) {
                case SimpleUnaryExprType.LogicalNot:
                    finalType = exprType;
                    isConst = true;
                    return true;

                case SimpleUnaryExprType.Positive:
                case SimpleUnaryExprType.Negative:
                case SimpleUnaryExprType.BitNot:
                case SimpleUnaryExprType.Dereference:
                    finalType = env.TypeUnknownValue;
                    isConst = false;
                    return false;

                default:
                    throw new NotImplementedException ("Operation not implemented.");
            }
        }

        private static bool UnaryOpCompat_Float (
            EchelonScriptEnvironment env,
            ES_TypeInfo* exprType,
            SimpleUnaryExprType op,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (exprType->TypeTag == ES_TypeTag.Float);

            switch (op) {
                case SimpleUnaryExprType.Positive:
                case SimpleUnaryExprType.Negative:
                    finalType = exprType;
                    isConst = true;
                    return true;

                case SimpleUnaryExprType.LogicalNot:
                case SimpleUnaryExprType.BitNot:
                case SimpleUnaryExprType.Dereference:
                    finalType = env.TypeUnknownValue;
                    isConst = false;
                    return false;

                default:
                    throw new NotImplementedException ("Operation not implemented.");
            }
        }

        private static bool UnaryOpCompat_Ref (
            EchelonScriptEnvironment env,
            ES_TypeInfo* exprType,
            SimpleUnaryExprType op,
            out ES_TypeInfo* finalType,
            out bool isConst
        ) {
            Debug.Assert (exprType->TypeTag == ES_TypeTag.Reference);

            var refType = (ES_ReferenceData*) exprType;

            switch (op) {
                case SimpleUnaryExprType.Dereference:
                    finalType = refType->PointedType;
                    isConst = false;
                    return true;

                case SimpleUnaryExprType.Positive:
                case SimpleUnaryExprType.Negative:
                case SimpleUnaryExprType.LogicalNot:
                case SimpleUnaryExprType.BitNot:
                    finalType = env.TypeUnknownValue;
                    isConst = false;
                    return false;

                default:
                    throw new NotImplementedException ("Operation not implemented.");
            }
        }
    }
}
