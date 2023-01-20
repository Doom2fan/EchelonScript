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
using EchelonScript.Common.Data.Types;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScript.Compiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
#if false
    private static bool IsTypeEquivalent (ES_TypeInfo* srcType, ES_TypeInfo* dstType) {
        if (srcType->TypeTag == ES_TypeTag.Const || srcType->TypeTag == ES_TypeTag.Immutable)
            srcType = ((ES_ConstData*) srcType)->InnerType;

        if (dstType->TypeTag == ES_TypeTag.Const || dstType->TypeTag == ES_TypeTag.Immutable)
            dstType = ((ES_ConstData*) dstType)->InnerType;

        if (srcType->TypeTag != dstType->TypeTag)
            return false;

        switch (srcType->TypeTag) {
            case ES_TypeTag.UNKNOWN:
            case ES_TypeTag.Null:
            case ES_TypeTag.Void:
            case ES_TypeTag.Bool:
                return true;

            case ES_TypeTag.Int: {
                var srcData = (ES_IntData*) srcType;
                var dstData = (ES_IntData*) dstType;

                return srcData->IntSize == dstData->IntSize && srcData->Unsigned == dstData->Unsigned;
            }

            case ES_TypeTag.Float: {
                var srcData = (ES_FloatData*) srcType;
                var dstData = (ES_FloatData*) dstType;

                return srcData->FloatSize == dstData->FloatSize;
            }

            case ES_TypeTag.FuncPrototype:
            case ES_TypeTag.Struct:
            case ES_TypeTag.Class:
            case ES_TypeTag.Interface:
                return srcType == dstType;

            case ES_TypeTag.Reference: {
                var srcData = (ES_ReferenceData*) srcType;
                var dstData = (ES_ReferenceData*) dstType;

                return IsTypeEquivalent (srcData->PointedType, dstData->PointedType);
            }

            case ES_TypeTag.Array: {
                var srcData = (ES_ArrayData*) srcType;
                var dstData = (ES_ArrayData*) dstType;

                return (
                    srcData->Rank == dstData->Rank &&
                    IsTypeEquivalent (srcData->ElementType, dstData->ElementType)
                );
            }

            case ES_TypeTag.Enum:
                throw new NotImplementedException ("[TODO] Type not implemented yet.");

            case ES_TypeTag.Const:
            case ES_TypeTag.Immutable:
                Debug.Fail ("This should never be reached.");
                throw new CompilationException (ES_BackendErrors.FrontendError);

            default:
                throw new NotImplementedException ("Type not implemented yet.");
        }
    }

    private static ExpressionData CompileExpression_Cast (
        ref PassData passData,
        ref FunctionData funcData,
        ESIR_CastExpression expr
    ) {
        var origExpr = CompileExpression (ref passData, ref funcData, expr.Expression);
        var destType = StripFirstConst (expr.DestType.Pointer);

        StripFirstConst (ref origExpr);

        var origVal = origExpr.Value!;
        Debug.Assert (origVal is not null);

        ES_TypeInfo* retType;
        ExpressionSyntax retValue;
        if (IsTypeEquivalent (origExpr.Type, destType)) {
            retType = destType;

            var origTypeRoslyn = GetRoslynType (origExpr.Type);
            var destTypeRoslyn = GetRoslynType (destType);
            if (!origTypeRoslyn.IsEquivalentTo (destTypeRoslyn))
                retValue = CastExpression (destTypeRoslyn, origVal);
            else
                retValue = origVal;
        } else {
            switch (origExpr.Type->TypeTag) {
                case ES_TypeTag.Int: {
                    var intSrc = (ES_IntData*) origExpr.Type;

                    if (destType->TypeTag == ES_TypeTag.Int) {
                        var intDest = (ES_IntData*) destType;

                        if (intDest->IntSize == intSrc->IntSize && intDest->Unsigned == intSrc->Unsigned) {
                            origExpr.Writable = false;
                            return origExpr;
                        }

                        var roslynDestType = GetIntType (intDest->IntSize, intDest->Unsigned);
                        retValue = CastExpression (roslynDestType, origVal);
                    } else if (destType->TypeTag == ES_TypeTag.Float) {
                        var floatDest = (ES_FloatData*) destType;

                        var roslynDestType = GetFloatType (floatDest->FloatSize);
                        retValue = CastExpression (roslynDestType, origVal);
                    } else
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    retType = destType;

                    break;
                }

                case ES_TypeTag.Float: {
                    var floatSrc = (ES_FloatData*) origExpr.Type;

                    if (destType->TypeTag == ES_TypeTag.Float) {
                        var floatDest = (ES_FloatData*) destType;

                        if (floatSrc->FloatSize == floatDest->FloatSize) {
                            origExpr.Writable = false;
                            return origExpr;
                        }

                        var roslynDestType = GetFloatType (floatDest->FloatSize);
                        retValue = CastExpression (roslynDestType, origVal);
                    } else if (destType->TypeTag == ES_TypeTag.Int) {
                        var intDest = (ES_IntData*) destType;

                        var roslynDestType = GetIntType (intDest->IntSize, intDest->Unsigned);
                        retValue = CastExpression (roslynDestType, origVal);
                    } else
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    retType = destType;

                    break;
                }

                default:
                    throw new NotImplementedException ("Cast not implemented.");
            }
        }

        return new ExpressionData { Type = retType, Value = retValue, };
    }
#endif
}
