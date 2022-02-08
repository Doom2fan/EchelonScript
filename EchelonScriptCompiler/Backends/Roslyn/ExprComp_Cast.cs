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
using EchelonScriptCompiler.CompilerCommon.IR;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private static ExpressionData CompileExpression_Cast (
            ref PassData passData,
            ref FunctionData funcData,
            ESIR_CastExpression expr
        ) {
            var origExpr = CompileExpression (ref passData, ref funcData, expr.Expression);
            var destType = expr.DestType.Pointer;

            var origVal = origExpr.Value!;
            Debug.Assert (origVal is not null);

            ES_TypeInfo* retType;
            ExpressionSyntax retValue;
            switch (origExpr.Type->TypeTag) {
                case ES_TypeTag.Int: {
                    var intSrc = (ES_IntTypeData*) origExpr.Type;

                    if (destType->TypeTag == ES_TypeTag.Int) {
                        var intDest = (ES_IntTypeData*) destType;

                        if (intDest->IntSize == intSrc->IntSize && intDest->Unsigned == intSrc->Unsigned) {
                            origExpr.Writable = false;
                            return origExpr;
                        }

                        var roslynDestType = GetIntType (intDest->IntSize, intDest->Unsigned);
                        retValue = CastExpression (roslynDestType, origVal);
                    } else if (destType->TypeTag == ES_TypeTag.Float) {
                        var floatDest = (ES_FloatTypeData*) destType;

                        var roslynDestType = GetFloatType (floatDest->FloatSize);
                        retValue = CastExpression (roslynDestType, origVal);
                    } else
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    retType = destType;

                    break;
                }

                case ES_TypeTag.Float: {
                    var floatSrc = (ES_FloatTypeData*) origExpr.Type;

                    if (destType->TypeTag == ES_TypeTag.Float) {
                        var floatDest = (ES_FloatTypeData*) destType;

                        if (floatSrc->FloatSize == floatDest->FloatSize) {
                            origExpr.Writable = false;
                            return origExpr;
                        }

                        var roslynDestType = GetFloatType (floatDest->FloatSize);
                        retValue = CastExpression (roslynDestType, origVal);
                    } else if (destType->TypeTag == ES_TypeTag.Int) {
                        var intDest = (ES_IntTypeData*) destType;

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

            return new ExpressionData { Type = retType, Value = retValue, };
        }
    }
}
