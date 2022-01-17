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
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private ExpressionData GenerateCode_Cast (ExpressionData src, ES_TypeInfo* dst) {
            var ret = src;
            ret.Type = dst;
            ret.Constant = false;
            ret.Writable = false;

            var srcVal = src.Value;
            Debug.Assert (srcVal is not null);

            switch (src.Type->TypeTag) {
                case ES_TypeTag.Int: {
                    var intSrc = (ES_IntTypeData*) src.Type;

                    if (dst->TypeTag == ES_TypeTag.Int) {
                        var intDst = (ES_IntTypeData*) dst;

                        if (intDst->IntSize == intSrc->IntSize && intDst->Unsigned == intSrc->Unsigned) {
                            src.Writable = false;
                            return src;
                        }

                        var dstType = GetIntType (intDst->IntSize, intDst->Unsigned);
                        ret.Value = CastExpression (dstType, srcVal);
                    } else if (dst->TypeTag == ES_TypeTag.Float) {
                        var fltDst = (ES_FloatTypeData*) dst;

                        var dstType = GetFloatType (fltDst->FloatSize);
                        ret.Value = CastExpression (dstType, srcVal);
                    } else
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    break;
                }

                case ES_TypeTag.Float: {
                    var fltSrc = (ES_FloatTypeData*) src.Type;

                    if (dst->TypeTag == ES_TypeTag.Float) {
                        var fltDst = (ES_FloatTypeData*) dst;

                        if (fltSrc->FloatSize == fltDst->FloatSize) {
                            src.Writable = false;
                            return src;
                        }

                        var dstType = GetFloatType (fltDst->FloatSize);
                        ret.Value = CastExpression (dstType, srcVal);
                    } else if (dst->TypeTag == ES_TypeTag.Int) {
                        var intDst = (ES_IntTypeData*) dst;

                        var dstType = GetIntType (intDst->IntSize, intDst->Unsigned);
                        ret.Value = CastExpression (dstType, srcVal);
                    } else
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    break;
                }

                default:
                    throw new NotImplementedException ("Cast not implemented.");
            }

            return ret;
        }
    }
}
