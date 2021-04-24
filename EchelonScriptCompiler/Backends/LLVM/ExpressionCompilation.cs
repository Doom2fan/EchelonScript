﻿/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Text;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Frontend;
using LLVMSharp.Interop;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Backends.LLVMBackend {
    public unsafe sealed partial class LLVMCompilerBackend {
        public struct ExpressionData {
            public ES_AstExpression Expr;
            public ES_TypeInfo* Type;
            public ES_TypeInfo* TypeInfo;
            public ES_FunctionData* Function;

            public LLVMValueRef Value;

            public bool Constant;
            public bool Addressable;
        }

        private bool GenerateCode_EnsureCompat (ES_TypeInfo* destType, ES_TypeInfo* givenType, ReadOnlySpan<char> src, ES_AstNodeBounds bounds) {
            /* if (!GenerateCode_MustBeCompat (destType, givenType)) {
                throw new CompilationException (ES_BackendErrors.FrontendError);
            }*/

            return true;
        }

        private ExpressionData GenerateCode_Expression (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_AstExpression expr, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (expr is not null);

            var idPool = env!.IdPool;
            var typeUnkn = env.TypeUnknownValue;

            switch (expr) {
                case ES_AstParenthesisExpression parenExpr:
                    return GenerateCode_Expression (ref transUnit, symbols, src, parenExpr.Inner, expectedType);

                #region Primary expressions

                case ES_AstFunctionCallExpression funcCallExpr:
                    return GenerateCode_Expression_FunctionCall (ref transUnit, symbols, src, funcCallExpr, expectedType);

                case ES_AstIndexingExpression indexExpr: {
                    throw new NotImplementedException ();
                    /*GenerateCode_Expression (ref transUnit, symbols, src, indexExpr.IndexedExpression);
                    foreach (var rank in indexExpr.RankExpressions) {
                        if (rank is not null)
                            GenerateCode_Expression (ref transUnit, symbols, src, rank);
                    }*/
                }

                case ES_AstNewExpression newExpr: {
                    throw new NotImplementedException ();
                    /*if (newExpr.TypeDeclaration is not null)
                        newExpr.TypeDeclaration = GenerateASTTypeRef (ref transUnit, symbols, src, newExpr.TypeDeclaration);

                    foreach (var args in newExpr.Arguments)
                        GenerateCode_Expression (ref transUnit, symbols, src, args.ValueExpression);*/
                }

                case ES_AstIntegerLiteralExpression intLitExpr: {
                    var type = envBuilder!.DetermineIntLiteralType (intLitExpr, expectedType);
                    var intType = (ES_IntTypeData*) type;

                    LLVMValueRef value;

                    bool unsigned = intType->Unsigned;
                    ulong constVal = intLitExpr.Value;
                    switch (intType->IntSize) {
                        case ES_IntSize.Int8:
                            value = LLVMValueRef.CreateConstInt (contextRef.Int8Type, constVal, !unsigned);
                            break;
                        case ES_IntSize.Int16:
                            value = LLVMValueRef.CreateConstInt (contextRef.Int16Type, constVal, !unsigned);
                            break;
                        case ES_IntSize.Int32:
                            value = LLVMValueRef.CreateConstInt (contextRef.Int32Type, constVal, !unsigned);
                            break;
                        case ES_IntSize.Int64:
                            value = LLVMValueRef.CreateConstInt (contextRef.Int64Type, constVal, !unsigned);
                            break;

                        default:
                            throw new NotImplementedException ();
                    }

                    return new ExpressionData { Expr = expr, Type = type, Value = value, Constant = true, Addressable = false };
                }

                case ES_AstBooleanLiteralExpression boolLitExpr: {
                    var value = LLVMValueRef.CreateConstInt (contextRef.Int1Type, boolLitExpr.Value ? 1uL : 0uL, false);
                    return new ExpressionData { Expr = expr, Type = env.TypeBool, Constant = true, Addressable = false };
                }

                case ES_AstFloatLiteralExpression floatLitExpr: {
                    var type = floatLitExpr.IsFloat ? env.TypeFloat32 : env.TypeFloat64;
                    var constVal = floatLitExpr.IsFloat ? floatLitExpr.ValueFloat : floatLitExpr.ValueDouble;
                    var floatType = (ES_FloatTypeData*) type;

                    var value = LLVM.ConstReal (GetFloatType (floatType->FloatSize), constVal);
                    return new ExpressionData { Expr = expr, Type = type, Value = value, Constant = true, Addressable = false };
                }

                case ES_AstStringLiteralExpression:
                    throw new NotImplementedException ();

                case ES_AstCharLiteralExpression:
                    throw new NotImplementedException ();

                case ES_AstNameExpression nameExpr: {
                    var id = idPool.GetIdentifier (nameExpr.Value.Text.Span);
                    var symbol = symbols.GetSymbol (id);

                    switch (symbol.Tag) {
                        case SymbolType.None:
                            throw new CompilationException (ES_BackendErrors.NonExistentSymbol);

                        case SymbolType.Variable: {
                            var varData = symbol.MatchVariable ();
                            return new ExpressionData { Expr = expr, Type = varData.Type, Value = varData.LLVMValue, Constant = false, Addressable = true };
                        }

                        case SymbolType.Type: {
                            if (expectedType is not null)
                                throw new CompilationException (ES_BackendErrors.FrontendError);

                            var type = symbol.MatchType ();
                            return new ExpressionData { Expr = expr, TypeInfo = type, Value = null, Constant = true, Addressable = true };
                        }

                        case SymbolType.Function: {
                            var func = symbol.MatchFunction ();
                            return new ExpressionData { Expr = expr, Function = func, Value = null, Constant = true, Addressable = true };
                        }

                        default:
                            throw new NotImplementedException ();
                    }
                }

                case ES_AstMemberAccessExpression memberAccessExpr:
                    throw new NotImplementedException ();

                #endregion

                case ES_AstIncDecExpression incDecExpr: {
                    var inner = GenerateCode_Expression (ref transUnit, symbols, src, incDecExpr.Inner, expectedType);

                    var ret = GenerateCode_IncDecExpression (inner, incDecExpr.Decrement, incDecExpr.Postfix);
                    ret.Expr = expr;
                    return ret;
                }

                #region Unary expressions

                case ES_AstSimpleUnaryExpression unaryExpr: {
                    var inner = GenerateCode_Expression (ref transUnit, symbols, src, unaryExpr.Inner, expectedType);

                    var ret = GenerateCode_UnaryExpr (inner, unaryExpr.ExpressionType);
                    ret.Expr = expr;
                    return ret;
                }

                case ES_AstCastExpression castExpr: {
                    var innerExpr = GenerateCode_Expression (ref transUnit, symbols, src, castExpr.InnerExpression, typeUnkn);
                    return GenerateCode_Cast (innerExpr, GetTypeRef (castExpr.DestinationType));
                }

                #endregion

                case ES_AstSimpleBinaryExpression simpleBinaryExpr: {
                    var lhs = GenerateCode_Expression (ref transUnit, symbols, src, simpleBinaryExpr.Left, expectedType);
                    var rhs = GenerateCode_Expression (ref transUnit, symbols, src, simpleBinaryExpr.Right, expectedType);

                    if (!envBuilder!.BinaryOpCompat (lhs.Type, rhs.Type, simpleBinaryExpr.ExpressionType, out _))
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    if (simpleBinaryExpr.ExpressionType.IsAssignment () && (!lhs.Addressable || lhs.Value.IsAAllocaInst == null))
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    var ret = GenerateCode_BinaryExpr (lhs, rhs, simpleBinaryExpr.ExpressionType);
                    ret.Expr = expr;
                    return ret;
                }

                case ES_AstConditionalExpression condExpr:
                    return GenerateCode_ConditionalExpression (ref transUnit, symbols, src, condExpr, expectedType);

                default:
                    throw new NotImplementedException ();
            }
        }

        private ExpressionData GenerateCode_Cast (ExpressionData src, ES_TypeInfo* dst) {
            var ret = src;
            ret.Type = dst;
            ret.Constant = false;
            ret.Addressable = false;

            var srcVal = GetLLVMValue (src.Value);

            switch (src.Type->TypeTag) {
                case ES_TypeTag.Int: {
                    var intSrc = (ES_IntTypeData*) src.Type;

                    if (dst->TypeTag == ES_TypeTag.Int) {
                        var intDst = (ES_IntTypeData*) dst;

                        if (intDst->IntSize == intSrc->IntSize && intDst->Unsigned == intSrc->Unsigned) {
                            src.Addressable = false;
                            return src;
                        }

                        var dstType = GetIntType (intDst->IntSize, intDst->Unsigned);
                        ret.Value = builderRef.BuildIntCast (srcVal, dstType, "intToIntCastTmp");
                    } else if (dst->TypeTag == ES_TypeTag.Float) {
                        var fltDst = (ES_FloatTypeData*) dst;

                        var dstType = GetFloatType (fltDst->FloatSize);

                        if (!intSrc->Unsigned)
                            ret.Value = builderRef.BuildSIToFP (srcVal, dstType, "intToFloatCastTmp");
                        else
                            ret.Value = builderRef.BuildUIToFP (srcVal, dstType, "uintToFloatCastTmp");
                    } else
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    break;
                }

                case ES_TypeTag.Float: {
                    var fltSrc = (ES_FloatTypeData*) src.Type;

                    if (dst->TypeTag == ES_TypeTag.Float) {
                        var fltDst = (ES_FloatTypeData*) dst;

                        if (fltSrc->FloatSize == fltDst->FloatSize) {
                            src.Addressable = false;
                            return src;
                        }

                        var dstType = GetFloatType (fltDst->FloatSize);
                        ret.Value = builderRef.BuildFPCast (srcVal, dstType, "floatToFloatTmp");
                    } else if (dst->TypeTag == ES_TypeTag.Int) {
                        var intDst = (ES_IntTypeData*) dst;

                        var dstType = GetIntType (intDst->IntSize, intDst->Unsigned);
                        if (!intDst->Unsigned)
                            ret.Value = builderRef.BuildFPToSI (srcVal, dstType, "floatToIntCastTmp");
                        else
                            ret.Value = builderRef.BuildFPToUI (srcVal, dstType, "floatToUintCastTmp");
                    } else
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    break;
                }

                default:
                    throw new NotImplementedException ();
            }

            return ret;
        }

        private ExpressionData GenerateCode_IncDecExpression (ExpressionData val, bool decrement, bool postfix) {
            if (!val.Addressable || val.Type is null || val.Value == null || val.Value.IsAAllocaInst == null)
                throw new CompilationException (ES_BackendErrors.FrontendError);

            LLVMValueRef outVal;
            var valueMem = val.Value;

            if (val.Type->TypeTag == ES_TypeTag.Int) {
                LLVMValueRef oldVal = builderRef.BuildLoad (valueMem);

                var addVal = LLVMValueRef.CreateConstInt (oldVal.TypeOf, (ulong) (!decrement ? 1 : -1), false);
                var newVal = builderRef.BuildAdd (oldVal, addVal, "incDecTmp");

                builderRef.BuildStore (newVal, valueMem);

                outVal = !postfix ? newVal : oldVal;
            } else if (val.Type->TypeTag == ES_TypeTag.Float) {
                LLVMValueRef oldVal = builderRef.BuildLoad (valueMem);

                var addVal = LLVMValueRef.CreateConstReal (oldVal.TypeOf, !decrement ? 1d : -1d);
                var newVal = builderRef.BuildFAdd (oldVal, addVal, "incDecTmp");

                builderRef.BuildStore (newVal, valueMem);

                outVal = !postfix ? newVal : oldVal;
            } else
                throw new CompilationException (ES_BackendErrors.FrontendError);

            return new ExpressionData { Type = val.Type, Value = outVal, Constant = false, Addressable = false };
        }

        #region SimpleBinaryExpr

        private ExpressionData GenerateCode_BinaryExpr (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            if (!envBuilder!.BinaryOpCompat (lhs.Type, rhs.Type, exprOp, out _))
                throw new CompilationException (ES_BackendErrors.FrontendError);

            Debug.Assert (lhs.Value != null);
            Debug.Assert (rhs.Value != null);

            if (lhs.Type->TypeTag == ES_TypeTag.Int && rhs.Type->TypeTag == ES_TypeTag.Int)
                return GenerateCode_BinaryExpr_IntInt (lhs, rhs, exprOp);

            if (lhs.Type->TypeTag == ES_TypeTag.Bool && rhs.Type->TypeTag == ES_TypeTag.Bool)
                return GenerateCode_BinaryExpr_FloatFloat (lhs, rhs, exprOp);

            if (lhs.Type->TypeTag == ES_TypeTag.Float && rhs.Type->TypeTag == ES_TypeTag.Float)
                return GenerateCode_BinaryExpr_BoolBool (lhs, rhs, exprOp);

            if (lhs.Type->TypeTag == ES_TypeTag.Float && rhs.Type->TypeTag == ES_TypeTag.Int)
                return GenerateCode_BinaryExpr_FloatInt (lhs, rhs, exprOp);

            throw new CompilationException (ES_BackendErrors.FrontendError);
        }

        private ExpressionData GenerateCode_BinaryExpr_IntInt (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            var boolType = env!.TypeBool;

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
            var originalLHS = lhs.Value;

            Debug.Assert (!isAssignment || originalLHS.IsAAllocaInst != null);

            lhs.Value = GetLLVMValue (lhs.Value);
            rhs.Value = GetLLVMValue (rhs.Value);

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

            LLVMValueRef value;
            switch (exprOp) {
                case SimpleBinaryExprType.Assign:
                    value = rhs.Value;
                    break;

                case SimpleBinaryExprType.Add:
                case SimpleBinaryExprType.AssignAdd:
                    value = builderRef.BuildAdd (lhs.Value, rhs.Value, "intAddTmp");
                    break;

                case SimpleBinaryExprType.Subtract:
                case SimpleBinaryExprType.AssignSubtract:
                    value = builderRef.BuildSub (lhs.Value, rhs.Value, "intSubTmp");
                    break;

                case SimpleBinaryExprType.Multiply:
                case SimpleBinaryExprType.AssignMultiply:
                    value = builderRef.BuildMul (lhs.Value, rhs.Value, "intMulTmp");
                    break;

                case SimpleBinaryExprType.Divide:
                case SimpleBinaryExprType.AssignDivide:
                    if (!lhsInt->Unsigned)
                        value = builderRef.BuildSDiv (lhs.Value, rhs.Value, "intSDivTmp");
                    else
                        value = builderRef.BuildUDiv (lhs.Value, rhs.Value, "intUDivTmp");
                    break;

                case SimpleBinaryExprType.Modulo:
                case SimpleBinaryExprType.AssignModulo:
                    if (!lhsInt->Unsigned)
                        value = builderRef.BuildSRem (lhs.Value, rhs.Value, "intSDivTmp");
                    else
                        value = builderRef.BuildURem (lhs.Value, rhs.Value, "intUDivTmp");
                    break;

                case SimpleBinaryExprType.ShiftLeft:
                case SimpleBinaryExprType.AssignShiftLeft:
                    value = builderRef.BuildShl (lhs.Value, rhs.Value, "intShlTmp");
                    break;

                case SimpleBinaryExprType.ShiftRight:
                case SimpleBinaryExprType.AssignShiftRight:
                    if (!lhsInt->Unsigned)
                        value = builderRef.BuildAShr (lhs.Value, rhs.Value, "intAShrTmp");
                    else
                        value = builderRef.BuildLShr (lhs.Value, rhs.Value, "intLShrTmp");
                    break;

                case SimpleBinaryExprType.ShiftRightUnsigned:
                case SimpleBinaryExprType.AssignShiftRightUnsigned:
                    value = builderRef.BuildLShr (lhs.Value, rhs.Value, "intLShrTmp");
                    break;

                case SimpleBinaryExprType.BitAnd:
                case SimpleBinaryExprType.AssignBitAnd:
                    value = builderRef.BuildAnd (lhs.Value, rhs.Value, "intAndTmp");
                    break;

                case SimpleBinaryExprType.BitOr:
                case SimpleBinaryExprType.AssignBitOr:
                    value = builderRef.BuildOr (lhs.Value, rhs.Value, "intOrTmp");
                    break;

                case SimpleBinaryExprType.BitXor:
                case SimpleBinaryExprType.AssignXor:
                    value = builderRef.BuildXor (lhs.Value, rhs.Value, "intXorTmp");
                    break;

                case SimpleBinaryExprType.Power:
                case SimpleBinaryExprType.AssignPower:
                    throw new NotImplementedException ();

                case SimpleBinaryExprType.Equals:
                    value = builderRef.BuildICmp (LLVMIntPredicate.LLVMIntEQ, lhs.Value, rhs.Value, "intEqTmp");
                    break;
                case SimpleBinaryExprType.NotEquals:
                    value = builderRef.BuildICmp (LLVMIntPredicate.LLVMIntNE, lhs.Value, rhs.Value, "intNotEqTmp");
                    break;

                case SimpleBinaryExprType.LesserThan:
                    value = builderRef.BuildICmp (
                        !unsigned ? LLVMIntPredicate.LLVMIntSLT : LLVMIntPredicate.LLVMIntULT,
                        lhs.Value, rhs.Value, "intLesserThanTmp"
                    );
                    break;
                case SimpleBinaryExprType.GreaterThan:
                    value = builderRef.BuildICmp (
                        !unsigned ? LLVMIntPredicate.LLVMIntSGT : LLVMIntPredicate.LLVMIntUGT,
                        lhs.Value, rhs.Value, "intGreaterThanTmp"
                    );
                    break;

                case SimpleBinaryExprType.LesserThanEqual:
                    value = builderRef.BuildICmp (
                        !unsigned ? LLVMIntPredicate.LLVMIntSLE : LLVMIntPredicate.LLVMIntULE,
                        lhs.Value, rhs.Value, "intLesserThanEqTmp"
                    );
                    break;
                case SimpleBinaryExprType.GreaterThanEqual:
                    value = builderRef.BuildICmp (
                        !unsigned ? LLVMIntPredicate.LLVMIntSGE : LLVMIntPredicate.LLVMIntUGE,
                        lhs.Value, rhs.Value, "intGreaterThanEqTmp"
                    );
                    break;

                default:
                    throw new NotImplementedException ();
            }

            if (isAssignment)
                builderRef.BuildStore (value, originalLHS);

            if (!exprOp.IsComparison ())
                return new ExpressionData { Type = lhs.Type, Value = value, Constant = false, Addressable = false, };
            else
                return new ExpressionData { Type = boolType, Value = value, Constant = false, Addressable = false, };
        }

        private ExpressionData GenerateCode_BinaryExpr_BoolBool (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            var boolType = env!.TypeBool;

            Debug.Assert (lhs.Type->TypeTag == ES_TypeTag.Bool);
            Debug.Assert (rhs.Type->TypeTag == ES_TypeTag.Bool);

            bool isAssignment = exprOp.IsAssignment ();
            var originalLHS = lhs.Value;

            Debug.Assert (!isAssignment || originalLHS.IsAAllocaInst != null);

            lhs.Value = GetLLVMValue (lhs.Value);
            rhs.Value = GetLLVMValue (rhs.Value);

            LLVMValueRef value;
            switch (exprOp) {
                case SimpleBinaryExprType.Assign:
                    value = rhs.Value;
                    break;

                case SimpleBinaryExprType.Equals:
                    value = builderRef.BuildICmp (LLVMIntPredicate.LLVMIntEQ, lhs.Value, rhs.Value, "boolEqTmp");
                    break;
                case SimpleBinaryExprType.NotEquals:
                    value = builderRef.BuildICmp (LLVMIntPredicate.LLVMIntNE, lhs.Value, rhs.Value, "boolNotEqTmp");
                    break;

                case SimpleBinaryExprType.BitAnd:
                case SimpleBinaryExprType.AssignBitAnd:
                    value = builderRef.BuildAnd (lhs.Value, rhs.Value, "boolAndTmp");
                    break;

                case SimpleBinaryExprType.BitOr:
                case SimpleBinaryExprType.AssignBitOr:
                    value = builderRef.BuildOr (lhs.Value, rhs.Value, "boolOrTmp");
                    break;

                case SimpleBinaryExprType.BitXor:
                case SimpleBinaryExprType.AssignXor:
                    value = builderRef.BuildXor (lhs.Value, rhs.Value, "boolXorTmp");
                    break;

                case SimpleBinaryExprType.LogicalAnd:
                case SimpleBinaryExprType.LogicalOr:
                    throw new NotImplementedException ();

                default:
                    throw new NotImplementedException ();
            }

            if (isAssignment)
                builderRef.BuildStore (value, originalLHS);

            return new ExpressionData { Type = boolType, Value = value, Constant = false, Addressable = false, };
        }

        private ExpressionData GenerateCode_BinaryExpr_FloatFloat (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            Debug.Assert (env is not null);
            Debug.Assert (lhs.Type->TypeTag == ES_TypeTag.Float);
            Debug.Assert (rhs.Type->TypeTag == ES_TypeTag.Float);

            var lhsFloat = (ES_FloatTypeData*) lhs.Type;
            var rhsFloat = (ES_FloatTypeData*) rhs.Type;

            bool isAssignment = exprOp.IsAssignment ();
            var originalLHS = lhs.Value;

            Debug.Assert (!isAssignment || originalLHS.IsAAllocaInst != null);

            lhs.Value = GetLLVMValue (lhs.Value);
            rhs.Value = GetLLVMValue (rhs.Value);

            if (lhsFloat->FloatSize != rhsFloat->FloatSize)
                throw new CompilationException (ES_BackendErrors.FrontendError);

            LLVMValueRef value;
            switch (exprOp) {
                case SimpleBinaryExprType.Assign:
                    value = rhs.Value;
                    break;

                case SimpleBinaryExprType.Add:
                case SimpleBinaryExprType.AssignAdd:
                    value = builderRef.BuildFAdd (lhs.Value, rhs.Value, "floatAddTmp");
                    break;

                case SimpleBinaryExprType.Subtract:
                case SimpleBinaryExprType.AssignSubtract:
                    value = builderRef.BuildFSub (lhs.Value, rhs.Value, "floatSubTmp");
                    break;

                case SimpleBinaryExprType.Multiply:
                case SimpleBinaryExprType.AssignMultiply:
                    value = builderRef.BuildFMul (lhs.Value, rhs.Value, "floatMulTmp");
                    break;

                case SimpleBinaryExprType.Divide:
                case SimpleBinaryExprType.AssignDivide:
                    value = builderRef.BuildFDiv (lhs.Value, rhs.Value, "floatDivTmp");
                    break;

                case SimpleBinaryExprType.Modulo:
                case SimpleBinaryExprType.AssignModulo:
                    value = builderRef.BuildFRem (lhs.Value, rhs.Value, "floatModTmp");
                    break;

                case SimpleBinaryExprType.Equals:
                    value = builderRef.BuildFCmp (LLVMRealPredicate.LLVMRealUEQ, lhs.Value, rhs.Value, "floatEqTmp");
                    break;
                case SimpleBinaryExprType.NotEquals:
                    value = builderRef.BuildFCmp (LLVMRealPredicate.LLVMRealUNE, lhs.Value, rhs.Value, "floatNotEqTmp");
                    break;

                case SimpleBinaryExprType.LesserThan:
                    value = builderRef.BuildFCmp (LLVMRealPredicate.LLVMRealULT, lhs.Value, rhs.Value, "floatLesserThanTmp");
                    break;
                case SimpleBinaryExprType.GreaterThan:
                    value = builderRef.BuildFCmp (LLVMRealPredicate.LLVMRealUGT, lhs.Value, rhs.Value, "floatGreaterThanTmp");
                    break;
                case SimpleBinaryExprType.LesserThanEqual:
                    value = builderRef.BuildFCmp (LLVMRealPredicate.LLVMRealULE, lhs.Value, rhs.Value, "floatLesserThanEqTmp");
                    break;
                case SimpleBinaryExprType.GreaterThanEqual:
                    value = builderRef.BuildFCmp (LLVMRealPredicate.LLVMRealUGE, lhs.Value, rhs.Value, "floatGreaterThanEqTmp");
                    break;

                case SimpleBinaryExprType.Power:
                case SimpleBinaryExprType.AssignPower:
                    throw new NotImplementedException ();

                default:
                    throw new NotImplementedException ();
            }

            if (isAssignment)
                builderRef.BuildStore (value, originalLHS);

            if (!exprOp.IsComparison ())
                return new ExpressionData { Type = lhs.Type, Value = value, Constant = false, Addressable = false, };
            else
                return new ExpressionData { Type = env!.TypeBool, Value = value, Constant = false, Addressable = false, };
        }

        private ExpressionData GenerateCode_BinaryExpr_FloatInt (ExpressionData lhs, ExpressionData rhs, SimpleBinaryExprType exprOp) {
            Debug.Assert (env is not null);
            Debug.Assert (lhs.Type->TypeTag == ES_TypeTag.Float);
            Debug.Assert (rhs.Type->TypeTag == ES_TypeTag.Int);

            var lhsFloat = (ES_FloatTypeData*) lhs.Type;
            var rhsFloat = (ES_IntTypeData*) rhs.Type;

            bool isAssignment = exprOp.IsAssignment ();
            var originalLHS = lhs.Value;

            Debug.Assert (!isAssignment || originalLHS.IsAAllocaInst != null);

            lhs.Value = GetLLVMValue (lhs.Value);
            rhs.Value = GetLLVMValue (rhs.Value);

            LLVMValueRef value;
            switch (exprOp) {
                case SimpleBinaryExprType.Power:
                case SimpleBinaryExprType.AssignPower:
                    throw new NotImplementedException ();

                default:
                    throw new NotImplementedException ();
            }

            if (isAssignment)
                builderRef.BuildStore (value, originalLHS);

            if (!exprOp.IsComparison ())
                return new ExpressionData { Type = lhs.Type, Value = value, Constant = false, Addressable = false, };
            else
                return new ExpressionData { Type = env!.TypeBool, Value = value, Constant = false, Addressable = false, };
        }

        #endregion

        #region SimpleUnaryExpr

        private ExpressionData GenerateCode_UnaryExpr (ExpressionData inner, SimpleUnaryExprType exprOp) {
            if (!envBuilder!.UnaryOpCompat (inner.Type, exprOp, out var _))
                throw new CompilationException (ES_BackendErrors.FrontendError);

            Debug.Assert (inner.Value != null);

            if (inner.Type->TypeTag == ES_TypeTag.Int)
                return GenerateCode_UnaryExpr_Int (inner, exprOp);
            else if (inner.Type->TypeTag == ES_TypeTag.Bool)
                return GenerateCode_UnaryExpr_Bool (inner, exprOp);
            else if (inner.Type->TypeTag == ES_TypeTag.Float)
                return GenerateCode_UnaryExpr_Float (inner, exprOp);
            else
                throw new NotImplementedException ();

            throw new CompilationException (ES_BackendErrors.FrontendError);
        }

        private ExpressionData GenerateCode_UnaryExpr_Int (ExpressionData inner, SimpleUnaryExprType exprOp) {
            Debug.Assert (inner.Type->TypeTag == ES_TypeTag.Int);

            var innerInt = (ES_IntTypeData*) inner.Type;

            inner.Value = GetLLVMValue (inner.Value);

            LLVMValueRef value;
            switch (exprOp) {
                case SimpleUnaryExprType.BitNot:
                    value = builderRef.BuildNot (inner.Value, "bitNotTmp");
                    break;
                case SimpleUnaryExprType.Negative:
                    if (innerInt->Unsigned)
                        throw new CompilationException (ES_BackendErrors.FrontendError);
                    value = builderRef.BuildNeg (inner.Value, "negationTmp");
                    break;
                case SimpleUnaryExprType.Positive:
                    value = inner.Value;
                    break;

                default:
                    throw new NotImplementedException ();
            }

            return new ExpressionData { Type = inner.Type, Value = value, Constant = inner.Constant, Addressable = false, };
        }

        private ExpressionData GenerateCode_UnaryExpr_Bool (ExpressionData inner, SimpleUnaryExprType exprOp) {
            Debug.Assert (inner.Type->TypeTag == ES_TypeTag.Bool);

            var innerInt = (ES_IntTypeData*) inner.Type;

            inner.Value = GetLLVMValue (inner.Value);

            LLVMValueRef value;
            switch (exprOp) {
                case SimpleUnaryExprType.LogicalNot:
                    value = builderRef.BuildNot (inner.Value, "boolNegTmp");
                    break;

                default:
                    throw new NotImplementedException ();
            }

            return new ExpressionData { Type = inner.Type, Value = value, Constant = inner.Constant, Addressable = false, };
        }

        private ExpressionData GenerateCode_UnaryExpr_Float (ExpressionData inner, SimpleUnaryExprType exprOp) {
            Debug.Assert (inner.Type->TypeTag == ES_TypeTag.Float);

            var innerInt = (ES_IntTypeData*) inner.Type;

            inner.Value = GetLLVMValue (inner.Value);

            LLVMValueRef value;
            switch (exprOp) {
                case SimpleUnaryExprType.Positive:
                    value = inner.Value;
                    break;
                case SimpleUnaryExprType.Negative:
                    value = builderRef.BuildFNeg (inner.Value, "negationTmp");
                    break;

                default:
                    throw new NotImplementedException ();
            }

            return new ExpressionData { Type = inner.Type, Value = value, Constant = inner.Constant, Addressable = false, };
        }

        #endregion

        private ExpressionData GenerateCode_Expression_FunctionCall (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_AstFunctionCallExpression funcCallExpr, ES_TypeInfo* expectedType
        ) {
            throw new NotImplementedException ();
            Debug.Assert (env is not null);

            var funcExpr = GenerateCode_Expression (ref transUnit, symbols, src, funcCallExpr.FunctionExpression, env.TypeUnknownValue);
            ES_FunctionData* func = null;
            ES_FunctionPrototypeData* funcType = null;

            if (funcExpr.Function is not null) {
                func = funcExpr.Function;
                funcType = func->FunctionType;
            } else {
                if (funcExpr.TypeInfo is not null) {
                    throw new CompilationException (ES_BackendErrors.FrontendError);
                } else if (funcExpr.Type is not null) {
                    // TODO: Some types might be allowed to have `()` overrides too in the future. But not now.
                    throw new CompilationException (ES_BackendErrors.FrontendError);
                } else
                    Debug.Fail ("???");
            }

            var funcName = funcType->TypeInfo.TypeName;
            int funcArgCount = funcType->ArgumentsList.Length;
            int callArgCount = funcCallExpr.Arguments.Length;
            int reqArgCount = 0;

            if (func is not null) {
                funcName = func->FullyQualifiedName;
                reqArgCount = funcArgCount - func->OptionalArgsCount;
            } else
                reqArgCount = funcArgCount;

            if (callArgCount < reqArgCount)
                throw new CompilationException (ES_BackendErrors.FrontendError);

            for (int argCount = 0; argCount < callArgCount; argCount++) {
                var arg = funcCallExpr.Arguments [argCount];
                ES_FunctionArgData* argData = null;
                ES_FunctionPrototypeArgData* argTypeData = null;

                if (argCount < funcArgCount) {
                    argData = func->Arguments.Elements + argCount;
                    argTypeData = funcType->ArgumentsList.Elements + argCount;
                } else if (argCount == funcArgCount) {
                    errorList.Add (ES_FrontendErrors.GenTooManyFuncArgs (
                        StringPool.Shared.GetOrAdd (funcName.Span, Encoding.ASCII), src,
                        arg.ValueExpression.NodeBounds
                    ));
                }

                ES_TypeInfo* argValType = null;
                if (argTypeData is not null) {
                    if (argTypeData->ArgType == ES_ArgumentType.Normal && arg.ArgType != ES_ArgumentType.Normal)
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    if (argTypeData->ArgType != ES_ArgumentType.Normal && arg.ArgType != argTypeData->ArgType)
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    argValType = argTypeData->ValueType;
                }

                var argExprData = GenerateCode_Expression (ref transUnit, symbols, src, arg.ValueExpression, argValType);

                if (argValType is not null)
                    GenerateCode_EnsureCompat (argValType, argExprData.Type, src, argExprData.Expr.NodeBounds);
            }

            return new ExpressionData { Expr = funcCallExpr, Type = (ES_TypeInfo*) funcType, Constant = true, Addressable = false };
        }

        private ExpressionData GenerateCode_ConditionalExpression (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_AstConditionalExpression expr, ES_TypeInfo* expectedType
        ) {
            var condExpr = GenerateCode_Expression (ref transUnit, symbols, src, expr.Condition, env!.TypeBool);

            GenerateCode_EnsureCompat (env.TypeBool, condExpr.Type, src, condExpr.Expr.NodeBounds);

            condExpr.Value = GetLLVMValue (condExpr.Value);

            var ownerFunc = builderRef.InsertBlock.Parent;

            var thenBlock = ownerFunc.AppendBasicBlock ("ternaryCond_then");
            var elseBlock = ownerFunc.AppendBasicBlock ("ternaryCond_else");
            var endBlock = ownerFunc.AppendBasicBlock ("ternaryCond_end");

            builderRef.BuildCondBr (condExpr.Value, thenBlock, elseBlock);

            // Then
            builderRef.PositionAtEnd (thenBlock);
            var leftExpr = GenerateCode_Expression (ref transUnit, symbols, src, expr.Then, expectedType);
            leftExpr.Value = GetLLVMValue (leftExpr.Value);

            builderRef.BuildBr (endBlock);
            thenBlock = builderRef.InsertBlock;

            // Else
            builderRef.PositionAtEnd (elseBlock);
            var rightExpr = GenerateCode_Expression (ref transUnit, symbols, src, expr.Else, expectedType);
            rightExpr.Value = GetLLVMValue (rightExpr.Value);

            builderRef.BuildBr (endBlock);
            elseBlock = builderRef.InsertBlock;

            // Checks
            bool isCompat = true;
            isCompat &= GenerateCode_EnsureCompat (expectedType, leftExpr.Type, src, leftExpr.Expr.NodeBounds);
            isCompat &= GenerateCode_EnsureCompat (expectedType, rightExpr.Type, src, rightExpr.Expr.NodeBounds);

            // End
            builderRef.PositionAtEnd (endBlock);
            var phi = builderRef.BuildPhi (GetLLVMType (expectedType), "ternaryCondTmp");
            phi.AddIncoming (leftExpr.Value, thenBlock);
            phi.AddIncoming (rightExpr.Value, elseBlock);

            bool constant = condExpr.Constant & leftExpr.Constant & rightExpr.Constant;

            var finalType = isCompat ? expectedType : env.TypeUnknownValue;

            return new ExpressionData { Expr = expr, Type = finalType, Value = phi, Constant = constant, Addressable = false };
        }
    }
}