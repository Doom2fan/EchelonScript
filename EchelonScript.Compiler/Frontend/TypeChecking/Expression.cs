/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Buffers;
using System.Diagnostics;
using ChronosLib.Pooled;
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.Utilities;

namespace EchelonScript.Compiler.Frontend;

internal unsafe static partial class Compiler_TypeChecking {
#if false
    private struct ExpressionData {
        public ES_AstExpression Expr;
        public ESC_TypeRef Type;
        public ESC_TypeRef TypeInfo;
        public ESC_Function? Function;

        public ExpressionList Expressions;
        public ESIR_Expression Value;
        public int? ValueRegister;

        public bool Writable;
        public bool CompileTimeConst;

        public static ExpressionData NewValue (
            ES_AstExpression expr,
            ESC_TypeRef type, ExpressionList exprs, ESIR_Expression value, int? reg,
            bool writable, bool compTimeConst
        ) {
            return new ExpressionData {
                Expr = expr,
                Type = type,

                TypeInfo = ESC_TypeRef.Null (),
                Function = null,

                Expressions = exprs,
                Value = value,
                ValueRegister = reg,

                Writable = writable,
                CompileTimeConst = compTimeConst,
            };
        }

        public static ExpressionData NewValue (
            ES_AstExpression expr,
            ESC_TypeRef type, ESIR_Expression value, int? reg,
            bool writable, bool compTimeConst
        ) {
            return new ExpressionData {
                Expr = expr,
                Type = type,

                TypeInfo = ESC_TypeRef.Null (),
                Function = null,

                Expressions = new ExpressionList (null, null),
                Value = value,
                ValueRegister = reg,

                Writable = writable,
                CompileTimeConst = compTimeConst,
            };
        }

        public static ExpressionData NewType (ES_AstExpression expr, ESC_TypeRef typeInfo) {
            Debug.Assert (typeInfo.Type is not null);

            var ret = new ExpressionData {
                Expr = expr,
                TypeInfo = typeInfo,

                Type = ESC_TypeRef.Null (),
                Function = null,

                Value = null!,
                ValueRegister = null,

                Writable = false,
                CompileTimeConst = false,
            };

            ret.Expressions.Initialize ();

            return ret;
        }

        public static ExpressionData NewFunction (
            ES_AstExpression expr,
            ESC_Function func, ESC_TypeRef funcType
        ) {
            var ret = new ExpressionData {
                Expr = expr,
                Function = func,
                TypeInfo = funcType,

                Type = ESC_TypeRef.Null (),

                Value = null!,
                ValueRegister = null,

                Writable = false,
                CompileTimeConst = false,
            };

            ret.Expressions.Initialize ();

            return ret;
        }

        public static ExpressionData NewFunction (
            ES_AstExpression expr,
            ESC_Function func, ESC_TypeRef funcType, ExpressionList values, ESIR_Expression value, int? reg
        ) {
            return new ExpressionData {
                Expr = expr,
                Function = func,
                TypeInfo = funcType,

                Type = ESC_TypeRef.Null (),

                Expressions = values,
                Value = value,
                ValueRegister = reg,

                Writable = false,
                CompileTimeConst = false,
            };
        }

        public static ExpressionData NewFunction (
            ES_AstExpression expr,
            ESC_Function func, ESC_TypeRef funcType, ESIR_Expression value, int? reg
        ) => NewFunction (expr, func, funcType, new ExpressionList (null, null), value, reg);
    }

    private struct ExpressionList : IDisposable {
        private ESIR_Expression [] exprs;
        private int [] regs;

        private int exprCount;
        private int regCount;

        public ReadOnlySpan<ESIR_Expression> Expressions => exprs.AsSpan (0, exprCount);
        public ReadOnlySpan<int> Registers => regs.AsSpan (0, regCount);

        public ExpressionList (ESIR_Expression? expr, int? reg) {
            exprs = Array.Empty<ESIR_Expression> ();
            regs = Array.Empty<int> ();
            exprCount = 0;
            regCount = 0;

            if (expr is not null)
                AddExpression (expr);
            AddRegister (reg);
        }

        public void Initialize () {
            exprs = Array.Empty<ESIR_Expression> ();
            regs = Array.Empty<int> ();
            exprCount = 0;
            regCount = 0;
        }

        private static void EnsureCapacity<T> (ref T [] curArr, int curCount, int min, bool clear) {
            var required = curCount + min;

            var oldArr = curArr;
            curArr = ArrayPool<T>.Shared.Rent (required);

            oldArr.AsSpan (0, curCount).CopyTo (curArr);

            ArrayPool<T>.Shared.Return (oldArr, clear);
        }

        private void EnsureCapacityExprs (int min) => EnsureCapacity (ref exprs, exprCount, min, true);
        private void EnsureCapacityRegs (int min) => EnsureCapacity (ref regs, regCount, min, false);

        public void AddExpression (ESIR_Expression expr) {
            EnsureCapacityExprs (1);
            exprs [exprCount++] = expr;
        }

        public void AddRegister (int expr) {
            EnsureCapacityRegs (1);
            regs [regCount++] = expr;
        }

        public void AddRegister (int? expr) {
            if (expr is null)
                return;

            AddRegister (expr.Value);
        }

        public void ReturnRegisters (ESIR_Writer irWriter) {
            irWriter.ReturnRegisters (Registers);
            regCount = 0;
        }

        public void Merge (ref ExpressionList other) {
            EnsureCapacityExprs (other.exprCount);
            EnsureCapacityRegs (other.regCount);

            other.Expressions.CopyTo (exprs.AsSpan (exprCount));
            other.Registers.CopyTo (regs.AsSpan (regCount));

            exprCount += other.exprCount;
            regCount += other.regCount;

            other.Dispose ();
        }

        public void Dispose () {
            ArrayPool<ESIR_Expression>.Shared.Return (exprs);
            ArrayPool<int>.Shared.Return (regs);
        }
    }

    private static ESC_Function? FindConstructor (
        ESC_TypeData objType, ReadOnlySpan<ESC_PrototypeArg> arguments
    ) {
        return null;
    }

    private static ExpressionData ExpressionError (
        ref CompileData compileData, ES_AstExpression expr, ESC_Constness constness = ESC_Constness.Mutable, bool writable = false
    ) => ExpressionData.NewValue (expr, compileData.GetUnknownType (constness), ErrorExpression (), null, writable, false);

    private static PooledArray<ESC_TypeRef> DeconstructType (ref CompileData compileData, ESC_TypeRef typeData) {
        using var typeList = new StructPooledList<ESC_TypeRef> (CL_ClearMode.Auto);

        // Deconstruct the type.
        var type = typeData.Type;
        var constness = typeData.Constness;
        while (type is not null) {
            switch (type) {
                case ESC_TypeUnknown:
                case ESC_TypeNull:
                case ESC_TypeVoid:
                case ESC_TypeBool:
                case ESC_TypeInt:
                case ESC_TypeFloat:
                case ESC_TypePrototype:
                case ESC_TypeStruct:
                case ESC_TypeClass:
                case ESC_TypeEnum:
                case ESC_TypeInterface:
                    typeList.Add (new (constness, type));
                    type = null;
                    break;

                case ESC_TypeReference typeRef: {
                    typeList.Add (new (constness, type));

                    type = typeRef.PointedType.Type;
                    constness = typeRef.PointedType.Constness;

                    break;
                }

                case ESC_TypeArray typeArray: {
                    typeList.Add (new (constness, type));

                    type = typeArray.ElementType.Type;
                    constness = typeArray.ElementType.Constness;

                    break;
                }

                default:
                    throw new NotImplementedException ("Type not implemented.");
            }
        }

        // Replace all types in the list with "clean" mutable versions.
        var typeSpan = typeList.Span;
        for (var i = typeSpan.Length - 1; i >= 0; i--) {
            ref var listType = ref typeSpan [i];

            switch (listType.Type) {
                case ESC_TypeUnknown:
                case ESC_TypeNull:
                case ESC_TypeVoid:
                case ESC_TypeBool:
                case ESC_TypeInt:
                case ESC_TypeFloat:
                case ESC_TypePrototype:
                case ESC_TypeStruct:
                case ESC_TypeClass:
                case ESC_TypeEnum:
                case ESC_TypeInterface:
                    break;

                case ESC_TypeReference: {
                    Debug.Assert (i + 1 < typeSpan.Length);
                    var innerType = typeSpan [i + 1].WithConst (ESC_Constness.Mutable);
                    listType = compileData.GetReferenceType (innerType, ESC_Constness.Mutable);

                    break;
                }

                case ESC_TypeArray typeArray: {
                    Debug.Assert (i + 1 < typeSpan.Length);
                    var innerType = typeSpan [i + 1].WithConst (ESC_Constness.Mutable);
                    listType = compileData.GetArrayType (innerType, typeArray.Rank, ESC_Constness.Mutable);

                    break;
                }

                default:
                    throw new NotImplementedException ("Type not implemented.");
            }
        }

        return typeList.MoveToArray ();
    }

    private static bool IsConstIgnorantType (ESC_TypeRef type) {
        return type.Type switch {
            ESC_TypeNull => true,
            ESC_TypeVoid => true,
            ESC_TypeBool => true,
            ESC_TypeInt => true,
            ESC_TypeFloat => true,
            ESC_TypeEnum => true,

            _ => false,
        };
    }

    private static bool CanConvertConstness (ESC_Constness thisConst, ESC_Constness destConst)
        => thisConst == destConst || destConst == ESC_Constness.Const;

    private static bool CanConvertConstness (ref CompileData compileData, ESC_TypeRef origType, ESC_TypeRef newType) {
        using var origDeconsArr = DeconstructType (ref compileData, origType);
        using var newDeconsArr = DeconstructType (ref compileData, newType);
        var origData = origDeconsArr.Span;
        var newData = newDeconsArr.Span;

        Debug.Assert (origData.Length > 0);
        Debug.Assert (newData.Length > 0);

        if (origData.Length != newData.Length)
            return false;

        if (origData [0].Type != newData [0].Type)
            return false;

        var typeLen = origData.Length;
        for (var i = typeLen - 1; i >= 0; i--) {
            var origConst = origData [i];
            var newConst = newData [i];

            Debug.Assert (origConst.Type == newConst.Type);

            if (i == 0 && IsConstIgnorantType (newConst))
                continue;

            if (!CanConvertConstness (origConst.Constness, newConst.Constness))
                return false;
        }

        return true;
    }

    private static bool CanMatchConstness (
        ref CompileData compileData,
        ESC_TypeRef lhs, ESC_TypeRef rhs, out ESC_TypeRef finalType
    ) {
        using var lhsDeconsArr = DeconstructType (ref compileData, lhs);
        using var rhsDeconsArr = DeconstructType (ref compileData, rhs);
        var lhsData = lhsDeconsArr.Span;
        var rhsData = rhsDeconsArr.Span;

        Debug.Assert (lhsData.Length > 0);
        Debug.Assert (rhsData.Length > 0);

        if (lhsData.Length != rhsData.Length || lhsData [0].Type != rhsData [0].Type) {
            finalType = ESC_TypeRef.Null ();
            return false;
        }

        var typeLen = lhsData.Length;
        using var finalDeconsArr = PooledArray<ESC_TypeRef>.GetArray (typeLen);
        var finalDecons = finalDeconsArr.Span;

        for (var i = 0; i < typeLen; i++) {
            var lhsConst = lhsData [i];
            var rhsConst = rhsData [i];
            ref var finalConst = ref finalDecons [i];

            Debug.Assert (lhsConst.Type == rhsConst.Type);

            var finalConstness = ESC_Constness.Const;
            if ((lhsConst.Constness, rhsConst.Constness) == (ESC_Constness.Mutable, ESC_Constness.Mutable))
                finalConstness = ESC_Constness.Mutable;
            else if ((lhsConst.Constness, rhsConst.Constness) == (ESC_Constness.Immutable, ESC_Constness.Immutable))
                finalConstness = ESC_Constness.Immutable;

            finalConst = lhsConst.WithConst (finalConstness);
        }

        var innerType = ESC_TypeRef.Null ();
        for (var i = finalDecons.Length - 1; i >= 0; i--) {
            ref var listType = ref finalDecons [i];

            switch (listType.Type) {
                case ESC_TypeNull:
                case ESC_TypeVoid:
                case ESC_TypeBool:
                case ESC_TypeInt:
                case ESC_TypeFloat:
                case ESC_TypePrototype:
                case ESC_TypeStruct:
                case ESC_TypeClass:
                case ESC_TypeEnum:
                case ESC_TypeInterface:
                    Debug.Assert (innerType.Type is null);
                    innerType = listType;
                    break;

                case ESC_TypeReference: {
                    Debug.Assert (innerType.Type is not null);
                    Debug.Assert (i + 1 < finalDecons.Length);

                    innerType = compileData.GetReferenceType (innerType, innerType.Constness);
                    break;
                }

                case ESC_TypeArray typeArray: {
                    Debug.Assert (innerType.Type is not null);
                    Debug.Assert (i + 1 < finalDecons.Length);

                    innerType = compileData.GetArrayType (innerType, typeArray.Rank, innerType.Constness);
                    break;
                }

                default:
                    throw new NotImplementedException ("Type not implemented.");
            }
        }

        finalType = innerType;
        return true;
    }

    private static bool IsWritable (this ESC_Constness constness) => constness == ESC_Constness.Mutable;

    private static bool MustBeCompat (
        ref CompileData compileData, ref PassData passData, ref ExpressionData exprData, ESC_TypeRef destType, bool noModify = false
    ) {
        // We don't need to do any checks here if they're *literally* the same type.
        if (destType == exprData.Type)
            return true;

        if (CanConvertConstness (ref compileData, exprData.Type, destType)) {
            if (!noModify)
                exprData.Value = CastExpression (exprData.Value, TypeNode (ref compileData, destType));
        } else if (destType.Type is ESC_TypeUnknown || exprData.Type.Type is ESC_TypeUnknown)
            return true;
        else if (exprData.Type == destType) {
            // Do nothing
        } else if (exprData.Type.Type is ESC_TypeNull) {
            if (!IsNullable (ref compileData, destType, out _))
                return false;

            if (!noModify) {
                exprData.Value = NullLiteralExpression (TypeNode (ref compileData, destType));
                exprData.Writable = false;
            }
        } else if (destType.Type is ESC_TypeInt destIntType && exprData.Type.Type is ESC_TypeInt givenIntType) {
            if (givenIntType.Unsigned != destIntType.Unsigned || givenIntType.Size > destIntType.Size)
                return false;

            if (!noModify) {
                exprData.Value = CastExpression (exprData.Value, TypeNode (ref compileData, destType));
                exprData.Writable = false;
            }
        } else
            return false;

        exprData.Type = destType;
        exprData.Writable &= destType.IsWritable ();

        return true;
    }

    private static bool ExplicitCast (
        ref CompileData compileData, ref PassData passData, ref ExpressionData exprData, ESC_TypeRef castType,
        out bool castRedundant, bool noModify = false
    ) {
        if (MustBeCompat (ref compileData, ref passData, ref exprData, castType, noModify)) {
            castRedundant = true;
            return true;
        }

        if (castType.Type is ESC_TypeInt castIntType && exprData.Type.Type is ESC_TypeInt exprIntType) {
            castRedundant = (
                castIntType.Size == exprIntType.Size &&
                castIntType.Unsigned == exprIntType.Unsigned
            );
        } else if (castType.Type is ESC_TypeFloat && exprData.Type.Type is ESC_TypeInt) {
            castRedundant = false;
        } else if (castType.Type is ESC_TypeInt && exprData.Type.Type is ESC_TypeFloat) {
            castRedundant = false;
        } else if (castType.Type is ESC_TypeFloat castFloatType && exprData.Type.Type is ESC_TypeFloat exprFloatType) {
            castRedundant = castFloatType.Size == exprFloatType.Size;
        } else {
            castRedundant = false;
            return false;
        }

        if (exprData.Type == castType && exprData.Type.Constness != castType.Constness)
            castRedundant = false;

        if (!noModify) {
            exprData.Type = castType;
            exprData.Value = CastExpression (exprData.Value, TypeNode (ref compileData, castType));
            exprData.Writable = false;
        }

        return true;
    }

    private static bool EnsureCompat (
        ref CompileData compileData, ref PassData passData,
        ref ExpressionData exprData, ESC_TypeRef destType,
        ES_AstNodeBounds bounds
    ) {
        if (exprData.Type.Type is ESC_TypeNull) {
            if (!IsNullable (ref compileData, destType, out var retType)) {
                compileData.ErrorList.Add (ES_FrontendErrors.GenTypeNotNullable (
                    compileData.GetNiceNameString (destType, true),
                    passData.Source, bounds
                ));

                exprData.Type = compileData.GetUnknownType (exprData.Type.Constness);
                exprData.Value = ErrorExpression ();
                return false;
            }

            exprData.Type = retType;
            exprData.Value = NullLiteralExpression (TypeNode (ref compileData, retType));
            return true;
        } else if (!MustBeCompat (ref compileData, ref passData, ref exprData, destType)) {
            if (ExplicitCast (ref compileData, ref passData, ref exprData, destType, out _, true)) {
                compileData.ErrorList.Add (ES_FrontendErrors.GenNoImplicitCast (
                    compileData.GetNiceNameString (destType, true),
                    compileData.GetNiceNameString (exprData.Type, true),
                    passData.Source, bounds
                ));
            } else {
                compileData.ErrorList.Add (ES_FrontendErrors.GenNoCast (
                    compileData.GetNiceNameString (destType, true),
                    compileData.GetNiceNameString (exprData.Type, true),
                    passData.Source, bounds
                ));
            }

            return false;
        }

        return true;
    }

    private static void CheckExpression_Dereference (ref ExpressionData exprData) {
        if (exprData.Type.Type is not ESC_TypeReference refTypeData)
            return;

        exprData.Type = refTypeData.PointedType.WithInheritedConst (exprData.Type.Constness);
        exprData.Value = UnaryExpression (ESIR_NodeKind.UnaryDereference, exprData.Value);
    }

    private static ExpressionData CheckExpression (
        ref CompileData compileData, ref PassData passData,
        ES_AstExpression expr,
        ESC_TypeRef expectedType
    ) {
        Debug.Assert (expr is not null);

        var typeBoolConst = compileData.GetBoolType (ESC_Constness.Const);

        switch (expr) {
            case ES_AstParenthesisExpression parenExpr:
                return CheckExpression (ref compileData, ref passData, parenExpr.Inner, expectedType);

#region Primary expressions

            case ES_AstFunctionCallExpression funcCallExpr:
                return CheckExpression_FunctionCall (ref compileData, ref passData, funcCallExpr);

            case ES_AstIndexingExpression indexExpr:
                return CheckExpression_Indexing (ref compileData, ref passData, indexExpr);

            case ES_AstNewObjectExpression newObjExpr:
                return CheckExpression_NewObject (ref compileData, ref passData, newObjExpr);

            case ES_AstNewArrayExpression newArrayExpr:
                return CheckExpression_NewArray (ref compileData, ref passData, newArrayExpr);

            case ES_AstIntegerLiteralExpression:
            case ES_AstBooleanLiteralExpression:
            case ES_AstFloatLiteralExpression:
                throw new CompilationException (ES_FrontendErrors.ConstFoldFailure);

            case ES_AstStringLiteralExpression:
                throw new NotImplementedException ("[TODO] String literals not implemented yet.");

            case ES_AstCharLiteralExpression:
                throw new NotImplementedException ("[TODO] Character literals not implemented yet.");

            case ES_AstIntegerConstantExpression intConstExpr: {
                var intType = intConstExpr.IntType.Type as ESC_TypeInt;
                Debug.Assert (intType is not null);

                var litVal = intType.Unsigned
                    ? ValueNode (intConstExpr.SignExtend ())
                    : ValueNode ((long) intConstExpr.SignExtend ());
                var litExpr = LiteralExpression (ESIR_NodeKind.LiteralInt, litVal);
                var value = CastExpression (litExpr, TypeNode (ref compileData, intConstExpr.IntType));

                return ExpressionData.NewValue (expr, intConstExpr.IntType, value, null, false, true);
            }

            case ES_AstBooleanConstantExpression boolConstExpr: {
                var value = boolConstExpr.Value ? LiteralTrueExpression () : LiteralFalseExpression ();
                return ExpressionData.NewValue (expr, typeBoolConst, value, null, false, true);
            }

            case ES_AstFloat32ConstantExpression float32ConstExpr: {
                var value = LiteralExpression (ESIR_NodeKind.LiteralFloat, ValueNode (float32ConstExpr.Value));
                return ExpressionData.NewValue (expr, compileData.GetFloat32Type (ESC_Constness.Mutable), value, null, false, true);
            }

            case ES_AstFloat64ConstantExpression float64ConstExpr: {
                var value = LiteralExpression (ESIR_NodeKind.LiteralFloat, ValueNode (float64ConstExpr.Value));
                return ExpressionData.NewValue (expr, compileData.GetFloat64Type (ESC_Constness.Mutable), value, null, false, true);
            }

            case ES_AstNullLiteralExpression:
                return ExpressionData.NewValue (expr, compileData.GetNullType (ESC_Constness.Mutable), null!, null, false, true);

            case ES_AstNameExpression nameExpr:
                return CheckExpression_Name (ref compileData, nameExpr, expectedType);

            case ES_AstMemberAccessExpression memberAccessExpr:
                return CheckExpression_MemberAccess (ref compileData, ref passData, memberAccessExpr);

#endregion

            case ES_AstIncDecExpression incDecExpr:
                return CheckExpression_IncDec (ref compileData, ref passData, incDecExpr, expectedType);

#region Unary expressions

            case ES_AstSimpleUnaryExpression unaryExpr:
                return CheckExpression_Unary (ref compileData, ref passData, unaryExpr, expectedType);

            case ES_AstCastExpression castExpr:
                return CheckExpression_Cast (ref compileData, ref passData, castExpr);

#endregion

            case ES_AstSimpleBinaryExpression binaryExpr:
                return CheckExpression_SimpleBinaryExpression (ref compileData, ref passData, binaryExpr);

            case ES_AstConditionalExpression condExpr: {
                var condData = CheckExpression (ref compileData, ref passData, condExpr.Condition, typeBoolConst);
                EnsureCompat (ref compileData, ref passData, ref condData, typeBoolConst, condData.Expr.NodeBounds);

                var leftExpr = CheckExpression (ref compileData, ref passData, condExpr.Then, expectedType);
                var rightExpr = CheckExpression (ref compileData, ref passData, condExpr.Else, expectedType);

                var isCompat = (
                    EnsureCompat (ref compileData, ref passData, ref leftExpr, expectedType, leftExpr.Expr.NodeBounds) &
                    EnsureCompat (ref compileData, ref passData, ref rightExpr, expectedType, rightExpr.Expr.NodeBounds)
                );

                var finalType = isCompat ? expectedType : compileData.GetUnknownType (ESC_Constness.Mutable);

                // Emit IR.
                var exprList = condData.Expressions;
                exprList.Merge (ref leftExpr.Expressions);
                exprList.Merge (ref rightExpr.Expressions);
                exprList.AddRegister (leftExpr.ValueRegister);
                exprList.AddRegister (rightExpr.ValueRegister);

                var value = ConditionalExpression (condData.Value, leftExpr.Value, rightExpr.Value);
                return ExpressionData.NewValue (expr, finalType, exprList, value, null, false, false);
            }

            default:
                throw new NotImplementedException ("Expression type not implemented.");
        }
    }

    private static ExpressionData CheckExpression_NewObject (
        ref CompileData compileData, ref PassData passData, ES_AstNewObjectExpression expr
    ) {
        var exprList = new ExpressionList (null, null);

        var irWriter = passData.IRWriter;
        var typeUnknMut = compileData.GetUnknownType (ESC_Constness.Mutable);

        var objType = GetTypeRef (expr.TypeDeclaration);
        var retType = compileData.GetReferenceType (objType, ESC_Constness.Mutable);

        Debug.Assert (objType.Type is not null);
        Debug.Assert (retType.Type is not null);

        var irObjType = TypeNode (ref compileData, objType);
        var irRefType = TypeNode (ref compileData, retType);

        var errorFound = false;

        if (objType.Type.Flags.HasFlag (ESC_TypeFlag.NoNew)) {
            compileData.ErrorList.Add (ES_FrontendErrors.GenNoTypeNew (
                compileData.GetNiceNameString (objType, true), passData.Source, expr.NodeBounds
            ));

            errorFound = true;
        }

        // Get the arg types.
        var argIdx = 0;
        var argValues = new StructPooledList<ESIR_Expression> (CL_ClearMode.Auto);

        using var argRegistersArr = PooledArray<int>.GetArray (expr.Arguments.Length);
        using var argsListArr = PooledArray<ESC_PrototypeArg>.GetArray (expr.Arguments.Length);
        var argRegisters = argRegistersArr.Span;
        var argsList = argsListArr.Span;

        foreach (var arg in expr.Arguments) {
            var argValueExpr = CheckExpression (ref compileData, ref passData, arg.ValueExpression, typeUnknMut);
            exprList.Merge (ref argValueExpr.Expressions);
            exprList.AddRegister (argValueExpr.ValueRegister);

            var argType = arg.ArgType;
            var argValueType = argValueExpr.Type;

            if (argValueExpr.Type.Type is null || argValueExpr.Type.Type is ESC_TypeUnknown)
                errorFound = true;

            var argProtoData = new ESC_PrototypeArg (argType, argValueType);

            var regIdx = irWriter.RentRegister (TypeNode (ref compileData, argValueType));
            var thisArgIdx = argIdx++;

            exprList.AddExpression (AssignmentExpression (
                LocalValueExpression (regIdx),
                argValueExpr.Value
            ));
            argValues.Add (argValueExpr.Value);
            argsList [thisArgIdx] = argProtoData;
        }

        if (errorFound) {
            irWriter.ReturnRegisters (argRegisters);
            exprList.Dispose ();
            return ExpressionError (ref compileData, expr);
        }

        // Get the constructor.
        var constructor = FindConstructor (objType.Type, argsList);

        // Error out if the constructor is null.
        if (constructor is null && expr.Arguments.Length > 0) {
            compileData.ErrorList.Add (ES_FrontendErrors.GenNoSuchConstructor (
                compileData.GetNiceNameString (objType, true),
                compileData.GetFunctionSignatureString (argsList),
                passData.Source, expr.NodeBounds
            ));
            retType = typeUnknMut;
        }

        // TODO: Handle constructors.
        if (constructor is not null)
            throw new NotImplementedException ("[TODO] Constructors not implemented yet.");

        var valueReg = irWriter.RentRegister (irRefType);
        var value = LocalValueExpression (valueReg);

        var ptrTypeInfo = (ES_ReferenceData*) compileData.ToTypeInfo (retType);
        Debug.Assert (ptrTypeInfo->TypeInfo.TypeTag == ES_TypeTag.Reference);
        exprList.AddExpression (AssignmentExpression (
            value,
            NewObjectExpression (ptrTypeInfo)
        ));
        exprList.AddExpression (AssignmentExpression (
            UnaryExpression (ESIR_NodeKind.UnaryDereference, value),
            DefaultValueExpression (irObjType)
        ));

        exprList.ReturnRegisters (irWriter);
        irWriter.ReturnRegisters (argRegisters);

        return ExpressionData.NewValue (expr, retType, exprList, value, valueReg, false, false);
    }

    private static ExpressionData CheckExpression_NewArray (
        ref CompileData compileData, ref PassData passData, ES_AstNewArrayExpression expr
    ) {
        var irWriter = passData.IRWriter;
        var exprList = new ExpressionList (null, null);

        var indexType = compileData.GetArrayIndexType ();
        var elemType = GetTypeRef (expr.ElementType);
        var arrType = compileData.GetArrayType (elemType, expr.Dimensions.Length, ESC_Constness.Mutable);

        Debug.Assert (indexType.Type is not null);
        Debug.Assert (elemType.Type is not null);
        Debug.Assert (arrType.Type is not null);

        var noNew = elemType.Type.Flags.HasFlag (ESC_TypeFlag.NoNew);
        if (noNew) {
            compileData.ErrorList.Add (ES_FrontendErrors.GenNoTypeNew (
                compileData.GetNiceNameString (elemType, true), passData.Source, expr.NodeBounds
            ));
        }

        using var dimValues = new StructPooledList<ESIR_Expression> (CL_ClearMode.Auto);
        foreach (var dim in expr.Dimensions) {
            Debug.Assert (dim is not null);

            var dimExpr = CheckExpression (ref compileData, ref passData, dim, indexType);
            if (!EnsureCompat (ref compileData, ref passData, ref dimExpr, indexType, dimExpr.Expr.NodeBounds)) {
                dimExpr.Expressions.Dispose ();
                continue;
            }

            exprList.Merge (ref dimExpr.Expressions);
            exprList.AddRegister (dimExpr.ValueRegister);
            dimValues.Add (dimExpr.Value);
        }

        if (noNew) {
            exprList.Dispose ();
            return ExpressionError (ref compileData, expr);
        }

        var valueReg = irWriter.RentRegister (TypeNode (ref compileData, arrType));
        var value = LocalValueExpression (valueReg);
        var arrTypeInfo = (ES_ArrayData*) compileData.ToTypeInfo (arrType);
        Debug.Assert (arrTypeInfo->TypeInfo.TypeTag == ES_TypeTag.Array);
        exprList.AddExpression (AssignmentExpression (
            value,
            NewArrayExpression (arrTypeInfo, List (dimValues.Span))
        ));

        exprList.ReturnRegisters (irWriter);

        return ExpressionData.NewValue (expr, arrType, exprList, value, valueReg, false, false);
    }

    private static ExpressionData CheckExpression_Indexing (
        ref CompileData compileData, ref PassData passData, ES_AstIndexingExpression expr
    ) {
        var irWriter = passData.IRWriter;

        var typeIndex = compileData.GetArrayIndexType ();
        var typeUnknMut = compileData.GetUnknownType (ESC_Constness.Mutable);

        var arrRank = expr.DimensionExpressions.Length;
        var indexedExprData = CheckExpression (ref compileData, ref passData, expr.IndexedExpression, typeUnknMut);

        var exprList = indexedExprData.Expressions;

        using var dimsList = new StructPooledList<ESIR_Expression> (CL_ClearMode.Auto);
        foreach (var dim in expr.DimensionExpressions) {
            Debug.Assert (dim is not null);

            var dimExprData = CheckExpression (ref compileData, ref passData, dim, typeIndex);
            EnsureCompat (ref compileData, ref passData, ref dimExprData, typeIndex, dim.NodeBounds);

            dimExprData.Expressions.ReturnRegisters (irWriter);
            exprList.Merge (ref dimExprData.Expressions);

            dimsList.Add (dimExprData.Value);
            exprList.AddRegister (dimExprData.ValueRegister);
        }

        var badRank = false;
        var elemType = typeUnknMut;
        var writable = false;
        if (indexedExprData.Type.Type is not null) {
            var indexedType = indexedExprData.Type;

            if (indexedType.Type is ESC_TypeUnknown) {
                badRank = false;
                elemType = typeUnknMut;
                writable = true;
            } else if (indexedType.Type is ESC_TypeArray typeArray) {
                badRank = arrRank != typeArray.Rank;
                elemType = typeArray.ElementType.WithInheritedConst (indexedExprData.Type.Constness);

                writable = true;
            } else {
                compileData.ErrorList.Add (ES_FrontendErrors.GenCantApplyIndexingToType (
                    compileData.GetNiceNameString (indexedType, true),
                    passData.Source, indexedExprData.Expr.NodeBounds
                ));
            }
        } else {
            compileData.ErrorList.Add (new (
                passData.Source, indexedExprData.Expr.NodeBounds, ES_FrontendErrors.CantApplyIndexing
            ));
        }

        writable &= elemType.IsWritable ();

        if (badRank) {
            compileData.ErrorList.Add (new (
                passData.Source, indexedExprData.Expr.NodeBounds, ES_FrontendErrors.IndexingBadRank
            ));
        }

        var value = IndexingExpression (indexedExprData.Value, List (dimsList.Span));

        return ExpressionData.NewValue (expr, elemType, exprList, value, null, writable, false);
    }

    private static ExpressionData CheckExpression_Name (
        ref CompileData compileData, ES_AstNameExpression expr, ESC_TypeRef expectedType
    ) {
        var id = compileData.IdPool.GetIdentifier (expr.Value.Text.Span);
        var symbol = compileData.Symbols.GetSymbol (id);

        var writable = symbol.Flags.HasFlag (FrontendSymbolFlags.Writable);
        var compTimeConst = symbol.Flags.HasFlag (FrontendSymbolFlags.CompileTimeConstant);

        switch (symbol.Tag) {
            case FrontendSymbolType.None: {
                var symbolName = expr.Value.Text.Span.GetPooledString ();
                compileData.ErrorList.Add (ES_FrontendErrors.GenCantFindSymbol (symbolName, expr.Value));

                return ExpressionError (ref compileData, expr);
            }

            case FrontendSymbolType.Variable: {
                var varData = symbol.MatchVar ();
                return ExpressionData.NewValue (expr, varData.Type, varData.IRExpression, null, writable, compTimeConst);
            }

            case FrontendSymbolType.Type: {
                if (expectedType.Type is not null) {
                    compileData.ErrorList.Add (ES_FrontendErrors.GenInvalidExprTerm (
                        expr.Value.Text.Span.GetPooledString (),
                        expr.Value
                    ));

                    return ExpressionError (ref compileData, expr);
                }

                return ExpressionData.NewType (expr, symbol.MatchType ());
            }

            case FrontendSymbolType.Function: {
                var func = symbol.MatchFunction ();
                return ExpressionData.NewFunction (expr, func, new (ESC_Constness.Mutable, func.Prototype));
            }

            default:
                throw new NotImplementedException ("Symbol type not implemented.");
        }
    }

    private static ExpressionData CheckExpression_MemberAccess (
        ref CompileData compileData, ref PassData passData, ES_AstMemberAccessExpression expr
    ) {
        Debug.Assert (expr.Member is not null);

        var idPool = compileData.IdPool;

        var parentExpr = CheckExpression (ref compileData, ref passData, expr.Parent, ESC_TypeRef.Null (ESC_Constness.Mutable));
        var memberId = idPool.GetIdentifier (expr.Member.Value.Text.Span);

        if (parentExpr.Type.Type is not null) {
            CheckExpression_Dereference (ref parentExpr);

            var type = parentExpr.Type;

            switch (type.Type) {
                case ESC_TypeUnknown:
                    return ExpressionError (ref compileData, expr);

                case ESC_TypeStruct: {
                    return CheckExpression_MemberAccess_Basic (
                        ref compileData, expr,
                        parentExpr, memberId
                    );
                }

                case ESC_TypeArray: {
                    var ret = CheckExpression_MemberAccess_Basic (
                        ref compileData, expr,
                        parentExpr, memberId
                    );

                    ret.Writable = false;

                    return ret;
                }

                default:
                    throw new NotImplementedException ("Type not implemented yet.");
            }
        } else if (parentExpr.TypeInfo.Type is not null) {
            var type = parentExpr.TypeInfo;

            switch (type.Type) {
                case ESC_TypeUnknown:
                    return ExpressionError (ref compileData, expr);

                case ESC_TypeStruct: {
                    return CheckExpression_MemberAccessStatic_Aggregate (
                        ref compileData, expr,
                        parentExpr, memberId
                    );
                }

                case ESC_TypeArray: {
                    compileData.ErrorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
                        compileData.GetNiceNameString (type, true),
                        expr.Member.Value.Text.Span.GetPooledString (),
                        expr.Member.Value
                    ));

                    return ExpressionError (ref compileData, expr);
                }

                default:
                    throw new NotImplementedException ("Type not implemented yet.");
            }
        } else if (parentExpr.Function is not null)
            throw new NotImplementedException ("Not supported. (yet?)");
        else
            throw new CompilationException ("<<Unknown expression type in CheckTypes_Expression_MemberAccess>>");
    }

    private static ExpressionData CheckExpression_MemberAccess_Basic (
        ref CompileData compileData, ES_AstMemberAccessExpression expr,
        ExpressionData parentExpr, ES_Identifier memberId
    ) {
        Debug.Assert (expr.Member is not null);
        Debug.Assert (parentExpr.Type.Type is not null);

        var exprList = parentExpr.Expressions;
        exprList.AddRegister (parentExpr.ValueRegister);

        foreach (var memberData in parentExpr.Type.Type.GetMembers ()) {
            if (!memberData.Name.Equals (memberId))
                continue;

            switch (memberData) {
                case ESC_TypeMember_Field memberField: {
                    var varType = memberField.FieldType.WithInheritedConst (parentExpr.Type.Constness);

                    if (memberField.Flags.HasFlag (ESC_MemberFlags.Static)) {
                        compileData.ErrorList.Add (ES_FrontendErrors.GenStaticAccessOnInst (
                            compileData.GetNiceNameString (parentExpr.Type, true),
                            expr.Member.Value.Text.Span.GetPooledString (),
                            expr.Member.Value
                        ));
                        exprList.Dispose ();
                    }

                    var writable = parentExpr.Writable & varType.IsWritable ();
                    var value = MemberAccessExpression (parentExpr.Value, memberData.Name);

                    return ExpressionData.NewValue (expr, varType, exprList, value, null, writable, false);
                }

                case ESC_TypeMember_Function:
                    throw new NotImplementedException ("[TODO] Member function access not implemented yet.");

                default:
                    throw new NotImplementedException ("Member type not implemented yet.");
            }
        }

        compileData.ErrorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
            compileData.GetNiceNameString (parentExpr.Type, true),
            expr.Member.Value.Text.Span.GetPooledString (),
            expr.Member.Value
        ));

        exprList.Dispose ();
        return ExpressionError (ref compileData, expr);
    }

    private static ExpressionData CheckExpression_MemberAccessStatic_Aggregate (
        ref CompileData compileData, ES_AstMemberAccessExpression expr,
        ExpressionData parentExpr, ES_Identifier memberId
    ) {
        Debug.Assert (expr.Member is not null);

        var type = parentExpr.TypeInfo;
        Debug.Assert (type.Type is not null);

        var exprList = parentExpr.Expressions;
        exprList.AddRegister (parentExpr.ValueRegister);

        foreach (var memberData in type.Type.GetMembers ()) {
            if (!memberData.Name.Equals (memberId))
                continue;

            switch (memberData) {
                case ESC_TypeMember_Field memberField: {
                    if (!memberField.Flags.HasFlag (ESC_MemberFlags.Static)) {
                        compileData.ErrorList.Add (ES_FrontendErrors.GenInstAccessOnStatic (
                            expr.Member.Value.Text.Span.GetPooledString (),
                            expr.Member.Value
                        ));
                        exprList.Dispose ();
                        return ExpressionError (ref compileData, expr);
                    }

                    var memberType = memberField.FieldType;
                    var writable = memberType.IsWritable ();

                    var mangledName = MangleStaticVar (ref compileData, type.Type.Name, memberData.Name);
                    var value = StaticVariableExpression (mangledName);
                    // TODO: Handle constants.
                    return ExpressionData.NewValue (expr, memberType, exprList, value, null, writable, false);
                }

                case ESC_TypeMember_Function:
                    throw new NotImplementedException ("[TODO] Static member function access not implemented yet.");

                default:
                    throw new NotImplementedException ("Member type not implemented yet.");
            }
        }

        compileData.ErrorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
            compileData.GetNiceNameString (type, true),
            expr.Member.Value.Text.Span.GetPooledString (),
            expr.Member.Value
        ));

        exprList.Dispose ();
        return ExpressionError (ref compileData, expr);
    }

    private static ExpressionData CheckExpression_IncDec (
        ref CompileData compileData, ref PassData passData, ES_AstIncDecExpression expr,
        ESC_TypeRef expectedType
    ) {
        var exprData = CheckExpression (ref compileData, ref passData, expr.Inner, expectedType);

        if (exprData.Type.Type is ESC_TypeUnknown) {
            exprData.Expressions.Dispose ();
            return ExpressionError (ref compileData, expr);
        }

        if (!exprData.Writable || !exprData.Type.IsWritable ()) {
            compileData.ErrorList.Add (new (
                passData.Source, expr.Inner.NodeBounds, ES_FrontendErrors.TempValueInIncDecOp
            ));
        }

        // Emit IR.
        ESIR_NodeKind op;
        if (expr.Postfix)
            op = !expr.Decrement ? ESIR_NodeKind.UnaryPostIncrement : ESIR_NodeKind.UnaryPostDecrement;
        else
            op = !expr.Decrement ? ESIR_NodeKind.UnaryPreIncrement : ESIR_NodeKind.UnaryPreDecrement;

        var exprList = exprData.Expressions;
        var value = UnaryExpression (op, exprData.Value);

        if (exprData.Type.Type is ESC_TypeInt || exprData.Type.Type is ESC_TypeFloat)
            return ExpressionData.NewValue (expr, exprData.Type, exprList, value, exprData.ValueRegister, false, false);
        else
            throw new NotImplementedException ("[TODO] ?");
    }

    private static ExpressionData CheckExpression_Unary (
        ref CompileData compileData, ref PassData passData, ES_AstSimpleUnaryExpression expr,
        ESC_TypeRef expectedType
    ) {
        var exprData = CheckExpression (ref compileData, ref passData, expr.Inner, expectedType);

        if (!compileData.UnaryOpCompat (exprData.Type, expr.ExpressionType, out var finalType, out _)) {
            compileData.ErrorList.Add (ES_FrontendErrors.GenCantApplyUnaryOp (
                expr.OperatorToken.Text.Span.GetPooledString (),
                compileData.GetNiceNameString (exprData.Type, true),
                passData.Source, exprData.Expr.NodeBounds
            ));

            exprData.Expressions.Dispose ();
            return ExpressionError (ref compileData, expr);
        }

        var retType = finalType.WithInheritedConst (exprData.Type.Constness);
        var writable = retType.IsWritable ();

        var value = expr.ExpressionType switch {
            SimpleUnaryExprType.Positive => exprData.Value,
            SimpleUnaryExprType.Negative => UnaryExpression (ESIR_NodeKind.UnaryNegative, exprData.Value),

            SimpleUnaryExprType.LogicalNot => UnaryExpression (ESIR_NodeKind.UnaryLogicalNot, exprData.Value),
            SimpleUnaryExprType.BitNot => UnaryExpression (ESIR_NodeKind.UnaryBitNot, exprData.Value),

            SimpleUnaryExprType.Dereference => UnaryExpression (ESIR_NodeKind.UnaryDereference, exprData.Value),

            _ => throw new NotImplementedException ("Unary operation not implemented."),
        };

        return ExpressionData.NewValue (expr, retType, exprData.Expressions, value, exprData.ValueRegister, writable, false);
    }

    private static ExpressionData CheckExpression_Cast (
        ref CompileData compileData, ref PassData passData, ES_AstCastExpression expr
    ) {
        var destType = GetTypeRef (expr.DestinationType);
        var exprData = CheckExpression (ref compileData, ref passData, expr.InnerExpression, compileData.GetUnknownType (ESC_Constness.Mutable));

        if (!ExplicitCast (ref compileData, ref passData, ref exprData, destType, out var castRedundant)) {
            compileData.ErrorList.Add (ES_FrontendErrors.GenNoExplicitCast (
                compileData.GetNiceNameString (destType, true),
                compileData.GetNiceNameString (exprData.Type, true),
                passData.Source, expr.NodeBounds
            ));

            exprData.Expressions.Dispose ();
            return ExpressionError (ref compileData, expr);
        }

        var exprlist = exprData.Expressions;

        if (castRedundant) {
            compileData.InfoList.Add (new (
                passData.Source, expr.CastBounds, ES_FrontendInfoMsg.RedundantCast
            ));
        }

        return ExpressionData.NewValue (expr, exprData.Type, exprlist, exprData.Value, exprData.ValueRegister, false, false);
    }

    private static ExpressionData CheckExpression_FunctionCall (
        ref CompileData compileData, ref PassData passData, ES_AstFunctionCallExpression expr
    ) {
        var funcExpr = CheckExpression (ref compileData, ref passData, expr.FunctionExpression, compileData.GetUnknownType (ESC_Constness.Mutable));

        if (funcExpr.Function is not null)
            return CheckExpression_SimpleFunctionCall (ref compileData, ref passData, expr, funcExpr);
        else if (funcExpr.TypeInfo.Type is not null) {
            compileData.ErrorList.Add (ES_FrontendErrors.GenCantInvokeType (
                compileData.GetNiceNameString (funcExpr.TypeInfo, true), passData.Source, funcExpr.Expr.NodeBounds
            ));
        } else if (funcExpr.Type.Type is not null) {
            // TODO: Some types might be allowed to have `()` overrides too in the future. But not now.
            if (funcExpr.Type.Type is not ESC_TypeUnknown) {
                compileData.ErrorList.Add (new (
                    passData.Source, funcExpr.Expr.NodeBounds, ES_FrontendErrors.CantInvokeExpr
                ));
            }
        } else
            Debug.Fail ("???");

        return ExpressionError (ref compileData, expr);
    }

    private static ExpressionData CheckExpression_SimpleFunctionCall (
        ref CompileData compileData, ref PassData passData, ES_AstFunctionCallExpression expr,
        ExpressionData funcExpr
    ) {
        Debug.Assert (funcExpr.Function is not null);

        var typeUnknMut = compileData.GetUnknownType (ESC_Constness.Mutable);

        var func = funcExpr.Function;
        var funcType = func.Prototype;
        var mangledName = MangleFunctionName (ref compileData, func);

        var funcArgCount = funcType.Arguments.Length;
        var callArgCount = expr.Arguments.Length;
        var reqArgCount = funcArgCount - func.OptionalArgsCount;
        var ignoreDefArgs = false;

        if (callArgCount < reqArgCount) {
            var errBounds = expr.FunctionExpression.NodeBounds;

            var arg = func.Arguments [callArgCount];
            compileData.ErrorList.Add (ES_FrontendErrors.GenMissingFuncArg (
                arg.Name.GetCharsSpan ().GetPooledString (),
                func.Name.GetCharsSpan ().GetPooledString (),
                passData.Source, errBounds
            ));
            ignoreDefArgs = true;
        }

        var exprList = funcExpr.Expressions;
        exprList.AddRegister (funcExpr.ValueRegister);

        using var argsList = new StructPooledList<ESIR_ArgumentValue> (CL_ClearMode.Auto);
        var argIdx = 0;
        for (; argIdx < callArgCount; argIdx++) {
            var arg = expr.Arguments [argIdx];

            if (argIdx >= funcArgCount) {
                if (argIdx == funcArgCount) {
                    compileData.ErrorList.Add (ES_FrontendErrors.GenTooManyFuncArgs (
                        func.Name.GetCharsSpan ().GetPooledString (),
                        passData.Source, arg.ValueExpression.NodeBounds
                    ));
                    ignoreDefArgs = true;
                }

                var exprData = CheckExpression (ref compileData, ref passData, arg.ValueExpression, typeUnknMut);
                exprData.Expressions.Dispose ();
                passData.IRWriter.ReturnRegister (exprData.ValueRegister);
                continue;
            }

            var argData = func.Arguments [argIdx];
            var argTypeData = funcType.Arguments [argIdx];

            if (arg.ArgType != argTypeData.ArgType && argTypeData.ArgType != ES_ArgumentType.In) {
                compileData.ErrorList.Add (ES_FrontendErrors.GenWrongArgType (
                    argData.Name.GetCharsSpan ().GetPooledString (),
                    arg.ArgType.ToString (), passData.Source, arg.ValueExpression.NodeBounds
                ));
            }

            var argValType = argTypeData.ValueType;
            var argExprData = CheckExpression (ref compileData, ref passData, arg.ValueExpression, argValType);
            EnsureCompat (ref compileData, ref passData, ref argExprData, argValType, argExprData.Expr.NodeBounds);

            exprList.Merge (ref argExprData.Expressions);
            exprList.AddRegister (argExprData.ValueRegister);

            argsList.Add (ArgumentValue (arg.ArgType, argExprData.Value));
        }

        if (!ignoreDefArgs) {
            for (; argIdx < funcArgCount; argIdx++) {
                var argData = func.Arguments [argIdx];
                var argTypeData = funcType.Arguments [argIdx];

                Debug.Assert (argData.DefaultValue is not null);

                var argValType = argTypeData.ValueType;
                var argExprData = CheckExpression (ref compileData, ref passData, argData.DefaultValue, argValType);
                EnsureCompat (ref compileData, ref passData, ref argExprData, argValType, argExprData.Expr.NodeBounds);

                exprList.Merge (ref argExprData.Expressions);
                exprList.AddRegister (argExprData.ValueRegister);

                argsList.Add (ArgumentValue (ES_ArgumentType.Normal, argExprData.Value));
            }
        }

        var value = FunctionCallExpression (mangledName, List (argsList.Span));

        return ExpressionData.NewValue (expr, funcType.ReturnType, exprList, value, null, false, false);
    }

    private static bool CheckExpression_SimpleBinaryExpression_Compat (
        ref CompileData compileData, ES_AstSimpleBinaryExpression expr,
        ref ExpressionData lhs, ref ExpressionData rhs,
        out ESC_TypeRef finalType
    ) {
        var op = expr.ExpressionType;

        var lhsNull = lhs.Type.Type is ESC_TypeNull;
        var rhsNull = rhs.Type.Type is ESC_TypeNull;
        var lhsRef = lhs.Type.Type?.IsReferenceType () ?? false;
        var rhsRef = rhs.Type.Type?.IsReferenceType () ?? false;

        Debug.Assert (!(lhsNull & rhsNull));
        if (!compileData.BinaryOpCompat (lhs.Type, rhs.Type, op, out finalType, out _))
            return false;

        if (lhsNull && rhsRef) {
            lhs.Type = rhs.Type;
            lhs.Value = NullLiteralExpression (TypeNode (ref compileData, rhs.Type));
        } else if (rhsNull && lhsRef) {
            rhs.Type = lhs.Type;
            rhs.Value = NullLiteralExpression (TypeNode (ref compileData, lhs.Type));
        } else if (lhs.Type.Type is ESC_TypeInt lhsInt && rhs.Type.Type is ESC_TypeInt rhsInt) {
            if (op.IsBitShift ()) {
                if (rhsInt.Size < ES_IntSize.Int32) {
                    rhs.Type = compileData.GetIntType (ES_IntSize.Int32, false, rhs.Type.Constness);
                    rhs.Value = CastExpression (rhs.Value, TypeNode (ref compileData, rhs.Type));
                }

                return true;
            }

            if (lhsInt.Size == rhsInt.Size)
                return true;

            if (lhsInt.Size > rhsInt.Size) {
                rhs.Type = lhs.Type;
                rhs.Value = CastExpression (rhs.Value, TypeNode (ref compileData, lhs.Type));
            } else {
                lhs.Type = rhs.Type;
                lhs.Value = CastExpression (lhs.Value, TypeNode (ref compileData, rhs.Type));
            }
        }

        return true;
    }

    private static ESIR_NodeKind? CheckExpression_SimpleBinaryExpression_Expr (SimpleBinaryExprType op) {
        return op switch {
            SimpleBinaryExprType.Add => ESIR_NodeKind.BinaryExprAdd,
            SimpleBinaryExprType.AssignAdd => ESIR_NodeKind.BinaryExprAdd,

            SimpleBinaryExprType.Subtract => ESIR_NodeKind.BinaryExprSubtract,
            SimpleBinaryExprType.AssignSubtract => ESIR_NodeKind.BinaryExprSubtract,

            SimpleBinaryExprType.Multiply => ESIR_NodeKind.BinaryExprMultiply,
            SimpleBinaryExprType.AssignMultiply => ESIR_NodeKind.BinaryExprMultiply,

            SimpleBinaryExprType.Divide => ESIR_NodeKind.BinaryExprDivide,
            SimpleBinaryExprType.AssignDivide => ESIR_NodeKind.BinaryExprDivide,

            SimpleBinaryExprType.Modulo => ESIR_NodeKind.BinaryExprModulo,
            SimpleBinaryExprType.AssignModulo => ESIR_NodeKind.BinaryExprModulo,

            SimpleBinaryExprType.Power => ESIR_NodeKind.BinaryExprPower,
            SimpleBinaryExprType.AssignPower => ESIR_NodeKind.BinaryExprPower,

            SimpleBinaryExprType.Concatenation => ESIR_NodeKind.BinaryExprConcat,
            SimpleBinaryExprType.AssignConcatenate => ESIR_NodeKind.BinaryExprConcat,

            SimpleBinaryExprType.ShiftLeft => ESIR_NodeKind.BinaryExprShiftLeft,
            SimpleBinaryExprType.AssignShiftLeft => ESIR_NodeKind.BinaryExprShiftLeft,

            SimpleBinaryExprType.ShiftRight => ESIR_NodeKind.BinaryExprShiftRight,
            SimpleBinaryExprType.AssignShiftRight => ESIR_NodeKind.BinaryExprShiftRight,

            SimpleBinaryExprType.ShiftRightUnsigned => ESIR_NodeKind.BinaryExprShiftRightUnsigned,
            SimpleBinaryExprType.AssignShiftRightUnsigned => ESIR_NodeKind.BinaryExprShiftRightUnsigned,

            SimpleBinaryExprType.LesserThan => ESIR_NodeKind.BinaryExprLesserThan,

            SimpleBinaryExprType.GreaterThan => ESIR_NodeKind.BinaryExprGreaterThan,

            SimpleBinaryExprType.LesserThanEqual => ESIR_NodeKind.BinaryExprLesserThanEqual,

            SimpleBinaryExprType.GreaterThanEqual => ESIR_NodeKind.BinaryExprGreaterThanEqual,

            SimpleBinaryExprType.Equals => ESIR_NodeKind.BinaryExprEquals,

            SimpleBinaryExprType.NotEquals => ESIR_NodeKind.BinaryExprNotEquals,

            SimpleBinaryExprType.BitAnd => ESIR_NodeKind.BinaryExprBitAnd,
            SimpleBinaryExprType.AssignBitAnd => ESIR_NodeKind.BinaryExprBitAnd,

            SimpleBinaryExprType.BitXor => ESIR_NodeKind.BinaryExprBitXor,
            SimpleBinaryExprType.AssignXor => ESIR_NodeKind.BinaryExprBitXor,

            SimpleBinaryExprType.BitOr => ESIR_NodeKind.BinaryExprBitOr,
            SimpleBinaryExprType.AssignBitOr => ESIR_NodeKind.BinaryExprBitOr,

            SimpleBinaryExprType.LogicalAnd => ESIR_NodeKind.BinaryExprLogicalAnd,

            SimpleBinaryExprType.LogicalOr => ESIR_NodeKind.BinaryExprLogicalOr,

            _ => null,
        };
    }

    private static ExpressionData CheckExpression_SimpleBinaryExpression (
        ref CompileData compileData, ref PassData passData,
        ES_AstSimpleBinaryExpression expr
    ) {
        var typeUnkn = compileData.GetUnknownType (ESC_Constness.Mutable);

        var leftExpr = CheckExpression (ref compileData, ref passData, expr.Left, typeUnkn);

        var expectedRightType = typeUnkn;
        if (expr.ExpressionType.IsBitShift () && leftExpr.Type.Type is ESC_TypeInt)
            expectedRightType = compileData.GetIntType (ES_IntSize.Int32, false, ESC_Constness.Const);
        else if (expr.ExpressionType.IsAssignment ())
            expectedRightType = leftExpr.Type.Type is not null ? leftExpr.Type : typeUnkn;

        var rightExpr = CheckExpression (ref compileData, ref passData, expr.Right, expectedRightType);

        if (leftExpr.Type.Type is null || rightExpr.Type.Type is null) {
            leftExpr.Expressions.Dispose ();
            rightExpr.Expressions.Dispose ();
            return ExpressionError (ref compileData, expr);
        }

        if (!CheckExpression_SimpleBinaryExpression_Compat (
            ref compileData, expr,
            ref leftExpr, ref rightExpr,
            out var finalType
        )) {
            compileData.ErrorList.Add (ES_FrontendErrors.GenCantApplyBinaryOp (
                expr.OperatorToken.Text.Span.GetPooledString (),
                compileData.GetNiceNameString (leftExpr.Type, true),
                compileData.GetNiceNameString (rightExpr.Type, true),
                passData.Source, expr.NodeBounds
            ));

            leftExpr.Expressions.Dispose ();
            rightExpr.Expressions.Dispose ();
            return ExpressionError (ref compileData, expr);
        }

        var isAssignment = expr.ExpressionType.IsAssignment ();
        if (isAssignment && !leftExpr.Writable) {
            compileData.ErrorList.Add (new (
                passData.Source, leftExpr.Expr.NodeBounds, ES_FrontendErrors.CannotAssignExpr
            ));
        }

        // Emit IR.
        var exprList = leftExpr.Expressions;
        exprList.Merge (ref rightExpr.Expressions);
        exprList.AddRegister (leftExpr.ValueRegister);
        exprList.AddRegister (rightExpr.ValueRegister);

        if (expr.ExpressionType == SimpleBinaryExprType.Assign) {
            var assignValue = AssignmentExpression (leftExpr.Value, rightExpr.Value);
            return ExpressionData.NewValue (expr, finalType, exprList, assignValue, null, false, false);
        }

        var opKind = CheckExpression_SimpleBinaryExpression_Expr (expr.ExpressionType);

        if (opKind is null) {
            exprList.Dispose ();

            compileData.ErrorList.Add (new (expr.OperatorToken, "Binary expression not handled."));
            return ExpressionError (ref compileData, expr);
        }

        ESIR_Expression value = SimpleBinaryExpression (opKind.Value, leftExpr.Value, rightExpr.Value);

        if (isAssignment)
            value = AssignmentExpression (leftExpr.Value, value);

        return ExpressionData.NewValue (expr, finalType, exprList, value, null, false, false);
    }
#endif
}
