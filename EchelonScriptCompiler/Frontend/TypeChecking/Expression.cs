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
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.CompilerCommon.IR;

using static EchelonScriptCompiler.CompilerCommon.IR.ESIR_Factory;

namespace EchelonScriptCompiler.Frontend;

internal unsafe static partial class Compiler_TypeChecking {
    private enum Constness {
        Mutable,
        Const,
        Immutable,
    }

    private struct TypeData {
        #region ================== Static properties

        public static TypeData Null => new (Constness.Mutable, null);

        #endregion

        #region ================== Instance properties

        public Constness Constness { get; private init; }
        public ES_TypeInfo* Type { get; private init; }

        public bool IsWritable => (Type is not null && Type->IsWritable ()) & Constness.IsWritable ();

        #endregion

        public TypeData (Constness constness, ES_TypeInfo* type) {
            Debug.Assert (
                type is null ||
                (type->TypeTag != ES_TypeTag.Const && type->TypeTag != ES_TypeTag.Immutable)
            );

            Constness = constness;
            Type = type;
        }

        #region ================== Instance methods

        public TypeData WithType (ES_TypeInfo* type) => new (Constness, type);
        public TypeData WithConst (Constness constness) => new (constness, Type);
        public TypeData WithInheritedConst (Constness constness)
            => WithConst (InheritConstness (constness, Constness));

        public ES_TypeInfo* ToType (ref PassData passData) {
            if (Type is null)
                return null;

            return Constness switch {
                Constness.Mutable => Type,
                Constness.Const => passData.EnvBuilder.CreateConstType (Type),
                Constness.Immutable => passData.EnvBuilder.CreateImmutableType (Type),

                _ => throw new NotImplementedException ("Constness type not implemented yet."),
            };
        }

        #region Equality

        public bool Equals (TypeData other) => Type == other.Type && Constness == other.Constness;

        public override bool Equals (object? obj) {
            if (obj is TypeData other)
                return Equals (other);

            return false;
        }

        public override int GetHashCode () => HashCode.Combine (Constness, (nint) Type);

        #endregion

        #endregion

        #region ================== Operators

        public static bool operator == (TypeData lhs, TypeData rhs) => lhs.Equals (rhs);
        public static bool operator != (TypeData lhs, TypeData rhs) => !lhs.Equals (rhs);

        #endregion
    }

    private struct ExpressionData {
        public ES_AstExpression Expr;
        public TypeData Type;
        public ES_TypeInfo* TypeInfo;
        public ES_FunctionData* Function;

        public ExpressionList Expressions;
        public ESIR_Expression Value;
        public int? ValueRegister;

        public bool Writable;
        public bool CompileTimeConst;

        public static ExpressionData NewValue (
            ES_AstExpression expr,
            TypeData type, ExpressionList exprs, ESIR_Expression value, int? reg,
            bool writable, bool compTimeConst
        ) {
            return new ExpressionData {
                Expr = expr,
                Type = type,

                TypeInfo = null,
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
            TypeData type, ESIR_Expression value, int? reg,
            bool writable, bool compTimeConst
        ) {
            return new ExpressionData {
                Expr = expr,
                Type = type,

                TypeInfo = null,
                Function = null,

                Expressions = new ExpressionList (null, null),
                Value = value,
                ValueRegister = reg,

                Writable = writable,
                CompileTimeConst = compTimeConst,
            };
        }

        public static ExpressionData NewType (ES_AstExpression expr, ES_TypeInfo* typeInfo) {
            var ret = new ExpressionData {
                Expr = expr,
                TypeInfo = typeInfo,

                Type = TypeData.Null,
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
            ES_FunctionData* func, ES_TypeInfo* funcType
        ) {
            var ret = new ExpressionData {
                Expr = expr,
                Function = func,
                TypeInfo = funcType,

                Type = TypeData.Null,

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
            ES_FunctionData* func, ES_TypeInfo* funcType, ExpressionList values, ESIR_Expression value, int? reg
        ) {
            return new ExpressionData {
                Expr = expr,
                Function = func,
                TypeInfo = funcType,

                Type = TypeData.Null,

                Expressions = values,
                Value = value,
                ValueRegister = reg,

                Writable = false,
                CompileTimeConst = false,
            };
        }

        public static ExpressionData NewFunction (
            ES_AstExpression expr,
            ES_FunctionData* func, ES_TypeInfo* funcType, ESIR_Expression value, int? reg
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

    private static ES_FunctionData* FindConstructor (
        ES_TypeInfo* objType, ReadOnlySpan<ES_FunctionPrototypeArgData> arguments
    ) {
        return null;
    }

    private static ExpressionData ExpressionError (
        ref PassData passData, ES_AstExpression expr, Constness constness = Constness.Mutable, bool writable = false
    ) => ExpressionData.NewValue (expr, passData.GetUnknownType (constness), ErrorExpression (), null, writable, false);

    private static Constness InheritConstness (Constness baseConst, Constness thisConst) {
        return baseConst switch {
            Constness.Mutable => thisConst,
            Constness.Const => (thisConst == Constness.Immutable) ? thisConst : baseConst,
            Constness.Immutable => Constness.Immutable,

            _ => throw new NotImplementedException ("Constness type not implemented."),
        };
    }

    private static PooledArray<TypeData> DeconstructType (ref PassData passData, TypeData typeData) {
        using var typeList = new StructPooledList<TypeData> (CL_ClearMode.Auto);

        // Deconstruct the type.
        var type = typeData.Type;
        var constness = typeData.Constness;
        while (type is not null) {
            switch (type->TypeTag) {
                case ES_TypeTag.UNKNOWN:
                case ES_TypeTag.Null:
                case ES_TypeTag.Void:
                case ES_TypeTag.Bool:
                case ES_TypeTag.Int:
                case ES_TypeTag.Float:
                case ES_TypeTag.FuncPrototype:
                case ES_TypeTag.Struct:
                case ES_TypeTag.Class:
                case ES_TypeTag.Enum:
                case ES_TypeTag.Interface:
                    typeList.Add (new (constness, type));
                    type = null;
                    break;

                case ES_TypeTag.Reference: {
                    var refData = (ES_ReferenceData*) type;

                    typeList.Add (new (constness, type));
                    type = refData->PointedType;
                    break;
                }

                case ES_TypeTag.Const:
                case ES_TypeTag.Immutable: {
                    var constData = (ES_ConstData*) type;

                    if (type->TypeTag == ES_TypeTag.Immutable)
                        constness = Constness.Immutable;
                    else if (constness != Constness.Immutable)
                        constness = Constness.Const;
                    type = constData->InnerType;
                    break;
                }

                case ES_TypeTag.Array: {
                    var arrData = (ES_ArrayTypeData*) type;

                    typeList.Add (new (constness, type));
                    type = arrData->ElementType;
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

            switch (listType.Type->TypeTag) {
                case ES_TypeTag.UNKNOWN:
                case ES_TypeTag.Null:
                case ES_TypeTag.Void:
                case ES_TypeTag.Bool:
                case ES_TypeTag.Int:
                case ES_TypeTag.Float:
                case ES_TypeTag.FuncPrototype:
                case ES_TypeTag.Struct:
                case ES_TypeTag.Class:
                case ES_TypeTag.Enum:
                case ES_TypeTag.Interface:
                    break;

                case ES_TypeTag.Reference: {
                    var refData = (ES_ReferenceData*) listType.Type;

                    Debug.Assert (i + 1 < typeSpan.Length);
                    var innerType = typeSpan [i + 1].Type;
                    var newRefType = passData.EnvBuilder.CreateReferenceType (innerType);
                    listType = listType.WithType (newRefType);

                    break;
                }

                case ES_TypeTag.Const:
                case ES_TypeTag.Immutable:
                    Debug.Fail ("This shouldn't happen.");
                    break;

                case ES_TypeTag.Array: {
                    var arrData = (ES_ArrayTypeData*) listType.Type;

                    Debug.Assert (i + 1 < typeSpan.Length);
                    var innerType = typeSpan [i + 1].Type;
                    var newArrType = passData.EnvBuilder.CreateArrayType (innerType, arrData->DimensionsCount);
                    listType = listType.WithType (newArrType);

                    break;
                }

                default:
                    throw new NotImplementedException ("Type not implemented.");
            }
        }

        return typeList.MoveToArray ();
    }

    private static bool IsConstIgnorantType (TypeData type) {
        return type.Type->TypeTag switch {
            ES_TypeTag.Null => true,
            ES_TypeTag.Void => true,
            ES_TypeTag.Bool => true,
            ES_TypeTag.Int => true,
            ES_TypeTag.Float => true,
            ES_TypeTag.Enum => true,

            _ => false,
        };
    }

    private static bool CanConvertConstness (Constness thisConst, Constness destConst)
        => thisConst == destConst || destConst == Constness.Const;

    private static bool CanConvertConstness (ref PassData passData, TypeData origType, TypeData newType) {
        using var origDeconsArr = DeconstructType (ref passData, origType);
        using var newDeconsArr = DeconstructType (ref passData, newType);
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

    private static bool CanMatchConstness (ref PassData passData, TypeData lhs, TypeData rhs, out TypeData finalType) {
        using var lhsDeconsArr = DeconstructType (ref passData, lhs);
        using var rhsDeconsArr = DeconstructType (ref passData, rhs);
        var lhsData = lhsDeconsArr.Span;
        var rhsData = rhsDeconsArr.Span;

        Debug.Assert (lhsData.Length > 0);
        Debug.Assert (rhsData.Length > 0);

        if (lhsData.Length != rhsData.Length || lhsData [0].Type != rhsData [0].Type) {
            finalType = TypeData.Null;
            return false;
        }

        var typeLen = lhsData.Length;
        using var finalDeconsArr = PooledArray<TypeData>.GetArray (typeLen);
        var finalDecons = finalDeconsArr.Span;

        for (var i = 0; i < typeLen; i++) {
            var lhsConst = lhsData [i];
            var rhsConst = rhsData [i];
            ref var finalConst = ref finalDecons [i];

            Debug.Assert (lhsConst.Type == rhsConst.Type);

            var finalConstness = Constness.Const;
            if ((lhsConst.Constness, rhsConst.Constness) == (Constness.Mutable, Constness.Mutable))
                finalConstness = Constness.Mutable;
            else if ((lhsConst.Constness, rhsConst.Constness) == (Constness.Immutable, Constness.Immutable))
                finalConstness = Constness.Immutable;

            finalConst = lhsConst.WithConst (finalConstness);
        }

        var innerType = TypeData.Null;
        for (var i = finalDecons.Length - 1; i >= 0; i--) {
            ref var listType = ref finalDecons [i];

            if (innerType.Type is not null && listType.Constness != innerType.Constness) {
                var innerTypePtr = innerType.Type;

                if (innerType.Constness == Constness.Const)
                    innerTypePtr = passData.EnvBuilder.CreateConstType (innerType.Type);
                else if (innerType.Constness == Constness.Immutable)
                    innerTypePtr = passData.EnvBuilder.CreateImmutableType (innerType.Type);

                innerType = listType.WithType (innerTypePtr);
            }

            switch (listType.Type->TypeTag) {
                case ES_TypeTag.Null:
                case ES_TypeTag.Void:
                case ES_TypeTag.Bool:
                case ES_TypeTag.Int:
                case ES_TypeTag.Float:
                case ES_TypeTag.FuncPrototype:
                case ES_TypeTag.Struct:
                case ES_TypeTag.Class:
                case ES_TypeTag.Enum:
                case ES_TypeTag.Interface:
                    Debug.Assert (innerType.Type is null);
                    innerType = listType;
                    break;

                case ES_TypeTag.Reference: {
                    Debug.Assert (innerType.Type is not null);
                    Debug.Assert (i + 1 < finalDecons.Length);

                    var refData = (ES_ReferenceData*) listType.Type;
                    innerType = innerType.WithType (passData.EnvBuilder.CreateReferenceType (innerType.Type));

                    break;
                }

                case ES_TypeTag.Const:
                case ES_TypeTag.Immutable:
                    Debug.Fail ("This shouldn't happen.");
                    break;

                case ES_TypeTag.Array: {
                    Debug.Assert (innerType.Type is not null);
                    Debug.Assert (i + 1 < finalDecons.Length);

                    var arrData = (ES_ArrayTypeData*) listType.Type;
                    innerType = innerType.WithType (passData.EnvBuilder.CreateArrayType (innerType.Type, arrData->DimensionsCount));

                    break;
                }

                default:
                    throw new NotImplementedException ("Type not implemented.");
            }
        }

        finalType = innerType;
        return true;
    }

    private static bool IsWritable (this Constness constness) => constness == Constness.Mutable;

    private static ES_TypeInfo* StripFirstConst (ES_TypeInfo* type, out Constness constness) {
        switch (type->TypeTag) {
            case ES_TypeTag.Const:
            case ES_TypeTag.Immutable: {
                var constType = (ES_ConstData*) type;

                constness = (type->TypeTag == ES_TypeTag.Immutable) ? Constness.Immutable : Constness.Const;

                Debug.Assert (constType->InnerType->TypeTag != ES_TypeTag.Const);
                Debug.Assert (constType->InnerType->TypeTag != ES_TypeTag.Immutable);

                return constType->InnerType;
            }

            default:
                constness = Constness.Mutable;
                return type;
        }
    }

    private static TypeData UnpackFirstConst (ES_TypeInfo* type) {
        var retType = StripFirstConst (type, out var retConst);
        return new (retConst, retType);
    }

    private static bool MustBeCompat (
        ref PassData passData, ref ExpressionData exprData, TypeData destType, bool noModify = false
    ) {
        // We don't need to do any checks here if they're *literally* the same type.
        if (destType == exprData.Type)
            return true;

        if (CanConvertConstness (ref passData, exprData.Type, destType)) {
            if (!noModify)
                exprData.Value = CastExpression (exprData.Value, TypeNode (ref passData, destType));
        } else if (destType.Type->TypeTag == ES_TypeTag.UNKNOWN || exprData.Type.Type->TypeTag == ES_TypeTag.UNKNOWN)
            return true;
        else if (exprData.Type == destType) {
            // Do nothing
        } else if (exprData.Type.Type->TypeTag == ES_TypeTag.Null) {
            if (!IsNullable (ref passData, destType, out _))
                return false;

            if (!noModify) {
                exprData.Value = NullLiteralExpression (TypeNode (ref passData, destType));
                exprData.Writable = false;
            }
        } else if (destType.Type->TypeTag == ES_TypeTag.Int && exprData.Type.Type->TypeTag == ES_TypeTag.Int) {
            var destIntType = (ES_IntTypeData*) destType.Type;
            var givenIntType = (ES_IntTypeData*) exprData.Type.Type;

            if (givenIntType->Unsigned != destIntType->Unsigned || givenIntType->IntSize > destIntType->IntSize)
                return false;

            if (!noModify) {
                exprData.Value = CastExpression (exprData.Value, TypeNode (ref passData, destType));
                exprData.Writable = false;
            }
        } else
            return false;

        exprData.Type = destType;
        exprData.Writable &= destType.IsWritable;

        return true;
    }

    private static bool ExplicitCast (
        ref PassData passData, ref ExpressionData exprData, TypeData castType,
        out bool castRedundant, bool noModify = false
    ) {
        if (MustBeCompat (ref passData, ref exprData, castType, noModify)) {
            castRedundant = true;
            return true;
        }

        if (exprData.Type == castType && exprData.Type.Constness != castType.Constness)
            castRedundant = false;

        if (castType.Type->TypeTag == ES_TypeTag.Int && exprData.Type.Type->TypeTag == ES_TypeTag.Int) {
            var castIntType = (ES_IntTypeData*) castType.Type;
            var exprIntType = (ES_IntTypeData*) exprData.Type.Type;

            castRedundant = (
                castIntType->IntSize == exprIntType->IntSize &&
                castIntType->Unsigned == exprIntType->Unsigned
            );
        } else if (castType.Type->TypeTag == ES_TypeTag.Float && exprData.Type.Type->TypeTag == ES_TypeTag.Int) {
            castRedundant = false;
        } else if (castType.Type->TypeTag == ES_TypeTag.Int && exprData.Type.Type->TypeTag == ES_TypeTag.Float) {
            castRedundant = false;
        } else if (castType.Type->TypeTag == ES_TypeTag.Float && exprData.Type.Type->TypeTag == ES_TypeTag.Float) {
            var castFloatType = (ES_FloatTypeData*) castType.Type;
            var exprFloatType = (ES_FloatTypeData*) exprData.Type.Type;

            castRedundant = castFloatType->FloatSize == exprFloatType->FloatSize;
        } else {
            castRedundant = false;
            return false;
        }

        if (!noModify) {
            exprData.Type = castType;
            exprData.Value = CastExpression (exprData.Value, TypeNode (ref passData, castType));
            exprData.Writable = false;
        }

        return true;
    }

    private static bool EnsureCompat (
        ref ExpressionData exprData, TypeData destType,
        ref PassData passData, ES_AstNodeBounds bounds
    ) {
        if (exprData.Type.Type->TypeTag == ES_TypeTag.Null) {
            if (!IsNullable (ref passData, destType, out var retType)) {
                passData.ErrorList.Add (ES_FrontendErrors.GenTypeNotNullable (
                    passData.Env.GetNiceTypeNameString (destType.ToType (ref passData), true),
                    passData.Source, bounds
                ));

                exprData.Type = exprData.Type.WithType (passData.Env.TypeUnknownValue);
                exprData.Value = ErrorExpression ();
                return false;
            }

            exprData.Type = retType;
            exprData.Value = NullLiteralExpression (TypeNode (ref passData, retType));
            return true;
        } else if (!MustBeCompat (ref passData, ref exprData, destType)) {
            if (ExplicitCast (ref passData, ref exprData, destType, out _, true)) {
                passData.ErrorList.Add (ES_FrontendErrors.GenNoImplicitCast (
                    passData.Env.GetNiceTypeNameString (destType.ToType (ref passData), true),
                    passData.Env.GetNiceTypeNameString (exprData.Type.ToType (ref passData), true),
                    passData.Source, bounds
                ));
            } else {
                passData.ErrorList.Add (ES_FrontendErrors.GenNoCast (
                    passData.Env.GetNiceTypeNameString (destType.ToType (ref passData), true),
                    passData.Env.GetNiceTypeNameString (exprData.Type.ToType (ref passData), true),
                    passData.Source, bounds
                ));
            }

            return false;
        }

        return true;
    }

    private static void CheckExpression_Dereference (ref ExpressionData exprData) {
        if (exprData.Type.Type->TypeTag != ES_TypeTag.Reference)
            return;

        var refTypeData = (ES_ReferenceData*) exprData.Type.Type;

        var pointedType = StripFirstConst (refTypeData->PointedType, out var pointedConstness);
        pointedConstness = InheritConstness (exprData.Type.Constness, pointedConstness);

        exprData.Type = new (pointedConstness, pointedType);
        exprData.Value = UnaryExpression (ESIR_NodeKind.UnaryDereference, exprData.Value);
    }

    private static ExpressionData CheckExpression (
        ref PassData passData, ES_AstExpression expr,
        TypeData expectedType
    ) {
        Debug.Assert (expr is not null);

        var idPool = passData.Env.IdPool;
        var irWriter = passData.IRWriter;
        var symbols = passData.Symbols;

        var typeUnkn = passData.Env.TypeUnknownValue;
        var typeBool = passData.Env.TypeBool;
        var typeBoolConst = passData.GetBoolType (Constness.Const);

        switch (expr) {
            case ES_AstParenthesisExpression parenExpr:
                return CheckExpression (ref passData, parenExpr.Inner, expectedType);

            #region Primary expressions

            case ES_AstFunctionCallExpression funcCallExpr:
                return CheckExpression_FunctionCall (ref passData, funcCallExpr, expectedType);

            case ES_AstIndexingExpression indexExpr:
                return CheckExpression_Indexing (ref passData, indexExpr, expectedType);

            case ES_AstNewObjectExpression newObjExpr:
                return CheckExpression_NewObject (ref passData, newObjExpr, expectedType);

            case ES_AstNewArrayExpression newArrayExpr:
                return CheckExpression_NewArray (ref passData, newArrayExpr, expectedType);

            case ES_AstIntegerLiteralExpression:
            case ES_AstBooleanLiteralExpression:
            case ES_AstFloatLiteralExpression:
                throw new CompilationException (ES_FrontendErrors.ConstFoldFailure);

            case ES_AstStringLiteralExpression:
                throw new NotImplementedException ("[TODO] String literals not implemented yet.");

            case ES_AstCharLiteralExpression:
                throw new NotImplementedException ("[TODO] Character literals not implemented yet.");

            case ES_AstIntegerConstantExpression intConstExpr: {
                Debug.Assert (intConstExpr.IntType->TypeTag == ES_TypeTag.Int);
                var intType = (ES_IntTypeData*) intConstExpr.IntType;

                var litVal = intType->Unsigned
                    ? ValueNode (intConstExpr.SignExtend ())
                    : ValueNode ((long) intConstExpr.SignExtend ());
                var litExpr = LiteralExpression (ESIR_NodeKind.LiteralInt, litVal);
                var value = CastExpression (litExpr, TypeNode (ref passData, intConstExpr.IntType));

                return ExpressionData.NewValue (expr, new (Constness.Mutable, &intType->TypeInfo), value, null, false, true);
            }

            case ES_AstBooleanConstantExpression boolConstExpr: {
                var value = boolConstExpr.Value ? LiteralTrueExpression () : LiteralFalseExpression ();
                return ExpressionData.NewValue (expr, typeBoolConst, value, null, false, true);
            }

            case ES_AstFloat32ConstantExpression float32ConstExpr: {
                var value = LiteralExpression (ESIR_NodeKind.LiteralFloat, ValueNode (float32ConstExpr.Value));
                return ExpressionData.NewValue (expr, passData.GetFloat32Type (Constness.Mutable), value, null, false, true);
            }

            case ES_AstFloat64ConstantExpression float64ConstExpr: {
                var value = LiteralExpression (ESIR_NodeKind.LiteralFloat, ValueNode (float64ConstExpr.Value));
                return ExpressionData.NewValue (expr, passData.GetFloat64Type (Constness.Mutable), value, null, false, true);
            }

            case ES_AstNullLiteralExpression nullLitExpr:
                return ExpressionData.NewValue (expr, new (Constness.Mutable, passData.Env.TypeNull), null!, null, false, true);

            case ES_AstNameExpression nameExpr:
                return CheckExpression_Name (ref passData, nameExpr, expectedType);

            case ES_AstMemberAccessExpression memberAccessExpr:
                return CheckExpression_MemberAccess (ref passData, memberAccessExpr, expectedType);

            #endregion

            case ES_AstIncDecExpression incDecExpr:
                return CheckExpression_IncDec (ref passData, incDecExpr, expectedType);

            #region Unary expressions

            case ES_AstSimpleUnaryExpression unaryExpr:
                return CheckExpression_Unary (ref passData, unaryExpr, expectedType);

            case ES_AstCastExpression castExpr:
                return CheckExpression_Cast (ref passData, castExpr, expectedType);

            #endregion

            case ES_AstSimpleBinaryExpression binaryExpr:
                return CheckExpression_SimpleBinaryExpression (ref passData, binaryExpr, expectedType);

            case ES_AstConditionalExpression condExpr: {
                var condData = CheckExpression (ref passData, condExpr.Condition, typeBoolConst);
                EnsureCompat (ref condData, typeBoolConst, ref passData, condData.Expr.NodeBounds);

                var leftExpr = CheckExpression (ref passData, condExpr.Then, expectedType);
                var rightExpr = CheckExpression (ref passData, condExpr.Else, expectedType);

                var isCompat = (
                    EnsureCompat (ref leftExpr, expectedType, ref passData, leftExpr.Expr.NodeBounds) &
                    EnsureCompat (ref rightExpr, expectedType, ref passData, rightExpr.Expr.NodeBounds)
                );

                var finalType = isCompat ? expectedType : passData.GetUnknownType (Constness.Mutable);

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
        ref PassData passData, ES_AstNewObjectExpression expr,
        TypeData expectedType
    ) {
        var exprList = new ExpressionList (null, null);

        var irWriter = passData.IRWriter;
        var typeUnkn = passData.Env.TypeUnknownValue;
        var typeUnknMut = passData.GetUnknownType (Constness.Mutable);

        var objType = GetTypeRef (expr.TypeDeclaration);
        var retType = UnpackFirstConst (passData.EnvBuilder.CreateReferenceType (objType));

        // Ensure the non-const version of the type exists.
        passData.EnvBuilder.RemoveConstness (retType.Type);

        var irObjType = TypeNode (ref passData, objType);
        var irRefType = TypeNode (ref passData, retType);

        var errorFound = false;

        if (objType->Flags.HasFlag (ES_TypeFlag.NoNew)) {
            passData.ErrorList.Add (ES_FrontendErrors.GenNoTypeNew (
                passData.Env.GetNiceTypeNameString (objType, true), passData.Source, expr.NodeBounds
            ));

            errorFound = true;
        }

        // Get the arg types.
        var argIdx = 0;
        var argValues = new StructPooledList<ESIR_Expression> (CL_ClearMode.Auto);
        Span<int> argRegisters = stackalloc int [expr.Arguments.Length];
        Span<ES_FunctionPrototypeArgData> argsList = stackalloc ES_FunctionPrototypeArgData [expr.Arguments.Length];
        foreach (var arg in expr.Arguments) {
            var argValueExpr = CheckExpression (ref passData, arg.ValueExpression, typeUnknMut);
            exprList.Merge (ref argValueExpr.Expressions);
            exprList.AddRegister (argValueExpr.ValueRegister);

            var argType = arg.ArgType;
            var argValueType = argValueExpr.Type.ToType (ref passData);

            if (argValueExpr.Type.Type is null || argValueExpr.Type.Type->TypeTag == ES_TypeTag.UNKNOWN)
                errorFound = true;

            var argProtoData = new ES_FunctionPrototypeArgData (argType, argValueType);

            var regIdx = irWriter.RentRegister (TypeNode (ref passData, argValueType));
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
            return ExpressionError (ref passData, expr);
        }

        // Get the constructor.
        var constructor = FindConstructor (objType, argsList);

        // Error out if the constructor is null.
        if (constructor is null && expr.Arguments.Length > 0) {
            passData.ErrorList.Add (ES_FrontendErrors.GenNoSuchConstructor (
                passData.Env.GetNiceTypeNameString (objType, true),
                passData.Env.GetFunctionSignatureString (argsList),
                passData.Source, expr.NodeBounds
            ));
            retType = typeUnknMut;
        }

        // TODO: Handle constructors.
        if (constructor is not null)
            throw new NotImplementedException ("[TODO] Constructors not implemented yet.");

        var valueReg = irWriter.RentRegister (irRefType);
        var value = LocalValueExpression (valueReg);

        exprList.AddExpression (AssignmentExpression (
            value,
            NewObjectExpression (ESIR_Factory.TypeNode (objType))
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
        ref PassData passData, ES_AstNewArrayExpression expr,
        TypeData expectedType
    ) {
        var irWriter = passData.IRWriter;
        var exprList = new ExpressionList (null, null);

        var indexTypeConst = passData.GetArrayIndexType ();
        var elemType = GetTypeRef (expr.ElementType);
        var arrType = passData.EnvBuilder.CreateArrayType (elemType, expr.Ranks.Length);
        var retType = UnpackFirstConst (arrType);

        var noNew = elemType->Flags.HasFlag (ES_TypeFlag.NoNew);
        if (noNew) {
            passData.ErrorList.Add (ES_FrontendErrors.GenNoTypeNew (
                passData.Env.GetNiceTypeNameString (elemType, true), passData.Source, expr.NodeBounds
            ));
        }

        // Ensure the non-const version of the type exists.
        passData.EnvBuilder.RemoveConstness (arrType);

        using var rankValues = new StructPooledList<ESIR_Expression> (CL_ClearMode.Auto);
        foreach (var rank in expr.Ranks) {
            Debug.Assert (rank is not null);

            var rankExpr = CheckExpression (ref passData, rank, indexTypeConst);
            if (!EnsureCompat (ref rankExpr, indexTypeConst, ref passData, rankExpr.Expr.NodeBounds)) {
                rankExpr.Expressions.Dispose ();
                continue;
            }

            exprList.Merge (ref rankExpr.Expressions);
            exprList.AddRegister (rankExpr.ValueRegister);
            rankValues.Add (rankExpr.Value);
        }

        if (noNew) {
            exprList.Dispose ();
            return ExpressionError (ref passData, expr);
        }

        var valueReg = irWriter.RentRegister (TypeNode (ref passData, arrType));
        var value = LocalValueExpression (valueReg);
        exprList.AddExpression (AssignmentExpression (
            value,
            NewArrayExpression (ESIR_Factory.TypeNode (elemType), List (rankValues.Span))
        ));

        exprList.ReturnRegisters (irWriter);

        return ExpressionData.NewValue (expr, retType, exprList, value, valueReg, false, false);
    }

    private static ExpressionData CheckExpression_Indexing (
        ref PassData passData, ES_AstIndexingExpression expr,
        TypeData expectedType
    ) {
        var irWriter = passData.IRWriter;

        var typeUnkn = passData.Env.TypeUnknownValue;
        var typeIndex = passData.GetArrayIndexType ();
        var typeUnknMut = passData.GetUnknownType (Constness.Mutable);

        var rankCount = expr.RankExpressions.Length;
        var indexedExprData = CheckExpression (ref passData, expr.IndexedExpression, typeUnknMut);

        var exprList = indexedExprData.Expressions;

        using var ranksList = new StructPooledList<ESIR_Expression> (CL_ClearMode.Auto);
        foreach (var rank in expr.RankExpressions) {
            Debug.Assert (rank is not null);

            var rankExprData = CheckExpression (ref passData, rank, typeIndex);
            EnsureCompat (ref rankExprData, typeIndex, ref passData, rank.NodeBounds);

            rankExprData.Expressions.ReturnRegisters (irWriter);
            exprList.Merge (ref rankExprData.Expressions);

            ranksList.Add (rankExprData.Value);
            exprList.AddRegister (rankExprData.ValueRegister);
        }

        var badRankCount = false;
        var elemType = typeUnknMut;
        var writable = false;
        if (indexedExprData.Type.Type is not null) {
            var indexedType = indexedExprData.Type;
            var indexedTypeTag = indexedType.Type->TypeTag;

            if (indexedTypeTag == ES_TypeTag.UNKNOWN) {
                badRankCount = false;
                elemType = typeUnknMut;
                writable = true;
            } else if (indexedTypeTag == ES_TypeTag.Array) {
                var arrayData = (ES_ArrayTypeData*) indexedExprData.Type.Type;

                badRankCount = rankCount != arrayData->DimensionsCount;
                elemType = UnpackFirstConst (arrayData->ElementType);
                elemType = elemType.WithInheritedConst (indexedExprData.Type.Constness);

                writable = true;
            } else {
                passData.ErrorList.Add (ES_FrontendErrors.GenCantApplyIndexingToType (
                    passData.Env.GetNiceTypeNameString (indexedExprData.Type.ToType (ref passData), true),
                    passData.Source, indexedExprData.Expr.NodeBounds
                ));
            }
        } else {
            passData.ErrorList.Add (new (
                passData.Source, indexedExprData.Expr.NodeBounds, ES_FrontendErrors.CantApplyIndexing
            ));
        }

        writable &= elemType.IsWritable;

        if (badRankCount) {
            passData.ErrorList.Add (new (
                passData.Source, indexedExprData.Expr.NodeBounds, ES_FrontendErrors.IndexingBadRankCount
            ));
        }

        var value = IndexingExpression (indexedExprData.Value, List (ranksList.Span));

        return ExpressionData.NewValue (expr, elemType, exprList, value, null, writable, false);
    }

    private static ExpressionData CheckExpression_Name (
        ref PassData passData, ES_AstNameExpression expr,
        TypeData expectedType
    ) {
        var id = passData.Env.IdPool.GetIdentifier (expr.Value.Text.Span);
        var symbol = passData.Symbols.GetSymbol (id);

        var writable = symbol.Flags.HasFlag (SymbolFlags.Writable);
        var compTimeConst = symbol.Flags.HasFlag (SymbolFlags.CompileTimeConstant);

        switch (symbol.Tag) {
            case SymbolType.None: {
                var symbolName = expr.Value.Text.Span.GetPooledString ();
                passData.ErrorList.Add (ES_FrontendErrors.GenCantFindSymbol (symbolName, expr.Value));

                return ExpressionError (ref passData, expr);
            }

            case SymbolType.Variable: {
                var varData = symbol.MatchVar ();
                return ExpressionData.NewValue (expr, varData.Type, varData.IRExpression, null, writable, compTimeConst);
            }

            case SymbolType.Type: {
                if (expectedType.Type is not null) {
                    passData.ErrorList.Add (ES_FrontendErrors.GenInvalidExprTerm (
                        expr.Value.Text.Span.GetPooledString (),
                        expr.Value
                    ));

                    return ExpressionError (ref passData, expr);
                }

                return ExpressionData.NewType (expr, symbol.MatchType ());
            }

            case SymbolType.Function: {
                var func = symbol.MatchFunction ();
                var type = (ES_TypeInfo*) func->FunctionType;
                return ExpressionData.NewFunction (expr, func, type);
            }

            default:
                throw new NotImplementedException ("Symbol type not implemented.");
        }
    }

    private static ExpressionData CheckExpression_MemberAccess (
        ref PassData passData, ES_AstMemberAccessExpression expr,
        TypeData expectedType
    ) {
        Debug.Assert (expr.Member is not null);

        var idPool = passData.Env.IdPool;
        var typeUnkn = passData.Env.TypeUnknownValue;

        var parentExpr = CheckExpression (ref passData, expr.Parent, TypeData.Null);
        var memberId = idPool.GetIdentifier (expr.Member.Value.Text.Span);

        if (parentExpr.Type.Type is not null) {
            CheckExpression_Dereference (ref parentExpr);

            var type = parentExpr.Type;

            switch (type.Type->TypeTag) {
                case ES_TypeTag.UNKNOWN:
                    return ExpressionError (ref passData, expr);

                case ES_TypeTag.Struct: {
                    return CheckExpression_MemberAccess_Basic (
                        ref passData,
                        expr, parentExpr, memberId, expectedType
                    );
                }

                case ES_TypeTag.Array: {
                    var ret = CheckExpression_MemberAccess_Basic (
                        ref passData,
                        expr, parentExpr, memberId, expectedType
                    );

                    ret.Writable = false;

                    return ret;
                }

                default:
                    throw new NotImplementedException ("Type not implemented yet.");
            }
        } else if (parentExpr.TypeInfo is not null) {
            var type = parentExpr.TypeInfo;

            switch (type->TypeTag) {
                case ES_TypeTag.UNKNOWN:
                    return ExpressionError (ref passData, expr);

                case ES_TypeTag.Struct: {
                    return CheckExpression_MemberAccessStatic_Aggregate (
                        ref passData,
                        expr, parentExpr, memberId, expectedType
                    );
                }

                case ES_TypeTag.Array: {
                    passData.ErrorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
                        passData.Env.GetNiceTypeNameString (type, true),
                        expr.Member.Value.Text.Span.GetPooledString (),
                        expr.Member.Value
                    ));

                    return ExpressionError (ref passData, expr);
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
        ref PassData passData, ES_AstMemberAccessExpression expr,
        ExpressionData parentExpr, ES_Identifier memberId,
        TypeData expectedType
    ) {
        Debug.Assert (expr.Member is not null);

        var typeUnkn = passData.Env.TypeUnknownValue;
        var membersArr = parentExpr.Type.Type->MembersList.MembersList;

        var exprList = parentExpr.Expressions;
        exprList.AddRegister (parentExpr.ValueRegister);

        foreach (var memberAddr in membersArr.Span) {
            var memberPtr = memberAddr.Address;

            if (!memberPtr->Name.Equals (memberId))
                continue;

            switch (memberPtr->MemberType) {
                case ES_MemberType.Field: {
                    var memberVar = (ES_MemberData_Variable*) memberPtr;
                    var varType = UnpackFirstConst (memberVar->Type).WithInheritedConst (parentExpr.Type.Constness);

                    if (memberVar->Info.Flags.HasFlag (ES_MemberFlags.Static)) {
                        passData.ErrorList.Add (ES_FrontendErrors.GenStaticAccessOnInst (
                            passData.Env.GetNiceTypeNameString (parentExpr.Type.ToType (ref passData), true),
                            expr.Member.Value.Text.Span.GetPooledString (),
                            expr.Member.Value
                        ));
                        exprList.Dispose ();
                    }

                    var writable = parentExpr.Writable & varType.IsWritable;
                    var value = MemberAccessExpression (parentExpr.Value, memberPtr->Name);

                    return ExpressionData.NewValue (expr, varType, exprList, value, null, writable, false);
                }

                case ES_MemberType.Function:
                    throw new NotImplementedException ("[TODO] Member function access not implemented yet.");

                default:
                    throw new NotImplementedException ("Member type not implemented yet.");
            }
        }

        passData.ErrorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
            passData.Env.GetNiceTypeNameString (parentExpr.Type.ToType (ref passData), true),
            expr.Member.Value.Text.Span.GetPooledString (),
            expr.Member.Value
        ));

        exprList.Dispose ();
        return ExpressionError (ref passData, expr);
    }

    private static ExpressionData CheckExpression_MemberAccessStatic_Aggregate (
        ref PassData passData, ES_AstMemberAccessExpression expr,
        ExpressionData parentExpr, ES_Identifier memberId,
        TypeData expectedType
    ) {
        Debug.Assert (expr.Member is not null);

        var type = parentExpr.TypeInfo;
        var typeUnkn = passData.Env.TypeUnknownValue;
        var membersArr = type->MembersList.MembersList;

        var exprList = parentExpr.Expressions;
        exprList.AddRegister (parentExpr.ValueRegister);

        foreach (var memberAddr in membersArr.Span) {
            var memberPtr = memberAddr.Address;

            if (!memberPtr->Name.Equals (memberId))
                continue;

            switch (memberPtr->MemberType) {
                case ES_MemberType.Field: {
                    var memberVar = (ES_MemberData_Variable*) memberPtr;

                    if (!memberVar->Info.Flags.HasFlag (ES_MemberFlags.Static)) {
                        passData.ErrorList.Add (ES_FrontendErrors.GenInstAccessOnStatic (
                            expr.Member.Value.Text.Span.GetPooledString (),
                            expr.Member.Value
                        ));
                        exprList.Dispose ();
                        return ExpressionError (ref passData, expr);
                    }

                    var memberType = UnpackFirstConst (memberVar->Type);
                    var writable = memberType.IsWritable;

                    var mangledName = MangleStaticVar (ref passData, type->Name, memberPtr->Name);
                    var value = StaticVariableExpression (mangledName);
                    // TODO: Handle constants.
                    return ExpressionData.NewValue (expr, memberType, exprList, value, null, writable, false);
                }

                case ES_MemberType.Function:
                    throw new NotImplementedException ("[TODO] Static member function access not implemented yet.");

                default:
                    throw new NotImplementedException ("Member type not implemented yet.");
            }
        }

        passData.ErrorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
            passData.Env.GetNiceTypeNameString (type, true),
            expr.Member.Value.Text.Span.GetPooledString (),
            expr.Member.Value
        ));

        exprList.Dispose ();
        return ExpressionError (ref passData, expr);
    }

    private static ExpressionData CheckExpression_IncDec (
        ref PassData passData, ES_AstIncDecExpression expr,
        TypeData expectedType
    ) {
        var exprData = CheckExpression (ref passData, expr.Inner, expectedType);

        if (exprData.Type.Type->TypeTag == ES_TypeTag.UNKNOWN) {
            exprData.Expressions.Dispose ();
            return ExpressionError (ref passData, expr);
        }

        if (!exprData.Writable || !exprData.Type.IsWritable) {
            passData.ErrorList.Add (new (
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

        if (exprData.Type.Type->TypeTag == ES_TypeTag.Int || exprData.Type.Type->TypeTag == ES_TypeTag.Float)
            return ExpressionData.NewValue (expr, exprData.Type, exprList, value, exprData.ValueRegister, false, false);
        else
            throw new NotImplementedException ("[TODO] ?");
    }

    private static ExpressionData CheckExpression_Unary (
        ref PassData passData, ES_AstSimpleUnaryExpression expr,
        TypeData expectedType
    ) {
        var irWriter = passData.IRWriter;
        var symbols = passData.Symbols;

        var exprData = CheckExpression (ref passData, expr.Inner, expectedType);

        if (!CompilerFrontend.UnaryOpCompat (passData.Env, exprData.Type.Type, expr.ExpressionType, out var finalType, out _)) {
            passData.ErrorList.Add (ES_FrontendErrors.GenCantApplyUnaryOp (
                expr.OperatorToken.Text.Span.GetPooledString (),
                passData.Env.GetNiceTypeNameString (exprData.Type.ToType (ref passData), true),
                passData.Source, exprData.Expr.NodeBounds
            ));

            exprData.Expressions.Dispose ();
            return ExpressionError (ref passData, expr);
        }

        var retType = UnpackFirstConst (finalType).WithInheritedConst (exprData.Type.Constness);
        var writable = retType.IsWritable;

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
        ref PassData passData, ES_AstCastExpression expr,
        TypeData expectedType
    ) {
        var destType = UnpackFirstConst (GetTypeRef (expr.DestinationType));
        var exprData = CheckExpression (ref passData, expr.InnerExpression, passData.GetUnknownType (Constness.Mutable));

        if (!ExplicitCast (ref passData, ref exprData, destType, out var castRedundant)) {
            passData.ErrorList.Add (ES_FrontendErrors.GenNoExplicitCast (
                passData.Env.GetNiceTypeNameString (destType.ToType (ref passData), true),
                passData.Env.GetNiceTypeNameString (exprData.Type.ToType (ref passData), true),
                passData.Source, expr.NodeBounds
            ));

            exprData.Expressions.Dispose ();
            return ExpressionError (ref passData, expr);
        }

        var exprlist = exprData.Expressions;

        if (castRedundant) {
            passData.InfoList.Add (new (
                passData.Source, expr.CastBounds, ES_FrontendInfoMsg.RedundantCast
            ));
        }

        return ExpressionData.NewValue (expr, exprData.Type, exprlist, exprData.Value, exprData.ValueRegister, false, false);
    }

    private static ExpressionData CheckExpression_FunctionCall (
        ref PassData passData, ES_AstFunctionCallExpression expr,
        TypeData expectedType
    ) {
        var funcExpr = CheckExpression (ref passData, expr.FunctionExpression, passData.GetUnknownType (Constness.Mutable));

        if (funcExpr.Function is not null)
            return CheckExpression_SimpleFunctionCall (ref passData, expr, funcExpr);
        else if (funcExpr.TypeInfo is not null) {
            passData.ErrorList.Add (ES_FrontendErrors.GenCantInvokeType (
                passData.Env.GetNiceTypeNameString (funcExpr.TypeInfo, true), passData.Source, funcExpr.Expr.NodeBounds
            ));
        } else if (funcExpr.Type.Type is not null) {
            // TODO: Some types might be allowed to have `()` overrides too in the future. But not now.
            if (funcExpr.Type.Type->TypeTag != ES_TypeTag.UNKNOWN) {
                passData.ErrorList.Add (new (
                    passData.Source, funcExpr.Expr.NodeBounds, ES_FrontendErrors.CantInvokeExpr
                ));
            }
        } else
            Debug.Fail ("???");

        return ExpressionError (ref passData, expr);
    }

    private static ExpressionData CheckExpression_SimpleFunctionCall (
        ref PassData passData, ES_AstFunctionCallExpression expr,
        ExpressionData funcExpr
    ) {
        var typeUnknMut = passData.GetUnknownType (Constness.Mutable);

        var func = funcExpr.Function;
        var funcType = func->FunctionType;
        var mangledName = MangleFunctionName (ref passData, func);

        var funcArgCount = funcType->ArgumentsList.Length;
        var callArgCount = expr.Arguments.Length;
        var reqArgCount = funcArgCount - func->OptionalArgsCount;
        var ignoreDefArgs = false;

        if (callArgCount < reqArgCount) {
            var errBounds = expr.FunctionExpression.NodeBounds;

            var arg = func->Arguments.Span [callArgCount];
            passData.ErrorList.Add (ES_FrontendErrors.GenMissingFuncArg (
                arg.Name.GetCharsSpan ().GetPooledString (),
                func->Name.TypeName.GetCharsSpan ().GetPooledString (),
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
                    passData.ErrorList.Add (ES_FrontendErrors.GenTooManyFuncArgs (
                        func->Name.TypeName.GetCharsSpan ().GetPooledString (),
                        passData.Source, arg.ValueExpression.NodeBounds
                    ));
                    ignoreDefArgs = true;
                }

                var exprData = CheckExpression (ref passData, arg.ValueExpression, typeUnknMut);
                exprData.Expressions.Dispose ();
                passData.IRWriter.ReturnRegister (exprData.ValueRegister);
                continue;
            }

            var argData = func->Arguments.Elements + argIdx;
            var argTypeData = funcType->ArgumentsList.Elements + argIdx;

            if (arg.ArgType != argTypeData->ArgType && argTypeData->ArgType != ES_ArgumentType.In) {
                passData.ErrorList.Add (ES_FrontendErrors.GenWrongArgType (
                    argData->Name.GetCharsSpan ().GetPooledString (),
                    arg.ArgType.ToString (), passData.Source, arg.ValueExpression.NodeBounds
                ));
            }

            var argValType = UnpackFirstConst (argTypeData->ValueType);
            var argExprData = CheckExpression (ref passData, arg.ValueExpression, argValType);
            EnsureCompat (ref argExprData, argValType, ref passData, argExprData.Expr.NodeBounds);

            exprList.Merge (ref argExprData.Expressions);
            exprList.AddRegister (argExprData.ValueRegister);

            argsList.Add (ArgumentValue (arg.ArgType, argExprData.Value));
        }

        if (!ignoreDefArgs) {
            if (!passData.EnvBuilder.PointerAstMap.TryGetValue ((IntPtr) func, out var funcASTNode))
                argIdx = funcArgCount;

            var funcDef = (ES_AstFunctionDefinition?) funcASTNode;
            for (; argIdx < funcArgCount; argIdx++) {
                var argData = func->Arguments.Elements + argIdx;
                var argTypeData = funcType->ArgumentsList.Elements + argIdx;
                var argDef = funcDef!.ArgumentsList [argIdx];

                var argValType = UnpackFirstConst (argTypeData->ValueType);
                var argExprData = CheckExpression (ref passData, argDef.DefaultExpression!, argValType);
                EnsureCompat (ref argExprData, argValType, ref passData, argExprData.Expr.NodeBounds);

                exprList.Merge (ref argExprData.Expressions);
                exprList.AddRegister (argExprData.ValueRegister);

                argsList.Add (ArgumentValue (ES_ArgumentType.Normal, argExprData.Value));
            }
        }

        var retType = UnpackFirstConst (funcType->ReturnType);
        var value = FunctionCallExpression (mangledName, List (argsList.Span));

        return ExpressionData.NewValue (expr, retType, exprList, value, null, false, false);
    }

    private static bool CheckExpression_SimpleBinaryExpression_Compat (
        ref PassData passData, ES_AstSimpleBinaryExpression expr,
        ref ExpressionData lhs, ref ExpressionData rhs,
        out ES_TypeInfo* finalType
    ) {
        var op = expr.ExpressionType;

        var lhsNull = lhs.Type.Type->TypeTag == ES_TypeTag.Null;
        var rhsNull = rhs.Type.Type->TypeTag == ES_TypeTag.Null;
        var lhsRef = lhs.Type.Type->IsReferenceType ();
        var rhsRef = rhs.Type.Type->IsReferenceType ();
        var lhsInt = lhs.Type.Type->TypeTag == ES_TypeTag.Int;
        var rhsInt = rhs.Type.Type->TypeTag == ES_TypeTag.Int;

        Debug.Assert (!(lhsNull & rhsNull));
        if (!CompilerFrontend.BinaryOpCompat (passData.Env, lhs.Type.Type, rhs.Type.Type, op, out finalType, out _))
            return false;

        if (lhsNull && rhsRef) {
            lhs.Type = rhs.Type;
            lhs.Value = NullLiteralExpression (TypeNode (ref passData, rhs.Type));
        } else if (rhsNull && lhsRef) {
            rhs.Type = lhs.Type;
            rhs.Value = NullLiteralExpression (TypeNode (ref passData, lhs.Type));
        } else if (lhsInt && rhsInt) {
            var lhsTypeInt = (ES_IntTypeData*) lhs.Type.Type;
            var rhsTypeInt = (ES_IntTypeData*) rhs.Type.Type;

            if (op.IsBitShift ()) {
                if (rhsTypeInt->IntSize < ES_IntSize.Int32) {
                    rhs.Type = rhs.Type.WithType (passData.Env.GetIntType (ES_IntSize.Int32, false));
                    rhs.Value = CastExpression (rhs.Value, TypeNode (ref passData, rhs.Type));
                }

                return true;
            }

            if (lhsTypeInt->IntSize == rhsTypeInt->IntSize)
                return true;

            if (lhsTypeInt->IntSize > rhsTypeInt->IntSize) {
                rhs.Type = lhs.Type;
                rhs.Value = CastExpression (rhs.Value, TypeNode (ref passData, lhs.Type));
            } else {
                lhs.Type = rhs.Type;
                lhs.Value = CastExpression (lhs.Value, TypeNode (ref passData, rhs.Type));
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
        ref PassData passData, ES_AstSimpleBinaryExpression expr,
        TypeData expectedType
    ) {
        var typeUnkn = passData.Env.TypeUnknownValue;
        var typeUnknMut = passData.GetUnknownType (Constness.Mutable);

        var leftExpr = CheckExpression (ref passData, expr.Left, typeUnknMut);

        var expectedRightType = typeUnknMut;
        if (expr.ExpressionType.IsBitShift () && leftExpr.Type.Type->TypeTag == ES_TypeTag.Int)
            expectedRightType = new (Constness.Const, passData.Env.GetIntType (((ES_IntTypeData*) expectedType.Type)->IntSize, true));
        else if (expr.ExpressionType.IsAssignment ())
            expectedRightType = leftExpr.Type.Type is not null ? leftExpr.Type : typeUnknMut;

        var rightExpr = CheckExpression (ref passData, expr.Right, expectedRightType);

        if (leftExpr.Type.Type is null || rightExpr.Type.Type is null) {
            leftExpr.Expressions.Dispose ();
            rightExpr.Expressions.Dispose ();
            return ExpressionError (ref passData, expr);
        }

        if (!CheckExpression_SimpleBinaryExpression_Compat (ref passData, expr, ref leftExpr, ref rightExpr, out var finalType)) {
            passData.ErrorList.Add (ES_FrontendErrors.GenCantApplyBinaryOp (
                expr.OperatorToken.Text.Span.GetPooledString (),
                passData.Env.GetNiceTypeNameString (leftExpr.Type.ToType (ref passData), true),
                passData.Env.GetNiceTypeNameString (rightExpr.Type.ToType (ref passData), true),
                passData.Source, expr.NodeBounds
            ));

            leftExpr.Expressions.Dispose ();
            rightExpr.Expressions.Dispose ();
            return ExpressionError (ref passData, expr);
        }

        var retType = UnpackFirstConst (finalType);

        var isAssignment = expr.ExpressionType.IsAssignment ();
        if (isAssignment && !leftExpr.Writable) {
            passData.ErrorList.Add (new (
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
            return ExpressionData.NewValue (expr, retType, exprList, assignValue, null, false, false);
        }

        var opKind = CheckExpression_SimpleBinaryExpression_Expr (expr.ExpressionType);

        if (opKind is null) {
            exprList.Dispose ();

            passData.ErrorList.Add (new (expr.OperatorToken, "Binary expression not handled."));
            return ExpressionError (ref passData, expr);
        }

        ESIR_Expression value = SimpleBinaryExpression (opKind.Value, leftExpr.Value, rightExpr.Value);

        if (isAssignment)
            value = AssignmentExpression (leftExpr.Value, value);

        return ExpressionData.NewValue (expr, retType, exprList, value, null, false, false);
    }
}
