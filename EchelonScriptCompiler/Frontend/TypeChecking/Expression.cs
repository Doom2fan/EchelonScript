/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Buffers;
using System.Diagnostics;
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Utilities;

using static EchelonScriptCompiler.CompilerCommon.IR.ESIR_Factory;

namespace EchelonScriptCompiler.Frontend {
    internal unsafe static partial class Compiler_TypeChecking {
        private struct ExpressionData {
            public ES_AstExpression Expr;
            public ES_TypeInfo* Type;
            public ES_TypeInfo* TypeInfo;
            public ES_FunctionData* Function;

            public ExpressionList Expressions;
            public ESIR_Expression Value;
            public int? ValueRegister;

            public bool Constant;
            public bool Writable;

            public static ExpressionData NewValue (
                ES_AstExpression expr,
                ES_TypeInfo* type, ExpressionList exprs, ESIR_Expression value, int? reg,
                bool isConst, bool writable
            ) {
                return new ExpressionData {
                    Expr = expr,
                    Type = type,

                    TypeInfo = null,
                    Function = null,

                    Expressions = exprs,
                    Value = value,
                    ValueRegister = reg,

                    Constant = isConst,
                    Writable = writable,
                };
            }

            public static ExpressionData NewValue (
                ES_AstExpression expr,
                ES_TypeInfo* type, ESIR_Expression value, int? reg,
                bool isConst, bool writable
            ) {
                return new ExpressionData {
                    Expr = expr,
                    Type = type,

                    TypeInfo = null,
                    Function = null,

                    Expressions = new ExpressionList (null, null),
                    Value = value,
                    ValueRegister = reg,

                    Constant = isConst,
                    Writable = writable,
                };
            }

            public static ExpressionData NewType (ES_AstExpression expr, ES_TypeInfo* typeInfo, bool isConst) {
                var ret = new ExpressionData {
                    Expr = expr,
                    TypeInfo = typeInfo,

                    Type = null,
                    Function = null,

                    Value = null!,
                    ValueRegister = null,

                    Constant = isConst,
                    Writable = false,
                };

                ret.Expressions.Initialize ();

                return ret;
            }

            public static ExpressionData NewFunction (
                ES_AstExpression expr,
                ES_FunctionData* func, ES_TypeInfo* funcType,
                bool isConst
            ) {
                var ret = new ExpressionData {
                    Expr = expr,
                    Function = func,
                    TypeInfo = funcType,

                    Type = null,

                    Value = null!,
                    ValueRegister = null,

                    Constant = isConst,
                    Writable = false,
                };

                ret.Expressions.Initialize ();

                return ret;
            }

            public static ExpressionData NewFunction (
                ES_AstExpression expr,
                ES_FunctionData* func, ES_TypeInfo* funcType, ExpressionList values, ESIR_Expression value, int? reg,
                bool isConst
            ) {
                return new ExpressionData {
                    Expr = expr,
                    Function = func,
                    TypeInfo = funcType,

                    Type = null,

                    Expressions = values,
                    Value = value,
                    ValueRegister = reg,

                    Constant = isConst,
                    Writable = false,
                };
            }

            public static ExpressionData NewFunction (
                ES_AstExpression expr,
                ES_FunctionData* func, ES_TypeInfo* funcType, ESIR_Expression value, int? reg,
                bool isConst
            ) => NewFunction (expr, func, funcType, new ExpressionList (null, null), value, reg, isConst);
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
            ref PassData passData, ES_AstExpression expr, bool constant = false, bool writable = false
        ) => ExpressionData.NewValue (expr, passData.Env.TypeUnknownValue, ErrorExpression (), null, constant, writable);

        private static bool MustBeCompat (
            ref PassData passData, ref ExpressionData exprData, ES_TypeInfo* destType
        ) {
            // We don't need to do any checks here if they're *literally* the same type.
            if (destType == exprData.Type)
                return true;

            if (destType->TypeTag == ES_TypeTag.UNKNOWN || exprData.Type->TypeTag == ES_TypeTag.UNKNOWN)
                return true;
            else if (exprData.Type->TypeTag == ES_TypeTag.Null) {
                if (!IsNullable (ref passData, destType, out _))
                    return false;

                exprData.Value = NullLiteralExpression (TypeNode (destType));
                exprData.Constant = true;
                exprData.Writable = false;

                return true;
            } else if (destType->TypeTag == ES_TypeTag.Int && exprData.Type->TypeTag == ES_TypeTag.Int) {
                var destIntType = (ES_IntTypeData*) destType;
                var givenIntType = (ES_IntTypeData*) exprData.Type;

                if (givenIntType->Unsigned != destIntType->Unsigned || givenIntType->IntSize > destIntType->IntSize)
                    return false;

                exprData.Value = CastExpression (exprData.Value, TypeNode (destType));
                exprData.Constant = true;
                exprData.Writable = false;

                return true;
            }

            return false;
        }

        private static bool ExplicitCast (
            ref PassData passData, ref ExpressionData exprData, ES_TypeInfo* castType,
            out bool castRedundant, bool noModify = false
        ) {
            if (MustBeCompat (ref passData, ref exprData, castType)) {
                castRedundant = true;
                return true;
            }

            if (castType->TypeTag == ES_TypeTag.Int && exprData.Type->TypeTag == ES_TypeTag.Int) {
                var castIntType = (ES_IntTypeData*) castType;
                var exprIntType = (ES_IntTypeData*) exprData.Type;

                castRedundant = (
                    castIntType->IntSize == exprIntType->IntSize &&
                    castIntType->Unsigned == exprIntType->Unsigned
                );
            } else if (castType->TypeTag == ES_TypeTag.Float && exprData.Type->TypeTag == ES_TypeTag.Int) {
                castRedundant = false;
            } else if (castType->TypeTag == ES_TypeTag.Int && exprData.Type->TypeTag == ES_TypeTag.Float) {
                castRedundant = false;
            } else if (castType->TypeTag == ES_TypeTag.Float && exprData.Type->TypeTag == ES_TypeTag.Float) {
                var castFloatType = (ES_FloatTypeData*) castType;
                var exprFloatType = (ES_FloatTypeData*) exprData.Type;

                castRedundant = castFloatType->FloatSize == exprFloatType->FloatSize;
            } else {
                if (!noModify)
                    exprData.Constant = false;
                castRedundant = false;
                return false;
            }

            if (!noModify) {
                exprData.Type = castType;
                exprData.Value = CastExpression (exprData.Value, TypeNode (castType));
                exprData.Writable = false;
            }

            return true;
        }

        private static bool EnsureCompat (
            ref ExpressionData exprData, ES_TypeInfo* destType,
            ref PassData passData, ES_AstNodeBounds bounds
        ) {
            if (exprData.Type->TypeTag == ES_TypeTag.Null) {
                if (!IsNullable (ref passData, destType, out var retType)) {
                    passData.ErrorList.Add (ES_FrontendErrors.GenTypeNotNullable (
                        destType->Name.GetNameAsTypeString (), passData.Source, bounds
                    ));

                    exprData.Type = passData.Env.TypeUnknownValue;
                    exprData.Value = ErrorExpression ();
                    exprData.Constant = false;
                    return false;
                }

                exprData.Type = retType;
                exprData.Value = NullLiteralExpression (TypeNode (retType));
                exprData.Constant = false;
                return true;
            } else if (!MustBeCompat (ref passData, ref exprData, destType)) {
                if (ExplicitCast (ref passData, ref exprData, destType, out _, true)) {
                    passData.ErrorList.Add (ES_FrontendErrors.GenNoImplicitCast (
                        destType->Name.GetNameAsTypeString (), exprData.Type->Name.GetNameAsTypeString (),
                        passData.Source, bounds
                    ));
                } else {
                    passData.ErrorList.Add (ES_FrontendErrors.GenNoCast (
                        destType->Name.GetNameAsTypeString (), exprData.Type->Name.GetNameAsTypeString (),
                        passData.Source, bounds
                    ));
                }

                exprData.Constant = false;
                return false;
            }

            return true;
        }

        private static void CheckExpression_Dereference (ref ExpressionData exprData) {
            if (exprData.Type->TypeTag != ES_TypeTag.Reference)
                return;

            var refTypeData = (ES_ReferenceData*) exprData.Type;

            exprData.Type = refTypeData->PointedType;
            exprData.Value = UnaryExpression (ESIR_NodeKind.UnaryDereference, exprData.Value);
        }

        private static ExpressionData CheckExpression (
            ref PassData passData, ES_AstExpression expr,
            ES_TypeInfo* expectedType
        ) {
            Debug.Assert (expr is not null);

            var idPool = passData.Env.IdPool;
            var irWriter = passData.IRWriter;
            var symbols = passData.Symbols;

            var typeUnkn = passData.Env.TypeUnknownValue;
            var typeBool = passData.Env.TypeBool;

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
                    var intType = (ES_IntTypeData*) intConstExpr.IntType;

                    var litVal = intType->Unsigned
                        ? ValueNode (intConstExpr.SignExtend ())
                        : ValueNode ((long) intConstExpr.SignExtend ());
                    var litExpr = LiteralExpression (ESIR_NodeKind.LiteralInt, litVal);
                    var value = CastExpression (litExpr, TypeNode (intConstExpr.IntType));

                    return ExpressionData.NewValue (expr, intConstExpr.IntType, value, null, true, false);
                }

                case ES_AstBooleanConstantExpression boolConstExpr: {
                    var value = boolConstExpr.Value ? LiteralTrueExpression () : LiteralFalseExpression ();
                    return ExpressionData.NewValue (expr, typeBool, value, null, true, false);
                }

                case ES_AstFloat32ConstantExpression float32ConstExpr: {
                    var value = LiteralExpression (ESIR_NodeKind.LiteralFloat, ValueNode (float32ConstExpr.Value));
                    return ExpressionData.NewValue (expr, passData.Env.TypeFloat32, value, null, true, false);
                }

                case ES_AstFloat64ConstantExpression float64ConstExpr: {
                    var value = LiteralExpression (ESIR_NodeKind.LiteralFloat, ValueNode (float64ConstExpr.Value));
                    return ExpressionData.NewValue (expr, passData.Env.TypeFloat64, value, null, true, false);
                }

                case ES_AstNullLiteralExpression nullLitExpr:
                    return ExpressionData.NewValue (expr, passData.Env.TypeNull, null!, null, true, false);

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
                    var condData = CheckExpression (ref passData, condExpr.Condition, typeBool);
                    EnsureCompat (ref condData, typeBool, ref passData, condData.Expr.NodeBounds);

                    var leftExpr = CheckExpression (ref passData, condExpr.Then, expectedType);
                    var rightExpr = CheckExpression (ref passData, condExpr.Else, expectedType);

                    var isCompat = (
                        EnsureCompat (ref leftExpr, expectedType, ref passData, leftExpr.Expr.NodeBounds) &
                        EnsureCompat (ref rightExpr, expectedType, ref passData, rightExpr.Expr.NodeBounds)
                    );

                    var finalType = isCompat ? expectedType : typeUnkn;

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
            ES_TypeInfo* expectedType
        ) {
            var exprList = new ExpressionList (null, null);

            var irWriter = passData.IRWriter;
            var typeUnkn = passData.Env.TypeUnknownValue;

            var objType = GetTypeRef (expr.TypeDeclaration);
            var refType = passData.EnvBuilder.CreateReferenceType (objType);
            var retType = refType;

            var irObjType = TypeNode (objType);
            var irRefType = TypeNode (refType);

            var errorFound = false;

            if (objType->Flags.HasFlag (ES_TypeFlag.NoNew)) {
                passData.ErrorList.Add (ES_FrontendErrors.GenNoTypeNew (
                    objType->Name.GetNameAsTypeString (), passData.Source, expr.NodeBounds
                ));

                errorFound = true;
            }

            // Get the arg types.
            var argIdx = 0;
            var argValues = new StructPooledList<ESIR_Expression> (CL_ClearMode.Auto);
            Span<int> argRegisters = stackalloc int [expr.Arguments.Length];
            Span<ES_FunctionPrototypeArgData> argsList = stackalloc ES_FunctionPrototypeArgData [expr.Arguments.Length];
            foreach (var arg in expr.Arguments) {
                var argValueExpr = CheckExpression (ref passData, arg.ValueExpression, typeUnkn);
                exprList.Merge (ref argValueExpr.Expressions);
                exprList.AddRegister (argValueExpr.ValueRegister);

                var argType = arg.ArgType;
                var argValueType = argValueExpr.Type;

                if (argValueExpr.Type is null || argValueExpr.Type->TypeTag == ES_TypeTag.UNKNOWN)
                    errorFound = true;

                var argProtoData = new ES_FunctionPrototypeArgData (argType, argValueType);

                var regIdx = irWriter.RentRegister (TypeNode (argValueType));
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
                    objType->Name.GetNameAsTypeString (), passData.Env.GetFunctionSignatureString (argsList),
                    passData.Source, expr.NodeBounds
                ));
                retType = typeUnkn;
            }

            // TODO: Handle constructors.
            if (constructor is not null)
                throw new NotImplementedException ("[TODO] Constructors not implemented yet.");

            var valueReg = irWriter.RentRegister (irRefType);
            var value = LocalValueExpression (valueReg);

            exprList.AddExpression (AssignmentExpression (value, NewObjectExpression (irObjType)));
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
            ES_TypeInfo* expectedType
        ) {
            var irWriter = passData.IRWriter;
            var exprList = new ExpressionList (null, null);

            var indexType = passData.Env.GetArrayIndexType ();
            var elemType = GetTypeRef (expr.ElementType);
            var arrType = passData.EnvBuilder.CreateArrayType (elemType, expr.Ranks.Length);

            var noNew = elemType->Flags.HasFlag (ES_TypeFlag.NoNew);
            if (noNew) {
                passData.ErrorList.Add (ES_FrontendErrors.GenNoTypeNew (
                    elemType->Name.GetNameAsTypeString (), passData.Source, expr.NodeBounds
                ));
            }

            using var rankValues = new StructPooledList<ESIR_Expression> (CL_ClearMode.Auto);
            foreach (var rank in expr.Ranks) {
                Debug.Assert (rank is not null);

                var rankExpr = CheckExpression (ref passData, rank, indexType);
                if (!EnsureCompat (ref rankExpr, indexType, ref passData, rankExpr.Expr.NodeBounds)) {
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

            var valueReg = irWriter.RentRegister (TypeNode (arrType));
            var value = LocalValueExpression (valueReg);
            exprList.AddExpression (AssignmentExpression (
                value,
                NewArrayExpression (TypeNode (elemType), List (rankValues.Span))
            ));

            exprList.ReturnRegisters (irWriter);

            return ExpressionData.NewValue (expr, arrType, exprList, value, valueReg, false, false);
        }

        private static ExpressionData CheckExpression_Indexing (
            ref PassData passData, ES_AstIndexingExpression expr,
            ES_TypeInfo* expectedType
        ) {
            var irWriter = passData.IRWriter;

            var typeUnkn = passData.Env.TypeUnknownValue;
            var typeIndex = passData.Env.GetArrayIndexType ();

            var rankCount = expr.RankExpressions.Length;
            var indexedExprData = CheckExpression (ref passData, expr.IndexedExpression, typeUnkn);

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
            var elemType = typeUnkn;
            var constant = false;
            var writable = false;
            if (indexedExprData.Type is not null) {
                var indexedType = indexedExprData.Type;
                var indexedTypeTag = indexedType->TypeTag;

                if (indexedTypeTag == ES_TypeTag.UNKNOWN) {
                    badRankCount = false;
                    elemType = typeUnkn;
                    constant = false;
                    writable = true;
                } else if (indexedTypeTag == ES_TypeTag.Array) {
                    var arrayData = (ES_ArrayTypeData*) indexedExprData.Type;

                    badRankCount = rankCount != arrayData->DimensionsCount;
                    elemType = arrayData->ElementType;

                    constant = indexedExprData.Constant;
                    writable = indexedExprData.Writable;
                } else {
                    passData.ErrorList.Add (ES_FrontendErrors.GenCantApplyIndexingToType (
                        indexedExprData.Type->Name.GetNameAsTypeString (),
                        passData.Source, indexedExprData.Expr.NodeBounds
                    ));
                }
            } else {
                passData.ErrorList.Add (new (
                    passData.Source, indexedExprData.Expr.NodeBounds, ES_FrontendErrors.CantApplyIndexing
                ));
            }

            constant |= elemType->IsConstant ();
            writable &= elemType->IsWritable ();

            if (badRankCount) {
                passData.ErrorList.Add (new (
                    passData.Source, indexedExprData.Expr.NodeBounds, ES_FrontendErrors.IndexingBadRankCount
                ));
            }

            var value = IndexingExpression (indexedExprData.Value, List (ranksList.Span));

            return ExpressionData.NewValue (expr, elemType, exprList, value, null, constant, writable);
        }

        private static ExpressionData CheckExpression_Name (
            ref PassData passData, ES_AstNameExpression expr,
            ES_TypeInfo* expectedType
        ) {
            var id = passData.Env.IdPool.GetIdentifier (expr.Value.Text.Span);
            var symbol = passData.Symbols.GetSymbol (id);

            var writable = symbol.Flags.HasFlag (SymbolFlags.Writable);
            var constant = symbol.Flags.HasFlag (SymbolFlags.Constant);

            switch (symbol.Tag) {
                case SymbolType.None: {
                    var symbolName = expr.Value.Text.Span.GetPooledString ();
                    passData.ErrorList.Add (ES_FrontendErrors.GenCantFindSymbol (symbolName, expr.Value));

                    return ExpressionError (ref passData, expr);
                }

                case SymbolType.Variable: {
                    var varData = symbol.MatchVar ();
                    return ExpressionData.NewValue (expr, varData.Type, varData.IRExpression, null, constant, writable);
                }

                case SymbolType.Type: {
                    if (expectedType is not null) {
                        passData.ErrorList.Add (ES_FrontendErrors.GenInvalidExprTerm (
                            expr.Value.Text.Span.GetPooledString (),
                            expr.Value
                        ));

                        return ExpressionError (ref passData, expr);
                    }

                    return ExpressionData.NewType (expr, symbol.MatchType (), constant);
                }

                case SymbolType.Function: {
                    var func = symbol.MatchFunction ();
                    var type = (ES_TypeInfo*) func->FunctionType;
                    return ExpressionData.NewFunction (expr, func, type, constant);
                }

                default:
                    throw new NotImplementedException ("Symbol type not implemented.");
            }
        }

        private static ExpressionData CheckExpression_MemberAccess (
            ref PassData passData, ES_AstMemberAccessExpression expr,
            ES_TypeInfo* expectedType
        ) {
            Debug.Assert (expr.Member is not null);

            var idPool = passData.Env.IdPool;
            var typeUnkn = passData.Env.TypeUnknownValue;

            var parentExpr = CheckExpression (ref passData, expr.Parent, null);
            var memberId = idPool.GetIdentifier (expr.Member.Value.Text.Span);

            if (parentExpr.Type is not null) {
                CheckExpression_Dereference (ref parentExpr);

                var type = parentExpr.Type;

                switch (type->TypeTag) {
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

                        ret.Constant = true;
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
                            type->Name.GetNameAsTypeString (),
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
            ExpressionData parentExpr, ArrayPointer<byte> memberId,
            ES_TypeInfo* expectedType
        ) {
            Debug.Assert (expr.Member is not null);

            var typeUnkn = passData.Env.TypeUnknownValue;
            var membersArr = parentExpr.Type->MembersList.MembersList;

            var exprList = parentExpr.Expressions;
            exprList.AddRegister (parentExpr.ValueRegister);

            foreach (var memberAddr in membersArr.Span) {
                var memberPtr = memberAddr.Address;

                if (!memberPtr->Name.Equals (memberId))
                    continue;

                switch (memberPtr->MemberType) {
                    case ES_MemberType.Field: {
                        var memberVar = (ES_MemberData_Variable*) memberPtr;
                        var varType = memberVar->Type;

                        if (memberVar->Info.Flags.HasFlag (ES_MemberFlags.Static)) {
                            passData.ErrorList.Add (ES_FrontendErrors.GenStaticAccessOnInst (
                                parentExpr.Type->Name.GetNameAsTypeString (),
                                expr.Member.Value.Text.Span.GetPooledString (),
                                expr.Member.Value
                            ));
                            exprList.Dispose ();
                        }

                        var writable = parentExpr.Writable & varType->IsWritable ();
                        var constant = parentExpr.Constant | varType->IsConstant ();

                        var value = MemberAccessExpression (parentExpr.Value, memberPtr->Name);

                        return ExpressionData.NewValue (expr, varType, exprList, value, null, constant, writable);
                    }

                    case ES_MemberType.Function:
                        throw new NotImplementedException ("[TODO] Member function access not implemented yet.");

                    default:
                        throw new NotImplementedException ("Member type not implemented yet.");
                }
            }

            passData.ErrorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
                parentExpr.Type->Name.GetNameAsTypeString (),
                expr.Member.Value.Text.Span.GetPooledString (),
                expr.Member.Value
            ));

            exprList.Dispose ();
            return ExpressionError (ref passData, expr);
        }

        private static ExpressionData CheckExpression_MemberAccessStatic_Aggregate (
            ref PassData passData, ES_AstMemberAccessExpression expr,
            ExpressionData parentExpr, ArrayPointer<byte> memberId,
            ES_TypeInfo* expectedType
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

                        var constant = memberVar->Type->IsConstant ();
                        var writable = memberVar->Type->IsWritable ();

                        var mangledName = MangleStaticVar (ref passData, type->Name, memberPtr->Name);
                        var value = StaticVariableExpression (mangledName);
                        return ExpressionData.NewValue (expr, memberVar->Type, exprList, value, null, constant, writable);
                    }

                    case ES_MemberType.Function:
                        throw new NotImplementedException ("[TODO] Static member function access not implemented yet.");

                    default:
                        throw new NotImplementedException ("Member type not implemented yet.");
                }
            }

            passData.ErrorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
                type->Name.GetNameAsTypeString (),
                expr.Member.Value.Text.Span.GetPooledString (),
                expr.Member.Value
            ));

            exprList.Dispose ();
            return ExpressionError (ref passData, expr);
        }

        private static ExpressionData CheckExpression_IncDec (
            ref PassData passData, ES_AstIncDecExpression expr,
            ES_TypeInfo* expectedType
        ) {
            var exprData = CheckExpression (ref passData, expr.Inner, expectedType);

            if (exprData.Type->TypeTag == ES_TypeTag.UNKNOWN) {
                exprData.Expressions.Dispose ();
                return ExpressionError (ref passData, expr);
            }

            if (!exprData.Writable || exprData.Constant) {
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

            if (exprData.Type->TypeTag == ES_TypeTag.Int || exprData.Type->TypeTag == ES_TypeTag.Float)
                return ExpressionData.NewValue (expr, exprData.Type, exprList, value, exprData.ValueRegister, false, false);
            else
                throw new NotImplementedException ("[TODO] ?");
        }

        private static ExpressionData CheckExpression_Unary (
            ref PassData passData, ES_AstSimpleUnaryExpression expr,
            ES_TypeInfo* expectedType
        ) {
            var irWriter = passData.IRWriter;
            var symbols = passData.Symbols;

            var typeUnkn = passData.Env.TypeUnknownValue;

            var exprData = CheckExpression (ref passData, expr.Inner, expectedType);

            if (!passData.EnvBuilder.UnaryOpCompat (exprData.Type, expr.ExpressionType, out var finalType, out _)) {
                passData.ErrorList.Add (ES_FrontendErrors.GenCantApplyUnaryOp (
                    expr.OperatorToken.Text.Span.GetPooledString (), exprData.Type->Name.GetNameAsTypeString (),
                    passData.Source, exprData.Expr.NodeBounds
                ));

                exprData.Expressions.Dispose ();
                return ExpressionError (ref passData, expr);
            }

            var constant = finalType->IsConstant ();
            var writable = false;

            ESIR_Expression value;
            switch (expr.ExpressionType) {
                case SimpleUnaryExprType.Positive:
                    value = exprData.Value;
                    break;

                case SimpleUnaryExprType.Negative:
                    value = UnaryExpression (ESIR_NodeKind.UnaryNegative, exprData.Value);
                    break;

                case SimpleUnaryExprType.LogicalNot:
                    value = UnaryExpression (ESIR_NodeKind.UnaryLogicalNot, exprData.Value);
                    break;

                case SimpleUnaryExprType.BitNot:
                    value = UnaryExpression (ESIR_NodeKind.UnaryBitNot, exprData.Value);
                    break;

                case SimpleUnaryExprType.Dereference:
                    value = UnaryExpression (ESIR_NodeKind.UnaryDereference, exprData.Value);
                    writable = true;
                    break;

                default:
                    throw new NotImplementedException ("Unary operation not implemented.");
            }

            if (exprData.Type->TypeTag == ES_TypeTag.Reference && expr.ExpressionType == SimpleUnaryExprType.Dereference)
                writable = true;

            return ExpressionData.NewValue (expr, finalType, exprData.Expressions, value, exprData.ValueRegister, constant, writable);
        }

        private static ExpressionData CheckExpression_Cast (
            ref PassData passData, ES_AstCastExpression expr,
            ES_TypeInfo* expectedType
        ) {
            var typeUnkn = passData.Env.TypeUnknownValue;

            var destType = GetTypeRef (expr.DestinationType);
            var exprData = CheckExpression (ref passData, expr.InnerExpression, typeUnkn);

            if (!ExplicitCast (ref passData, ref exprData, destType, out bool castRedundant)) {
                passData.ErrorList.Add (ES_FrontendErrors.GenNoExplicitCast (
                    destType->Name.GetNameAsTypeString (), exprData.Type->Name.GetNameAsTypeString (),
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
            ES_TypeInfo* expectedType
        ) {
            var typeUnkn = passData.Env.TypeUnknownValue;

            var funcExpr = CheckExpression (ref passData, expr.FunctionExpression, typeUnkn);

            if (funcExpr.Function is not null)
                return CheckExpression_SimpleFunctionCall (ref passData, expr, funcExpr);
            else if (funcExpr.TypeInfo is not null) {
                passData.ErrorList.Add (ES_FrontendErrors.GenCantInvokeType (
                    funcExpr.TypeInfo->Name.GetNameAsTypeString (), passData.Source, funcExpr.Expr.NodeBounds
                ));
            } else if (funcExpr.Type is not null) {
                // TODO: Some types might be allowed to have `()` overrides too in the future. But not now.
                if (funcExpr.Type->TypeTag != ES_TypeTag.UNKNOWN) {
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
            var typeUnkn = passData.Env.TypeUnknownValue;

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
                    arg.Name.GetPooledString (Encoding.ASCII),
                    func->Name.TypeName.GetPooledString (Encoding.ASCII),
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
                            func->Name.TypeName.GetPooledString (Encoding.ASCII), passData.Source,
                            arg.ValueExpression.NodeBounds
                        ));
                        ignoreDefArgs = true;
                    }

                    var exprData = CheckExpression (ref passData, arg.ValueExpression, typeUnkn);
                    exprData.Expressions.Dispose ();
                    passData.IRWriter.ReturnRegister (exprData.ValueRegister);
                    continue;
                }

                var argData = func->Arguments.Elements + argIdx;
                var argTypeData = funcType->ArgumentsList.Elements + argIdx;

                if (arg.ArgType != argTypeData->ArgType) {
                    passData.ErrorList.Add (ES_FrontendErrors.GenWrongArgType (
                        argData->Name.GetPooledString (Encoding.ASCII),
                        arg.ArgType.ToString (), passData.Source, arg.ValueExpression.NodeBounds
                    ));
                }

                var argValType = argTypeData->ValueType;
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

                    var argValType = argTypeData->ValueType;
                    var argExprData = CheckExpression (ref passData, argDef.DefaultExpression!, argValType);
                    EnsureCompat (ref argExprData, argValType, ref passData, argExprData.Expr.NodeBounds);

                    exprList.Merge (ref argExprData.Expressions);
                    exprList.AddRegister (argExprData.ValueRegister);

                    argsList.Add (ArgumentValue (ES_ArgumentType.Normal, argExprData.Value));
                }
            }

            var constant = funcType->ReturnType->IsConstant ();
            var value = FunctionCallExpression (mangledName, List (argsList.Span));

            return ExpressionData.NewValue (expr, funcType->ReturnType, exprList, value, null, constant, false);
        }

        private static bool CheckExpression_SimpleBinaryExpression_Compat (
            ref PassData passData, ES_AstSimpleBinaryExpression expr,
            ref ExpressionData lhs, ref ExpressionData rhs,
            out ES_TypeInfo* finalType
        ) {
            var op = expr.ExpressionType;

            var lhsNull = lhs.Type->TypeTag == ES_TypeTag.Null;
            var rhsNull = rhs.Type->TypeTag == ES_TypeTag.Null;
            var lhsRef = lhs.Type->IsReferenceType ();
            var rhsRef = rhs.Type->IsReferenceType ();
            var lhsInt = lhs.Type->TypeTag == ES_TypeTag.Int;
            var rhsInt = rhs.Type->TypeTag == ES_TypeTag.Int;

            Debug.Assert (!(lhsNull & rhsNull));
            if (!passData.EnvBuilder.BinaryOpCompat (lhs.Type, rhs.Type, op, out finalType, out _))
                return false;

            if (lhsNull && rhsRef) {
                lhs.Type = rhs.Type;
                lhs.Value = NullLiteralExpression (TypeNode (rhs.Type));
            } else if (rhsNull && lhsRef) {
                rhs.Type = lhs.Type;
                rhs.Value = NullLiteralExpression (TypeNode (lhs.Type));
            } else if (lhsInt && rhsInt) {
                var lhsTypeInt = (ES_IntTypeData*) lhs.Type;
                var rhsTypeInt = (ES_IntTypeData*) rhs.Type;

                if (op.IsBitShift ()) {
                    if (rhsTypeInt->IntSize < ES_IntSize.Int32) {
                        rhs.Type = passData.Env.GetIntType (ES_IntSize.Int32, false);
                        rhs.Value = CastExpression (rhs.Value, TypeNode (rhs.Type));
                    }

                    return true;
                }

                if (lhsTypeInt->IntSize == rhsTypeInt->IntSize)
                    return true;

                if (lhsTypeInt->IntSize > rhsTypeInt->IntSize) {
                    rhs.Type = lhs.Type;
                    rhs.Value = CastExpression (rhs.Value, TypeNode (lhs.Type));
                } else {
                    lhs.Type = rhs.Type;
                    lhs.Value = CastExpression (lhs.Value, TypeNode (rhs.Type));
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
            ES_TypeInfo* expectedType
        ) {
            var typeUnkn = passData.Env.TypeUnknownValue;

            var leftExpr = CheckExpression (ref passData, expr.Left, typeUnkn);

            var expectedRightType = typeUnkn;
            if (expr.ExpressionType.IsBitShift () && leftExpr.Type->TypeTag == ES_TypeTag.Int)
                expectedRightType = passData.Env.GetIntType (((ES_IntTypeData*) expectedType)->IntSize, true);
            else if (expr.ExpressionType.IsAssignment ())
                expectedRightType = leftExpr.Type is not null ? leftExpr.Type : typeUnkn;

            var rightExpr = CheckExpression (ref passData, expr.Right, expectedRightType);

            if (leftExpr.Type is null || rightExpr.Type is null) {
                leftExpr.Expressions.Dispose ();
                rightExpr.Expressions.Dispose ();
                return ExpressionError (ref passData, expr);
            }

            if (!CheckExpression_SimpleBinaryExpression_Compat (ref passData, expr, ref leftExpr, ref rightExpr, out var finalType)) {
                passData.ErrorList.Add (ES_FrontendErrors.GenCantApplyBinaryOp (
                    expr.OperatorToken.Text.Span.GetPooledString (),
                    leftExpr.Type->Name.GetNameAsTypeString (), rightExpr.Type->Name.GetNameAsTypeString (),
                    passData.Source, expr.NodeBounds
                ));

                leftExpr.Expressions.Dispose ();
                rightExpr.Expressions.Dispose ();
                return ExpressionError (ref passData, expr);
            }

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
                return ExpressionData.NewValue (expr, finalType, exprList, assignValue, null, false, false);
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

            return ExpressionData.NewValue (expr, finalType, exprList, value, null, false, false);
        }
    }
}
