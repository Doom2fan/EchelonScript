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
using ChronosLib.Pooled;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Frontend;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        public struct ExpressionData {
            public ES_AstExpression Expr;
            public ES_TypeInfo* Type;
            public ES_TypeInfo* TypeInfo;
            public ES_FunctionData* Function;

            public ExpressionSyntax? Value;

            public bool Constant;
            public bool Addressable;

            public bool TypeIsPointer (ES_TypeInfo* baseType = null) {
                if (Type is null)
                    return false;

                if (Type->TypeTag != ES_TypeTag.Reference)
                    return false;

                var refType = (ES_ReferenceData*) Type;

                return (baseType is null || refType->PointedType == baseType);
            }
        }

        private void GenerateCode_EnsureImplicitCompat (ref ExpressionData exprData, ES_TypeInfo* dstType) {
            var srcType = exprData.Type;

            Debug.Assert (srcType is not null);

            if (exprData.Type == dstType)
                return;

            if (dstType->TypeTag == ES_TypeTag.Int && dstType->TypeTag == ES_TypeTag.Int) {
                var dstIntType = (ES_IntTypeData*) dstType;
                var srcIntType = (ES_IntTypeData*) srcType;

                if (srcIntType->IntSize <= dstIntType->IntSize && dstIntType->Unsigned == srcIntType->Unsigned) {
                    exprData = GenerateCode_Cast (exprData, dstType);
                    return;
                }
            }

            throw new CompilationException (ES_BackendErrors.FrontendError);
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
                    throw new NotImplementedException ("[TODO] Indexing expressions not implemented yet.");
                    /*GenerateCode_Expression (ref transUnit, symbols, src, indexExpr.IndexedExpression);
                    foreach (var rank in indexExpr.RankExpressions) {
                        if (rank is not null)
                            GenerateCode_Expression (ref transUnit, symbols, src, rank);
                    }*/
                }

                case ES_AstNewObjectExpression newObjExpr:
                    return GenerateCode_Expression_NewObject (ref transUnit, symbols, src, newObjExpr, expectedType);

                case ES_AstNewArrayExpression newArrayExpr:
                    throw new NotImplementedException ("[TODO] 'new' array expressions not implemented yet.");

                case ES_AstIntegerLiteralExpression:
                case ES_AstBooleanLiteralExpression:
                case ES_AstFloatLiteralExpression:
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                case ES_AstIntegerConstantExpression intConstExpr: {
                    Debug.Assert (intConstExpr.IntType->TypeTag == ES_TypeTag.Int);
                    var type = intConstExpr.IntType;
                    var intType = (ES_IntTypeData*) type;

                    SyntaxToken value;

                    bool unsigned = intType->Unsigned;
                    ulong constVal = intConstExpr.Value;
                    switch (intType->IntSize) {
                        case ES_IntSize.Int8:
                            value = !unsigned ? Literal ((sbyte) constVal) : Literal ((byte) constVal);
                            break;
                        case ES_IntSize.Int16:
                            value = !unsigned ? Literal ((short) constVal) : Literal ((ushort) constVal);
                            break;
                        case ES_IntSize.Int32:
                            value = !unsigned ? Literal ((int) constVal) : Literal ((uint) constVal);
                            break;
                        case ES_IntSize.Int64:
                            value = !unsigned ? Literal ((long) constVal) : Literal (constVal);
                            break;

                        default:
                            throw new NotImplementedException ("Size not implemented.");
                    }

                    var valueExpr = LiteralExpression (SyntaxKind.NumericLiteralExpression, value);
                    return new ExpressionData { Expr = expr, Type = type, Value = valueExpr, Constant = true, Addressable = false };
                }

                case ES_AstBooleanConstantExpression boolConstExpr: {
                    var value = LiteralExpression (boolConstExpr.Value ? SyntaxKind.TrueLiteralExpression : SyntaxKind.FalseLiteralExpression);
                    return new ExpressionData { Expr = expr, Type = env.TypeBool, Value = value, Constant = true, Addressable = false };
                }

                case ES_AstFloat32ConstantExpression floatConstLit: {
                    var value = LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (floatConstLit.Value));
                    return new ExpressionData { Expr = expr, Type = env.TypeFloat32, Value = value, Constant = true, Addressable = false };
                }

                case ES_AstFloat64ConstantExpression doubleConstLit: {
                    var value = LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (doubleConstLit.Value));
                    return new ExpressionData { Expr = expr, Type = env.TypeFloat64, Value = value, Constant = true, Addressable = false };
                }

                case ES_AstStringLiteralExpression:
                    throw new NotImplementedException ("[TODO] String literals not implemented yet.");

                case ES_AstCharLiteralExpression:
                    throw new NotImplementedException ("[TODO] Character literals not implemented yet.");

                case ES_AstNameExpression nameExpr: {
                    var id = idPool.GetIdentifier (nameExpr.Value.Text.Span);
                    var symbol = symbols.GetSymbol (id);

                    switch (symbol.Tag) {
                        case SymbolType.None:
                            throw new CompilationException (ES_BackendErrors.NonExistentSymbol);

                        case SymbolType.Variable: {
                            var varData = symbol.MatchVariable ();
                            var valueExpr = IdentifierName (varData.RoslynName);
                            return new ExpressionData { Expr = expr, Type = varData.Type, Value = valueExpr, Constant = false, Addressable = true };
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
                            throw new NotImplementedException ("Symbol type not implemented.");
                    }
                }

                case ES_AstMemberAccessExpression memberAccessExpr:
                    return GenerateCode_Expression_MemberAccess (ref transUnit, symbols, src, memberAccessExpr, expectedType);

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
                    if (simpleBinaryExpr.ExpressionType.IsLogical ())
                        return GenerateCode_LogicalBinaryExpr (ref transUnit, symbols, src, simpleBinaryExpr);

                    var expectedRHSType = expectedType;

                    var lhs = GenerateCode_Expression (ref transUnit, symbols, src, simpleBinaryExpr.Left, expectedType);

                    if (simpleBinaryExpr.ExpressionType.IsBitShift () && lhs.Type->TypeTag == ES_TypeTag.Int) {
                        var intName = ES_PrimitiveTypes.GetIntName (((ES_IntTypeData*) expectedType)->IntSize, true);
                        expectedRHSType = env.GetFullyQualifiedType (env.GlobalTypesNamespace, idPool.GetIdentifier (intName));
                    }

                    var rhs = GenerateCode_Expression (ref transUnit, symbols, src, simpleBinaryExpr.Right, expectedRHSType);

                    if (!envBuilder!.BinaryOpCompat (lhs.Type, rhs.Type, simpleBinaryExpr.ExpressionType, out _, out _))
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    if (simpleBinaryExpr.ExpressionType.IsAssignment () && !lhs.Addressable)
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    var ret = GenerateCode_BinaryExpr (lhs, rhs, simpleBinaryExpr.ExpressionType);
                    ret.Expr = expr;
                    return ret;
                }

                case ES_AstConditionalExpression condExpr:
                    return GenerateCode_ConditionalExpression (ref transUnit, symbols, src, condExpr, expectedType);

                default:
                    throw new NotImplementedException ("Expression type not implemented.");
            }
        }

        private ExpressionData GenerateCode_IncDecExpression (ExpressionData val, bool decrement, bool postfix) {
            if (!val.Addressable || val.Type is null || val.Value is null)
                throw new CompilationException (ES_BackendErrors.FrontendError);

            ExpressionSyntax value;

            if (val.Type->TypeTag == ES_TypeTag.Int || val.Type->TypeTag == ES_TypeTag.Float) {
                if (postfix) {
                    var exprType = !decrement ? SyntaxKind.PostIncrementExpression : SyntaxKind.PostDecrementExpression;
                    value = PostfixUnaryExpression (exprType, val.Value);
                } else {
                    var exprType = !decrement ? SyntaxKind.PreIncrementExpression : SyntaxKind.PreDecrementExpression;
                    value = PrefixUnaryExpression (exprType, val.Value);
                }
            } else
                throw new CompilationException (ES_BackendErrors.FrontendError);

            return new ExpressionData { Type = val.Type, Value = value, Constant = false, Addressable = false };
        }

        private ExpressionData GenerateCode_Expression_FunctionCall (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_AstFunctionCallExpression funcCallExpr, ES_TypeInfo* expectedType
        ) {
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

            int funcArgCount = funcType->ArgumentsList.Length;
            int callArgCount = funcCallExpr.Arguments.Length;
            int reqArgCount = 0;

            if (!envBuilder!.PointerAstMap.TryGetValue ((IntPtr) func, out var funcASTNode))
                throw new CompilationException (ES_BackendErrors.NonExistentASTMap);

            var funcAST = funcASTNode as ES_AstFunctionDefinition;
            Debug.Assert (funcAST is not null);

            if (func is not null)
                reqArgCount = funcArgCount - func->OptionalArgsCount;
            else
                reqArgCount = funcArgCount;

            if (callArgCount < reqArgCount)
                throw new CompilationException (ES_BackendErrors.FrontendError);
            if (callArgCount > funcArgCount)
                throw new CompilationException (ES_BackendErrors.FrontendError);

            ExpressionSyntax innerExpression;
            if (true) {
                innerExpression = MemberAccessExpression (
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName (GlobalStorageTypeName),
                    IdentifierName (MangleGlobalFunctionName (func))
                );
            } else {
                // TODO: Handle member functions.
            }

            using var argsArr = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
            argsArr.EnsureCapacity (funcArgCount + Math.Max (funcArgCount - 1, 0));
            int argIdx = 0;

            // Handle passed-in args.
            for (; argIdx < callArgCount; argIdx++) {
                var arg = funcCallExpr.Arguments [argIdx];
                var argData = func->Arguments.Elements + argIdx;
                var argTypeData = funcType->ArgumentsList.Elements + argIdx;

                if (argTypeData->ArgType == ES_ArgumentType.Normal && arg.ArgType != ES_ArgumentType.Normal)
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                if (argTypeData->ArgType != ES_ArgumentType.Normal && arg.ArgType != argTypeData->ArgType)
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                var argValType = argTypeData->ValueType;
                var argExprData = GenerateCode_Expression (ref transUnit, symbols, src, arg.ValueExpression, argValType);

                GenerateCode_EnsureImplicitCompat (ref argExprData, argValType);
                Debug.Assert (argExprData.Value is not null);

                if (argIdx > 0)
                    argsArr.Add (Token (SyntaxKind.CommaToken));
                argsArr.Add (Argument (argExprData.Value));
            }

            // Handle default args.
            for (; argIdx < funcArgCount; argIdx++) {
                var arg = funcAST.ArgumentsList [argIdx];
                var argData = func->Arguments.Elements + argIdx;
                var argTypeData = funcType->ArgumentsList.Elements + argIdx;

                if (arg.DefaultExpression is null)
                    throw new CompilationException (ES_BackendErrors.FrontendError);
                if (argTypeData->ArgType != ES_ArgumentType.Normal && argTypeData->ArgType != ES_ArgumentType.In)
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                var argValType = argTypeData->ValueType;
                var argExprData = GenerateCode_Expression (ref transUnit, symbols, src, arg.DefaultExpression, argValType);

                GenerateCode_EnsureImplicitCompat (ref argExprData, argValType);
                Debug.Assert (argExprData.Value is not null);

                if (argIdx > 0)
                    argsArr.Add (Token (SyntaxKind.CommaToken));
                argsArr.Add (Argument (argExprData.Value));
            }

            var value = InvocationExpression (innerExpression)
                .WithArgumentList (ArgumentList (SeparatedList<ArgumentSyntax> (argsArr)));

            return new ExpressionData { Expr = funcCallExpr, Type = funcType->ReturnType, Value = value, Constant = false, Addressable = false };
        }

        /*private LLVMValueRef GenerateCode_GetAllocObjFunc () {
            const string funcName = "GC_AllocObj";

            var func = moduleRef.GetNamedFunction (funcName);
            if (func is not null)
                return func;

            var voidPtrType = LLVMTypeRef.CreatePointer (contextRef.VoidType, 0);

            Span<LLVMTypeRef> funcTypeArgs = stackalloc LLVMTypeRef [1] {
                voidPtrType,
            };
            var funcType = LLVMTypeRef.CreateFunction (voidPtrType, funcTypeArgs, false);

            func = moduleRef.AddFunction (funcName, funcType);

            return func;
        }*/

        private ExpressionSyntax GenerateCode_NewObject (ES_TypeInfo* type, ExpressionSyntax assignValue) {
            // Generate the member access. ("ImmixGC.AllocObject")
            var accessExpr = MemberAccessExpression (SyntaxKind.SimpleMemberAccessExpression,
                IdentifierName ("ImmixGC"),
                GenericName (Identifier ("AllocObject")).WithTypeArgumentList (TypeArgumentList (
                    SingletonSeparatedList (GetRoslynType (type))
                ))
            );

            // Construct the types list.
            using var argsList = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
            argsList.EnsureCapacity (3);

            // Generate the pointer type syntax for the type pointer.
            var pointerTypeSyntax = PointerType (IdentifierName ("ES_TypeInfo"));

            // Add the type pointer.
            argsList.Add (Argument (PointerLiteral (type, pointerTypeSyntax)));

            // Add the value to assign.
            argsList.Add (Token (SyntaxKind.CommaToken));
            argsList.Add (Argument (assignValue));

            // Generate the function call.
            return InvocationExpression (accessExpr)
                .WithArgumentList (ArgumentList (SeparatedList<ArgumentSyntax> (argsList)));
        }

        private ExpressionData GenerateCode_Expression_NewObject (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_AstNewObjectExpression newObjExpr, ES_TypeInfo* expectedType
        ) {
            //var typeUnkn = env!.TypeUnknownValue;

            var objType = GetTypeRef (newObjExpr.TypeDeclaration);
            var ptrType = envBuilder!.CreateReferenceType (objType);

            // Evaluate the constructor arguments.
            var args = new StructPooledList<ExpressionData> (CL_ClearMode.Auto);
            args.EnsureCapacity (newObjExpr.Arguments.Length);

            if (newObjExpr.Arguments.Length > 0) {
                throw new NotImplementedException ("[TODO] Parametrized constructors not implemented yet.");
            }
            /*int argCount = 0;
            foreach (var arg in newObjExpr.Arguments)
                args [argCount++] = GenerateCode_Expression (ref transUnit, symbols, src, arg.ValueExpression, typeUnkn);*/

            var value = GenerateCode_NewObject (objType, GetDefaultValue (objType));

            // Default-initialize the object.
            return new ExpressionData { Expr = newObjExpr, Type = ptrType, Value = value, Constant = false, Addressable = false };
        }

        private ExpressionData GenerateCode_ConditionalExpression (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_AstConditionalExpression expr, ES_TypeInfo* expectedType
        ) {
            var condExpr = GenerateCode_Expression (ref transUnit, symbols, src, expr.Condition, env!.TypeBool);

            GenerateCode_EnsureImplicitCompat (ref condExpr, env.TypeBool);

            // Then
            var leftExpr = GenerateCode_Expression (ref transUnit, symbols, src, expr.Then, expectedType);

            // Else
            var rightExpr = GenerateCode_Expression (ref transUnit, symbols, src, expr.Else, expectedType);

            // Checks
            GenerateCode_EnsureImplicitCompat (ref leftExpr, expectedType);
            GenerateCode_EnsureImplicitCompat (ref rightExpr, expectedType);

            Debug.Assert (condExpr.Value is not null);
            Debug.Assert (leftExpr.Value is not null);
            Debug.Assert (rightExpr.Value is not null);

            var exprValue = ConditionalExpression (condExpr.Value, leftExpr.Value, rightExpr.Value);
            bool constant = condExpr.Constant & leftExpr.Constant & rightExpr.Constant;

            return new ExpressionData { Expr = expr, Type = leftExpr.Type, Value = exprValue, Constant = constant, Addressable = false };
        }
    }
}
