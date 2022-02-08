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
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.GarbageCollection;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Utilities;
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
            public bool Writable;
        }

        private static ExpressionSyntax CompileCode_StaticVarsMem ()
            => SimpleMemberAccess (GlobalStorageTypeName, StaticVarsMemName);

        private static ExpressionSyntax CompileCode_NewObject (ES_TypeInfo* type, ExpressionSyntax? assignValue) {
            bool isReference = type->TypeTag == ES_TypeTag.Reference;

            // Get the roslyn type.
            var intPtrType = IdentifierName (nameof (IntPtr));
            var roslynType = GetRoslynType (type);

            // Generate the member access.
            var accessExpr = MemberAccessExpression (SyntaxKind.SimpleMemberAccessExpression,
                IdentifierName (nameof (ES_GarbageCollector)),
                GenericName (Identifier (nameof (ES_GarbageCollector.AllocObject))).WithTypeArgumentList (
                    TypeArgumentList (SingletonSeparatedList (!isReference ? roslynType : intPtrType))
                )
            );

            // Construct the args list.
            using var argsList = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
            argsList.EnsureCapacity (3);

            // Generate the pointer type syntax for the type pointer.
            var pointerTypeSyntax = PointerType (IdentifierName (nameof (ES_TypeInfo)));

            // Add the type pointer.
            argsList.Add (Argument (PointerLiteral (type, pointerTypeSyntax)));

            // Add the "pinned" bool.
            argsList.Add (Token (SyntaxKind.CommaToken));
            argsList.Add (Argument (BoolLiteral (false)));

            // Add the value to assign.
            if (assignValue is not null) {
                argsList.Add (Token (SyntaxKind.CommaToken));
                if (isReference)
                    assignValue = CastExpression (intPtrType, assignValue);
                argsList.Add (Argument (assignValue));
            }

            // Generate the function call.
            ExpressionSyntax ret = InvocationExpression (accessExpr)
                .WithArgumentList (ArgumentList (SeparatedListSpan<ArgumentSyntax> (argsList.Span)));

            if (isReference)
                ret = CastExpression (PointerType (roslynType), ret);

            return ret;
        }

        private static ExpressionSyntax CompileCode_NewArray (ES_ArrayTypeData* arrayType, ReadOnlySpan<ExpressionData> ranks, ExpressionSyntax? elemAssignValue) {
            Debug.Assert (arrayType->DimensionsCount == ranks.Length);

            // Get the roslyn type.
            var roslynArrType = GetRoslynType (&arrayType->TypeInfo);
            var roslynElemType = GetRoslynType (arrayType->ElementType);

            /** Construct the args list **/
            using var argsList = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
            argsList.EnsureCapacity (1 + ranks.Length * 2 + (elemAssignValue is not null ? 2 : 0));

            // Add the "pinned" bool.
            argsList.Add (Argument (BoolLiteral (false)));

            // Add the ranks.
            foreach (var rank in ranks) {
                Debug.Assert (rank.Value is not null);

                argsList.Add (Token (SyntaxKind.CommaToken));
                argsList.Add (Argument (rank.Value!));
            }

            // Add the value to assign, if any.
            if (elemAssignValue is not null) {
                argsList.Add (Token (SyntaxKind.CommaToken));
                argsList.Add (Argument (elemAssignValue));
            }

            /** Generate the function call **/
            var ret =
                InvocationExpression (MemberAccessExpression (
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName (MangleTypeName (&arrayType->TypeInfo)),
                    IdentifierName (ArrayAllocFuncName)
                )).WithArgumentList (ArgumentList (SeparatedListSpan<ArgumentSyntax> (argsList.Span)));

            return ret;
        }

        private static ExpressionData CompileExpression (
            ref PassData passData,
            ref FunctionData funcData,
            ESIR_Expression expr
        ) {
            if (expr is ESIR_SimpleBinaryExpression simpleBinaryExpr)
                return CompileExpression_SimpleBinary (ref passData, ref funcData, simpleBinaryExpr);
            else if (expr is ESIR_UnaryExpression unaryExpr)
                return CompileExpression_Unary (ref passData, ref funcData, unaryExpr);

            switch (expr.Kind) {
                case ESIR_NodeKind.ErrorExpression:
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                case ESIR_NodeKind.AssignExpression when expr is ESIR_AssignExpression assignExpr: {
                    var assigneeExpr = CompileExpression (ref passData, ref funcData, assignExpr.Assignee);
                    var valueExpr = CompileExpression (ref passData, ref funcData, assignExpr.Value);

                    Debug.Assert (assigneeExpr.Type == valueExpr.Type);

                    var value = AssignmentExpression (
                        SyntaxKind.SimpleAssignmentExpression,
                        assigneeExpr.Value!,
                        valueExpr.Value!
                    );

                    return new ExpressionData { Type = assigneeExpr.Type, Value = value, };
                }

                case ESIR_NodeKind.LiteralTrue:
                case ESIR_NodeKind.LiteralFalse:
                    return new ExpressionData { Type = passData.Env.TypeBool, Value = BoolLiteral (expr.Kind == ESIR_NodeKind.LiteralTrue), };

                case ESIR_NodeKind.LiteralInt when expr is ESIR_LiteralExpression litIntExpr: {
                    bool unsigned;
                    ExpressionSyntax value;

                    if (litIntExpr.Value!.TryGetInt (out var longVal)) {
                        value = NumericLiteral (longVal);
                        unsigned = false;
                    } else if (litIntExpr.Value!.TryGetUInt (out var ulongVal)) {
                        value = NumericLiteral (ulongVal);
                        unsigned = true;
                    } else
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    var type = passData.Env.GetIntType (ES_IntSize.Int64, unsigned);
                    return new ExpressionData { Type = type, Value = value, };
                }

                case ESIR_NodeKind.LiteralFloat when expr is ESIR_LiteralExpression litFloatExpr: {
                    ES_TypeInfo* type;
                    ExpressionSyntax value;

                    if (litFloatExpr.Value!.TryGetFloat32 (out var float32Val)) {
                        value = NumericLiteral (float32Val);
                        type = passData.Env.TypeFloat32;
                    } else if (litFloatExpr.Value!.TryGetFloat64 (out var float64Val)) {
                        value = NumericLiteral (float64Val);
                        type = passData.Env.TypeFloat64;
                    } else
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    return new ExpressionData { Type = type, Value = value, };
                }

                //case ESIR_NodeKind.LiteralChar;

                case ESIR_NodeKind.LiteralNull when expr is ESIR_NullLiteralExpression nullLitExpr: {
                    var nullType = nullLitExpr.Type.Pointer;

                    if (nullType is null || nullType->TypeTag == ES_TypeTag.Null || nullType->TypeTag == ES_TypeTag.UNKNOWN)
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    ExpressionSyntax value;
                    switch (nullType->TypeTag) {
                        case ES_TypeTag.Reference:
                        case ES_TypeTag.Array:
                            value = LiteralExpression (SyntaxKind.NullLiteralExpression);
                            break;

                        default:
                            throw new CompilationException (ES_BackendErrors.FrontendError);
                    }

                    return new ExpressionData { Type = nullType, Value = value, };
                }

                //case ESIR_NodeKind.StringConstant:

                case ESIR_NodeKind.StaticVariableExpression when expr is ESIR_StaticVariableExpression staticVarExpr: {
                    if (!passData.StaticVariables.TryGetValue (staticVarExpr.Name, out var staticVar))
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    var mangledName = MangleStaticVariable (staticVar);
                    var value = SimpleMemberAccess (GlobalStorageTypeName, mangledName);

                    return new ExpressionData { Type = staticVar.Type.Pointer, Value = value, };
                }

                case ESIR_NodeKind.ArgumentExpression when expr is ESIR_ArgumentExpression argExpr: {
                    var type = funcData.Args [argExpr.Index].ValueType.Pointer;
                    var value = IdentifierName (GetArgName (argExpr.Index));
                    return new ExpressionData { Type = type, Value = value };
                }

                case ESIR_NodeKind.LocalValueExpression when expr is ESIR_LocalValueExpression localExpr: {
                    var type = funcData.Locals [localExpr.Index].Pointer;
                    var value = IdentifierName (GetLocalVarName (localExpr.Index));
                    return new ExpressionData { Type = type, Value = value };
                }

                case ESIR_NodeKind.DefaultValueExpression when expr is ESIR_DefaultValueExpression defExpr: {
                    var type = defExpr.Type.Pointer;
                    var value = GetDefaultValue (type);
                    return new ExpressionData { Type = type, Value = value, };
                }

                case ESIR_NodeKind.MemberAccessExpression when expr is ESIR_MemberAccessExpression memberAccessExpr:
                    return CompileExpression_MemberAccess (ref passData, ref funcData, memberAccessExpr);

                case ESIR_NodeKind.FunctionCallExpression when expr is ESIR_FunctionCallExpression callExpr: {
                    var funcNameStr = callExpr.Name.GetPooledString (Encoding.ASCII);
                    var funcId = IdentifierName (funcNameStr);

                    if (!passData.Functions.TryGetValue (callExpr.Name, out var funcDef))
                        throw new CompilationException ($"Unknown function {funcNameStr}");

                    var argValues = callExpr.Arguments.Elements;
                    var argDefs = funcDef.Arguments.Elements;

                    using var argsArr = PooledArray<ArgumentSyntax>.GetArray (argValues.Length);
                    var argsSpan = argsArr.Span;
                    for (int i = 0; i < argValues.Length; i++) {
                        var argValue = argValues [i];
                        var argExpr = CompileExpression (ref passData, ref funcData, argValue.Expression);
                        var arg = Argument (argExpr.Value!);

                        if (argDefs [i].ArgType != argValue.ArgType)
                            throw new CompilationException ("Mismatched arg types.");

                        switch (argValues [i].ArgType) {
                            case ES_ArgumentType.Normal: break;

                            case ES_ArgumentType.Ref:
                                arg = arg.WithRefKindKeyword (Token (SyntaxKind.RefKeyword));
                                break;

                            default:
                                throw new NotImplementedException ("Arg type not implemented yet.");
                        }

                        argsSpan [i] = arg;
                    }

                    var value = InvocationExpression (
                        funcId,
                        ArgumentList (SimpleSeparatedList (argsSpan, Token (SyntaxKind.CommaToken)))
                    );

                    return new ExpressionData { Type = funcDef.ReturnType.Pointer, Value = value, };
                }

                //case ESIR_NodeKind.VirtualCallExpression:

                case ESIR_NodeKind.IndexingExpression when expr is ESIR_IndexingExpression indexExpr:
                    return CompileExpression_Indexing (ref passData, ref funcData, indexExpr);

                case ESIR_NodeKind.NewObjectExpression when expr is ESIR_NewObjectExpression newObjExpr:
                    return CompileExpression_NewObject (ref passData, ref funcData, newObjExpr);

                case ESIR_NodeKind.NewArrayExpression when expr is ESIR_NewArrayExpression newArrExpr:
                    return CompileExpression_NewArray (ref passData, ref funcData, newArrExpr);

                case ESIR_NodeKind.CastExpression when expr is ESIR_CastExpression castExpr:
                    return CompileExpression_Cast (ref passData, ref funcData, castExpr);

                case ESIR_NodeKind.ConditionalExpression when expr is ESIR_ConditionalExpression condExpr:
                    return CompileExpression_Conditional (ref passData, ref funcData, condExpr);

                default:
                    throw new NotImplementedException ("Expression type not implemented.");
            }
        }

        private static ExpressionData CompileExpression_Conditional (
            ref PassData passData,
            ref FunctionData funcData,
            ESIR_ConditionalExpression expr
        ) {
            var condExpr = CompileExpression (ref passData, ref funcData, expr.Condition);
            var thenExpr = CompileExpression (ref passData, ref funcData, expr.ThenExpression);
            var elseExpr = CompileExpression (ref passData, ref funcData, expr.ElseExpression);

            Debug.Assert (condExpr.Type->TypeTag == ES_TypeTag.Bool);
            Debug.Assert (thenExpr.Type == elseExpr.Type);

            var value = ConditionalExpression (condExpr.Value!, thenExpr.Value!, elseExpr.Value!);
            return new ExpressionData { Type = thenExpr.Type, Value = value, };
        }

        private static ExpressionData CompileExpression_Indexing (
            ref PassData passData,
            ref FunctionData funcData,
            ESIR_IndexingExpression expr
        ) {
            var typeUnkn = passData.Env.TypeUnknownValue;
            var typeIndex = passData.Env.GetArrayIndexType ();

            var indexedExpr = CompileExpression (ref passData, ref funcData, expr.IndexedExpr);
            var indices = expr.Indices.Elements;

            using var idxExprs = new StructPooledList<ExpressionData> (CL_ClearMode.Auto);
            idxExprs.EnsureCapacity (indices.Length);
            foreach (var idxExpr in indices) {
                var rankExprData = CompileExpression (ref passData, ref funcData, idxExpr);
                if (rankExprData.Type != typeIndex)
                    throw new CompilationException ("Index expression must be of the index type.");

                idxExprs.Add (rankExprData);
            }

            if (indexedExpr.Type is not null) {
                var indexedTypeTag = indexedExpr.Type->TypeTag;

                if (indexedTypeTag == ES_TypeTag.Array)
                    return CompileExpression_Indexing_Array (ref passData, ref funcData, expr, indexedExpr, idxExprs.Span);
            }

            throw new NotImplementedException ("Indexing not implemented for type.");
        }

        private static ExpressionData CompileExpression_Indexing_Array (
            ref PassData passData,
            ref FunctionData funcData,
            ESIR_IndexingExpression expr,
            ExpressionData indexedExpr,
            ReadOnlySpan<ExpressionData> indices
        ) {
            var arrayType = (ES_ArrayTypeData*) indexedExpr.Type;
            var elemType = arrayType->ElementType;
            var dimCount = arrayType->DimensionsCount;

            Debug.Assert (dimCount == indices.Length);

            var arrayPtr = indexedExpr.Value!;

            using var argsList = new StructPooledList<ArgumentSyntax> (CL_ClearMode.Auto);
            argsList.Add (Argument (arrayPtr));

            for (int i = 0; i < dimCount; i++)
                argsList.Add (Argument (indices [i].Value!));

            var value = (
                InvocationExpression (MemberAccessExpression (
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName (MangleTypeName (&arrayType->TypeInfo)),
                    IdentifierName (ArrayIndexFuncName)
                )).WithArgumentList (ArgumentList (
                    SimpleSeparatedList (argsList.Span, Token (SyntaxKind.CommaToken))
                ))
            );

            return new ExpressionData { Type = elemType, Value = value, };
        }

        private static ExpressionData CompileExpression_NewObject (
            ref PassData passData,
            ref FunctionData funcData,
            ESIR_NewObjectExpression expr
        ) {
            var objType = expr.Type.Pointer;
            var ptrType = passData.EnvBuilder.CreateReferenceType (objType);

            var value = CompileCode_NewObject (objType, GetDefaultValue (objType));

            // Default-initialize the object.
            return new ExpressionData { Type = ptrType, Value = value, };
        }

        private static ExpressionData CompileExpression_NewArray (
            ref PassData passData,
            ref FunctionData funcData,
            ESIR_NewArrayExpression expr
        ) {
            var typeIndex = passData.Env.GetArrayIndexType ();

            var elemType = expr.ElementType.Pointer;
            var ranksExprs = expr.Ranks.Elements;
            var arrType = (ES_ArrayTypeData*) passData.EnvBuilder.CreateArrayType (elemType, ranksExprs.Length);

            // Evaluate the ranks.
            using var ranks = PooledArray<ExpressionData>.GetArray (ranksExprs.Length);

            int rankCount = 0;
            foreach (var rank in ranksExprs)
                ranks.Span [rankCount++] = CompileExpression (ref passData, ref funcData, rank);

            // Get default values.
            var defaultElemValue = GetDefaultValue (elemType);

            var value = CompileCode_NewArray (arrType, ranks.Span, defaultElemValue);

            return new ExpressionData { Type = &arrType->TypeInfo, Value = value, };
        }
    }
}
