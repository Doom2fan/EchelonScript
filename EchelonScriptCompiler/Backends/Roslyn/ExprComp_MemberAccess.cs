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
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Utilities;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private ExpressionData GenerateCode_Expression_MemberAccess (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_AstMemberAccessExpression expr, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (env is not null);
            Debug.Assert (expr.Member is not null);

            var idPool = env.IdPool;
            var typeUnkn = env.TypeUnknownValue;

            var parentExpr = GenerateCode_Expression (ref transUnit, symbols, src, expr.Parent, null);
            var memberChars = expr.Member.Value.Text.Span;
            var memberId = idPool.GetIdentifier (memberChars);

            if (parentExpr.Type is not null) {
                var typeTag = parentExpr.Type->TypeTag;
                if (typeTag == ES_TypeTag.Reference)
                    typeTag = ((ES_ReferenceData*) parentExpr.Type)->PointedType->TypeTag;

                switch (typeTag) {
                    case ES_TypeTag.UNKNOWN:
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    case ES_TypeTag.Struct:
                        return GenerateCode_Expression_MemberAccess_Struct (src, expr, ref parentExpr, memberId);

                    case ES_TypeTag.Array:
                        return GenerateCode_Expression_MemberAccess_Array (src, expr, ref parentExpr, memberChars);

                    default:
                        throw new NotImplementedException ("Type not implemented yet.");
                }
            } else if (parentExpr.TypeInfo is not null) {
                var type = parentExpr.TypeInfo;

                switch (type->TypeTag) {
                    case ES_TypeTag.UNKNOWN:
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    case ES_TypeTag.Struct:
                        return GenerateCode_Expression_MemberAccessStatic_Aggregate (src, expr, type, memberId);

                    default:
                        throw new NotImplementedException ("Type not implemented yet.");
                }
            } else if (parentExpr.Function is not null)
                throw new NotImplementedException ("Not supported. (yet?)");
            else
                throw new CompilationException ("<<Unknown expression type in GenerateCode_Expression_MemberAccess>>");
        }

        private ExpressionSyntax GenerateCode_NullCheck (ExpressionSyntax ptrExpr) {
            return (
                InvocationExpression (
                    SimpleMemberAccess (nameof (ES_ObjectAddress), nameof (ES_ObjectAddress.NullCheck))
                ).WithArgumentList (ArgumentList (
                    SingletonSeparatedList (Argument (ptrExpr))
                ))
            );
        }

        private ExpressionData GenerateCode_Expression_MemberAccess_Struct (
            ReadOnlySpan<char> src, ES_AstMemberAccessExpression expr,
            ref ExpressionData parentExpr, ArrayPointer<byte> memberId
        ) {
            Debug.Assert (parentExpr.Type is not null);
            Debug.Assert (parentExpr.Value is not null);

            var type = parentExpr.Type;
            var isRef = false;

            var structExpr = parentExpr.Value;

            if (type->TypeTag == ES_TypeTag.Reference) {
                type = ((ES_ReferenceData*) parentExpr.Type)->PointedType;
                isRef = true;

                structExpr = GenerateCode_NullCheck (structExpr);
            }

            var membersArr = type->MembersList.MembersList;
            foreach (var memberAddr in membersArr.Span) {
                var memberPtr = memberAddr.Address;

                if (!memberPtr->Name.Equals (memberId))
                    continue;

                bool isStatic = memberPtr->Flags.HasFlag (ES_MemberFlags.Static);

                switch (memberPtr->MemberType) {
                    case ES_MemberType.Field: {
                        var memberVar = (ES_MemberData_Variable*) memberPtr;
                        bool addressable = true;

                        if (isStatic)
                            throw new CompilationException (ES_BackendErrors.FrontendError);

                        var value = MemberAccessExpression (
                            !isRef ? SyntaxKind.SimpleMemberAccessExpression : SyntaxKind.PointerMemberAccessExpression,
                            structExpr, IdentifierName (memberId.GetPooledString (Encoding.ASCII))
                        );

                        return new ExpressionData { Expr = expr, Type = memberVar->Type, Value = value, Constant = false, Writable = addressable };
                    }

                    case ES_MemberType.Function:
                        throw new NotImplementedException ("[TODO] Member function access not implemented yet.");

                    default:
                        throw new NotImplementedException ("Member type not implemented yet.");
                }
            }

            throw new CompilationException (ES_BackendErrors.FrontendError);
        }

        private ExpressionData GenerateCode_Expression_MemberAccess_Array (
            ReadOnlySpan<char> src, ES_AstMemberAccessExpression expr,
            ref ExpressionData parentExpr, ReadOnlySpan<char> memberChars
        ) {
            Debug.Assert (env is not null);
            Debug.Assert (parentExpr.Type is not null);
            Debug.Assert (parentExpr.Value is not null);
            Debug.Assert (parentExpr.Type->TypeTag == ES_TypeTag.Array);

            var typeArr = (ES_ArrayTypeData*) parentExpr.Type;
            var typeIndex = env.GetArrayIndexType ();

            var arrExpr = GenerateCode_NullCheck (parentExpr.Value);

            var addressable = false;
            ES_TypeInfo* memberType;
            ExpressionSyntax value;

            var lengthName = typeArr->DimensionsCount < 2 ? "Length" : "TotalLength";
            var dimLenPrefix = "LengthD";
            if (memberChars.Equals (lengthName, StringComparison.Ordinal)) {
                memberType = typeIndex;
                value = PointerMemberAccess (arrExpr, IdentifierName (lengthName));
            } else if (memberChars.Equals ("Rank", StringComparison.Ordinal)) {
                memberType = env.GetIntType (ES_IntSize.Int8, true);
                value = PointerMemberAccess (arrExpr, IdentifierName ("Rank"));
            } else if (memberChars.StartsWith (dimLenPrefix, StringComparison.Ordinal)) {
                var num = memberChars.Slice (dimLenPrefix.Length);

                if (!int.TryParse (num, 0, null, out var dimIndex))
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                memberType = typeIndex;
                value = PointerMemberAccess (arrExpr, IdentifierName (GetArrayDimensionMember (dimIndex)));
            } else
                throw new CompilationException (ES_BackendErrors.FrontendError);

            return new ExpressionData { Expr = expr, Type = memberType, Value = value, Constant = false, Writable = addressable };
        }

        private ExpressionData GenerateCode_Expression_MemberAccessStatic_Aggregate (
            ReadOnlySpan<char> src, ES_AstMemberAccessExpression expr,
            ES_TypeInfo* type, ArrayPointer<byte> memberId
        ) {
            Debug.Assert (type is not null);

            var membersArr = type->MembersList.MembersList;

            foreach (var memberAddr in membersArr.Span) {
                var memberPtr = memberAddr.Address;

                if (!memberPtr->Name.Equals (memberId))
                    continue;

                switch (memberPtr->MemberType) {
                    case ES_MemberType.Field: {
                        var memberVar = (ES_MemberData_Variable*) memberPtr;
                        bool addressable = true;

                        if (!memberVar->Info.Flags.HasFlag (ES_MemberFlags.Static))
                            throw new CompilationException (ES_BackendErrors.FrontendError);

                        var value = MemberAccessExpression (
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName (MangleTypeName (type)),
                            IdentifierName (memberVar->Info.Name.GetPooledString (Encoding.ASCII))
                        );

                        return new ExpressionData { Expr = expr, Type = memberVar->Type, Value = value, Constant = false, Writable = addressable };
                    }

                    case ES_MemberType.Function:
                        throw new NotImplementedException ("[TODO] Member function access not implemented yet.");

                    default:
                        throw new NotImplementedException ("Member type not implemented yet.");
                }
            }

            throw new CompilationException (ES_BackendErrors.FrontendError);
        }
    }
}
