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
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Utilities;
using Microsoft.CodeAnalysis.CSharp;
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
            var memberId = idPool.GetIdentifier (expr.Member.Value.Text.Span);

            if (parentExpr.Type is not null) {
                var typeTag = parentExpr.Type->TypeTag;
                if (typeTag == ES_TypeTag.Reference)
                    typeTag = ((ES_ReferenceData*) parentExpr.Type)->PointedType->TypeTag;

                switch (typeTag) {
                    case ES_TypeTag.Struct:
                        return GenerateCode_Expression_MemberAccess_Struct (src, expr, ref parentExpr, memberId);

                    case ES_TypeTag.UNKNOWN:
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    default:
                        throw new NotImplementedException ("Type not implemented yet.");
                }
            } else if (parentExpr.TypeInfo is not null) {
                var type = parentExpr.TypeInfo;

                switch (type->TypeTag) {
                    case ES_TypeTag.Struct:
                        return GenerateCode_Expression_MemberAccessStatic_Aggregate (src, expr, type, memberId);

                    case ES_TypeTag.UNKNOWN:
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    default:
                        throw new NotImplementedException ("Type not implemented yet.");
                }
            } else if (parentExpr.Function is not null)
                throw new NotImplementedException ("Not supported. (yet?)");
            else
                throw new CompilationException ("<<Unknown expression type in GenerateCode_Expression_MemberAccess>>");
        }

        private ExpressionData GenerateCode_Expression_MemberAccess_Struct (
            ReadOnlySpan<char> src, ES_AstMemberAccessExpression expr,
            ref ExpressionData parentExpr, ArrayPointer<byte> memberId
        ) {
            Debug.Assert (parentExpr.Type is not null);
            Debug.Assert (parentExpr.Value is not null);

            var type = parentExpr.Type;
            var isRef = false;

            if (type->TypeTag == ES_TypeTag.Reference) {
                type = ((ES_ReferenceData*) parentExpr.Type)->PointedType;
                isRef = true;
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
                            parentExpr.Value, IdentifierName (memberId.GetPooledString (Encoding.ASCII))
                        );

                        return new ExpressionData { Expr = expr, Type = memberVar->Type, Value = value, Constant = false, Addressable = addressable };
                    }

                    case ES_MemberType.Function:
                        throw new NotImplementedException ("[TODO] Member function access not implemented yet.");

                    default:
                        throw new NotImplementedException ("Member type not implemented yet.");
                }
            }

            throw new CompilationException (ES_BackendErrors.FrontendError);
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

                        using var mangledTypeName = MangleTypeName (type);
                        var value = MemberAccessExpression (
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName (mangledTypeName.GetPooledString ()),
                            IdentifierName (memberVar->Info.Name.GetPooledString (Encoding.ASCII))
                        );

                        return new ExpressionData { Expr = expr, Type = memberVar->Type, Value = value, Constant = false, Addressable = addressable };
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
