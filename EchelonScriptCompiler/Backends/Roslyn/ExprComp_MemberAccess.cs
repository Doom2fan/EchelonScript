/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using ChronosLib.Pooled;
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon.IR;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
    private static ExpressionSyntax CompileCode_NullCheck (ExpressionSyntax ptrExpr) {
        return InvocationExpression (
            SimpleMemberAccess (nameof (ES_ObjectAddress), nameof (ES_ObjectAddress.NullCheck))
        ).WithArgumentList (ArgumentList (
            SingletonSeparatedList (Argument (ptrExpr))
        ));
    }

    private static ExpressionSyntax CompileCode_NullCheck (ExpressionSyntax ptrExpr, ES_TypeInfo* pointedType) {
        var isRef = pointedType->IsReferenceType ();
        if (isRef) {
            ptrExpr = CastExpression (
                PointerType (PredefinedType (Token (SyntaxKind.ByteKeyword))),
                ptrExpr
            );
        }

        var value = CompileCode_NullCheck (ptrExpr);

        if (isRef) {
            value = CastExpression (
                PointerType (GetRoslynType (pointedType)),
                ptrExpr
            );
        }

        return value;
    }

    private static ExpressionData CompileExpression_MemberAccess (
        ref PassData passData,
        ref FunctionData funcData,
        ESIR_MemberAccessExpression expr
    ) {
        var parentExpr = CompileExpression (ref passData, ref funcData, expr.ExprParent);
        StripFirstConst (ref parentExpr);

        Debug.Assert (parentExpr.Type is not null);

        return parentExpr.Type->TypeTag switch {
            ES_TypeTag.UNKNOWN => throw new CompilationException (ES_BackendErrors.FrontendError),

            ES_TypeTag.Struct => CompileExpression_MemberAccess_Struct (ref passData, ref funcData, expr, parentExpr),
            ES_TypeTag.Array => CompileExpression_MemberAccess_Array (ref passData, ref funcData, expr, parentExpr),
            ES_TypeTag.Reference => throw new CompilationException ("Reference types must be dereferenced manually before access."),

            _ => throw new NotImplementedException ("Type not implemented."),
        };
    }

    private static ExpressionData CompileExpression_MemberAccess_Struct (
        ref PassData passData,
        ref FunctionData funcData,
        ESIR_MemberAccessExpression expr,
        ExpressionData parentExpr
    ) {
        Debug.Assert (parentExpr.Type->TypeTag == ES_TypeTag.Struct);

        if (!passData.Structs.TryGetValue (parentExpr.Type, out var structDef))
            throw new CompilationException ("Struct doesn't exist.");

        var structExpr = parentExpr.Value!;
        foreach (var member in structDef.Members.Elements) {
            if (member.Kind != ESIR_NodeKind.Field)
                continue;

            var field = (ESIR_Field) member;

            if (!field.Name.Equals (expr.Name))
                continue;

            var value = MemberAccessExpression (
                SyntaxKind.SimpleMemberAccessExpression,
                structExpr,
                IdentifierName (expr.Name.GetCharsSpan ().GetPooledString ())
            );
            return new ExpressionData { Type = field.Type.Pointer, Value = value };
        }

        throw new CompilationException ("Struct field does not exist.");
    }

    private static ExpressionData CompileExpression_MemberAccess_Array (
        ref PassData passData,
        ref FunctionData funcData,
        ESIR_MemberAccessExpression expr,
        ExpressionData parentExpr
    ) {
        Debug.Assert (parentExpr.Type->TypeTag == ES_TypeTag.Array);

        var typeArr = (ES_ArrayTypeData*) parentExpr.Type;
        var typeIndex = passData.Env.GetArrayIndexType ();

        var memberChars = expr.Name.GetCharsSpan ();
        var arrExpr = CompileCode_NullCheck (parentExpr.Value!);

        ES_TypeInfo* memberType;
        ExpressionSyntax value;

        var lengthName = typeArr->DimensionsCount < 2 ? "Length" : "TotalLength";
        var dimLenPrefix = "LengthD";
        if (memberChars.Equals (lengthName, StringComparison.Ordinal)) {
            memberType = typeIndex;
            value = PointerMemberAccess (arrExpr, IdentifierName (lengthName));
        } else if (memberChars.Equals ("Rank", StringComparison.Ordinal)) {
            memberType = passData.Env.GetIntType (ES_IntSize.Int8, true);
            value = PointerMemberAccess (arrExpr, IdentifierName ("Rank"));
        } else if (memberChars.StartsWith (dimLenPrefix, StringComparison.Ordinal)) {
            var num = memberChars [dimLenPrefix.Length..];

            if (!int.TryParse (num, 0, null, out var dimIndex))
                throw new CompilationException ("Invalid array dimension length.");

            memberType = typeIndex;
            value = PointerMemberAccess (arrExpr, IdentifierName (GetArrayDimensionMember (dimIndex)));
        } else
            throw new CompilationException ("Unknown array member.");

        return new ExpressionData { Type = memberType, Value = value, };
    }
}
