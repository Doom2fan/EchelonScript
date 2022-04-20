/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Runtime.InteropServices;
using ChronosLib.Pooled;
using EchelonScriptCompiler.CompilerCommon.IR;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
    private static void CompileStruct (ref PassData passData, ESIR_Struct structDef) {
        using var memberNodes = new StructPooledList<SyntaxNode> (CL_ClearMode.Auto);

        var mangledStructName = MangleTypeName (structDef.Type);

        // Create the struct's members.
        foreach (var member in structDef.Members.Elements) {
            switch (member.Kind) {
                case ESIR_NodeKind.Field when member is ESIR_Field fieldDef: {
                    var roslynType = GetRoslynType (fieldDef.Type.Pointer);
                    var variableName = fieldDef.Name.GetCharsSpan ().GetPooledString ();

                    var variablesList = SingletonSeparatedList (VariableDeclarator (Identifier (variableName)));
                    memberNodes.Add (
                        FieldDeclaration (
                            VariableDeclaration (
                            roslynType
                            ).WithVariables (variablesList)
                        ).WithModifiers (TokenList (
                            Token (SyntaxKind.PublicKeyword)
                        )).WithAttributeLists (SingletonList (SingletonAttributeList (
                            Attribute_FieldOffset (fieldDef.Offset)
                        )))
                    );

                    break;
                }

                case ESIR_NodeKind.StaticField:
                    // Ignore.
                    break;

                default:
                    throw new NotImplementedException ("Member type not implemented.");
            }
        }

        // Generate the default value function.
        var defValFunc = CompileStruct_DefaultExpr (structDef);
        memberNodes.Add (defValFunc);

        // Generate the struct declaration.
        passData.Types.Add (
            StructDeclaration (
                mangledStructName
            ).WithMembers (
                ListSpan (memberNodes.Span)
            ).WithModifiers (TokenList (
                Token (SyntaxKind.PublicKeyword),
                Token (SyntaxKind.UnsafeKeyword)
            )).WithAttributeLists (SingletonList (
                SingletonAttributeList (
                    Attribute_StructLayout (
                        nameof (LayoutKind.Explicit),
                        null,
                        LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (structDef.Type->RuntimeSize))
                    )
                )
            ))
        );
    }

    private static MethodDeclarationSyntax CompileStruct_DefaultExpr (ESIR_Struct structDef) {
        var structType = GetRoslynType (structDef.Type);
        var structName = IdentifierName (MangleTypeName (structDef.Type));

        // Generate the field initialization nodes,
        using var initExprNodes = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
        foreach (var member in structDef.Members.Elements) {
            if (member is not ESIR_Field memberField)
                continue;

            var memberVarType = memberField.Type.Pointer;

            var varValue = GetDefaultValue (memberVarType);
            initExprNodes.Add (AssignmentExpression (
                SyntaxKind.SimpleAssignmentExpression,
                IdentifierName (memberField.Name.GetCharsSpan ().GetPooledString ()),
                varValue
            ));
            initExprNodes.Add (Token (SyntaxKind.CommaToken));
        }

        // Generate the return expression.
        var initExpr = InitializerExpression (
            SyntaxKind.ObjectInitializerExpression,
            SeparatedListSpan<ExpressionSyntax> (initExprNodes.ToArray ())
        );
        var retValue = ObjectCreationExpression (structName).WithInitializer (initExpr);

        return (
            MethodDeclaration (
                structType,
                DefaultValueFuncName
            ).WithModifiers (TokenList (
                Token (SyntaxKind.PublicKeyword),
                Token (SyntaxKind.StaticKeyword)
            )).WithAttributeLists (ListParams (
                SingletonAttributeList (Attribute_ExcludeFromStackTrace ()),
                SingletonAttributeList (Attribute_MethodImpl_AggressiveInlining ())
            )).WithExpressionBody (ArrowExpressionClause (
                retValue
            ))
        );
    }
}
