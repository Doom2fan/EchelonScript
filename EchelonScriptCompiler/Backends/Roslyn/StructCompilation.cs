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
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Utilities;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private StructDeclarationSyntax GenerateCode_Struct (
            ref TranslationUnitData transUnit, ref AstUnitData astUnit,
            SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            [DisallowNull] ES_AstStructDefinition structDef, [DisallowNull] ES_StructData* structData
        ) {
            Debug.Assert (envBuilder is not null);

            using var memberTypes = new StructPooledList<SyntaxNode> (CL_ClearMode.Auto);
            using var staticConsBody = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);

            var mangledStructName = MangleStructName (structData);

            // Create the struct's members.
            foreach (var memberAddr in structData->TypeInfo.MembersList.MembersList.Span) {
                // Skip anything that isn't a field.
                if (memberAddr.Address->MemberType != ES_MemberType.Field)
                    continue;

                var memberPtr = (ES_MemberData_Variable*) memberAddr.Address;

                var roslynType = GetRoslynType (memberPtr->Type);
                var variableName = memberPtr->Info.Name.GetPooledString (Encoding.ASCII);

                MemberDeclarationSyntax varDeclaration;
                if (memberPtr->Info.Flags.HasFlag (ES_MemberFlags.Static)) {
                    if (!envBuilder.PointerAstMap.TryGetValue ((IntPtr) memberPtr, out var varDefNode))
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    var varOffset = envBuilder.AllocateStaticVar (memberPtr->Type);

                    var pointerExpr = PrefixUnaryExpression (
                        SyntaxKind.PointerIndirectionExpression,
                        CastExpression (
                            PointerType (roslynType),
                            BinaryExpression (
                                SyntaxKind.AddExpression,
                                GenerateCode_StaticVarsMem (),
                                LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (varOffset))
                            )
                        )
                    );

                    varDeclaration = (
                        PropertyDeclaration (
                            RefType (roslynType),
                            variableName
                        ).WithModifiers (TokenList (
                            Token (SyntaxKind.PublicKeyword),
                            Token (SyntaxKind.StaticKeyword)
                        )).WithAccessorList (AccessorList (SingletonList (
                            AccessorDeclaration (SyntaxKind.GetAccessorDeclaration).WithExpressionBody (
                                ArrowExpressionClause (RefExpression (pointerExpr))
                            ).WithAttributeLists (SingletonList (
                                SingletonAttributeList (Attribute_MethodImpl_AggressiveInlining ())
                            ))
                        )))
                    );

                    ExpressionSyntax initValue;

                    var varDef = (ES_AstMemberVarDefinition) varDefNode;
                    if (varDef.InitializationExpression is null)
                        initValue = GetDefaultValue (memberPtr->Type);
                    else {
                        var initExpr = GenerateCode_Expression (ref transUnit, symbols, src, varDef.InitializationExpression, memberPtr->Type);

                        if (!initExpr.Constant)
                            throw new CompilationException (ES_BackendErrors.FrontendError);

                        GenerateCode_EnsureImplicitCompat (ref initExpr, memberPtr->Type);

                        Debug.Assert (initExpr.Value is not null);
                        initValue = initExpr.Value;
                    }

                    staticConsBody.Add (ExpressionStatement (AssignmentExpression (
                        SyntaxKind.SimpleAssignmentExpression,
                        IdentifierName (variableName),
                        initValue
                    )));
                } else {
                    var variablesList = SingletonSeparatedList (VariableDeclarator (Identifier (variableName)));
                    varDeclaration = (
                        FieldDeclaration (
                            VariableDeclaration (
                            roslynType
                            ).WithVariables (variablesList)
                        ).WithModifiers (TokenList (
                            Token (SyntaxKind.PublicKeyword)
                        )).WithAttributeLists (SingletonList (SingletonAttributeList (
                            Attribute_FieldOffset (memberPtr->Offset)
                        )))
                    );
                }

                memberTypes.Add (varDeclaration);
            }

            // Add the static constructor to the members list.
            memberTypes.Add (
                MethodDeclaration (
                    PredefinedType (Token (SyntaxKind.VoidKeyword)),
                    DefaultStaticConsName
                ).WithModifiers (TokenList (
                    Token (SyntaxKind.InternalKeyword),
                    Token (SyntaxKind.StaticKeyword)
                )).WithBody (
                    BlockSpan (staticConsBody.Span)
                )
            );

            // Create the struct declaration.
            var structDecl = StructDeclaration (mangledStructName)
                .WithMembers (ListSpan (memberTypes.Span))
                .WithModifiers (TokenList (
                    Token (SyntaxKind.PublicKeyword),
                    Token (SyntaxKind.UnsafeKeyword)
                ))
                .WithAttributeLists (SingletonList (SingletonAttributeList (Attribute_StructLayout (
                    nameof (LayoutKind.Explicit),
                    null,
                    LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (structData->TypeInfo.RuntimeSize))
                ))));

            return structDecl;
        }
    }
}
