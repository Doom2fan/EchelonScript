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
using EchelonScriptCommon;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Utilities;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
    private static void CompileStruct (ref PassData passData, ESIR_Struct structDef) {
        using var memberTypes = new StructPooledList<SyntaxNode> (CL_ClearMode.Auto);

        var mangledStructName = MangleTypeName (structDef.Type);

        // Create the struct's members.
        foreach (var member in structDef.Members.Elements) {
            switch (member.Kind) {
                case ESIR_NodeKind.Field when member is ESIR_Field fieldDef: {
                    var roslynType = GetRoslynType (fieldDef.Type.Pointer);
                    var variableName = fieldDef.Name.GetPooledString (ES_Encodings.Identifier);

                    var variablesList = SingletonSeparatedList (VariableDeclarator (Identifier (variableName)));
                    memberTypes.Add (
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

        // Generate the struct declaration.
        passData.Types.Add (
            StructDeclaration (
                mangledStructName
            ).WithMembers (
                ListSpan (memberTypes.Span)
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
}
