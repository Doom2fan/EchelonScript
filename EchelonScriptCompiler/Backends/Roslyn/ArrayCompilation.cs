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
using System.Runtime.InteropServices;
using ChronosLib.Pooled;
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.GarbageCollection;
using EchelonScriptCompiler.Utilities;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private string GetArrayDimensionMember (int index) {
            const string dimPrefix = "LengthD";

            Span<char> chars = stackalloc char [dimPrefix.Length + 3];
            var charCount = 0;

            dimPrefix.AsSpan ().CopyTo (chars [charCount..]);
            charCount += dimPrefix.Length;

            if (!index.TryFormat (chars [charCount..], out var idxCharCount))
                Debug.Fail ("Failed to format dimensions index.");
            charCount += idxCharCount;

            return chars [..charCount].GetPooledString ();
        }

        private MemberDeclarationSyntax GenerateCode_Array (ES_ArrayTypeData* arrayType) {
            Debug.Assert (env is not null);
            Debug.Assert (envBuilder is not null);

            using var memberTypes = new StructPooledList<SyntaxNode> (CL_ClearMode.Auto);

            var typeIndex = GetRoslynType (env.GetArrayIndexType ());
            var typeHeader = IdentifierName (nameof (ES_ArrayHeader));

            var attrAggressiveInlining = Attribute_MethodImpl_AggressiveInlining ();

            var basicGetter = AccessorDeclaration (
                SyntaxKind.GetAccessorDeclaration
            ).WithAttributeLists (
                SingletonList (SingletonAttributeList (attrAggressiveInlining))
            );

            // Add the header.
            memberTypes.Add (
                SimpleFieldDeclaration (
                    typeHeader, Identifier ("header")
                ).WithModifiers (TokenList (Token (SyntaxKind.PrivateKeyword)))
            );

            // Add the dimension lengths.
            for (int i = 0; i < arrayType->DimensionsCount; i++) {
                var dimId = Identifier (GetArrayDimensionMember (i));
                memberTypes.Add (
                    SimpleFieldDeclaration (
                        typeIndex, dimId
                    ).WithModifiers (TokenList (Token (SyntaxKind.PublicKeyword)))
                );
            }

            // Add the total length.
            var lengthName = arrayType->DimensionsCount < 2 ? "Length" : "TotalLength";
            memberTypes.Add (
                PropertyDeclaration (typeIndex, lengthName).WithAccessorList (AccessorList (SingletonList (
                    basicGetter.WithExpressionBody (ArrowExpressionClause (
                        SimpleMemberAccess ("header", nameof (ES_ArrayHeader.Length))
                    ))
                ))).WithModifiers (TokenList (Token (SyntaxKind.PublicKeyword)))
            );

            // Add the array rank.
            memberTypes.Add (
                PropertyDeclaration (typeIndex, "Rank").WithAccessorList (AccessorList (SingletonList (
                    basicGetter.WithExpressionBody (ArrowExpressionClause (
                        SimpleMemberAccess ("header", nameof (ES_ArrayHeader.Rank))
                    ))
                ))).WithModifiers (TokenList (Token (SyntaxKind.PublicKeyword)))
            );

            // Add the alloc functions to the members.
            var allocFuncs = GenerateCode_Array_AllocFunc (arrayType);
            memberTypes.Add (allocFuncs.Item1);
            memberTypes.Add (allocFuncs.Item2);

            // Create the declaration.
            var arrayDecl = StructDeclaration (
                MangleArrayType (arrayType)
            ).WithMembers (
                ListSpan (memberTypes.Span)
            ).WithModifiers (TokenList (
                Token (SyntaxKind.PublicKeyword),
                Token (SyntaxKind.UnsafeKeyword)
            )).WithAttributeLists (SingletonList (
                SingletonAttributeList (Attribute (
                    IdentifierName (nameof (StructLayoutAttribute)),
                    SimpleAttributeArgumentList (
                        AttributeArgument (SimpleMemberAccess (nameof (LayoutKind), nameof (LayoutKind.Sequential))),
                        AttributeArgument (NameEquals ("Pack"), null, LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (1)))
                    )
                ))
            ));

            return arrayDecl;
        }

        private (MethodDeclarationSyntax, MethodDeclarationSyntax) GenerateCode_Array_AllocFunc (ES_ArrayTypeData* arrayType) {
            var typeIndex = GetRoslynType (env!.GetArrayIndexType ());
            var typeIntPtr = IdentifierName (nameof (IntPtr));
            var typeArray = GetRoslynType (&arrayType->TypeInfo);

            var elemIsRef = arrayType->ElementType->TypeTag == ES_TypeTag.Reference;
            var elemRoslynType = GetRoslynType (arrayType->ElementType);

            var dimsCount = arrayType->DimensionsCount;

            using var dimSizesExpressions = PooledArray<SyntaxNodeOrToken>.GetArray (dimsCount * 2 - 1);

            /** Generate the parameters list **/
            using var parametersList = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
            parametersList.EnsureCapacity (1 + dimsCount * 2 + 2);

            // [Params] "pinned" bool.
            const string paramNamePinBool = "pinned";
            parametersList.Add (Parameter (Identifier (paramNamePinBool))
                .WithType (PredefinedType (Token (SyntaxKind.BoolKeyword)))
            );

            // Add the rank size params.
            const string paramNameDimSizePrefix = "dimSize";
            using var dimSizeNameArr = PooledArray<char>.GetArray (paramNameDimSizePrefix.Length + 3);
            paramNameDimSizePrefix.AsSpan ().CopyTo (dimSizeNameArr.Span);
            for (int i = 0; i < dimsCount; i++) {
                if (!i.TryFormat (dimSizeNameArr.Span [^3..], out var charsWritten))
                    Debug.Fail ("Too many array dimensions.");

                var dimSizeParamName = dimSizeNameArr.Span [..(paramNameDimSizePrefix.Length + charsWritten)].GetPooledString ();

                parametersList.Add (Token (SyntaxKind.CommaToken));
                parametersList.Add (Parameter (Identifier (dimSizeParamName)).WithType (typeIndex));

                dimSizesExpressions.Span [i * 2] = IdentifierName (dimSizeParamName);
                if (i < dimsCount - 1)
                    dimSizesExpressions.Span [i * 2 + 1] = Token (SyntaxKind.CommaToken);
            }

            // [Params] Elements value.
            const string elemValsName = "elemsVal";
            parametersList.Add (Token (SyntaxKind.CommaToken));
            parametersList.Add (Parameter (Identifier (elemValsName)).WithType (elemRoslynType));

            /** Generate the args list **/
            using var argsList = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
            argsList.EnsureCapacity (3 + dimsCount * 2 + 2);

            // [Args] Type pointer.
            var pointerTypeSyntax = PointerType (IdentifierName (nameof (ES_ArrayTypeData)));
            argsList.Add (Argument (PointerLiteral (arrayType, pointerTypeSyntax)));

            // [Args] Dimensions.
            argsList.Add (Token (SyntaxKind.CommaToken));
            argsList.Add (Argument (StackAllocArrayCreationExpression (
                ArrayType (IdentifierName (nameof (ES_ArrayIndex))).WithRankSpecifiers (
                    SingletonList (ArrayRankSpecifier (SingletonSeparatedList<ExpressionSyntax> (OmittedArraySizeExpression ())))
                ),
                InitializerExpression (
                    SyntaxKind.ArrayInitializerExpression,
                    SeparatedListSpan<ExpressionSyntax> (dimSizesExpressions.Span)
                )
            )));

            // [Args] "pinned" bool.
            argsList.Add (Token (SyntaxKind.CommaToken));
            argsList.Add (Argument (IdentifierName (paramNamePinBool)));

            // [Args] Add the value to assign.
            argsList.Add (Token (SyntaxKind.CommaToken));
            if (elemIsRef)
                argsList.Add (Argument (CastExpression (typeIntPtr, IdentifierName (elemValsName))));
            else
                argsList.Add (Argument (IdentifierName (elemValsName)));

            /** Generate the base function definition **/
            var allocFuncDef = MethodDeclaration (typeArray, ArrayAllocFuncName);
            allocFuncDef = allocFuncDef.WithModifiers (TokenList (
                Token (SyntaxKind.PublicKeyword),
                Token (SyntaxKind.StaticKeyword)
            ));
            allocFuncDef = allocFuncDef.WithAttributeLists (SingletonList (
                SingletonAttributeList (Attribute_MethodImpl_AggressiveInlining ())
            ));

            /*Attribute (
                IdentifierName (nameof (MethodImplAttribute)),
                */

            /** Generate the basic function **/
            var allocArrayFunctionAccessBasic = SimpleMemberAccess (nameof (ES_GarbageCollector), nameof (ES_GarbageCollector.AllocArray));

            var allocFuncDefBasic = allocFuncDef;
            allocFuncDefBasic = allocFuncDefBasic.WithParameterList (ParameterList (SeparatedListSpan<ParameterSyntax> (parametersList.Span [..^2])));
            allocFuncDefBasic = allocFuncDefBasic.WithExpressionBody (ArrowExpressionClause (
                CastExpression (typeArray, InvocationExpression (allocArrayFunctionAccessBasic, ArgumentList (SeparatedListSpan<ArgumentSyntax> (
                    argsList.Span [..^2]
                ))))
            ));

            /** Generate the function with element setting **/
            var allocArrayFunctionAccessElemDefault = SimpleMemberAccess (
                IdentifierName (nameof (ES_GarbageCollector)),
                GenericName (Identifier (nameof (ES_GarbageCollector.AllocArray))).WithTypeArgumentList (
                    TypeArgumentList (SingletonSeparatedList (!elemIsRef ? elemRoslynType : typeIntPtr))
                )
            );

            ExpressionSyntax allocFuncDefElemDefaultCall = CastExpression (typeArray, InvocationExpression (
                allocArrayFunctionAccessElemDefault,
                ArgumentList (SeparatedListSpan<ArgumentSyntax> (argsList.Span))
            ));

            var allocFuncDefElemDefault = allocFuncDef;
            allocFuncDefElemDefault = allocFuncDefElemDefault.WithParameterList (ParameterList (SeparatedListSpan<ParameterSyntax> (parametersList.Span)));
            allocFuncDefElemDefault = allocFuncDefElemDefault.WithExpressionBody (ArrowExpressionClause (allocFuncDefElemDefaultCall));

            return (allocFuncDefBasic, allocFuncDefElemDefault);
        }
    }
}
