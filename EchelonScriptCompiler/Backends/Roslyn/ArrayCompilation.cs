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
using System.Runtime.InteropServices;
using ChronosLib.Pooled;
using CommunityToolkit.HighPerformance;
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.GarbageCollection;
using EchelonScriptCommon.Utilities;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
    private static string GetArrayDimensionMember (int index) {
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

    private static ExpressionSyntax CompileCode_BoundsCheck (
        int dimNum, ExpressionSyntax boundsExpr, ExpressionSyntax valExpr
    ) {
        return (InvocationExpression (
                SimpleMemberAccess (nameof (ES_ArrayIndex), nameof (ES_ArrayIndex.CheckBounds))
            ).WithArgumentList (ArgumentList (SimpleSeparatedList (
                Token (SyntaxKind.CommaToken),

                Argument (LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (dimNum))),
                Argument (boundsExpr),
                Argument (valExpr)
            )))
        );
    }

    private static void CompileCode_Array (ref PassData passData, ES_ArrayTypeData* arrayType) {
        using var memberTypes = new StructPooledList<SyntaxNode> (CL_ClearMode.Auto);

        var typeIndex = GetRoslynType (passData.Env.GetArrayIndexType ());
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
        for (var i = 0; i < arrayType->DimensionsCount; i++) {
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

        // Add the alloc functions.
        var allocFuncs = CompileCode_Array_AllocFunc (ref passData, arrayType);
        memberTypes.Add (allocFuncs.Item1);
        memberTypes.Add (allocFuncs.Item2);

        // Add the index function.
        memberTypes.Add (CompileCode_Array_IndexFunc (ref passData, arrayType));

        // Add the concat function.
        if (arrayType->DimensionsCount == 1)
            memberTypes.Add (CompileCode_Array_ConcatFunc (ref passData, arrayType));

        // Create the declaration.
        var arrayDecl = StructDeclaration (
            MangleArrayType (arrayType)
        ).WithMembers (
            ListSpan (memberTypes.Span)
        ).WithModifiers (TokenList (
            Token (SyntaxKind.PublicKeyword),
            Token (SyntaxKind.UnsafeKeyword)
        )).WithAttributeLists (SingletonList (
            SingletonAttributeList (Attribute_StructLayout (
                nameof (LayoutKind.Sequential),
                LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (1)),
                null
            ))
        ));

        passData.GlobalMembers.Add (arrayDecl);
    }

    private static (MethodDeclarationSyntax, MethodDeclarationSyntax) CompileCode_Array_AllocFunc (
        ref PassData passData,
        ES_ArrayTypeData* arrayType
    ) {
        var typeIndex = GetRoslynType (passData.Env.GetArrayIndexType ());
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
        for (var i = 0; i < dimsCount; i++) {
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
        var allocFuncDef = (
            MethodDeclaration (
                typeArray,
                ArrayAllocFuncName
            ).WithModifiers (TokenList (
                Token (SyntaxKind.PublicKeyword),
                Token (SyntaxKind.StaticKeyword)
            )).WithAttributeLists (SingletonList (
                SingletonAttributeList (Attribute_MethodImpl_AggressiveInlining ())
            ))
        );

        /** Generate the basic function **/
        var allocArrayFunctionAccessBasic = SimpleMemberAccess (nameof (ES_GarbageCollector), nameof (ES_GarbageCollector.AllocArray));

        var allocFuncDefBasic = (allocFuncDef
            .WithParameterList (ParameterList (
                SeparatedListSpan<ParameterSyntax> (parametersList.Span [..^2])
            )).WithExpressionBody (ArrowExpressionClause (
                CastExpression (typeArray, InvocationExpression (allocArrayFunctionAccessBasic, ArgumentList (SeparatedListSpan<ArgumentSyntax> (
                    argsList.Span [..^2]
                ))))
            ))
        );

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

        var allocFuncDefElemDefault = (allocFuncDef
            .WithParameterList (ParameterList (
                SeparatedListSpan<ParameterSyntax> (parametersList.Span)
            )).WithExpressionBody (
                ArrowExpressionClause (allocFuncDefElemDefaultCall)
            )
        );

        return (allocFuncDefBasic, allocFuncDefElemDefault);
    }

    private static ExpressionSyntax CompileCode_GetArrayBaseExpr (ES_ArrayTypeData* arrayType, ExpressionSyntax arrayPtr) {
        var arrayArgumentList = ArgumentList (SingletonSeparatedList (Argument (arrayPtr)));

        var elemPtrRoslynType = PointerType (GetRoslynType (arrayType->ElementType));
        var typeArrayHeader = IdentifierName (nameof (ES_ArrayHeader));

        return CastExpression (elemPtrRoslynType,
            InvocationExpression (
                MemberAccessExpression (SyntaxKind.SimpleMemberAccessExpression,
                    typeArrayHeader,
                    IdentifierName (nameof (ES_ArrayHeader.GetArrayDataPointer))
                )
            ).WithArgumentList (ArgumentList (
                SingletonSeparatedList (Argument (
                    CastExpression (PointerType (typeArrayHeader), arrayPtr)
                ))
            ))
        );
    }

    private static MethodDeclarationSyntax CompileCode_Array_IndexFunc (ref PassData passData, ES_ArrayTypeData* arrayType) {
        const string arrayArgName = "arrayPtr";

        var elemType = arrayType->ElementType;
        var dimCount = arrayType->DimensionsCount;

        var elemRoslynType = GetRoslynType (elemType);
        var elemPtrRoslynType = PointerType (elemRoslynType);

        var typeArray = GetRoslynType (&arrayType->TypeInfo);
        var typeArrayHeader = IdentifierName (nameof (ES_ArrayHeader));
        var typeIndex = GetRoslynType (passData.Env.GetArrayIndexType ());

        using var paramsList = new StructPooledList<ParameterSyntax> (CL_ClearMode.Auto);
        using var funcBody = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);

        // Generate the null check.
        funcBody.Add (ExpressionStatement (CompileCode_NullCheck (IdentifierName (arrayArgName))));

        // Calculate the rank sizes.
        using var rankSizeExprs = PooledArray<ExpressionSyntax>.GetArray (dimCount);
        var rankSizeExprsSpan = rankSizeExprs.Span;

        for (var i = 0; i < dimCount; i++) {
            rankSizeExprsSpan [i] = MemberAccessExpression (
                SyntaxKind.PointerMemberAccessExpression,
                IdentifierName (arrayArgName),
                IdentifierName (GetArrayDimensionMember (i))
            );
        }

        // Add the array parameter.
        paramsList.Add (Parameter (Identifier (arrayArgName)).WithType (typeArray));

        // Generate the parameters, dimension expressions and bounds checks.
        const string paramNameDimValuePrefix = "dimVal";
        using var dimValueNameArr = PooledArray<char>.GetArray (paramNameDimValuePrefix.Length + 3);
        using var rankExprs = PooledArray<ExpressionSyntax>.GetArray (dimCount);
        paramNameDimValuePrefix.AsSpan ().CopyTo (dimValueNameArr.Span);
        for (var i = 0; i < dimCount; i++) {
            if (!i.TryFormat (dimValueNameArr.Span [^3..], out var charsWritten))
                Debug.Fail ("Too many array dimensions.");

            var dimValueParamName = dimValueNameArr.Span [..(paramNameDimValuePrefix.Length + charsWritten)].GetPooledString ();

            paramsList.Add (Parameter (Identifier (dimValueParamName)).WithType (typeIndex));

            var rankExpr = IdentifierName (dimValueParamName);
            rankExprs.Span [i] = rankExpr;

            funcBody.Add (ExpressionStatement (CompileCode_BoundsCheck (i, rankSizeExprsSpan [i], rankExpr)));
        }

        // Get the base address of the array's data.
        var baseAddrExpr = CompileCode_GetArrayBaseExpr (arrayType, IdentifierName (arrayArgName));

        // Calculate the flattened index.
        var flattenedIndex = rankExprs.Span [0];
        for (var i = 1; i < dimCount; i++) {
            var rankIndex = rankExprs.Span [i];

            for (var j = 0; j < i; j++)
                rankIndex = BinaryExpression (SyntaxKind.MultiplyExpression, rankIndex, rankSizeExprsSpan [j]);

            flattenedIndex = BinaryExpression (SyntaxKind.AddExpression, flattenedIndex, rankIndex);
        }

        funcBody.Add (ReturnStatement (
            RefExpression (
                ElementAccessExpression (
                    baseAddrExpr
                ).WithArgumentList (BracketedArgumentList (
                    SingletonSeparatedList (Argument (flattenedIndex))
                ))
            )
        ));

        return (
            MethodDeclaration (
                RefType (elemRoslynType),
                ArrayIndexFuncName
            ).WithModifiers (TokenList (
                Token (SyntaxKind.PublicKeyword),
                Token (SyntaxKind.StaticKeyword)
            )).WithAttributeLists (ListParams (
                SingletonAttributeList (Attribute_ExcludeFromStackTrace ()),
                SingletonAttributeList (Attribute_MethodImpl_AggressiveInlining ())
            )).WithParameterList (ParameterList (
                SimpleSeparatedList (
                    paramsList.Span,
                    Token (SyntaxKind.CommaToken)
                )
            )).WithBody (
                Block (ListSpan (funcBody.Span))
            )
        );
    }
    private static MethodDeclarationSyntax CompileCode_Array_ConcatFunc (ref PassData passData, ES_ArrayTypeData* arrayType) {
        static ExpressionSyntax ArrayLength (ExpressionSyntax arr) {
            return MemberAccessExpression (
                SyntaxKind.PointerMemberAccessExpression,
                arr,
                IdentifierName ("Length")
            );
        }

        static ExpressionSyntax CreateSpan (ExpressionSyntax arr, ES_ArrayTypeData* arrType) {
            var baseAddrExpr = CompileCode_GetArrayBaseExpr (arrType, arr);
            return Roslyn_CreateSpan (GetRoslynType (arrType->ElementType), baseAddrExpr, ArrayLength (arr));
        }

        static ExpressionSyntax CopySpan (ExpressionSyntax src, ExpressionSyntax dst, ExpressionSyntax? start) {
            if (start is not null) {
                dst = ElementAccessExpression (
                    dst,
                    BracketedArgumentList (SingletonSeparatedList (
                        Argument (RangeExpression (
                            start, null
                        ))
                    ))
                );
            }

            return InvocationExpression (
                MemberAccessExpression (
                        SyntaxKind.SimpleMemberAccessExpression,
                        src,
                        IdentifierName ("CopyTo")
                ),
                ArgumentList (
                    SingletonSeparatedList (Argument (dst))
                )
            );
        }

        const string lhsArrName = "lhsArr";
        const string rhsArrName = "rhsArr";

        var roslynArrType = GetRoslynType (&arrayType->TypeInfo);
        var roslynElemType = GetRoslynType (arrayType->ElementType);

        using var funcBody = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);

        var lhsArr = IdentifierName (lhsArrName);
        var rhsArr = IdentifierName (rhsArrName);

        funcBody.Add (ExpressionStatement (CompileCode_NullCheck (lhsArr)));
        funcBody.Add (ExpressionStatement (CompileCode_NullCheck (rhsArr)));

        var lhsLen = ArrayLength (lhsArr);
        var rhsLen = ArrayLength (rhsArr);

        var sizeExpr = new ExpressionData {
            Value = BinaryExpression (
                SyntaxKind.AddExpression,
                lhsLen,
                rhsLen
            )
        };
        var dimsSpan = MemoryMarshal.CreateSpan (ref sizeExpr, 1);

        const string newArrName = "newArr";
        var newArr = IdentifierName (newArrName);

        funcBody.Add (LocalDeclarationStatement (
            VariableDeclaration (
                roslynArrType,
                SimpleSeparatedList (
                    Token (SyntaxKind.CommaToken),
                    VariableDeclarator (newArrName)
                )
            )
        ));
        funcBody.Add (ExpressionStatement (
            AssignmentExpression (
                SyntaxKind.SimpleAssignmentExpression,
                newArr,
                CompileCode_NewArray (arrayType, dimsSpan, null)
            )
        ));


        var lhsArrSpan = CreateSpan (lhsArr, arrayType);
        var rhsArrSpan = CreateSpan (rhsArr, arrayType);
        var newArrSpan = CreateSpan (newArr, arrayType);

        funcBody.Add (ExpressionStatement (CopySpan (lhsArrSpan, newArrSpan, null)));
        funcBody.Add (ExpressionStatement (CopySpan (rhsArrSpan, newArrSpan, lhsLen)));

        funcBody.Add (ReturnStatement (newArr));

        return MethodDeclaration (
            roslynArrType,
            ArrayConcatFuncName
        ).WithModifiers (TokenList (
            Token (SyntaxKind.PublicKeyword),
            Token (SyntaxKind.StaticKeyword)
        )).WithAttributeLists (ListParams (
            SingletonAttributeList (Attribute_ExcludeFromStackTrace ()),
            SingletonAttributeList (Attribute_MethodImpl_AggressiveInlining ())
        )).WithParameterList (ParameterList (SimpleSeparatedList (
            Token (SyntaxKind.CommaToken),
            Parameter (Identifier (lhsArrName)).WithType (roslynArrType),
            Parameter (Identifier (rhsArrName)).WithType (roslynArrType)
        ))).WithBody (
            Block (ListSpan (funcBody.Span))
        );
    }
}
