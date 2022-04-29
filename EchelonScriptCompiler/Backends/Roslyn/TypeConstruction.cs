/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScriptCommon.Data.Types;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
    private static TypeSyntax GetRoslynType (ES_TypeInfo* type) {
        switch (type->TypeTag) {
            case ES_TypeTag.Void: return PredefinedType (Token (SyntaxKind.VoidKeyword));

            case ES_TypeTag.Bool: return PredefinedType (Token (SyntaxKind.BoolKeyword));
            case ES_TypeTag.Int: {
                var intType = (ES_IntData*) type;
                return GetIntType (intType->IntSize, intType->Unsigned);
            }

            case ES_TypeTag.Float: {
                var floatType = (ES_FloatData*) type;
                return GetFloatType (floatType->FloatSize);
            }

            case ES_TypeTag.Struct: return IdentifierName (MangleTypeName (type));

            case ES_TypeTag.Reference: {
                var pointerType = (ES_ReferenceData*) type;
                var pointedType = GetRoslynType (pointerType->PointedType);
                return PointerType (pointedType);
            }

            case ES_TypeTag.Array: return PointerType (IdentifierName (MangleTypeName (type)));

            case ES_TypeTag.FuncPrototype:
            case ES_TypeTag.Class:
            case ES_TypeTag.Enum:
            case ES_TypeTag.Interface:

            case ES_TypeTag.Const:
            case ES_TypeTag.Immutable: {
                var constData = (ES_ConstData*) type;
                return GetRoslynType (constData->InnerType);
            }

            default:
                throw new NotImplementedException ("Type not implemented.");
        }
    }

    private static TypeSyntax GetIntType (ES_IntSize size, bool unsigned) {
        return size switch {
            ES_IntSize.Int8 => PredefinedType (Token (!unsigned ? SyntaxKind.SByteKeyword : SyntaxKind.ByteKeyword)),
            ES_IntSize.Int16 => PredefinedType (Token (!unsigned ? SyntaxKind.ShortKeyword : SyntaxKind.UShortKeyword)),
            ES_IntSize.Int32 => PredefinedType (Token (!unsigned ? SyntaxKind.IntKeyword : SyntaxKind.UIntKeyword)),
            ES_IntSize.Int64 => PredefinedType (Token (!unsigned ? SyntaxKind.LongKeyword : SyntaxKind.ULongKeyword)),

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }

    private static TypeSyntax GetFloatType (ES_FloatSize size) {
        return size switch {
            ES_FloatSize.Single => PredefinedType (Token (SyntaxKind.FloatKeyword)),
            ES_FloatSize.Double => PredefinedType (Token (SyntaxKind.DoubleKeyword)),

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }
}
