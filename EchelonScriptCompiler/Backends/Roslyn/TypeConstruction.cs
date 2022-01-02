/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        private TypeSyntax GetRoslynType (ES_TypeInfo* type) {
            switch (type->TypeTag) {
                case ES_TypeTag.Void: return PredefinedType (Token (SyntaxKind.VoidKeyword));

                case ES_TypeTag.Bool: return PredefinedType (Token (SyntaxKind.BoolKeyword));
                case ES_TypeTag.Int: {
                    var intType = (ES_IntTypeData*) type;
                    return GetIntType (intType->IntSize, intType->Unsigned);
                }

                case ES_TypeTag.Float: {
                    var floatType = (ES_FloatTypeData*) type;
                    return GetFloatType (floatType->FloatSize);
                }

                case ES_TypeTag.Struct: return IdentifierName (MangleTypeName (type));

                case ES_TypeTag.Reference: {
                    var pointerType = (ES_ReferenceData*) type;
                    var pointedType = GetRoslynType (pointerType->PointedType);
                    return PointerType (pointedType);
                }

                case ES_TypeTag.Array:
                    return PointerType (IdentifierName (nameof (ES_ArrayHeader)));

                case ES_TypeTag.Function:
                case ES_TypeTag.Class:
                case ES_TypeTag.Enum:
                case ES_TypeTag.Interface:
                case ES_TypeTag.Const:
                case ES_TypeTag.Immutable:
                    throw new NotImplementedException ("[TODO] Type not implemented yet.");

                default:
                    throw new NotImplementedException ("Type not implemented.");
            }
        }

        private TypeSyntax GetIntType (ES_IntSize size, bool unsigned) {
            switch (size) {
                case ES_IntSize.Int8: return PredefinedType (Token (!unsigned ? SyntaxKind.SByteKeyword : SyntaxKind.ByteKeyword));
                case ES_IntSize.Int16: return PredefinedType (Token (!unsigned ? SyntaxKind.ShortKeyword : SyntaxKind.UShortKeyword));
                case ES_IntSize.Int32: return PredefinedType (Token (!unsigned ? SyntaxKind.IntKeyword : SyntaxKind.UIntKeyword));
                case ES_IntSize.Int64: return PredefinedType (Token (!unsigned ? SyntaxKind.LongKeyword : SyntaxKind.ULongKeyword));

                default:
                    throw new NotImplementedException ("Size not implemented.");
            }
        }

        private TypeSyntax GetFloatType (ES_FloatSize size) {
            switch (size) {
                case ES_FloatSize.Single: return PredefinedType (Token (SyntaxKind.FloatKeyword));
                case ES_FloatSize.Double: return PredefinedType (Token (SyntaxKind.DoubleKeyword));

                default:
                    throw new NotImplementedException ("Size not implemented.");
            }
        }
    }
}
