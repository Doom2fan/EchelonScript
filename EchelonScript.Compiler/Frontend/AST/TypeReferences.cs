/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using ChronosLib.Pooled;

namespace EchelonScript.Compiler.Frontend.AST;

#if false
public abstract class ES_AstTypeDeclaration : ES_AstNode {
    public const string NullInnerName = "INVALID";

    public ES_AstTypeDeclaration (int nothing) : base (1) { }

    public abstract int GetStringLength ();

    public abstract void ToString (Span<char> chars);

    public override string ToString () {
        using var chars = PooledArray<char>.GetArray (GetStringLength ());
        ToString (chars);

        return new string (chars.Span);
    }
}

public sealed class ES_AstTypeDeclaration_TypeName : ES_AstTypeDeclaration {
    public override ES_AstNodeBounds NodeBounds => TypeName.NodeBounds;

    public ES_AstNamespaceIdentifier? Namespace;
    public ES_AstDottableIdentifier TypeName;

    public ES_AstTypeDeclaration_TypeName (ES_AstDottableIdentifier name) : base (1) {
        Namespace = null;
        TypeName = name;
    }

    public ES_AstTypeDeclaration_TypeName (ES_AstNamespaceIdentifier nm, ES_AstDottableIdentifier name) : base (1) {
        Namespace = nm;
        TypeName = name;
    }

    public override int GetStringLength () {
        return Namespace == null
            ? TypeName.GetStringLength ()
            : Namespace.GetStringLength () + TypeName.GetStringLength () + 2;
    }

    public override void ToString (Span<char> chars) {
        if (chars.Length < GetStringLength ())
            throw new ArgumentException ("The chars span is too small.", nameof (chars));

        if (Namespace == null)
            TypeName.ToString (chars);
        else {
            var len = 0;

            Namespace.ToString (chars [len..]);
            len += Namespace.GetStringLength ();

            chars [len++] = ':';
            chars [len++] = ':';

            TypeName.ToString (chars [len..]);
        }
    }
}

public sealed class ES_AstTypeDeclaration_Basic : ES_AstTypeDeclaration {
    private const string constStartStr = "const (";
    private const string immutableStartStr = "immutable (";

    public enum DeclType {
        Nullable,
        Reference,
        Const,
        Immutable,
    }

    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public DeclType Type;
    public ES_AstTypeDeclaration? Inner;

    public ES_AstTypeDeclaration_Basic (DeclType type, ES_AstTypeDeclaration? inner, ES_AstNodeBounds bounds) : base (1) {
        Type = type;
        Inner = inner;

        this.bounds = bounds;
    }

    public override int GetStringLength () {
        var innerLen = (Inner?.GetStringLength () ?? NullInnerName.Length);
        return Type switch {
            DeclType.Const => constStartStr.Length + innerLen + 1,
            DeclType.Immutable => immutableStartStr.Length + innerLen + 1,
            DeclType.Reference => innerLen + 1,
            DeclType.Nullable => innerLen + 1,

            _ => throw new NotImplementedException ("Declaration type not implemented."),
        };
    }

    public override void ToString (Span<char> chars) {
        if (chars.Length < GetStringLength ())
            throw new ArgumentException ("The chars array is too small.", nameof (chars));

        var innerLen = Inner?.GetStringLength () ?? NullInnerName.Length;

        switch (Type) {
            case DeclType.Const: {
                var len = 0;
                constStartStr.AsSpan ().CopyTo (chars);
                len += constStartStr.Length;

                if (Inner != null)
                    Inner!.ToString (chars [len..]);
                else
                    NullInnerName.AsSpan ().CopyTo (chars [len..]);
                len += innerLen;

                chars [len] = ')';

                break;
            }
            case DeclType.Immutable: {
                var len = 0;
                immutableStartStr.AsSpan ().CopyTo (chars);
                len += immutableStartStr.Length;

                if (Inner != null)
                    Inner!.ToString (chars [len..]);
                else
                    NullInnerName.AsSpan ().CopyTo (chars [len..]);
                len += innerLen;

                chars [len] = ')';

                break;
            }

            case DeclType.Reference: {
                Inner?.ToString (chars);
                chars [^1] = '&';

                break;
            }
            case DeclType.Nullable: {
                Inner?.ToString (chars);
                chars [^1] = '?';

                break;
            }

            default:
                throw new NotImplementedException ("Declaration type not implemented.");
        }
    }
}

public sealed class ES_AstTypeDeclaration_Array : ES_AstTypeDeclaration {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstTypeDeclaration ElementType;
    public int Dimensions;

    public ES_AstTypeDeclaration_Array (
        ES_AstTypeDeclaration elementType, int dims,
        int endPos
    ) : base (1) {
        ElementType = elementType;
        Dimensions = dims;

        bounds = new (elementType.NodeBounds.StartPos, endPos);
    }

    public override int GetStringLength () {
        var len = ElementType?.GetStringLength () ?? NullInnerName.Length;
        len++; // Space
        len++; // Opening bracket

        if (Dimensions > 0)
            len += Dimensions - 1;

        len++; // Closing bracket

        return len;
    }

    public override void ToString (Span<char> chars) {
        if (chars.Length < GetStringLength ())
            throw new ArgumentException ("The chars array is too small.", nameof (chars));

        var len = 0;

        if (ElementType != null)
            ElementType.ToString (chars);
        else
            NullInnerName.AsSpan ().CopyTo (chars);
        len += ElementType?.GetStringLength () ?? NullInnerName.Length;

        chars [len++] = ' ';
        chars [len++] = '[';

        for (var i = 1; i < Dimensions; i++)
            chars [len++] = ',';

        chars [len] = ']';
    }
}

internal class ES_AstTypeDeclaration_TypeReference : ES_AstTypeDeclaration {
    //TODO: FIXME
    public override ES_AstNodeBounds NodeBounds => throw new NotImplementedException ();

    public override int GetStringLength () => throw new NotImplementedException ();
    public override void ToString (Span<char> chars) => throw new NotImplementedException ();

    private ES_AstTypeDeclaration_TypeReference () : base (0) { }
}
#endif
