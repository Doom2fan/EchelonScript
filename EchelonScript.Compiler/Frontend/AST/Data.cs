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
using EchelonScript.Common.Data;
using EchelonScript.Compiler.CompilerCommon;

namespace EchelonScript.Compiler.Frontend.AST;

public sealed class ES_AstDottableIdentifier : ES_AstNode {
    public override ES_AstNodeBounds NodeBounds {
        get {
            if (Parts.Length == 0)
                return new ES_AstNodeBounds ();

            return new ES_AstNodeBounds {
                StartPos = Parts [0].TextStartPos,
                EndPos = (Parts [^1].TextEndPos),
            };
        }
    }

    public readonly EchelonScriptToken [] Parts;

    public ES_AstDottableIdentifier (EchelonScriptToken [] parts) : base (1) => Parts = parts;

    #region ================== Instance methods

    public int GetStringLength () {
        var strLen = 0;

        foreach (var part in Parts)
            strLen += part.Text.Length;
        strLen += Math.Max (Parts.Length - 1, 0);

        return strLen;
    }

    public PooledArray<char> ToPooledChars () {
        var str = PooledArray<char>.GetArray (GetStringLength ());

        ToString (str);

        return str;
    }

    public string ToPooledString () {
        using var str = PooledArray<char>.GetArray (GetStringLength ());

        ToString (str);

        return str.Span.GetPooledString ();
    }

    public ES_Identifier ToIdentifier (ES_IdentifierPool idPool) {
        using var chars = ToPooledChars ();
        return idPool.GetIdentifier (chars);
    }

    public override string ToString () {
        Span<char> idText = stackalloc char [GetStringLength ()];

        ToString (idText);

        return new string (idText);
    }

    public void ToString (Span<char> idText) {
        if (idText.Length < GetStringLength ())
            throw new ArgumentException ("The specified span is not long enough to contain the identifier", nameof (idText));

        var strLen = 0;
        for (var i = 0; i < Parts.Length; i++) {
            if (i > 0) { // Account for the period
                idText [strLen] = '.';
                strLen++;
            }

            var partSpan = Parts [i].Text.Span;
            partSpan.CopyTo (idText.Slice (strLen, partSpan.Length));
            strLen += partSpan.Length;
        }
    }

    #endregion
}

public sealed class ES_AstNamespaceIdentifier : ES_AstNode {
    public override ES_AstNodeBounds NodeBounds {
        get {
            if (Parts.Length == 0)
                return new ES_AstNodeBounds ();

            return new ES_AstNodeBounds {
                StartPos = Parts [0].TextStartPos,
                EndPos = (Parts [^1].TextEndPos),
            };
        }
    }

    public readonly EchelonScriptToken [] Parts;

    public ES_AstNamespaceIdentifier (EchelonScriptToken [] parts) : base (1) => Parts = parts;

    #region ================== Instance methods

    public int GetStringLength () {
        var strLen = 0;

        foreach (var part in Parts)
            strLen += part.Text.Length;
        strLen += Math.Max ((Parts.Length - 1) * 2, 0);

        return strLen;
    }

    public PooledArray<char> ToPooledChars () {
        var str = PooledArray<char>.GetArray (GetStringLength ());

        ToString (str);

        return str;
    }

    public string ToPooledString () {
        using var str = PooledArray<char>.GetArray (GetStringLength ());

        ToString (str);

        return str.Span.GetPooledString ();
    }

    public ES_Identifier ToIdentifier (ES_IdentifierPool idPool) {
        using var chars = ToPooledChars ();
        return idPool.GetIdentifier (chars);
    }

    public override string ToString () {
        Span<char> idText = stackalloc char [GetStringLength ()];

        ToString (idText);

        return new string (idText);
    }

    public void ToString (Span<char> idText) {
        if (idText.Length < GetStringLength ())
            throw new ArgumentException ("The specified span is not long enough to contain the identifier", nameof (idText));

        var strLen = 0;
        for (var i = 0; i < Parts.Length; i++) {
            if (i > 0) { // Account for the period
                idText [strLen++] = ':';
                idText [strLen++] = ':';
            }

            var partSpan = Parts [i].Text.Span;
            partSpan.CopyTo (idText.Slice (strLen, partSpan.Length));
            strLen += partSpan.Length;
        }
    }

    #endregion
}
