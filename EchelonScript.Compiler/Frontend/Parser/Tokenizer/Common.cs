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
using System.Text;

namespace EchelonScript.Compiler.Frontend.Parser.Tokenizer;

public ref partial struct ES_Tokenizer {
    #region Character reading

    private char? PeekChar () => curPos < text.Length ? text [curPos] : null;

    private char? PeekChar (int offset) => curPos + offset < text.Length ? text [curPos + offset] : null;

    private ReadOnlySpan<char> PeekChars (int count)
        => text.Slice (curPos, Math.Min (count, text.Length - curPos));

    private char? ReadChar () {
        if (curPos >= text.Length)
            return null;

        return text [curPos++];
    }

    private ReadOnlySpan<char> ReadChars (int count) {
        count = Math.Min (count, text.Length - curPos);

        var ret = text.Slice (curPos, count);
        curPos += count;

        return ret;
    }

    #endregion

    #region Character reading utilities

    private char? ReadUntil (Predicate<char> func, bool readReq) {
        char? c;

        while ((c = PeekChar ()) != null) {
            if (func (c.Value)) {
                if (readReq)
                    ReadChar ();
                return c;
            }

            ReadChar ();
        }

        return null;
    }

    private int ReadStringWhile (Predicate<char> func) {
        var length = 0;

        char? c;
        while ((c = PeekChar ()) != null && func (c.Value)) {
            ReadChar ();
            length++;
        }

        return length;
    }

    #endregion

    #region Common pattern reading

    private bool TryReadNumber (Func<char?, bool> digitFunction) {
        var sepAtStart = PeekChar () == NumberSeparator;
        if (!digitFunction (PeekChar ()) && !sepAtStart)
            return false;

        while (true) {
            var c = PeekChar ();

            if (!digitFunction (c) && c != NumberSeparator)
                break;

            ReadChar ();

            if (c == NumberSeparator) {
                c = PeekChar ();
                if (!digitFunction (c) && c != NumberSeparator)
                    return false;
            }
        }

        return !sepAtStart;
    }

    private bool TryReadIntSuffix () {
        if (!IsIntegerSuffix (PeekChar ()))
            return false;

        ReadChar ();
        if (PeekChar () == '8')
            ReadChar ();
        else {
            var peekedChars = PeekChars (2);
            if (
                peekedChars.Equals ("16", StringComparison.InvariantCulture) ||
                peekedChars.Equals ("32", StringComparison.InvariantCulture) ||
                peekedChars.Equals ("64", StringComparison.InvariantCulture)
            ) {
                ReadChars (2);
            }
        }

        return true;
    }

    private bool? TryReadFloatExponent () {
        if (!IsExponentStart (PeekChar ()))
            return null;

        ReadChar ();
        if (PeekChar () is '+' or '-')
            ReadChar ();

        return TryReadNumber (IsIntegerDigit);
    }

    private bool TryReadFloatSuffix () {
        var c = PeekChar ();
        if (!IsFloatSuffix (c))
            return false;

        ReadChar ();
        return true;
    }

    private bool? TryReadFloatFractional () {
        if (PeekChar () != '.' || (!IsIntegerDigit (PeekChar (1)) && PeekChar (1) != NumberSeparator))
            return null;

        ReadChar ();
        return TryReadNumber (IsIntegerDigit);
    }

    #endregion

    public static bool TryDecodeStringEscapeCode (ReadOnlySpan<char> input, out Rune unescapedChar, out int length, bool charLit = false) {
        Debug.Assert (input.Length > 0);

        int hexCharLen;
        switch (input [0]) {
            case '\'':
                if (charLit)
                    goto case '\\';
                goto default;
            case '"':
                if (!charLit)
                    goto case '\\';
                goto default;
            case '\\':
                unescapedChar = new (input [0]);
                length = 1;
                return true;

            case '0': unescapedChar = new ('\0'    ); length = 1; return true;
            case 'a': unescapedChar = new ('\u0007'); length = 1; return true;
            case 'b': unescapedChar = new ('\u0008'); length = 1; return true;
            case 'f': unescapedChar = new ('\u000C'); length = 1; return true;
            case 'n': unescapedChar = new ('\u000A'); length = 1; return true;
            case 'r': unescapedChar = new ('\u000D'); length = 1; return true;
            case 't': unescapedChar = new ('\u0009'); length = 1; return true;
            case 'v': unescapedChar = new ('\u000B'); length = 1; return true;

            case 'x':
                hexCharLen = 2;
                goto ParseHexChar;

            case 'u':
                hexCharLen = 4;
                goto ParseHexChar;

            case 'U':
                hexCharLen = 8;
                goto ParseHexChar;

            ParseHexChar:
                if (input.Length - 1 < hexCharLen) {
                    unescapedChar = default;
                    length = 1;
                    return false;
                }

                var hexChars = input.Slice (1, hexCharLen);
                foreach (var c in hexChars) {
                    if (!IsHexDigit (c)) {
                        unescapedChar = default;
                        length = 1;
                        return false;
                    }
                }

                var charValue = int.Parse (hexChars, System.Globalization.NumberStyles.AllowHexSpecifier, null);
                length = hexCharLen + 1;

                if (!Rune.IsValid (charValue)) {
                    unescapedChar = default;
                    return false;
                }

                unescapedChar = new (charValue);
                return true;

            default:
                unescapedChar = default;
                length = 1;
                return false;
        }
    }
}
