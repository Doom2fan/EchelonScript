/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Text;
using ChronosLib.Pooled;
using EchelonScript.Compiler.Data;

namespace EchelonScript.Compiler.Frontend.Parser.Tokenizer;

public partial class EchelonScriptTokenizer {
    private int CalcColumn (int columnStart, int textPos) => CalcColumn (data.Span, columnStart, textPos);

    #region Character reading

    private char? PeekChar () => curPos < data.Length ? data.Span [curPos] : null;

    private char? PeekChar (int offset) => curPos + offset < data.Length ? data.Span [curPos + offset] : null;

    private ReadOnlySpan<char> PeekChars (int count)
        => data.Span.Slice (curPos, Math.Min (count, data.Length - curPos));

    private char? ReadChar () {
        if (curPos >= data.Length)
            return null;

        var ret = data.Span [curPos++];

        if (ret == '\n') {
            curLine++;
            curLineStartPos = curPos;
        }

        return ret;
    }

    private ReadOnlySpan<char> ReadChars (int count) {
        count = Math.Min (count, data.Length - curPos);

        var ret = data.Span.Slice (curPos, count);
        curPos += count;

        foreach (var c in ret) {
            if (c == '\n') {
                curLine++;
                curLineStartPos = curPos;
            }
        }

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

    private bool TryReadIntSuffix () {
        var peekedChars = PeekChars (3);

        if (peekedChars.Length < 1)
            return false;

        var c = peekedChars [0];
        if (c == 's' || c == 'S' || c == 'u' || c == 'U') {
            ReadChar ();

            if (peekedChars.Length >= 2) {
                var c1 = peekedChars [1];
                var c2 = peekedChars.Length >= 3 ? peekedChars [2] : 0;

                if (c1 == '8')
                    ReadChar ();
                else if (
                    c1 == '1' && c2 == '6' ||
                    c1 == '3' && c2 == '2' ||
                    c1 == '6' && c2 == '4'
                ) {
                    ReadChars (2);
                }
            }

            return true;
        }

        return false;
    }

    private void TryReadFloatExponent (int line, int column, int startPos) {
        var c = PeekChar ();
        if (c == 'e' || c == 'E') {
            ReadChar ();

            var hasSign = false;
            c = PeekChar ();
            if (c == '+' || c == '-') {
                ReadChar ();
                hasSign = true;
            }

            if (!IsIntegerDigit (PeekChar ())) {
                Errors.Add (new EchelonScriptErrorMessage (
                    ES_FrontendErrors.InvalidFloatLiteral,
                    startPos, curPos - startPos,
                    fileName, line, column
                ));
                curPos -= hasSign ? 2 : 1;
                return;
            }

            ReadStringWhile ((c) => IsIntegerDigit (c));
        }
    }

    private bool TryReadFloatSuffix () {
        var c = PeekChar ();
        if (c != null && IsFloatSuffix (c.Value)) {
            ReadChar ();
            return true;
        }

        return false;
    }

    private bool TryReadFloatFractional () {
        if (PeekChar () == '.' && IsIntegerDigit (PeekChar (1))) {
            ReadChar ();

            if (PeekChar () != NumberSeparator)
                ReadStringWhile ((c) => IsIntegerDigit (c) || c == NumberSeparator);

            return true;
        }

        return false;
    }

    #endregion

    private void DecodeStringToken (
        ReadOnlySpan<char> input,
        out string outputUTF16,
        out int [] outputUTF32,
        int curLine, int lineStartPos, int curPos,
        bool verbatim
    ) {
        void EmitError (string message, int line, int col, int pos, int len)
            => Errors.Add (new EchelonScriptErrorMessage (message, pos, len, fileName, line, col));

        using var charList = new StructPooledList<(int Line, int Col, int Pos, Rune Rune)> (CL_ClearMode.Auto);
        using var newCharList = new StructPooledList<Rune> (CL_ClearMode.Auto);

        (int Line, int Col, int Pos, Rune Rune)? TryGetRune (int pos) {
            if (pos >= charList.Count)
                return null;

            return charList [pos];
        }

        for (var offs = 0; offs < input.Length;) {
            Rune.DecodeFromUtf16 (input [offs..^0], out var rune, out var consumed);

            if (rune.Value == '\n') {
                curLine++;
                curLineStartPos = curPos + offs;
            }

            charList.Add ((curLine, CalcColumn (lineStartPos, curPos + offs), curPos + offs, rune));
            offs += consumed;
        }

        var hexCharLen = 0;
        for (var i = 0; i < charList.Count;) {
            var runeData = charList [i++];
            var rune = runeData.Rune;

            if (rune.Value == '\\') {
                var nextRune = TryGetRune (i);

                if (!verbatim && nextRune != null) {
                    i++;

                    switch (nextRune.Value.Rune.Value) {
                        case '\'':
                            newCharList.Add (new Rune ('\''));
                            break;

                        case '"':
                            newCharList.Add (new Rune ('"'));
                            break;

                        case '\\':
                            newCharList.Add (new Rune ('\\'));
                            break;

                        case '0':
                            newCharList.Add (new Rune ('\0'));
                            break;

                        case 'a':
                            newCharList.Add (new Rune ('\u0007'));
                            break;

                        case 'b':
                            newCharList.Add (new Rune ('\u0008'));
                            break;

                        case 'f':
                            newCharList.Add (new Rune ('\u000C'));
                            break;

                        case 'n':
                            newCharList.Add (new Rune ('\u000A'));
                            break;

                        case 'r':
                            newCharList.Add (new Rune ('\u000D'));
                            break;

                        case 't':
                            newCharList.Add (new Rune ('\u0009'));
                            break;

                        case 'v':
                            newCharList.Add (new Rune ('\u000B'));
                            break;

                        case 'x':
                            hexCharLen = 2;
                        ParseHexChar:
                            {
                                var startPos = i - 1;
                                Span<char> charBuf = stackalloc char [hexCharLen];
                                var invalid = false;
                                var len = 0;
                                for (var j = 0; j < hexCharLen; j++) {
                                    var c = TryGetRune (i++);

                                    if (c != null && IsHexDigit (c.Value.Rune)) {
                                        var cChar = (char) c.Value.Rune.Value;

                                        charBuf [j] = char.ToLowerInvariant (cChar);
                                    } else
                                        invalid = true;

                                    if (c != null)
                                        len += c.Value.Rune.Utf16SequenceLength;
                                }
                                i = Math.Min (i, charList.Count);

                                if (invalid) {
                                    for (var k = startPos; k < i; k++)
                                        newCharList.Add (charList [k].Rune);
                                    EmitError (
                                        ES_FrontendErrors.UnrecognizedEscape,
                                        runeData.Line, runeData.Col,
                                        runeData.Pos, rune.Utf16SequenceLength + len
                                    );
                                } else {
                                    var charValue = int.Parse (charBuf, System.Globalization.NumberStyles.AllowHexSpecifier, null);
                                    newCharList.Add (new Rune (charValue));
                                }
                                break;
                            }

                        case 'u':
                            hexCharLen = 4;
                            goto ParseHexChar;

                        case 'U':
                            hexCharLen = 8;
                            goto ParseHexChar;

                        default:
                            newCharList.Add (rune);
                            newCharList.Add (nextRune.Value.Rune);
                            EmitError (
                                ES_FrontendErrors.UnrecognizedEscape,
                                runeData.Line, runeData.Col,
                                runeData.Pos, runeData.Rune.Utf16SequenceLength
                            );
                            break;
                    }
                } else if (verbatim && nextRune != null) {
                    if (nextRune.Value.Rune.Value == '"') {
                        i++;
                        newCharList.Add (new Rune ('"'));
                    } else
                        newCharList.Add (new Rune ('\\'));
                }
            } else
                newCharList.Add (rune);
        }

        var strLen = 0;
        foreach (var rune in newCharList)
            strLen += rune.Utf16SequenceLength;

        using var retUTF16 = PooledArray<char>.GetArray (strLen);
        outputUTF32 = new int [newCharList.Count];

        var utf16Span = retUTF16.Span;
        for (var i = 0; i < newCharList.Count; i++) {
            outputUTF32 [i] = newCharList [i].Value;

            var offs = newCharList [i].EncodeToUtf16 (utf16Span);
            utf16Span = utf16Span [offs..];
        }

        outputUTF16 = retUTF16.Span.ToString ();
    }
}
