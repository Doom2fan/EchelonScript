/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Text;
using ChronosLib.Pooled;
using EchelonScript.Common.Utilities;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;

namespace EchelonScript.Compiler.Frontend.Parser.Tokenizer;

public sealed partial class EchelonScriptTokenizer : IDisposable {
    #region ================== Constants

    public const char NumberSeparator = '\'';

    #endregion

    #region ================== Static fields

    private static readonly List<TokenParser> tokenParsers;
    private static readonly int tokenParsersMaxLength;

    #endregion

    #region ================== Instance fields

    private int curPos;
    private ReadOnlyMemory<char> fileName;
    private ReadOnlyMemory<char> data;
    private StructPooledList<(EchelonScriptToken, EchelonScriptToken?)> tokenBuffer;

    private int curLine;
    private int curLineStartPos;

    #endregion

    #region ================== Instance properties

    public List<EchelonScriptErrorMessage> Errors { get; private set; }

    #endregion

    #region ================== Constructors

    static EchelonScriptTokenizer () {
        tokenParsers = new List<TokenParser> {
            new HexTokenParser (),
            new BinaryTokenParser (),
            new DecimalTokenParser (),
            new IdentifierTokenParser (),
            new StringLiteralTokenParser (),
            new CharLiteralTokenParser (),
            new MiscTokenParser ()
        };

        tokenParsersMaxLength = 1;
        foreach (var parser in tokenParsers)
            tokenParsersMaxLength = Math.Max (Math.Max (tokenParsersMaxLength, parser.MinimumStartPeek), parser.RequestedStartPeek);
    }

    public EchelonScriptTokenizer (List<EchelonScriptErrorMessage> errorsList) {
        curPos = 0;
        fileName = null;
        data = null;
        tokenBuffer = new StructPooledList<(EchelonScriptToken, EchelonScriptToken?)> (CL_ClearMode.Auto);

        Errors = errorsList;
    }

    #endregion

    #region ================== Static methods

    public static int CalcColumn (ReadOnlySpan<char> srcText, int columnStart, int textPos) {
        var charCount = 0;
        var maxPos = Math.Min (srcText.Length, textPos);

        for (var i = columnStart; i < maxPos;) {
            Rune.DecodeFromUtf16 (srcText [i..^0], out _, out var offs);
            i += offs;
            charCount++;
        }

        return charCount + 1;
    }

    public static void CalcLine (ReadOnlySpan<char> srcText, int curPos, out int curLine, out int curLineStart) {
        var lineCount = 1;
        var lineStart = 0;
        var maxPos = Math.Min (srcText.Length, curPos);

        for (var i = 0; i < maxPos;) {
            Rune.DecodeFromUtf16 (srcText [i..^0], out var rune, out var offs);
            i += offs;

            if (rune.Value == '\n') {
                lineCount++;
                lineStart = i;
            }
        }

        curLine = lineCount;
        curLineStart = lineStart;
    }

    #region Common pattern matching

    public static bool IsLatinLetter (char? c) => c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';

    public static bool IsIntegerDigit (char? c) => c >= '0' && c <= '9';

    public static bool IsHexDigit (char? c) => IsIntegerDigit (c) || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F';

    public static bool IsBinaryDigit (char? c) => c == '0' || c == '1';

    public static bool IsFloatSuffix (char? c) => c == 'f' || c == 'F' || c == 'd' || c == 'D';

    public static bool IsHexDigit (Rune? c) => c != null && c.Value.IsBmp && IsHexDigit ((char) c.Value.Value);

    #endregion

    #endregion

    #region ================== Instance methods

    #region Public methods

    /// <summary>Resets the tokenizer to its initial state.</summary>
    public void Reset () {
        curPos = 0;
        curLine = 1;
        curLineStartPos = 0;
        tokenBuffer.Clear ();
    }

    /// <summary>Sets the source data for the input text.</summary>
    /// <param name="newData">The new source data.</param>
    public void SetSource (ReadOnlyMemory<char> newName, ReadOnlyMemory<char> newData) {
        Reset ();
        fileName = newName;
        data = newData;
    }

    /// <summary>Peeks a token from the input text.</summary>
    /// <returns>The next token in the input text.</returns>
    public (EchelonScriptToken tk, EchelonScriptToken? doc) PeekNextToken () {
        if (tokenBuffer.Count > 0)
            return tokenBuffer [0];

        var newToken = InternalNextToken ();
        tokenBuffer.Add (newToken);
        return newToken;
    }

    /// <summary>Peeks a token from the input text with the specified offset.</summary>
    /// <param name="offset">The offset of the token.</param>
    /// <returns>The next token in the input text.</returns>
    public (EchelonScriptToken tk, EchelonScriptToken? doc) PeekNextToken (int offset) {
        while (tokenBuffer.Count < offset + 1)
            tokenBuffer.Add (InternalNextToken ());

        return tokenBuffer [offset];
    }

    /// <summary>Reads a token from the input text.</summary>
    /// <returns>The next token in the input text.</returns>
    public (EchelonScriptToken tk, EchelonScriptToken? doc) NextToken () {
        if (tokenBuffer.Count < 1)
            return InternalNextToken ();

        var curToken = tokenBuffer [0];
        tokenBuffer.RemoveAt (0);
        return curToken;
    }

    #endregion

    #region Private methods

    private (EchelonScriptToken tk, EchelonScriptToken? doc) InternalNextToken () {
        var docTk = SkipWhitespace ();

        var retToken = new EchelonScriptToken {
            Type = EchelonScriptTokenType.Invalid,
            //FileName = fileName,
            Text = ES_String.Empty,
            TextStartPos = curPos,
            TextLine = curLine,
            TextColumn = CalcColumn (curLineStartPos, curPos)
        };

        if (PeekChar () == null) {
            retToken.Type = EchelonScriptTokenType.EOF;
            return (retToken, docTk);
        }

        var peekedChars = PeekChars (tokenParsersMaxLength);

        foreach (var parser in tokenParsers) {
            if (peekedChars.Length < parser.MinimumStartPeek)
                continue;

            if (parser.IsStartValid (this, peekedChars) && parser.ParseToken (this, peekedChars, ref retToken))
                break;
        }

        if (retToken.Type == EchelonScriptTokenType.Invalid) {
            ReadChar ();
            //TODO: FIXME
            //retToken.Text = data [retToken.TextStartPos..curPos];
        }

        return (retToken, docTk);
    }

    private EchelonScriptToken? SkipWhitespace () {
        EchelonScriptToken? ret = null;
        int blockCommentDepth;

        while (true) {
            var c = PeekChar ();

            switch (c) {
                case ' ':
                case '\t':
                case '\r':
                case '\n':
                    ReadChar ();
                    break;

                case '/':
                    c = PeekChar (1);

                    if (c == '/') {
                        ReadChars (2);
                        c = ReadUntil (ch => ch == '\n', true);

                        break;
                    } else if (c == '*') {
                        ReadChars (2);

                        blockCommentDepth = 1;
                        while (blockCommentDepth > 0) {
                            c = ReadUntil (ch => ch == '*' || ch == '/', true);
                            var c2 = PeekChar ();

                            if (c == null)
                                break;
                            else if (c == '*' && c2 == '/') {
                                ReadChar ();
                                blockCommentDepth--;
                            } else if (c == '/' && c2 == '*') {
                                ReadChar ();
                                blockCommentDepth++;
                            }
                        }

                        continue;
                    } else if (c == '+') {
                        var docStartPos = curPos;
                        var docLine = curLine;
                        var docColumn = CalcColumn (curLineStartPos, docStartPos);
                        ReadChars (2);

                        while (true) {
                            c = ReadUntil (ch => ch == '+', true);
                            if (c == null)
                                break;

                            if (PeekChar () == '/') {
                                ReadChar ();
                                var docEndPos = curPos;

                                var docCom = new EchelonScriptToken {
                                    Type = EchelonScriptTokenType.DocComment,
                                    //TODO: FIXME
                                    //Text = data [docStartPos..docEndPos],
                                    TextStartPos = docStartPos,
                                    TextLine = docLine,
                                    TextColumn = docColumn
                                };
                                ret = docCom;
                                break;
                            }
                        }

                        continue;
                    }
                    goto default;

                case null:
                default:
                    return ret;
            }
        }
    }

    #endregion

    #endregion

    #region ================== IDisposable Support

    public bool IsDisposed { get; private set; }

    ~EchelonScriptTokenizer () {
        if (!IsDisposed)
            DoDispose ();
    }

    private void DoDispose () {
        if (IsDisposed)
            return;

        tokenBuffer.Dispose ();

        IsDisposed = true;
    }

    public void Dispose () {
        GC.SuppressFinalize (this);
        DoDispose ();
    }

    #endregion
}
