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
using System.Collections.Immutable;
using System.Diagnostics;
using System.Text;
using ChronosLib.Pooled;
using EchelonScript.Common.Utilities;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.Parser.Internal;

namespace EchelonScript.Compiler.Frontend.Parser.Tokenizer;

[NonCopyable]
public ref partial struct ES_Tokenizer {
    #region ================== Constants

    public const char NumberSeparator = '\'';

    #endregion

    #region ================== Static fields

    private static readonly List<TokenParser> tokenParsers;
    private static readonly int tokenParsersMaxLength;

    #endregion

    #region ================== Instance fields

    private int curPos;

    private ES_CompilerContext? context;
    private SourceFile? sourceFile;
    private ReadOnlySpan<char> text;

    private ImmutableArray<ES_TriviaToken>.Builder triviaBuilder;
    private StructPooledList<ES_Token> tokenBuffer;

    #endregion

    #region ================== Constructors

    static ES_Tokenizer () {
        tokenParsers = new List<TokenParser> {
            new HexTokenParser (),
            new BinaryTokenParser (),
            new DecimalTokenParser (),
            new FloatTokenParser (),
            new IdentifierTokenParser (),
            new StringLiteralTokenParser (),
            new CharLiteralTokenParser (),
            new MiscTokenParser ()
        };

        tokenParsersMaxLength = 1;
        foreach (var parser in tokenParsers)
            tokenParsersMaxLength = Math.Max (Math.Max (tokenParsersMaxLength, parser.MinimumStartPeek), parser.RequestedStartPeek);
    }

    public ES_Tokenizer () {
        curPos = 0;

        context = null;
        sourceFile = null;
        text = null;

        triviaBuilder = ImmutableArray.CreateBuilder<ES_TriviaToken> ();
        tokenBuffer = new StructPooledList<ES_Token> (CL_ClearMode.Auto);
    }

    #endregion

    #region ================== Static methods

    #region Common pattern matching

    public static bool IsLatinLetter (char? c) => c is >= 'a' and <= 'z' || c is >= 'A' and <= 'Z';

    public static bool IsIntegerDigit (char? c) => c is >= '0' and <= '9';

    public static bool IsHexDigit (char? c) => IsIntegerDigit (c) || c is >= 'a' and <= 'f' || c is >= 'A' and <= 'F';

    public static bool IsBinaryDigit (char? c) => c is '0' or '1';

    public static bool IsHexDigit (Rune? c) => c != null && c.Value.IsBmp && IsHexDigit ((char) c.Value.Value);

    public static bool IsFloatSuffix (char? c) => c is 'f' or 'F' or 'd' or 'D';

    public static bool IsExponentStart (char? c) => c is 'e' or 'E';

    public static bool IsIntegerSuffix (char? c) => c is 's' or 'S' or 'u' or 'U';

    public static bool IsWhitespace (char? c) => c switch {
        ' ' or '\t' => true,
        _ => false,
    };

    #endregion

    #endregion

    #region ================== Instance methods

    #region Public methods

    /// <summary>Resets the tokenizer to its initial state.</summary>
    public void Reset () {
        curPos = 0;

        context = null;
        sourceFile = null;
        text = null;

        tokenBuffer.Clear ();
    }

    /// <summary>Sets the source data for the input text.</summary>
    /// <param name="newText">The new source data.</param>
    public void SetSource (ES_CompilerContext newContext, SourceSpan span) {
        Reset ();

        sourceFile = newContext.SourceMap.TryGetFile (span);
        if (sourceFile is null)
            throw new CompilationException ("Invalid source span");

        context = newContext;
        text = sourceFile.GetText (span).AsSpan ();
    }

    /// <summary>Peeks a token from the input text.</summary>
    /// <returns>The next token in the input text.</returns>
    public ES_Token PeekNextToken () {
        if (tokenBuffer.Count > 0)
            return tokenBuffer [0];

        var newToken = InternalNextToken ();
        tokenBuffer.Add (newToken);
        return newToken;
    }

    /// <summary>Peeks a token from the input text with the specified offset.</summary>
    /// <param name="offset">The offset of the token.</param>
    /// <returns>The next token in the input text.</returns>
    public ES_Token PeekNextToken (int offset) {
        while (tokenBuffer.Count < offset + 1)
            tokenBuffer.Add (InternalNextToken ());

        return tokenBuffer [offset];
    }

    /// <summary>Reads a token from the input text.</summary>
    /// <returns>The next token in the input text.</returns>
    public ES_Token NextToken () {
        if (tokenBuffer.Count < 1)
            return InternalNextToken ();

        var curToken = tokenBuffer [0];
        tokenBuffer.RemoveAt (0);
        return curToken;
    }

    #endregion

    #region Private methods

    private readonly void Diag (ES_DiagnosticDescriptor diagDesc, params object? []? messageArgs)
        => context!.ReportDiagnostic (diagDesc.Create (messageArgs));

    private readonly void Diag (ES_DiagnosticDescriptor diagDesc, int startPos, int endPos, params object? []? messageArgs) {
        Debug.Assert (context is not null);

        var location = context.SourceMap.GetLocation (GetSpan (startPos, endPos));
        context.ReportDiagnostic (diagDesc.Create (location, messageArgs));
    }

    private readonly SourceSpan GetSpan (int startPos, int endPos) => sourceFile!.Span [startPos..endPos];

    private readonly ImmutableArray<ES_TriviaToken> FlushTrivia () {
        if (triviaBuilder.Count == triviaBuilder.Capacity)
            return triviaBuilder.MoveToImmutable ();

        var ret = triviaBuilder.ToImmutable ();
        triviaBuilder.Clear ();
        return ret;
    }

    private ES_Token InternalNextToken () {
        SkipWhitespace (false);

        var retToken = new ES_Token {
            Type = ES_TokenType.Invalid,

            TextSpan = default,

            LeadingTrivia = FlushTrivia (),
            TrailingTrivia = ImmutableArray<ES_TriviaToken>.Empty,
        };

        if (PeekChar () == null)
            return retToken with { Type = ES_TokenType.EOF, };

        var peekedChars = PeekChars (tokenParsersMaxLength);

        foreach (var parser in tokenParsers) {
            if (peekedChars.Length < parser.MinimumStartPeek)
                continue;

            if (parser.IsStartValid (ref this, peekedChars) && parser.ParseToken (ref this, peekedChars, ref retToken))
                break;
        }

        if (retToken.Type == ES_TokenType.Invalid) {
            ReadChar ();
            retToken = retToken with { TextSpan = GetSpan (curPos - 1, curPos), };
        }

        SkipWhitespace (true);
        return retToken with { TrailingTrivia = FlushTrivia (), };
    }

    private void SkipWhitespace_ReadComment () {
        var startPos = curPos;
        if (!ReadChars (2).Equals ("//", StringComparison.InvariantCulture))
            throw new CompilationException ($"Invalid call to {nameof (SkipWhitespace_ReadComment)}");

        char? c;
        while ((c = PeekChar ()) != null) {
            if (c == '\n' || c == '\r')
                break;

            ReadChar ();
        }

        triviaBuilder.Add (new () {
            Type = ES_TriviaType.LineComment,
            TextSpan = GetSpan (startPos, curPos),
        });
    }

    private void SkipWhitespace_ReadBlockComment () {
        var startPos = curPos;
        if (!ReadChars (2).Equals ("/*", StringComparison.InvariantCulture))
            throw new CompilationException ($"Invalid call to {nameof (SkipWhitespace_ReadBlockComment)}");

        var blockCommentDepth = 1;
        while (blockCommentDepth > 0) {
            var c = ReadUntil (ch => ch == '*' || ch == '/', true);
            var c2 = PeekChar ();

            if (c == null) {
                Diag (DiagnosticDescriptors.UnclosedBlockComment, startPos, curPos);
                break;
            } else if (c == '*' && c2 == '/') {
                ReadChar ();
                blockCommentDepth--;
            } else if (c == '/' && c2 == '*') {
                ReadChar ();
                blockCommentDepth++;
            }
        }

        triviaBuilder.Add (new () {
            Type = ES_TriviaType.BlockComment,
            TextSpan = GetSpan (startPos, curPos),
        });
    }

    private void SkipWhitespace_ReadDocComment () {
        var startPos = curPos;
        if (!ReadChars (2).Equals ("/+", StringComparison.InvariantCulture))
            throw new CompilationException ($"Invalid call to {nameof (SkipWhitespace_ReadDocComment)}");

        while (true) {
            var c = ReadUntil (ch => ch == '+', true);
            if (c == null) {
                Diag (DiagnosticDescriptors.UnclosedDocComment, startPos, curPos);
                break;
            }

            if (PeekChar () == '/') {
                ReadChar ();
                break;
            }
        }

        triviaBuilder.Add (new () {
            Type = ES_TriviaType.DocComment,
            TextSpan = GetSpan (startPos, curPos),
        });
    }

    private bool SkipWhitespace_TryReadWhitespace () {
        if (!IsWhitespace (PeekChar ()))
            return false;

        var startPos = curPos;
        while (IsWhitespace (PeekChar ()))
            ReadChar ();

        triviaBuilder.Add (new () {
            Type = ES_TriviaType.Whitespace,
            TextSpan = GetSpan (startPos, curPos)
        });

        return true;
    }

    private bool SkipWhitespace_TryReadLineFeed (bool trailing) {
        var startPos = curPos;
        int length;

        var c = PeekChar ();
        if (c == '\r' && PeekChar (1) == '\n')
            length = 2;
        else if (c == '\n' || c == '\r')
            length = 1;
        else
            return false;

        if (!trailing)
            ReadChars (length);

        triviaBuilder.Add (new () {
            Type = ES_TriviaType.EndOfLine,
            TextSpan = GetSpan (startPos, startPos + length)
        });

        return true;
    }

    private void SkipWhitespace (bool trailing) {
         while (true) {
            if (SkipWhitespace_TryReadWhitespace ())
                continue;
            else if (SkipWhitespace_TryReadLineFeed (trailing)) {
                if (trailing)
                    return;

                continue;
            }

            var c = PeekChar ();
            switch (c) {
                case '/':
                    c = PeekChar (1);

                    if (c == '/') {
                        SkipWhitespace_ReadComment ();
                        continue;
                    } else if (c == '*') {
                        SkipWhitespace_ReadBlockComment ();
                        continue;
                    } else if (c == '+') {
                        if (trailing)
                            return;

                        SkipWhitespace_ReadDocComment ();
                        continue;
                    }
                    goto default;

                case null:
                default:
                    return;
            }
        }
    }

    #endregion

    #endregion

    #region ================== IDisposable Support

    public bool IsDisposed { get; private set; }

    private void DoDispose () {
        if (IsDisposed)
            return;

        tokenBuffer.Dispose ();

        IsDisposed = true;
    }

    public void Dispose () {
        DoDispose ();
    }

    #endregion
}
