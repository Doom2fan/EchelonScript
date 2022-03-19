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
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Frontend.Parser;

public class EchelonScriptTokenizer : IDisposable {
    #region ================== Token parsers

    public abstract class TokenParser {
        public abstract int MinimumStartPeek { get; }
        public abstract int RequestedStartPeek { get; }

        public abstract bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars);
        public abstract bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken);
    }

    public class HexTokenParser : TokenParser {
        public override int MinimumStartPeek => 2;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars.StartsWith ("0x", StringComparison.InvariantCultureIgnoreCase);

        public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
            var startPos = tokenizer.curPos;
            tokenizer.ReadChars (2);

            if (tokenizer.PeekChar () == NumberSeparator)
                tokenizer.Errors.Add (new EchelonScriptErrorMessage (retToken, ES_FrontendErrors.InvalidHexLiteral));
            else
                tokenizer.ReadStringWhile (c => IsHexDigit (c) || c == NumberSeparator);

            tokenizer.TryReadIntSuffix ();

            retToken.Type = EchelonScriptTokenType.HexIntegerLiteral;
            retToken.Text = tokenizer.data [startPos..tokenizer.curPos];

            return true;
        }
    }

    public class BinaryTokenParser : TokenParser {
        public override int MinimumStartPeek => 2;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars.StartsWith ("0b", StringComparison.InvariantCultureIgnoreCase);

        public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
            var startPos = tokenizer.curPos;
            tokenizer.ReadChars (2);

            if (tokenizer.PeekChar () == NumberSeparator)
                tokenizer.Errors.Add (new EchelonScriptErrorMessage (retToken, ES_FrontendErrors.InvalidBinaryLiteral));
            else
                tokenizer.ReadStringWhile (c => IsBinaryDigit (c) || c == NumberSeparator);

            tokenizer.TryReadIntSuffix ();

            retToken.Type = EchelonScriptTokenType.BinIntegerLiteral;
            retToken.Text = tokenizer.data [startPos..tokenizer.curPos];

            return true;
        }
    }

    public class DecimalTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) {
            return (
                peekedChars [0] == '0' ||
                IsIntegerDigit (peekedChars [0]) ||
                (peekedChars [0] == '.' && peekedChars.Length >= 2 && IsIntegerDigit (peekedChars [1]))
            );
        }

        public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
            if (IsIntegerDigit (peekedChars [0])) {
                var startPos = tokenizer.curPos;

                tokenizer.ReadStringWhile ((c) => c == '\'' || IsIntegerDigit (c));

                var c = tokenizer.PeekChar ();
                if (c != null) {
                    if (tokenizer.TryReadFloatFractional ()) {
                        tokenizer.TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                        tokenizer.TryReadFloatSuffix ();

                        retToken.Type = EchelonScriptTokenType.FloatLiteral;
                    } else if (c == 'e' || c == 'E') {
                        tokenizer.TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                        tokenizer.TryReadFloatSuffix ();

                        retToken.Type = EchelonScriptTokenType.FloatLiteral;
                    } else if (IsFloatSuffix (c.Value)) {
                        tokenizer.ReadChar ();
                        retToken.Type = EchelonScriptTokenType.FloatLiteral;
                    } else {
                        tokenizer.TryReadIntSuffix ();

                        retToken.Type = EchelonScriptTokenType.DecIntegerLiteral;
                    }
                } else
                    retToken.Type = EchelonScriptTokenType.DecIntegerLiteral;

                retToken.Text = tokenizer.data [startPos..tokenizer.curPos];
            } else if (peekedChars [0] == '.' && peekedChars.Length >= 2 && IsIntegerDigit (peekedChars [1])) {
                var startPos = tokenizer.curPos;

                tokenizer.TryReadFloatFractional ();
                tokenizer.TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                tokenizer.TryReadFloatSuffix ();

                retToken.Type = EchelonScriptTokenType.FloatLiteral;
                retToken.Text = tokenizer.data [startPos..tokenizer.curPos];
            }

            return true;
        }
    }

    public class IdentifierTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 1;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars [0] == '_' || IsLatinLetter (peekedChars [0]);

        public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
            var startPos = tokenizer.curPos;
            var len = tokenizer.ReadStringWhile ((c) => c == '_' || IsIntegerDigit (c) || IsLatinLetter (c));

            retToken.Type = EchelonScriptTokenType.Identifier;
            retToken.Text = tokenizer.data.Slice (startPos, len);

            return true;
        }
    }

    public class StringLiteralTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars [0] == '"' || (peekedChars.Length >= 2 && peekedChars.StartsWith ("@\""));

        public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
            var startPos = tokenizer.curPos;
            var verbatim = false;

            if (peekedChars [0] == '@') {
                tokenizer.ReadChar ();

                verbatim = true;
                retToken.Type = EchelonScriptTokenType.VerbatimStringLiteral;
            } else
                retToken.Type = EchelonScriptTokenType.RegularStringLiteral;

            tokenizer.ReadChar ();
            var textStartLine = tokenizer.curLine;
            var textStartLinePos = tokenizer.curLineStartPos;
            var textStartPos = tokenizer.curPos;

            var unclosed = false;
            while (true) {
                var c = tokenizer.ReadChar ();

                if (c == '"')
                    break;
                else if (c == null) {
                    tokenizer.Errors.Add (new EchelonScriptErrorMessage (
                        ES_FrontendErrors.UnclosedStringLiteral,
                        startPos, tokenizer.curPos - startPos,
                        tokenizer.fileName, retToken.TextLine, retToken.TextColumn
                    ));
                    unclosed = true;
                    break;
                } else if (c == '\\' && tokenizer.PeekChar () == '"')
                    tokenizer.ReadChar ();
                else if (!verbatim && (c == '\r' || c == '\n')) {
                    var err = new EchelonScriptErrorMessage (
                        c == '\r'
                            ? ES_FrontendErrors.NoCRInRegularStrings
                            : ES_FrontendErrors.NoLFInRegularStrings,
                        tokenizer.curPos, 1,
                        tokenizer.fileName,
                        tokenizer.curLine,
                        tokenizer.CalcColumn (tokenizer.curLineStartPos, tokenizer.curPos)
                    );
                    tokenizer.Errors.Add (err);
                }
            }

            retToken.Text = tokenizer.data [startPos..tokenizer.curPos];
            if (!unclosed) {
                tokenizer.DecodeStringToken (
                    retToken.Text.Span [(!verbatim ? 1 : 2)..^1],
                    out retToken.DecodedStringUTF16,
                    out retToken.DecodedStringUTF32,
                    textStartLine, textStartLinePos, textStartPos,
                    verbatim
                );
            }

            return true;
        }
    }

    public class CharLiteralTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 1;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars [0] == '\'';

        public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
            var startPos = tokenizer.curPos;
            retToken.Type = EchelonScriptTokenType.CharacterLiteral;

            tokenizer.ReadChar ();
            var textStartLine = tokenizer.curLine;
            var textStartLinePos = tokenizer.curLineStartPos;
            var textStartPos = tokenizer.curPos;

            var unclosed = false;
            while (true) {
                var c = tokenizer.ReadChar ();

                if (c == '\'')
                    break;
                else if (c == null) {
                    tokenizer.Errors.Add (new EchelonScriptErrorMessage (
                        ES_FrontendErrors.UnclosedCharLiteral,
                        startPos, tokenizer.curPos - startPos,
                        tokenizer.fileName, retToken.TextLine, retToken.TextColumn
                    ));
                    unclosed = true;
                    break;
                } else if (c == '\\' && tokenizer.PeekChar () == '\'')
                    tokenizer.ReadChar ();
                else if (c == '\r' || c == '\n') {
                    var err = new EchelonScriptErrorMessage (
                        c == '\r'
                            ? ES_FrontendErrors.NoCRInCharLiterals
                            : ES_FrontendErrors.NoLFInCharLiterals,
                        tokenizer.curPos, 1,
                        tokenizer.fileName,
                        tokenizer.curLine,
                        tokenizer.CalcColumn (tokenizer.curLineStartPos, tokenizer.curPos)
                    );
                    tokenizer.Errors.Add (err);
                }
            }

            retToken.Text = tokenizer.data [startPos..tokenizer.curPos];
            if (!unclosed) {
                tokenizer.DecodeStringToken (
                    retToken.Text.Span [1..^1],
                    out retToken.DecodedStringUTF16,
                    out retToken.DecodedStringUTF32,
                    textStartLine, textStartLinePos, textStartPos,
                    false
                );
            }

            if (retToken.DecodedStringUTF32?.Length > 1)
                tokenizer.Errors.Add (new EchelonScriptErrorMessage (retToken, ES_FrontendErrors.TooLongCharLiteral));
            else if (retToken.DecodedStringUTF32?.Length < 1)
                tokenizer.Errors.Add (new EchelonScriptErrorMessage (retToken, ES_FrontendErrors.EmptyCharLiteral));

            return true;
        }
    }

    public class MiscTokenParser : TokenParser {
        protected static readonly int tokenStringsMinLength;
        protected static readonly int tokenStringsMaxLength;
        protected static readonly List<(EchelonScriptTokenType Tok, string Str)> tokenStrings;

        static MiscTokenParser () {
            tokenStrings = new List<(EchelonScriptTokenType, string)> {
                (EchelonScriptTokenType.Dot, "."),
                (EchelonScriptTokenType.DotDot, ".."),
                (EchelonScriptTokenType.NamespaceOp, "::"),

                (EchelonScriptTokenType.AndAnd, "&&"),
                (EchelonScriptTokenType.OrOr, "||"),

                (EchelonScriptTokenType.Bang, "!"),
                (EchelonScriptTokenType.Plus, "+"),
                (EchelonScriptTokenType.Minus, "-"),
                (EchelonScriptTokenType.Asterisk, "*"),
                (EchelonScriptTokenType.Divide, "/"),
                (EchelonScriptTokenType.Modulo, "%"),
                (EchelonScriptTokenType.PowerOp, "**"),
                (EchelonScriptTokenType.PlusPlus, "++"),
                (EchelonScriptTokenType.MinusMinus, "--"),

                (EchelonScriptTokenType.And, "&"),
                (EchelonScriptTokenType.BitOr, "|"),
                (EchelonScriptTokenType.Xor, "^"),
                (EchelonScriptTokenType.Tilde, "~"),
                (EchelonScriptTokenType.ShiftLeft, "<<"),
                (EchelonScriptTokenType.ShiftRight, ">>"),
                (EchelonScriptTokenType.ShiftRightU, ">>>"),

                (EchelonScriptTokenType.LesserThan, "<"),
                (EchelonScriptTokenType.GreaterThan, ">"),
                (EchelonScriptTokenType.LesserThanEq, "<="),
                (EchelonScriptTokenType.GreaterThanEq, ">="),

                (EchelonScriptTokenType.Equals, "="),
                (EchelonScriptTokenType.PlusEq, "+="),
                (EchelonScriptTokenType.MinusEq, "-="),
                (EchelonScriptTokenType.MultiplyEq, "*="),
                (EchelonScriptTokenType.DivideEq, "/="),
                (EchelonScriptTokenType.ModuloEq, "%="),
                (EchelonScriptTokenType.PowerOpEq, "**="),
                (EchelonScriptTokenType.ConcatEq, "..="),

                (EchelonScriptTokenType.AndEq, "&="),
                (EchelonScriptTokenType.BitOrEq, "|="),
                (EchelonScriptTokenType.XorEq, "^="),
                (EchelonScriptTokenType.ShiftLeftEq, "<<="),
                (EchelonScriptTokenType.ShiftRightEq, ">>="),
                (EchelonScriptTokenType.ShiftRightUEq, ">>>="),

                (EchelonScriptTokenType.EqualsEquals, "=="),
                (EchelonScriptTokenType.NotEquals, "!="),

                (EchelonScriptTokenType.ParenOpen, "("),
                (EchelonScriptTokenType.ParenClose, ")"),
                (EchelonScriptTokenType.BracketOpen, "["),
                (EchelonScriptTokenType.BracketClose, "]"),
                (EchelonScriptTokenType.BraceOpen, "{"),
                (EchelonScriptTokenType.BraceClose, "}"),

                (EchelonScriptTokenType.Question, "?"),
                (EchelonScriptTokenType.Colon, ":"),
                (EchelonScriptTokenType.Comma, ","),
                (EchelonScriptTokenType.Semicolon, ";"),
                (EchelonScriptTokenType.LambdaArrow, "=>"),
            };

            tokenStrings.Sort ((x, y) => -x.Str.Length.CompareTo (y.Str.Length));
            tokenStringsMinLength = tokenStrings [^1].Str.Length;
            tokenStringsMaxLength = tokenStrings [0].Str.Length;
        }

        public override int MinimumStartPeek => tokenStringsMinLength;
        public override int RequestedStartPeek => tokenParsersMaxLength;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) => true;

        public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
            foreach (var tk in tokenStrings) {
                if (peekedChars.Length < tk.Str.Length)
                    continue;

                if (peekedChars.StartsWith (tk.Str)) {
                    tokenizer.ReadChars (tk.Str.Length);
                    retToken.Type = tk.Tok;
                    retToken.Text = tokenizer.data [retToken.TextStartPos..tokenizer.curPos];

                    return true;
                }
            }

            return false;
        }
    }

    #endregion

    #region ================== Constants

    public const char NumberSeparator = '\'';

    #endregion

    #region ================== Static fields

    protected static readonly List<TokenParser> tokenParsers;
    protected static readonly int tokenParsersMaxLength;

    #endregion

    #region ================== Instance fields

    protected int curPos;
    protected ReadOnlyMemory<char> fileName;
    protected ReadOnlyMemory<char> data;
    protected StructPooledList<(EchelonScriptToken, EchelonScriptToken?)> tokenBuffer;

    protected int curLine;
    protected int curLineStartPos;

    #endregion

    #region ================== Instance properties

    public List<EchelonScriptErrorMessage> Errors { get; protected set; }

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

    #region ================== Static functions

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

    public static bool IsLatinLetter (char? c) => (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');

    public static bool IsIntegerDigit (char? c) => c >= '0' && c <= '9';

    public static bool IsHexDigit (char? c) => IsIntegerDigit (c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');

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
        while (tokenBuffer.Count < (offset + 1))
            tokenBuffer.Add (InternalNextToken ());

        return tokenBuffer [offset];
    }

    /// <summary>Reads a token from the input text.</summary>
    /// <returns>The next token in the input text.</returns>
    public (EchelonScriptToken tk, EchelonScriptToken? doc) NextToken () {
        if (tokenBuffer.Count > 0) {
            var curToken = tokenBuffer [0];
            tokenBuffer.RemoveAt (0);
            return curToken;
        }

        return InternalNextToken ();
    }

    #endregion

    #region Protected methods

    protected int CalcColumn (int columnStart, int textPos) => CalcColumn (data.Span, columnStart, textPos);

    protected (EchelonScriptToken tk, EchelonScriptToken? doc) InternalNextToken () {
        var docTk = SkipWhitespace ();

        var retToken = new EchelonScriptToken {
            Type = EchelonScriptTokenType.Invalid,
            FileName = fileName,
            Text = null,
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
            retToken.Text = data [retToken.TextStartPos..curPos];
        }

        return (retToken, docTk);
    }

    #region Character reading

    protected char? PeekChar () => curPos < data.Length ? data.Span [curPos] : null;

    protected char? PeekChar (int offset) => (curPos + offset) < data.Length ? data.Span [curPos + offset] : null;

    protected ReadOnlySpan<char> PeekChars (int count)
        => data.Span.Slice (curPos, Math.Min (count, data.Length - curPos));

    protected ReadOnlySpan<char> PeekChars (int offset, int count)
        => data.Span.Slice (Math.Min (data.Length - 1, curPos + offset), Math.Min (count, data.Length - curPos));

    protected char? ReadChar () {
        if (curPos >= data.Length)
            return null;

        var ret = data.Span [curPos++];

        if (ret == '\n') {
            curLine++;
            curLineStartPos = curPos;
        }

        return ret;
    }

    protected ReadOnlySpan<char> ReadChars (int count) {
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

    protected EchelonScriptToken? SkipWhitespace () {
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
                                    Text = data [docStartPos..docEndPos],
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

    #region Character reading utilities

    protected char? ReadUntil (Predicate<char> func, bool readReq) {
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

    protected int ReadStringWhile (Predicate<char> func) {
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

    protected bool ReadEscapeSequence () {
        bool ReadRequiredChars (int count, Predicate<char> isValid) {
            var validCharsCount = 0;

            for (var i = 0; i < count; i++) {
                var c = PeekChar ();
                if (c != null && IsHexDigit (c.Value)) {
                    validCharsCount++;
                    ReadChar ();
                }
            }

            return validCharsCount == count;
        }

        var c = ReadChar ();
        if (c == null)
            return false;

        return c switch {
            '\'' => true,
            '"'  => true,
            '\\'  => true,
            '0'  => true,
            'a'  => true,
            'b'  => true,
            'f'  => true,
            'n'  => true,
            'r'  => true,
            't'  => true,
            'v'  => true,

            'x' => ReadRequiredChars (2, c => IsHexDigit (c)),
            'u' => ReadRequiredChars (4, c => IsHexDigit (c)),
            'U' => ReadRequiredChars (8, c => IsHexDigit (c)),

            _ => false,
        };
    }

    protected bool TryReadIntSuffix () {
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
                    (c1 == '1' && c2 == '6') ||
                    (c1 == '3' && c2 == '2') ||
                    (c1 == '6' && c2 == '4')
                ) {
                    ReadChars (2);
                }
            }

            return true;
        }

        return false;
    }

    protected void TryReadFloatExponent (int line, int column, int startPos) {
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

    protected bool TryReadFloatSuffix () {
        var c = PeekChar ();
        if (c != null && IsFloatSuffix (c.Value)) {
            ReadChar ();
            return true;
        }

        return false;
    }

    protected bool TryReadFloatFractional () {
        if (PeekChar () == '.' && IsIntegerDigit (PeekChar (1))) {
            ReadChar ();

            if (PeekChar () != NumberSeparator)
                ReadStringWhile ((c) => IsIntegerDigit (c) || c == NumberSeparator);

            return true;
        }

        return false;
    }

    #endregion

    protected void DecodeStringToken (
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

    #endregion

    #endregion

    #region ================== IDisposable Support

    public bool IsDisposed { get; private set; }

    protected virtual void DoDispose () {
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
