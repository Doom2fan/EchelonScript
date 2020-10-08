/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

using System;
using System.Collections.Generic;

namespace EchelonScriptCompiler.Parser {
    public class EchelonScriptTokenizer {
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

            public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) {
                return peekedChars.StartsWith ("0x", StringComparison.InvariantCultureIgnoreCase);
            }

            public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
                int startPos = tokenizer.curPos;
                tokenizer.ReadChars (2);

                if (tokenizer.PeekChar () == NumberSeparator)
                    tokenizer.Errors.Add (new EchelonScriptErrorMessage (retToken, "Invalid hex literal."));
                else
                    tokenizer.ReadStringWhile (c => IsHexDigit (c) || c == NumberSeparator);

                tokenizer.TryReadIntSuffix ();

                retToken.Type = EchelonScriptTokenType.HexIntegerLiteral;
                retToken.Text = tokenizer.data.Slice (startPos, tokenizer.curPos - startPos);

                return true;
            }
        }

        public class BinaryTokenParser : TokenParser {
            public override int MinimumStartPeek => 2;
            public override int RequestedStartPeek => 2;

            public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) {
                return peekedChars.StartsWith ("0b", StringComparison.InvariantCultureIgnoreCase);
            }

            public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
                int startPos = tokenizer.curPos;
                tokenizer.ReadChars (2);

                if (tokenizer.PeekChar () == NumberSeparator)
                    tokenizer.Errors.Add (new EchelonScriptErrorMessage (retToken, "Invalid binary literal."));
                else
                    tokenizer.ReadStringWhile (c => IsBinaryDigit (c) || c == NumberSeparator);

                tokenizer.TryReadIntSuffix ();

                retToken.Type = EchelonScriptTokenType.BinIntegerLiteral;
                retToken.Text = tokenizer.data.Slice (startPos, tokenizer.curPos - startPos);

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
                if (peekedChars [0] == '0') {
                    int startPos = tokenizer.curPos;

                    tokenizer.ReadChar ();
                    if (tokenizer.PeekChar () == '.') {
                        tokenizer.TryReadFloatFractional ();
                        tokenizer.TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                        tokenizer.TryReadFloatSuffix ();

                        retToken.Type = EchelonScriptTokenType.FloatLiteral;
                    } else {
                        tokenizer.TryReadIntSuffix ();
                        retToken.Type = EchelonScriptTokenType.DecIntegerLiteral;
                    }

                    retToken.Text = tokenizer.data.Slice (startPos, tokenizer.curPos - startPos);
                } else if (IsIntegerDigit (peekedChars [0])) {
                    int startPos = tokenizer.curPos;

                    tokenizer.ReadStringWhile ((c) => c == '\'' || IsIntegerDigit (c));

                    var c = tokenizer.PeekChar ();
                    if (c != null) {
                        if (c == '.') {
                            tokenizer.TryReadFloatFractional ();
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
                    }

                    retToken.Text = tokenizer.data.Slice (startPos, tokenizer.curPos - startPos);
                } else if (peekedChars [0] == '.' && peekedChars.Length >= 2 && IsIntegerDigit (peekedChars [1])) {
                    int startPos = tokenizer.curPos;

                    tokenizer.TryReadFloatFractional ();
                    tokenizer.TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                    tokenizer.TryReadFloatSuffix ();

                    retToken.Type = EchelonScriptTokenType.FloatLiteral;
                    retToken.Text = tokenizer.data.Slice (startPos, tokenizer.curPos - startPos);
                }

                return true;
            }
        }

        public class IdentifierTokenParser : TokenParser {
            public override int MinimumStartPeek => 1;
            public override int RequestedStartPeek => 1;

            public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) {
                return peekedChars [0] == '_' || IsLatinLetter (peekedChars [0]);
            }

            public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
                int startPos = tokenizer.curPos;
                int len = tokenizer.ReadStringWhile ((c) => c == '_' || IsIntegerDigit (c) || IsLatinLetter (c));

                retToken.Type = EchelonScriptTokenType.Identifier;
                retToken.Text = tokenizer.data.Slice (startPos, len);

                return true;
            }
        }

        public class StringLiteralTokenParser : TokenParser {
            public override int MinimumStartPeek => 1;
            public override int RequestedStartPeek => 2;

            public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) {
                return peekedChars [0] == '"' || (peekedChars.Length >= 2 && peekedChars.StartsWith ("@\""));
            }

            public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
                int startPos = tokenizer.curPos;
                bool verbatim = false;

                if (peekedChars [0] == '@') {
                    tokenizer.ReadChar ();

                    verbatim = true;
                    retToken.Type = EchelonScriptTokenType.VerbatimStringLiteral;
                } else
                    retToken.Type = EchelonScriptTokenType.RegularStringLiteral;

                tokenizer.ReadChar ();

                while (true) {
                    var c = tokenizer.ReadUntil (ch => ch == '\\' || ch == '"' || ch == '\r' || ch == '\n', false);
                    int stopPos = tokenizer.curPos;
                    int stopLine = tokenizer.curLine;
                    int stopColumn = CalcColumn (tokenizer.curLineStartPos, stopPos);
                    tokenizer.ReadChar ();

                    if (c == '"')
                        break;
                    else if (c == null) {
                        tokenizer.Errors.Add (new EchelonScriptErrorMessage ("Unclosed string literal.", startPos, tokenizer.curPos - startPos, retToken.TextLine, retToken.TextColumn));
                        break;
                    } else if (c == '\\') {
                        if (!verbatim && !tokenizer.ReadEscapeSequence ())
                            tokenizer.Errors.Add (new EchelonScriptErrorMessage ("Unrecognized escape sequence.", stopPos, 2, stopLine, stopColumn));
                        if (verbatim && tokenizer.PeekChar () == '"')
                            tokenizer.ReadChar ();
                    } else if (c == '\r' && !verbatim)
                        tokenizer.Errors.Add (new EchelonScriptErrorMessage ("Carriage return characters are not allowed in regular strings.", stopPos, 1, stopLine, stopColumn));
                    else if (c == '\n' && !verbatim)
                        tokenizer.Errors.Add (new EchelonScriptErrorMessage ("Newline characters are not allowed in regular strings.", stopPos, 1, stopLine, stopColumn));
                }

                retToken.Text = tokenizer.data.Slice (startPos, tokenizer.curPos - startPos);

                return true;
            }
        }

        public class CharLiteralTokenParser : TokenParser {
            public override int MinimumStartPeek => 1;
            public override int RequestedStartPeek => 1;

            public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) {
                return peekedChars [0] == '\'';
            }

            public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
                int startPos = tokenizer.curPos;
                retToken.Type = EchelonScriptTokenType.CharacterLiteral;

                tokenizer.ReadChar ();

                int charCount = 0;
                while (true) {
                    var c = tokenizer.ReadUntil (ch => ch == '\\' || ch == '\'' || ch == '\r' || ch == '\n', false);
                    int stopPos = tokenizer.curPos;
                    int stopLine = tokenizer.curLine;
                    int stopColumn = CalcColumn (tokenizer.curLineStartPos, stopPos);
                    tokenizer.ReadChar ();

                    if (c == '\'')
                        break;
                    else if (c == null) {
                        tokenizer.Errors.Add (new EchelonScriptErrorMessage ("Unclosed character literal.", startPos, tokenizer.curPos - startPos, retToken.TextLine, retToken.TextColumn));
                        break;
                    } else if (c == '\\' && !tokenizer.ReadEscapeSequence ())
                        tokenizer.Errors.Add (new EchelonScriptErrorMessage ("Unrecognized escape sequence.", stopPos, 2, stopLine, stopColumn));
                    else if (c == '\r')
                        tokenizer.Errors.Add (new EchelonScriptErrorMessage ("Carriage return characters are not allowed in character literals.", stopPos, 1, stopLine, stopColumn));
                    else if (c == '\n')
                        tokenizer.Errors.Add (new EchelonScriptErrorMessage ("Newline characters are not allowed in character literals.", stopPos, 1, stopLine, stopColumn));

                    charCount++;
                }

                retToken.Text = tokenizer.data.Slice (startPos, tokenizer.curPos - startPos);

                if (charCount > 1)
                    tokenizer.Errors.Add (new EchelonScriptErrorMessage (retToken, "Too many characters in character literal."));

                return true;
            }
        }

        public class MiscTokenParser : TokenParser {
            protected static readonly int tokenStringsMaxLength;
            protected static readonly List<(EchelonScriptTokenType Tok, string Str)> tokenStrings;

            static MiscTokenParser () {
                tokenStrings = new List<(EchelonScriptTokenType, string)> {
                    (EchelonScriptTokenType.Dot, "."),
                    (EchelonScriptTokenType.DotDot, ".."),

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

                    (EchelonScriptTokenType.AndEq, "&="),
                    (EchelonScriptTokenType.BitOrEq, "|="),
                    (EchelonScriptTokenType.XorEq, "^="),
                    (EchelonScriptTokenType.TildeEq, "~="),
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
                tokenStringsMaxLength = tokenStrings [0].Str.Length;
            }

            public override int MinimumStartPeek => tokenParsersMaxLength;
            public override int RequestedStartPeek => tokenParsersMaxLength;

            public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) {
                return true;
            }

            public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
                foreach (var tk in tokenStrings) {
                    if (peekedChars.Length < tk.Str.Length)
                        continue;

                    if (peekedChars.StartsWith (tk.Str)) {
                        tokenizer.ReadChars (tk.Str.Length);
                        retToken.Type = tk.Tok;
                        retToken.Text = tokenizer.data.Slice (retToken.TextStartPos, tokenizer.curPos - retToken.TextStartPos);

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

        #region ================== Instance fields

        protected int curPos;
        protected ReadOnlyMemory<char> data;
        protected (EchelonScriptToken, EchelonScriptToken?)? currentToken;

        protected int curLine;
        protected int curLineStartPos;

        protected static readonly List<TokenParser> tokenParsers;
        protected static readonly int tokenParsersMaxLength;

        #endregion

        #region ================== Instance properties

        public List<EchelonScriptErrorMessage> Errors { get; protected set; }

        #endregion

        #region ================== Constructors

        static EchelonScriptTokenizer () {
            tokenParsers = new List<TokenParser> ();

            tokenParsers.Add (new HexTokenParser ());
            tokenParsers.Add (new BinaryTokenParser ());
            tokenParsers.Add (new DecimalTokenParser ());
            tokenParsers.Add (new IdentifierTokenParser ());
            tokenParsers.Add (new StringLiteralTokenParser ());
            tokenParsers.Add (new CharLiteralTokenParser ());
            tokenParsers.Add (new MiscTokenParser ());

            tokenParsersMaxLength = 1;
            foreach (var parser in tokenParsers)
                tokenParsersMaxLength = Math.Max (Math.Max (tokenParsersMaxLength, parser.MinimumStartPeek), parser.RequestedStartPeek);
        }

        public EchelonScriptTokenizer (List<EchelonScriptErrorMessage> errorsList) {
            curPos = 0;
            data = null;
            currentToken = null;

            Errors = errorsList;
        }

        #endregion

        #region ================== Instance methods

        #region Public methods

        /// <summary>Resets the tokenizer to its initial state.</summary>
        public void Reset () {
            curPos = 0;
            curLine = 1;
            curLineStartPos = 0;
            currentToken = null;
        }

        /// <summary>Sets the source data for the input text.</summary>
        /// <param name="newData">The new source data.</param>
        public void SetSource (ReadOnlyMemory<char> newData) {
            Reset ();
            data = newData;
        }

        /// <summary>Peeks a token from the input text.</summary>
        /// <returns>The next token in the input text.</returns>
        public (EchelonScriptToken tk, EchelonScriptToken? doc) PeekNextToken () {
            if (currentToken != null)
                return currentToken.Value;

            currentToken = NextToken ();
            return currentToken.Value;
        }

        /// <summary>Reads a token from the input text.</summary>
        /// <returns>The next token in the input text.</returns>
        public (EchelonScriptToken tk, EchelonScriptToken? doc) NextToken () {
            if (currentToken != null) {
                var curToken = currentToken.Value;
                currentToken = null;
                return curToken;
            }

            var docTk = SkipWhitespace ();
            var retToken = new EchelonScriptToken ();
            retToken.Type = EchelonScriptTokenType.Invalid;
            retToken.Text = null;
            retToken.TextStartPos = curPos;
            retToken.TextLine = curLine;
            retToken.TextColumn = CalcColumn (curLineStartPos, curPos);

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
                retToken.Text = data.Slice (retToken.TextStartPos, curPos - retToken.TextStartPos);
            }

            return (retToken, docTk);
        }

        #endregion

        #region Protected methods

        protected static int CalcColumn (int columnStart, int textPos) {
            return textPos - columnStart + 1;
        }

        #region Character reading

        protected char? PeekChar () {
            if (curPos >= data.Length)
                return null;

            return data.Span [curPos];
        }

        protected char? PeekChar (int offset) {
            if ((curPos + offset) >= data.Length)
                return null;

            return data.Span [curPos + offset];
        }

        protected ReadOnlySpan<char> PeekChars (int count) {
            count = Math.Min (count, data.Length - curPos);
            return data.Span.Slice (curPos, count);
        }

        protected ReadOnlySpan<char> PeekChars (int offset, int count) {
            int startPos = Math.Min (data.Length - 1, curPos + offset);
            count = Math.Min (count, data.Length - curPos);

            return data.Span.Slice (startPos, count);
        }

        protected char? ReadChar () {
            if (curPos >= data.Length)
                return null;

            var ret = data.Span [curPos];
            curPos++;

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
                            int docStartPos = curPos;
                            int docLine = curLine;
                            int docColumn = CalcColumn (curLineStartPos, docStartPos);
                            ReadChars (2);

                            while (true) {
                                c = ReadUntil (ch => ch == '+', true);
                                if (c == null)
                                    break;

                                if (PeekChar () == '/') {
                                    ReadChar ();
                                    int docEndPos = curPos;

                                    var docCom = new EchelonScriptToken ();
                                    docCom.Type = EchelonScriptTokenType.DocComment;
                                    docCom.Text = data.Slice (docStartPos, docEndPos - docStartPos);
                                    docCom.TextStartPos = docStartPos;
                                    docCom.TextLine = docLine;
                                    docCom.TextColumn = docColumn;
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
            var c = PeekChar ();

            while (c != null) {
                if (func (c.Value)) {
                    if (readReq)
                        ReadChar ();
                    return c;
                }

                ReadChar ();
                c = PeekChar ();
            }

            return null;
        }

        protected int ReadStringWhile (Predicate<char> func) {
            var c = PeekChar ();
            int length = 0;

            while (c != null && func (c.Value)) {
                ReadChar ();
                length++;
                c = PeekChar ();
            }

            return length;
        }

        #endregion

        #region Common pattern matching

        protected static bool IsLatinLetter (char? c) {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
        }

        protected static bool IsIntegerDigit (char? c) {
            return c >= '0' && c <= '9';
        }

        protected static bool IsHexDigit (char? c) {
            if (IsIntegerDigit (c))
                return true;

            return (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
        }

        protected static bool IsBinaryDigit (char? c) {
            return c == '0' || c == '1';
        }

        protected static bool IsFloatSuffix (char? c) {
            switch (c) {
                case 'f':
                case 'F':
                case 'd':
                case 'D':
                    return true;

                default:
                    return false;
            }
        }

        #endregion

        #region Common pattern reading

        protected bool ReadEscapeSequence () {
            bool ReadRequiredChars (int count, Predicate<char> isValid) {
                int validCharsCount = 0;

                for (int i = 0; i < count; i++) {
                    var c = ReadChar ();
                    if (c != null && IsHexDigit (c.Value))
                        validCharsCount++;
                }

                return validCharsCount == count;
            }

            char? c = ReadChar ();
            if (c == null)
                return false;

            switch (c) {
                case '\'':
                case '"':
                case '\\':
                case '0':
                case 'a':
                case 'b':
                case 'f':
                case 'n':
                case 'r':
                case 't':
                case 'v':
                    return true;

                case 'x':
                    return ReadRequiredChars (2, c => IsHexDigit (c));
                case 'u':
                    return ReadRequiredChars (4, c => IsHexDigit (c));
                case 'U':
                    return ReadRequiredChars (8, c => IsHexDigit (c));
            }

            return false;
        }

        protected bool TryReadIntSuffix () {
            var peekedChars = PeekChars (2);

            if (peekedChars.Length < 1)
                return false;

            if (peekedChars [0] == 'L') {
                ReadChar ();
                return true;
            } else if (peekedChars [0] == 'u' || peekedChars [0] == 'U') {
                ReadChar ();

                if (peekedChars.Length >= 2 && peekedChars [1] == 'L')
                    ReadChar ();

                return true;
            }

            return false;
        }

        protected void TryReadFloatExponent (int line, int column, int startPos) {
            var c = PeekChar ();
            if (c == 'e' || c == 'E') {
                ReadChar ();

                bool hasSign = false;
                c = PeekChar ();
                if (c == '+' || c == '-') {
                    ReadChar ();
                    hasSign = true;
                }

                if (!IsIntegerDigit (PeekChar ())) {
                    Errors.Add (new EchelonScriptErrorMessage ("Invalid float literal.", startPos, curPos - startPos, line, column));
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
            if (PeekChar () == '.') {
                ReadChar ();

                if (PeekChar () != NumberSeparator)
                    ReadStringWhile ((c) => IsIntegerDigit (c) || c == NumberSeparator);

                return true;
            }

            return false;
        }

        #endregion

        #endregion

        #endregion
    }
}