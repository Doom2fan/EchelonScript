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
    public enum EchelonScriptTokenType {
        Invalid = -1,

        EOF = 0,
        DocComment,

        Dot,
        DotDot,

        AndAnd,
        OrOr,

        Bang,
        Plus,
        Minus,
        Asterisk,
        Divide,
        Modulo,
        PowerOp,
        PlusPlus,
        MinusMinus,

        And,
        BitOr,
        ShiftLeft,
        ShiftRight,
        ShiftRightU,

        Question,
        Colon,

        LesserThan,
        GreaterThan,
        LesserThanEq,
        GreaterThanEq,

        Equals,
        PlusEq,
        MinusEq,
        MultiplyEq,
        DivideEq,
        ModuloEq,
        PowerOpEq,

        AndEq,
        BitOrEq,
        ShiftLeftEq,
        ShiftRightEq,
        ShiftRightUEq,

        EqualsEquals,
        NotEquals,

        ParenOpen,
        ParenClose,
        BracketOpen,
        BracketClose,

        BraceOpen,
        BraceClose,

        Comma,
        Semicolon,

        Identifier,

        RegularStringLiteral,
        VerbatimStringLiteral,

        CharacterLiteral,

        DecIntegerLiteral,
        HexIntegerLiteral,
        BinIntegerLiteral,
        FloatLiteral,
    }

    public struct EchelonScriptToken {
        public EchelonScriptTokenType Type;
        public int TextStartPos;
        public int TextLine;
        public int TextColumn;

        public ReadOnlyMemory<char> Text;
    }

    public struct EchelonScriptErrorMessage {
        public string Message { get; }

        public int StartPos { get; }
        public int Length { get; }

        public int Line { get; }
        public int Column { get; }

        public EchelonScriptErrorMessage (EchelonScriptToken tk, string message = null) {
            Message = message;

            StartPos = tk.TextStartPos;
            Length = tk.Text.Length;

            Line = tk.TextLine;
            Column = tk.TextColumn;
        }

        public EchelonScriptErrorMessage (string message, int startPos, int length, int line, int column) {
            Message = message;

            StartPos = startPos;
            Length = length;

            Line = line;
            Column = column;
        }
    }

    public class EchelonScriptTokenizer {
        #region ================== Constants

        public const char NumberSeparator = '\'';

        #endregion

        #region ================== Instance fields

        protected int curPos;
        protected ReadOnlyMemory<char> data;
        protected (EchelonScriptToken, EchelonScriptToken?)? currentToken;

        protected int curLine;
        protected int curLineStartPos;

        protected static readonly List<(EchelonScriptTokenType Tok, string Str)> tokenStrings;
        protected static readonly int tokenStringsMaxLength;

        #endregion

        #region ================== Instance properties

        public List<EchelonScriptErrorMessage> Errors { get; protected set; }

        #endregion

        #region ================== Constructors

        static EchelonScriptTokenizer () {
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
                (EchelonScriptTokenType.ShiftLeft, "<<"),
                (EchelonScriptTokenType.ShiftRight, ">>"),
                (EchelonScriptTokenType.ShiftRightU, ">>>"),

                (EchelonScriptTokenType.Question, "?"),
                (EchelonScriptTokenType.Colon, ":"),

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

                (EchelonScriptTokenType.Comma, ","),
                (EchelonScriptTokenType.Semicolon, ";")
            };

            tokenStrings.Sort ((x, y) => -x.Str.Length.CompareTo (y.Str.Length));

            tokenStringsMaxLength = 4;
            foreach (var token in tokenStrings)
                tokenStringsMaxLength = Math.Max (tokenStringsMaxLength, token.Str.Length);
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

            var peekedChars = PeekChars (tokenStringsMaxLength);

            if (peekedChars.StartsWith ("0x", StringComparison.InvariantCultureIgnoreCase)) {
                int startPos = curPos;
                ReadChars (2);

                if (PeekChar () == NumberSeparator)
                    Errors.Add (new EchelonScriptErrorMessage (retToken, "Invalid hex literal."));
                else
                    ReadStringWhile (c => IsHexDigit (c) || c == NumberSeparator);

                TryReadIntSuffix ();

                retToken.Type = EchelonScriptTokenType.HexIntegerLiteral;
                retToken.Text = data.Slice (startPos, curPos - startPos);
            } else if (peekedChars.StartsWith ("0b", StringComparison.InvariantCultureIgnoreCase)) {
                int startPos = curPos;
                ReadChars (2);

                if (PeekChar () == NumberSeparator)
                    Errors.Add (new EchelonScriptErrorMessage (retToken, "Invalid binary literal."));
                else
                    ReadStringWhile (c => IsBinaryDigit (c) || c == NumberSeparator);

                TryReadIntSuffix ();

                retToken.Type = EchelonScriptTokenType.BinIntegerLiteral;
                retToken.Text = data.Slice (startPos, curPos - startPos);
            } else if (peekedChars [0] == '.' && peekedChars.Length >= 2 && IsIntegerDigit (peekedChars [1])) {
                int startPos = curPos;

                TryReadFloatFractional ();
                TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                TryReadFloatSuffix ();

                retToken.Type = EchelonScriptTokenType.FloatLiteral;
                retToken.Text = data.Slice (startPos, curPos - startPos);
            } else if (peekedChars [0] == '0') {
                int startPos = curPos;

                ReadChar ();
                if (PeekChar () == '.') {
                    TryReadFloatFractional ();
                    TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                    TryReadFloatSuffix ();

                    retToken.Type = EchelonScriptTokenType.FloatLiteral;
                } else {
                    TryReadIntSuffix ();
                    retToken.Type = EchelonScriptTokenType.DecIntegerLiteral;
                }

                retToken.Text = data.Slice (startPos, curPos - startPos);
            } else if (IsIntegerDigit (peekedChars [0])) {
                int startPos = curPos;

                ReadStringWhile ((c) => c == '\'' || IsIntegerDigit (c));

                var c = PeekChar ();
                if (c != null) {
                    if (c == '.') {
                        TryReadFloatFractional ();
                        TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                        TryReadFloatSuffix ();

                        retToken.Type = EchelonScriptTokenType.FloatLiteral;
                    } else if (c == 'e' || c == 'E') {
                        TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                        TryReadFloatSuffix ();

                        retToken.Type = EchelonScriptTokenType.FloatLiteral;
                    } else if (IsFloatSuffix (c.Value)) {
                        ReadChar ();
                        retToken.Type = EchelonScriptTokenType.FloatLiteral;
                    } else {
                        TryReadIntSuffix ();

                        retToken.Type = EchelonScriptTokenType.DecIntegerLiteral;
                    }
                }
                retToken.Text = data.Slice (startPos, curPos - startPos);
            } else if (peekedChars [0] == '_' || IsLatinLetter (peekedChars [0])) {
                int startPos = curPos;
                int len = ReadStringWhile ((c) => c == '_' || IsIntegerDigit (c) || IsLatinLetter (c));

                retToken.Type = EchelonScriptTokenType.Identifier;
                retToken.Text = data.Slice (startPos, len);
            } else if (peekedChars [0] == '"' || (peekedChars.Length >= 2 && peekedChars.StartsWith ("@\""))) {
                int startPos = curPos;
                bool verbatim = false;

                if (peekedChars [0] == '@') {
                    ReadChar ();

                    verbatim = true;
                    retToken.Type = EchelonScriptTokenType.VerbatimStringLiteral;
                } else
                    retToken.Type = EchelonScriptTokenType.RegularStringLiteral;

                ReadChar ();

                while (true) {
                    var c = ReadUntil (ch => ch == '\\' || ch == '"' || ch == '\r' || ch == '\n', false);
                    int stopPos = curPos;
                    int stopLine = curLine;
                    int stopColumn = CalcColumn (curLineStartPos, stopPos);
                    ReadChar ();

                    if (c == '"')
                        break;
                    else if (c == null) {
                        Errors.Add (new EchelonScriptErrorMessage ("Unclosed string literal.", startPos, curPos - startPos, retToken.TextLine, retToken.TextColumn));
                        break;
                    } else if (c == '\\') {
                        if (!verbatim && !ReadEscapeSequence ())
                            Errors.Add (new EchelonScriptErrorMessage ("Unrecognized escape sequence.", stopPos, 2, stopLine, stopColumn));
                        if (verbatim && PeekChar () == '"')
                            ReadChar ();
                    } else if (c == '\r' && !verbatim)
                        Errors.Add (new EchelonScriptErrorMessage ("Carriage return characters are not allowed in regular strings.", stopPos, 1, stopLine, stopColumn));
                    else if (c == '\n' && !verbatim)
                        Errors.Add (new EchelonScriptErrorMessage ("Newline characters are not allowed in regular strings.", stopPos, 1, stopLine, stopColumn));
                }

                retToken.Text = data.Slice (startPos, curPos - startPos);
            } else if (peekedChars [0] == '\'') {
                int startPos = curPos;
                retToken.Type = EchelonScriptTokenType.CharacterLiteral;

                ReadChar ();

                int charCount = 0;
                while (true) {
                    var c = ReadUntil (ch => ch == '\\' || ch == '\'' || ch == '\r' || ch == '\n', false);
                    int stopPos = curPos;
                    int stopLine = curLine;
                    int stopColumn = CalcColumn (curLineStartPos, stopPos);
                    ReadChar ();

                    if (c == '\'')
                        break;
                    else if (c == null) {
                        Errors.Add (new EchelonScriptErrorMessage ("Unclosed character literal.", startPos, curPos - startPos, retToken.TextLine, retToken.TextColumn));
                        break;
                    } else if (c == '\\' && !ReadEscapeSequence ())
                        Errors.Add (new EchelonScriptErrorMessage ("Unrecognized escape sequence.", stopPos, 2, stopLine, stopColumn));
                    else if (c == '\r')
                        Errors.Add (new EchelonScriptErrorMessage ("Carriage return characters are not allowed in character literals.", stopPos, 1, stopLine, stopColumn));
                    else if (c == '\n')
                        Errors.Add (new EchelonScriptErrorMessage ("Newline characters are not allowed in character literals.", stopPos, 1, stopLine, stopColumn));

                    charCount++;
                }

                retToken.Text = data.Slice (startPos, curPos - startPos);

                if (charCount > 1)
                    Errors.Add (new EchelonScriptErrorMessage (retToken, "Too many characters in character literal."));
            } else {
                foreach (var tk in tokenStrings) {
                    if (peekedChars.Length < tk.Str.Length)
                        continue;

                    if (peekedChars.StartsWith (tk.Str)) {
                        ReadChars (tk.Str.Length);
                        retToken.Type = tk.Tok;
                        retToken.Text = data.Slice (retToken.TextStartPos, curPos - retToken.TextStartPos);

                        return (retToken, docTk);
                    }
                }
            }

            if (retToken.Type == EchelonScriptTokenType.Invalid) {
                ReadChar ();
                retToken.Text = data.Slice (retToken.TextStartPos, curPos - retToken.TextStartPos);
            }

            return (retToken, docTk);
        }

        #endregion

        #region Protected methods

        protected int CalcColumn (int columnStart, int textPos) {
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

        protected bool IsLatinLetter (char? c) {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
        }

        protected bool IsIntegerDigit (char? c) {
            return c >= '0' && c <= '9';
        }

        protected bool IsHexDigit (char? c) {
            if (IsIntegerDigit (c))
                return true;

            return (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
        }

        protected bool IsBinaryDigit (char? c) {
            return c == '0' || c == '1';
        }

        protected bool IsFloatSuffix (char? c) {
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