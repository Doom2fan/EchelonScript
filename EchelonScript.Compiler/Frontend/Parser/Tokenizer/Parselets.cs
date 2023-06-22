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
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Frontend.Parser.Internal;

namespace EchelonScript.Compiler.Frontend.Parser.Tokenizer;

public ref partial struct ES_Tokenizer {
    public abstract class TokenParser {
        public abstract int MinimumStartPeek { get; }
        public abstract int RequestedStartPeek { get; }

        public abstract bool IsStartValid (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars);
        public abstract bool ParseToken (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref ES_Token retToken);
    }

    public class HexTokenParser : TokenParser {
        public override int MinimumStartPeek => 2;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars.StartsWith ("0x", StringComparison.InvariantCultureIgnoreCase);

        public override bool ParseToken (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref ES_Token retToken) {
            var startPos = tokenizer.curPos;
            tokenizer.ReadChars (2);

            var reportError = !tokenizer.TryReadNumber (IsHexDigit);
            tokenizer.TryReadIntSuffix ();

            var endPos = tokenizer.curPos;
            if (reportError)
                tokenizer.Diag (DiagnosticDescriptors.InvalidNumber, startPos, endPos);

            retToken = retToken with {
                Type = ES_TokenType.HexIntegerLiteral,
                TextSpan = tokenizer.GetSpan (startPos, endPos),
            };

            return true;
        }
    }

    public class BinaryTokenParser : TokenParser {
        public override int MinimumStartPeek => 2;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars.StartsWith ("0b", StringComparison.InvariantCultureIgnoreCase);

        public override bool ParseToken (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref ES_Token retToken) {
            var startPos = tokenizer.curPos;
            tokenizer.ReadChars (2);

            var reportError = !tokenizer.TryReadNumber (IsBinaryDigit);
            tokenizer.TryReadIntSuffix ();

            var endPos = tokenizer.curPos;
            if (reportError)
                tokenizer.Diag (DiagnosticDescriptors.InvalidNumber, startPos, endPos);

            retToken = retToken with {
                Type = ES_TokenType.BinIntegerLiteral,
                TextSpan = tokenizer.GetSpan (startPos, endPos),
            };

            return true;
        }
    }

    public class DecimalTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 1;

        public override bool IsStartValid (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => IsIntegerDigit (peekedChars [0]);

        public override bool ParseToken (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref ES_Token retToken) {
            var startPos = tokenizer.curPos;
            var isInvalid = false;
            ES_TokenType tokenType;

            isInvalid |= !tokenizer.TryReadNumber (IsIntegerDigit);

            if (tokenizer.PeekChar () == null || tokenizer.TryReadIntSuffix ())
                tokenType = ES_TokenType.DecIntegerLiteral;
            else if (tokenizer.TryReadFloatSuffix ())
                tokenType = ES_TokenType.FloatLiteral;
            else if (IsExponentStart (tokenizer.PeekChar ())) {
                isInvalid |= tokenizer.TryReadFloatExponent () == false;
                tokenizer.TryReadFloatSuffix ();

                tokenType = ES_TokenType.FloatLiteral;
            } else if (tokenizer.TryReadFloatFractional () is bool fractionValid) {
                isInvalid |= !fractionValid;
                isInvalid |= tokenizer.TryReadFloatExponent () == false;
                tokenizer.TryReadFloatSuffix ();

                tokenType = ES_TokenType.FloatLiteral;
            } else
                tokenType = ES_TokenType.DecIntegerLiteral;

            var endPos = tokenizer.curPos;
            if (isInvalid)
                tokenizer.Diag (DiagnosticDescriptors.InvalidNumber, startPos, endPos);

            retToken = retToken with {
                Type = tokenType,
                TextSpan = tokenizer.GetSpan (startPos, endPos),
            };

            return true;
        }
    }

    public class FloatTokenParser : TokenParser {
        public override int MinimumStartPeek => 2;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars [0] == '.' && (IsIntegerDigit (peekedChars [1]) || peekedChars [1] == NumberSeparator);

        public override bool ParseToken (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref ES_Token retToken) {
            var startPos = tokenizer.curPos;
            var isInvalid = false;

            isInvalid |= tokenizer.TryReadFloatFractional () == false;
            isInvalid |= tokenizer.TryReadFloatExponent () == false;
            tokenizer.TryReadFloatSuffix ();

            var endPos = tokenizer.curPos;
            if (isInvalid)
                tokenizer.Diag (DiagnosticDescriptors.InvalidNumber, startPos, endPos);

            retToken = retToken with {
                Type = ES_TokenType.FloatLiteral,
                TextSpan = tokenizer.GetSpan (startPos, endPos),
            };

            return true;
        }
    }

    public class IdentifierTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 1;

        public override bool IsStartValid (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars [0] == '_' || IsLatinLetter (peekedChars [0]);

        public override bool ParseToken (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref ES_Token retToken) {
            var startPos = tokenizer.curPos;
            tokenizer.ReadStringWhile ((c) => c == '_' || IsIntegerDigit (c) || IsLatinLetter (c));

            retToken = retToken with {
                Type = ES_TokenType.Identifier,
                TextSpan = tokenizer.GetSpan (startPos, tokenizer.curPos),
            };

            return true;
        }
    }

    public class StringLiteralTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars [0] == '"' || peekedChars.Length >= 2 && peekedChars.StartsWith ("@\"");

        public override bool ParseToken (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref ES_Token retToken) {
            var startPos = tokenizer.curPos;
            var verbatim = false;

            if (peekedChars [0] == '@') {
                tokenizer.ReadChar ();

                verbatim = true;
            }

            tokenizer.ReadChar ();

            var unclosed = false;
            var newlineInConst = false;
            while (true) {
                var c = tokenizer.ReadChar ();

                if (c == null) {
                    unclosed = true;
                    break;
                } else if (c == '"') {
                    if (verbatim && tokenizer.PeekChar () == '"')
                        tokenizer.ReadChar ();
                    else
                        break;
                } else if (!verbatim && c == '\\' && tokenizer.PeekChar () is '"' or '\\')
                    tokenizer.ReadChar ();
                else if (!verbatim && (c == '\r' || c == '\n'))
                    newlineInConst = true;
            }

            var endPos = tokenizer.curPos;
            if (unclosed)
                tokenizer.Diag (DiagnosticDescriptors.UnclosedStringLiteral, startPos, tokenizer.curPos);
            else if (!verbatim) {
                var textSpan = tokenizer.text [(startPos + 1)..(endPos - 1)];
                for (var i = 0; i < textSpan.Length; i++) {
                    if (textSpan [i] != '\\')
                        continue;

                    if (textSpan.Length - i < 2)
                        continue;

                    var escapeStart = startPos + i + 1;
                    if (!TryDecodeStringEscapeCode (textSpan [(i + 1)..], out _, out var escapeLength))
                        tokenizer.Diag (DiagnosticDescriptors.UnrecognizedEscape, escapeStart, escapeStart + escapeLength + 1);

                    i += escapeLength;
                }

                if (newlineInConst)
                    tokenizer.Diag (DiagnosticDescriptors.NewlineInConstant, startPos, endPos);
            }

            retToken = retToken with {
                Type = !verbatim ? ES_TokenType.RegularStringLiteral : ES_TokenType.VerbatimStringLiteral,
                TextSpan = tokenizer.GetSpan (startPos, endPos),
            };

            return true;
        }
    }

    public class CharLiteralTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 1;

        public override bool IsStartValid (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars [0] == '\'';

        public override bool ParseToken (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref ES_Token retToken) {
            var startPos = tokenizer.curPos;
            tokenizer.ReadChar ();

            var unclosed = false;
            var newlineInConst = false;
            while (true) {
                var c = tokenizer.ReadChar ();

                if (c == null) {
                    unclosed = true;
                    break;
                } else if (c == '\'')
                        break;
                else if (c == '\\' && tokenizer.PeekChar () is '\'' or '\\')
                    tokenizer.ReadChar ();
                else if (c == '\r' || c == '\n')
                    newlineInConst = true;
            }

            var endPos = tokenizer.curPos;
            if (unclosed)
                tokenizer.Diag (DiagnosticDescriptors.UnclosedCharLiteral, startPos, tokenizer.curPos);
            else {
                var textSpan = tokenizer.text [(startPos + 1)..(endPos - 1)];
                if (textSpan.Length < 1)
                    tokenizer.Diag (DiagnosticDescriptors.EmptyCharLiteral, startPos, endPos);
                else if (textSpan [0] == '\\') {
                    if (!TryDecodeStringEscapeCode (textSpan [1..], out _, out var escapeLength, charLit: true))
                        tokenizer.Diag (DiagnosticDescriptors.UnrecognizedEscape, startPos + 1, startPos + escapeLength + 1);
                    else if (escapeLength + 1 < textSpan.Length)
                        tokenizer.Diag (DiagnosticDescriptors.TooLongCharLiteral, startPos, endPos);
                } else if (textSpan.Length > 1)
                    tokenizer.Diag (DiagnosticDescriptors.TooLongCharLiteral, startPos, endPos);

                if (newlineInConst)
                    tokenizer.Diag (DiagnosticDescriptors.NewlineInConstant, startPos, endPos);
            }

            retToken = retToken with {
                Type = ES_TokenType.CharacterLiteral,
                TextSpan = tokenizer.GetSpan (startPos, endPos),
            };

            return true;
        }
    }

    public class MiscTokenParser : TokenParser {
        protected static readonly int tokenStringsMinLength;
        protected static readonly int tokenStringsMaxLength;
        protected static readonly List<(ES_TokenType Type, string Text)> tokenStrings;

        static MiscTokenParser () {
            tokenStrings = new List<(ES_TokenType, string)> {
                (ES_TokenType.Dot, "."),
                (ES_TokenType.DotDot, ".."),
                (ES_TokenType.NamespaceOp, "::"),

                (ES_TokenType.AndAnd, "&&"),
                (ES_TokenType.OrOr, "||"),

                (ES_TokenType.Bang, "!"),
                (ES_TokenType.Plus, "+"),
                (ES_TokenType.Minus, "-"),
                (ES_TokenType.Asterisk, "*"),
                (ES_TokenType.Divide, "/"),
                (ES_TokenType.Modulo, "%"),
                (ES_TokenType.PowerOp, "**"),
                (ES_TokenType.PlusPlus, "++"),
                (ES_TokenType.MinusMinus, "--"),

                (ES_TokenType.And, "&"),
                (ES_TokenType.BitOr, "|"),
                (ES_TokenType.Xor, "^"),
                (ES_TokenType.Tilde, "~"),
                (ES_TokenType.ShiftLeft, "<<"),
                (ES_TokenType.ShiftRight, ">>"),
                (ES_TokenType.ShiftRightU, ">>>"),

                (ES_TokenType.LesserThan, "<"),
                (ES_TokenType.GreaterThan, ">"),
                (ES_TokenType.LesserThanEq, "<="),
                (ES_TokenType.GreaterThanEq, ">="),

                (ES_TokenType.Equals, "="),
                (ES_TokenType.PlusEq, "+="),
                (ES_TokenType.MinusEq, "-="),
                (ES_TokenType.MultiplyEq, "*="),
                (ES_TokenType.DivideEq, "/="),
                (ES_TokenType.ModuloEq, "%="),
                (ES_TokenType.PowerOpEq, "**="),
                (ES_TokenType.ConcatEq, "..="),

                (ES_TokenType.AndEq, "&="),
                (ES_TokenType.BitOrEq, "|="),
                (ES_TokenType.XorEq, "^="),
                (ES_TokenType.ShiftLeftEq, "<<="),
                (ES_TokenType.ShiftRightEq, ">>="),
                (ES_TokenType.ShiftRightUEq, ">>>="),

                (ES_TokenType.EqualsEquals, "=="),
                (ES_TokenType.NotEquals, "!="),

                (ES_TokenType.ParenOpen, "("),
                (ES_TokenType.ParenClose, ")"),
                (ES_TokenType.BracketOpen, "["),
                (ES_TokenType.BracketClose, "]"),
                (ES_TokenType.BraceOpen, "{"),
                (ES_TokenType.BraceClose, "}"),

                (ES_TokenType.Question, "?"),
                (ES_TokenType.Colon, ":"),
                (ES_TokenType.Comma, ","),
                (ES_TokenType.Semicolon, ";"),
                (ES_TokenType.LambdaArrow, "=>"),
            };

            tokenStrings.Sort ((x, y) => -x.Text.Length.CompareTo (y.Text.Length));
            tokenStringsMinLength = tokenStrings [^1].Text.Length;
            tokenStringsMaxLength = tokenStrings [0].Text.Length;
        }

        public override int MinimumStartPeek => tokenStringsMinLength;
        public override int RequestedStartPeek => tokenStringsMaxLength;

        public override bool IsStartValid (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars) => true;

        public override bool ParseToken (ref ES_Tokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref ES_Token retToken) {
            foreach (var tk in tokenStrings) {
                if (peekedChars.Length < tk.Text.Length)
                    continue;

                if (peekedChars.StartsWith (tk.Text)) {
                    var startPos = tokenizer.curPos;

                    tokenizer.ReadChars (tk.Text.Length);
                    retToken = retToken with {
                        Type = tk.Type,
                        TextSpan = tokenizer.GetSpan (startPos, tokenizer.curPos),
                    };

                    return true;
                }
            }

            return false;
        }
    }
}
