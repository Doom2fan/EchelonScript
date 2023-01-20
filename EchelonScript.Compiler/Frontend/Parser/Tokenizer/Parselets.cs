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
using EchelonScript.Compiler.Data;

namespace EchelonScript.Compiler.Frontend.Parser.Tokenizer;

public partial class EchelonScriptTokenizer {
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
            //TODO: FIXME
            //retToken.Text = tokenizer.data [startPos..tokenizer.curPos];

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
            //TODO: FIXME
            //retToken.Text = tokenizer.data [startPos..tokenizer.curPos];

            return true;
        }
    }

    public class DecimalTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) {
            return
                peekedChars [0] == '0' ||
                IsIntegerDigit (peekedChars [0]) ||
                peekedChars [0] == '.' && peekedChars.Length >= 2 && IsIntegerDigit (peekedChars [1])
            ;
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

                //TODO: FIXME
                //retToken.Text = tokenizer.data [startPos..tokenizer.curPos];
            } else if (peekedChars [0] == '.' && peekedChars.Length >= 2 && IsIntegerDigit (peekedChars [1])) {
                var startPos = tokenizer.curPos;

                tokenizer.TryReadFloatFractional ();
                tokenizer.TryReadFloatExponent (retToken.TextLine, retToken.TextColumn, startPos);
                tokenizer.TryReadFloatSuffix ();

                retToken.Type = EchelonScriptTokenType.FloatLiteral;
                //TODO: FIXME
                //retToken.Text = tokenizer.data [startPos..tokenizer.curPos];
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
            //TODO: FIXME
            //retToken.Text = tokenizer.data.Slice (startPos, len);

            return true;
        }
    }

    public class StringLiteralTokenParser : TokenParser {
        public override int MinimumStartPeek => 1;
        public override int RequestedStartPeek => 2;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars)
            => peekedChars [0] == '"' || peekedChars.Length >= 2 && peekedChars.StartsWith ("@\"");

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

            //TODO: FIXME
            //retToken.Text = tokenizer.data [startPos..tokenizer.curPos];
            if (!unclosed) {
                //TODO: FIXME
                /*tokenizer.DecodeStringToken (
                    retToken.Text.Span [(!verbatim ? 1 : 2)..^1],
                    out retToken.DecodedStringUTF16,
                    out retToken.DecodedStringUTF32,
                    textStartLine, textStartLinePos, textStartPos,
                    verbatim
                );*/
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
                } else if (c == '\\' && (tokenizer.PeekChar () == '\'' || tokenizer.PeekChar () == '\\'))
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

            //TODO: FIXME
            //retToken.Text = tokenizer.data [startPos..tokenizer.curPos];
            if (!unclosed) {
                //TODO: FIXME
                /*tokenizer.DecodeStringToken (
                    retToken.Text.Span [1..^1],
                    out retToken.DecodedStringUTF16,
                    out retToken.DecodedStringUTF32,
                    textStartLine, textStartLinePos, textStartPos,
                    false
                );*/
            }

            //TODO: FIXME
            /*if (retToken.DecodedStringUTF32?.Length > 1)
                tokenizer.Errors.Add (new EchelonScriptErrorMessage (retToken, ES_FrontendErrors.TooLongCharLiteral));
            else if (retToken.DecodedStringUTF32?.Length < 1)
                tokenizer.Errors.Add (new EchelonScriptErrorMessage (retToken, ES_FrontendErrors.EmptyCharLiteral));*/

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
        public override int RequestedStartPeek => tokenStringsMaxLength;

        public override bool IsStartValid (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars) => true;

        public override bool ParseToken (EchelonScriptTokenizer tokenizer, ReadOnlySpan<char> peekedChars, ref EchelonScriptToken retToken) {
            foreach (var tk in tokenStrings) {
                if (peekedChars.Length < tk.Str.Length)
                    continue;

                if (peekedChars.StartsWith (tk.Str)) {
                    tokenizer.ReadChars (tk.Str.Length);
                    retToken.Type = tk.Tok;
                    //TODO: FIXME
                    //retToken.Text = tokenizer.data [retToken.TextStartPos..tokenizer.curPos];

                    return true;
                }
            }

            return false;
        }
    }
}
