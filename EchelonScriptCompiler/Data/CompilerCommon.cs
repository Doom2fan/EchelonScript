/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Frontend.Parser;

namespace EchelonScriptCompiler.Data {
    public enum EchelonScriptTokenType {
        Invalid = -1,

        EOF = 0,
        DocComment,

        Dot,
        DotDot,
        NamespaceOp,

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
        Xor,
        Tilde,
        ShiftLeft,
        ShiftRight,
        ShiftRightU,

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
        XorEq,
        TildeEq,
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

        Question,
        Colon,
        Comma,
        Semicolon,
        LambdaArrow,

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
        public int TextEndPos => TextStartPos + Text.Length;

        public ReadOnlyMemory<char> Text;
        public string DecodedStringUTF16;
        public int [] DecodedStringUTF32;
    }

    public struct EchelonScriptErrorMessage {
        public string? Message { get; }

        public int StartPos { get; }
        public int Length { get; }

        public int Line { get; }
        public int Column { get; }

        public EchelonScriptErrorMessage (EchelonScriptToken tk, string? message = null) {
            Message = message;

            StartPos = tk.TextStartPos;
            Length = tk.Text.Length;

            Line = tk.TextLine;
            Column = tk.TextColumn;
        }

        public EchelonScriptErrorMessage (ReadOnlySpan<char> srcText, ES_AstNodeBounds bounds, string? message = null) {
            Message = message;

            StartPos = bounds.StartPos;
            Length = bounds.EndPos - bounds.StartPos;

            EchelonScriptTokenizer.CalcLine (srcText, StartPos, out var curLine, out var curLineStart);
            Line = curLine;
            Column = EchelonScriptTokenizer.CalcColumn (srcText, curLineStart, StartPos);
        }

        public EchelonScriptErrorMessage (string message, int startPos, int length, int line, int column) {
            Message = message;

            StartPos = startPos;
            Length = length;

            Line = line;
            Column = column;
        }
    }

    public enum ES_AccessModifier {
        /// <summary>The symbol is only accessible from the context it was defined in.</summary>
        Private,
        /// <summary>The symbol is accessible from the context it was defined in and anything inheriting from it.</summary>
        Protected,
        /// <summary>The symbol is accessible from the context it was defined in, anything inheriting from it and from
        /// anything defined in the same program/library as the context it was defined in.</summary>
        ProtectedInternal,
        /// <summary>The symbol is accessible from the context it was defined in and from anything defined in the same
        /// program/library as the context it was defined in.</summary>
        Internal,
        /// <summary>The symbol is accessible from any context.</summary>
        Public,
    }

    public enum ES_VirtualnessModifier {
        None,
        Virtual,
        Abstract,
        Override,
    }
}
