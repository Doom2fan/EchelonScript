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

        public ReadOnlyMemory<char> Text;
        public string DecodedStringUTF16;
        public int [] DecodedStringUTF32;
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
}
