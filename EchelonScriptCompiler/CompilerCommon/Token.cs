/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;

namespace EchelonScriptCompiler.CompilerCommon {
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

        public ReadOnlyMemory<char> FileName;

        public ReadOnlyMemory<char> Text;
        public string DecodedStringUTF16;
        public int [] DecodedStringUTF32;
    }
}
