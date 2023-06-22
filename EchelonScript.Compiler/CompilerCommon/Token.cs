/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Collections.Immutable;

namespace EchelonScript.Compiler.CompilerCommon;

public enum ES_TokenType {
    Invalid = -1,

    EOF = 0,

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
    ConcatEq,

    AndEq,
    BitOrEq,
    XorEq,
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

public readonly struct ES_Token {
    public ES_TokenType Type { get; init; }

    public SourceSpan TextSpan { get; init; }

    public ImmutableArray<ES_TriviaToken> LeadingTrivia { get; init; }
    public ImmutableArray<ES_TriviaToken> TrailingTrivia { get; init; }
}
