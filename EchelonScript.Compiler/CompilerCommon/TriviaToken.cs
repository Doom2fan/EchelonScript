/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScript.Compiler.CompilerCommon;

public enum ES_TriviaType {
    Invalid = -1,

    Whitespace = 0,
    EndOfLine,
    LineComment,
    BlockComment,
    DocComment,
}

public readonly struct ES_TriviaToken {
    public ES_TriviaType Type { get; init; }
    public SourceSpan TextSpan { get; init; }
}
