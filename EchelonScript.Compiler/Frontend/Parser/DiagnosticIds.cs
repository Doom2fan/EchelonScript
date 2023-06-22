/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;

namespace EchelonScript.Compiler.Frontend.Parser.Internal;

static partial class DiagnosticDescriptors {
    private enum DiagnosticId : ushort {
        UnclosedBlockComment,
        UnclosedDocComment,
        InvalidNumber,

        UnrecognizedEscape,
        UnclosedStringLiteral,
        NewlineInConstant,

        EmptyCharLiteral,
        TooLongCharLiteral,
        UnclosedCharLiteral,
    }

    private static string GetExportErrorId (DiagnosticId id) {
        Debug.Assert ((ushort) id <= 999);
        return $"ES1{(int) id:D3}";
    }
}
