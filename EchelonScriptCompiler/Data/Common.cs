/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Frontend.Parser;

namespace EchelonScriptCompiler.Data {
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
}
