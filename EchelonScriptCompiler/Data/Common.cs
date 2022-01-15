/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using ChronosLib.Pooled;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Frontend.Parser;

namespace EchelonScriptCompiler.Data {
    public ref struct SourceData {
        public ReadOnlySpan<char> Code;
        public ReadOnlyMemory<char> FileName;
    }

    public struct EchelonScriptErrorMessage {
        public string? Message { get; }

        public int StartPos { get; }
        public int Length { get; }

        public string FileName { get; }
        public int Line { get; }
        public int Column { get; }

        public EchelonScriptErrorMessage (EchelonScriptToken tk, string? message = null) {
            Message = message;

            StartPos = tk.TextStartPos;
            Length = tk.Text.Length;

            FileName = tk.FileName.Span.GetPooledString ();
            Line = tk.TextLine;
            Column = tk.TextColumn;
        }

        public EchelonScriptErrorMessage (ReadOnlySpan<char> srcText, ReadOnlyMemory<char> fileName, ES_AstNodeBounds bounds, string? message = null) {
            Message = message;

            StartPos = bounds.StartPos;
            Length = bounds.EndPos - bounds.StartPos;

            EchelonScriptTokenizer.CalcLine (srcText, StartPos, out var curLine, out var curLineStart);
            FileName = fileName.Span.GetPooledString ();
            Line = curLine;
            Column = EchelonScriptTokenizer.CalcColumn (srcText, curLineStart, StartPos);
        }

        public EchelonScriptErrorMessage (SourceData sourceData, ES_AstNodeBounds bounds, string? message = null)
            : this (sourceData.Code, sourceData.FileName, bounds, message) {
        }

        public EchelonScriptErrorMessage (string message, int startPos, int length, ReadOnlyMemory<char> fileName, int line, int column) {
            Message = message;

            StartPos = startPos;
            Length = length;

            FileName = fileName.Span.GetPooledString ();
            Line = line;
            Column = column;
        }
    }
}
