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
using System.Collections.Immutable;

namespace EchelonScript.Compiler.CompilerCommon;

public readonly struct SourceSpan {
    #region ================== Instance properties

    public int Start { get; init; }
    public int End { get; init; }

    public readonly int Length => End - Start;

    #endregion

    public SourceSpan (int start, int end) {
        Start = start;
        End = end;
    }

    #region ================== Instance methods

    public readonly SourceSpan Slice (int start, int length) {
        if (start < 0)
            throw new IndexOutOfRangeException (nameof (start));
        else if (length < 0)
            throw new IndexOutOfRangeException (nameof (length));
        else if ((start + length) > Length)
            throw new IndexOutOfRangeException (nameof (length));

        var newStart = Start + start;
        return new (newStart, newStart + length);
    }

    public readonly bool Contains (SourceSpan other) => other.Start <= End && other.End >= Start;

    #endregion
}

public class SourceFile {
    public string FileName { get; init; } = "<anon>";
    public required string SourceUnit { get; init; }

    public ImmutableArray<char> Text { get; init; }
    public ImmutableArray<int> Lines { get; init; }
    public SourceSpan Span { get; init; }

    public ImmutableArray<char> GetText (SourceSpan span) {
        var start = span.Start - Span.Start;
        return Text.Slice (start, span.Length);
    }
}

public struct SourceLocation {
    public readonly int StartPos { get; init; }
    public readonly int EndPos { get; init; }
    public readonly int Length => EndPos - StartPos;

    public readonly int Line { get; init; }
    public readonly int Column { get; init; }

    public readonly string FileName { get; init; }
    public readonly string SourceUnit { get; init; }
}

public class SourceMap {
    #region ================== Instance fields

    private uint usedAddressSpace = 0;
    private List<SourceFile> files = new ();

    #endregion

    #region ================== Instance methods

    private int AllocateAddressSpace (int length) {
        if (length < 0)
            throw new ArgumentOutOfRangeException ("Length cannot be negative", nameof (length));

        var start = usedAddressSpace;
        if (start + length > int.MaxValue)
            throw new Exception ($"SourceMap overflow (length = {length})");

        usedAddressSpace += (uint) length + 1;

        return (int) start;
    }

    public void AddFile (string sourceUnit, string fileName, ImmutableArray<char> text) {
        var fileStart = AllocateAddressSpace (text.Length);

        var lines = new List<int> { 0 };
        for (int i = 0; i < text.Length; i++) {
            var c = text [i];

            if (c == '\r') {
                if (i + 1 < text.Length && text [i + 1] == '\n')
                    i++;

                lines.Add (fileStart + i + 1);
            } else if (c == '\n')
                lines.Add (fileStart + i + 1);
        }

        files.Add (new () {
            FileName = fileName,
            SourceUnit = sourceUnit,

            Text = text,
            Lines = lines.ToImmutableArray (),
            Span = new (fileStart, fileStart + text.Length),
        });
    }

    public IEnumerable<SourceFile> EnumerateFiles () => files;

    public SourceFile? TryGetFile (SourceSpan span) {
        foreach (var file in files) {
            if (file.Span.Contains (span))
                return file;
        }

        return null;
    }

    private SourceFile GetFile (SourceSpan span) {
        var file = TryGetFile (span);
        if (file is null)
            throw new ArgumentException ("Span is not contained by any file", nameof (span));

        return file;
    }

    public ImmutableArray<char> GetFileText (SourceSpan span) => GetFile (span).Text;

    public ImmutableArray<char> GetText (SourceSpan span) => GetFile (span).GetText (span);

    public SourceLocation GetLocation (SourceSpan span) {
        var file = GetFile (span);

        var lineIdx = 0;
        for (; lineIdx < file.Lines.Length; lineIdx++) {
            if (span.Start < file.Lines [lineIdx])
                break;
        }

        lineIdx = Math.Min (lineIdx - 1, file.Lines.Length - 1);
        var column = span.Start - file.Lines [lineIdx] + 1;

        return new () {
            StartPos = span.Start - file.Span.Start,
            EndPos = span.End - file.Span.Start,

            Line = lineIdx + 1,
            Column = column,

            FileName = file.FileName,
            SourceUnit = file.SourceUnit,
        };
    }

    #endregion
}

public readonly struct TranslationUnitData {
    public string Name { get; init; }
    public ImmutableArray<SourceSpan> Files { get; init; }
}
