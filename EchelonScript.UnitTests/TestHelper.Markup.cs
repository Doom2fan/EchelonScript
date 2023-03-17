/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using System.Text;
using Microsoft.CodeAnalysis;

namespace EchelonScript.UnitTests;

internal readonly struct DiagnosticResultLocation {
    public readonly int StartIndex { get; init; }
    public readonly int Length { get; init; }
    public readonly int Line { get; init; }
    public readonly int Column { get; init; }
}

internal readonly struct DiagnosticResult {
    public readonly long LocationIndex { get; init; }
    public readonly string Id { get; init; }
    public readonly DiagnosticSeverity Severity { get; init; }

    public static DiagnosticResult FromDescriptor (long locationIdx, DiagnosticDescriptor descriptor)
        => new () { LocationIndex = locationIdx, Id = descriptor.Id, Severity = descriptor.DefaultSeverity, };
}

internal static partial class TestHelper {
    private readonly struct MarkupData {
        public readonly IReadOnlyDictionary<long, DiagnosticResultLocation> DiagnosticLocations { get; init; }
        public readonly string ProcessedCode { get; init; }
    }

    private static MarkupData ProcessMarkup (ReadOnlySpan<char> markupCode) {
        const string markerOpen = "[|";
        const string markerClose = "|]";
        const string markerIndexSeparator = ":";

        var curCodePos = 0;
        var curPos = 0;
        var codeRanges = new Queue<Range> ();
        var locationStack = new Stack<(long idx, int start)> ();
        var diagLocations = new Dictionary<long, DiagnosticResultLocation> ();

        while (true) {
            var newPosOpen = markupCode [curPos..].IndexOf (markerOpen);
            var newPosClose = markupCode [curPos..].IndexOf (markerClose);

            if (newPosOpen == -1 && newPosClose == -1) {
                if (locationStack.Count > 0)
                    throw new Exception ("Invalid markup: Unclosed diagnostic location.");

                codeRanges.Enqueue (curPos..);
                break;
            } else if (newPosOpen != -1 && (newPosClose == -1 || newPosOpen < newPosClose)) {
                if (newPosOpen > 0) {
                    codeRanges.Enqueue (curPos..(curPos + newPosOpen));
                    curCodePos += newPosOpen;
                }

                var codeStartPos = curCodePos;
                curPos += newPosOpen + markerOpen.Length;

                var sepIndex = markupCode [curPos..].IndexOf (markerIndexSeparator);
                if (sepIndex == -1)
                    throw new Exception ("Invalid markup: Missing index separator.");

                var numSpan = markupCode [curPos..(curPos + sepIndex)];
                foreach (var c in numSpan) {
                    if (c < '0' || c > '9')
                        throw new Exception ("Invalid markup: Non-numeric character in index separator.");
                }

                if (!long.TryParse (numSpan, out var locIndex))
                    throw new Exception ("Invalid markup: Could not parse location index.");

                curPos += sepIndex + markerIndexSeparator.Length;

                locationStack.Push ((locIndex, codeStartPos));
            } else if (newPosClose != -1 && (newPosOpen == -1 || newPosClose < newPosOpen)) {
                if (locationStack.Count < 1)
                    throw new Exception ("Invalid markup: Encountered a closing marker with no matching opening marker.");

                if (newPosClose > 0) {
                    codeRanges.Enqueue (curPos..(curPos + newPosClose));
                    curCodePos += newPosClose;
                }
                curPos += newPosClose + markerClose.Length;

                var codeEndPos = curCodePos;
                var diagLocStart = locationStack.Pop ();

                var diagLoc = new DiagnosticResultLocation {
                    StartIndex = diagLocStart.start,
                    Length = codeEndPos - diagLocStart.start,

                    Line = -1,
                    Column = -1,
                };

                if (!diagLocations.TryAdd (diagLocStart.idx, diagLoc))
                    throw new Exception ($"Invalid markup: Location index #{0} present multiple times.");
            } else if (newPosOpen == newPosClose)
                throw new Exception ("???");
        }

        Debug.Assert (locationStack.Count < 1);

        var sb = new StringBuilder ();
        while (codeRanges.Count > 0)
            sb.Append (markupCode [codeRanges.Dequeue ()]);

        var curLine = 0;
        for (curPos = 0; curPos < markupCode.Length;) {
            var lineStart = curPos;
            var lineEnd = lineStart;
            for (; lineEnd < markupCode.Length; lineEnd++) {
                var c = markupCode [lineEnd];
                if (c == '\r' && markupCode.Length - lineEnd > 0 && markupCode [lineEnd + 1] == '\n') {
                    curPos = lineEnd += 2;
                    break;
                } else if (c == '\n' || c == '\r') {
                    curPos = lineEnd++;
                    break;
                }
            }

            foreach (var diagLocKVP in diagLocations) {
                var diagLoc = diagLocKVP.Value;
                if (diagLoc.StartIndex < lineStart || diagLoc.StartIndex > lineEnd)
                    continue;

                diagLocations [diagLocKVP.Key] = new () {
                    StartIndex = diagLoc.StartIndex,
                    Length = diagLoc.Length,
                    Line = curLine,
                    Column = diagLoc.StartIndex - lineStart,
                };
            }

            curLine++;
        }

        foreach (var diagLoc in diagLocations.Values) {
            if (diagLoc.StartIndex == -1 || diagLoc.Length == -1 || diagLoc.Line == -1 || diagLoc.Column == -1)
                throw new Exception ("Invalid markup");
        }

        return new () {
            DiagnosticLocations = diagLocations,
            ProcessedCode = sb.ToString (),
        };
    }
}
