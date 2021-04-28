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
    public unsafe struct ArrayPointer<T>
        : IEquatable<ArrayPointer<T>>
        where T : unmanaged {
        public static ArrayPointer<T> Null = new ArrayPointer<T> (null, 0);

        public int Length;
        public T* Elements;

        public ArrayPointer (T* ptr, int len) {
            Elements = ptr;
            Length = len;
        }

        public Span<T> Span => new Span<T> (Elements, Length);

        public bool Equals (ArrayPointer<T> other) {
            if (Elements == null && other.Elements == null)
                return true;
            else if (Elements == null || other.Elements == null)
                return false;
            else if (Elements == other.Elements && Length == other.Length)
                return true;
            else
                return false;
        }

        public override int GetHashCode () {
            return HashCode.Combine (((IntPtr) Elements).GetHashCode (), Length.GetHashCode ());
        }
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
