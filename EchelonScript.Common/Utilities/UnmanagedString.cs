/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;
using CommunityToolkit.HighPerformance;
using CommunityToolkit.HighPerformance.Buffers;

namespace EchelonScript.Common.Utilities;

[DebuggerDisplay ("{Span}")]
public unsafe struct ES_String {
    internal const int MaxLocalTextSize = 192;

    public static ES_String Empty => new (ReadOnlySpan<char>.Empty);

    #region ================== Instance fields and properties

    public readonly int Length { get; private init; }
    private fixed char localText [MaxLocalTextSize];

    private readonly ArrayPointer<char> refText;

    public ReadOnlySpan<char> Span => Length < MaxLocalTextSize ? GetLocalTextSpan () : refText!.Span;

    #endregion

    internal ES_String (ReadOnlySpan<char> text) {
        Debug.Assert (text.Length < MaxLocalTextSize);

        Length = text.Length;
        refText = ArrayPointer<char>.Null;
        text.CopyTo (GetLocalTextSpan ());
    }
    internal ES_String (ArrayPointer<char> text) {
        Length = text.Length;

        if (Length < MaxLocalTextSize) {
            refText = ArrayPointer<char>.Null;
            text.Span.CopyTo (GetLocalTextSpan ());
        } else
            refText = text;
    }

    #region ================== Indexers

    public char this [int index] {
        get {
            if (index < 0)
                throw new IndexOutOfRangeException ("Index is negative.");
            if (index >= Length)
                throw new IndexOutOfRangeException ("Index >= Length.");

            return Span [index];
        }
    }

    public ES_String this [Range range] {
        get {
            var (offs, len) = range.GetOffsetAndLength (Length);

            if (Length < MaxLocalTextSize)
                return new (GetLocalTextSpan ().Slice (offs, len));

            Debug.Assert (refText.Elements != null);
            return new (refText.Slice (offs, len));
        }
    }

    #endregion

    #region ================== Instance methods

    private Span<char> GetLocalTextSpan () => MemoryMarshal.CreateSpan (ref localText [0], Length);

    #endregion
}

[StructLayout (LayoutKind.Explicit)]
[DebuggerDisplay ("{GetString ()}")]
public unsafe struct ES_Utf8String : IEquatable<ES_Utf8String> {
    public const int MaxLocalTextSize = 32;

    public static ES_Utf8String Empty => new (ReadOnlySpan<byte>.Empty);

    #region ================== Instance fields and properties

    [FieldOffset (0)]
    private readonly int length;

    [FieldOffset (4)]
    private fixed byte localText [MaxLocalTextSize];
    [FieldOffset (4)]
    private readonly byte* refText;

    public readonly int Length => length;
    public ReadOnlySpan<byte> Span => Length <= MaxLocalTextSize ? GetLocalTextSpan () : new (refText, length);

    #endregion

    internal ES_Utf8String (ReadOnlySpan<byte> text) {
        Debug.Assert (text.Length <= MaxLocalTextSize);

        length = text.Length;
        refText = null;
        text.CopyTo (GetLocalTextSpan ());
    }
    internal ES_Utf8String (ArrayPointer<byte> text) {
        length = text.Length;

        if (length <= MaxLocalTextSize) {
            refText = null;
            text.Span.CopyTo (GetLocalTextSpan ());
        } else
            refText = text.Elements;
    }

    #region ================== Indexers

    public byte this [int index] {
        get {
            if (index < 0)
                throw new IndexOutOfRangeException ("Index is negative.");
            if (index >= length)
                throw new IndexOutOfRangeException ("Index >= Length.");

            return Span [index];
        }
    }

    public ES_Utf8String this [Range range] {
        get {
            var (offs, len) = range.GetOffsetAndLength (length);

            if (length <= MaxLocalTextSize)
                return new (GetLocalTextSpan ().Slice (offs, len));

            Debug.Assert (refText != null);
            return new (Span.Slice (offs, len));
        }
    }

    #endregion

    #region ================== Instance methods

    private Span<byte> GetLocalTextSpan () => MemoryMarshal.CreateSpan (ref localText [0], length);

    public string GetString () => Encoding.UTF8.GetString (Span);

    public string GetPooledString () => StringPool.Shared.GetOrAdd (Span, Encoding.UTF8);

    public override bool Equals ([NotNullWhen (true)] object? obj) => obj is ES_Utf8String other ? Equals (other) : false;

    public bool Equals (ES_Utf8String other) {
        if (length != other.length)
            return false;

        if (length <= MaxLocalTextSize)
            return GetLocalTextSpan ().SequenceEqual (other.GetLocalTextSpan ());

        if (refText == other.refText)
            return true;

        return Span.SequenceEqual (other.Span);
    }

    public override int GetHashCode () => HashCode.Combine (length, Span.GetDjb2HashCode ());

    public override string ToString () => GetString ();

    #endregion

    #region ================== Operators

    public static bool operator == (ES_Utf8String a, ES_Utf8String b) => a.Equals (b);
    public static bool operator != (ES_Utf8String a, ES_Utf8String b) => a.Equals (b);

    #endregion
}
