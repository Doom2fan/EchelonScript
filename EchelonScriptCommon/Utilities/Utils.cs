/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Text;
using ChronosLib.Pooled;
using CommunityToolkit.HighPerformance.Buffers;

namespace EchelonScriptCommon.Utilities;

public static partial class ES_Utils {
    public unsafe static string GetPooledString (this Span<byte> bytes, Encoding encoding)
        => StringPool.Shared.GetOrAdd (bytes, encoding);

    public unsafe static string GetPooledString (this ArrayPointer<byte> bytes, Encoding encoding)
        => StringPool.Shared.GetOrAdd (bytes.Span, encoding);

    public unsafe static string GetPooledString (this PooledArray<byte> bytes, Encoding encoding)
        => StringPool.Shared.GetOrAdd (bytes.Span, encoding);

    public static string GetPooledString (this ArrayPointer<char> chars) => StringPool.Shared.GetOrAdd (chars.Span);

    public static string GetPooledString (this PooledArray<char> chars) => StringPool.Shared.GetOrAdd (chars.Span);

    public static void GetChars (this ReadOnlySpan<byte> bytes, ref StructPooledList<char> chars, Encoding enc) {
        var charsSpan = chars.AddSpan (enc.GetCharCount (bytes));
        var charsWritten = enc.GetChars (bytes, charsSpan);
        chars.RemoveEnd (charsSpan.Length - charsWritten);
    }

    public static PooledArray<T> Merge<T> (this PooledArray<T> left, PooledArray<T> right) => left.Merge (right.Span);

    public static PooledArray<T> Merge<T> (this PooledArray<T> left, ReadOnlySpan<T> right) {
        var newArr = PooledArray<T>.GetArray (left.RealLength + right.Length);

        left.Span.CopyTo (newArr.Span);
        right.CopyTo (newArr.Span [left.RealLength..]);

        return newArr;
    }

    public static PooledArray<T> Add<T> (this PooledArray<T> left, T right) {
        var newArr = PooledArray<T>.GetArray (left.RealLength + 1);

        left.Span.CopyTo (newArr.Span);
        newArr.Span [left.RealLength] = right;

        return newArr;
    }

}