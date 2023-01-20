/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using ChronosLib.Unmanaged;

namespace EchelonScript.Common.Utilities;

// Store global compiler data in unmanaged memory?
[NonCopyable]
public unsafe struct ES_MemoryManager {
    private IMemoryManager memManager;

    public IntPtr GetMemory (nint bytesCount) => memManager.GetMemory (bytesCount);
    public IntPtr GetMemory (int bytesCount) => memManager.GetMemory (bytesCount);
    public IntPtr GetMemoryAligned (nint bytesCount, nint alignment) => memManager.GetMemoryAligned (bytesCount, alignment);

    public unsafe T* GetMemory<T> (int count = 1) where T : unmanaged => memManager.GetMemory<T> (count);
    public unsafe T* GetMemoryAligned<T> (nint alignment, int count = 1) where T : unmanaged
        => memManager.GetMemoryAligned<T> (alignment, count);

    public ArrayPointer<T> GetArray<T> (int count) where T : unmanaged {
        if (count < 1)
            return ArrayPointer<T>.Null;

        var mem = memManager.GetMemory<T> (count);
        return new ArrayPointer<T> (mem, count);
    }
    public ArrayPointer<T> GetArrayAligned<T> (int count, int alignment) where T : unmanaged {
        if (count < 1)
            return ArrayPointer<T>.Null;

        var mem = memManager.GetMemoryAligned<T> (alignment, count);
        return new ArrayPointer<T> (mem, count);
    }

    public unsafe void ReturnMemory (void* pointer) => memManager.ReturnMemory (pointer);
    public unsafe void ReturnMemory (IntPtr pointer) => memManager.ReturnMemory (pointer);
    public void ReturnMemory<T> (ArrayPointer<T> mem) where T : unmanaged => memManager.ReturnMemory (mem.Elements);

    public ES_String AllocString (ReadOnlySpan<char> text) {
        if (text.Length <= ES_String.MaxLocalTextSize)
            return new (text);

        var mem = GetArray<char> (text.Length);
        text.CopyTo (mem.Span);

        return new (mem);
    }
    public ES_String AllocString (ReadOnlyMemory<char> text) => AllocString (text.Span);
    public ES_String AllocString (string text) => AllocString (text.AsSpan ());
    public ES_String AllocString (string text, Range range) => AllocString (text.AsSpan () [range]);
    public ES_String AllocString (string text, int start) => AllocString (text.AsSpan (start));
    public ES_String AllocString (string text, int start, int len) => AllocString (text.AsSpan (start, len));
}
