/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Runtime.InteropServices;
using ChronosLib.Unmanaged;

namespace EchelonScriptCommon.Utilities;

[StructLayout (LayoutKind.Sequential)]
public unsafe struct Pointer<T>
    where T : unmanaged {
    public T* Address;

    public Pointer (T* address) => Address = address;

    public static implicit operator T* (Pointer<T> pointer) => pointer.Address;

    public static implicit operator Pointer<T> (T* pointer) => new (pointer);

    public static implicit operator IntPtr (Pointer<T> pointer) => (IntPtr) pointer.Address;
}

//[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ArrayPointer<T>
    : IEquatable<ArrayPointer<T>>
    where T : unmanaged {
    public static ArrayPointer<T> Null => new (null, 0);

    public int Length;
    public T* Elements;

    public ArrayPointer (T* ptr, int len) {
        Elements = ptr;
        Length = len;
    }

    public Span<T> Span => new (Elements, Length);

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

    public override bool Equals (object? obj) {
        if (obj is ArrayPointer<T> other)
            return Equals (other);

        return false;
    }

    public override int GetHashCode () => HashCode.Combine (((IntPtr) Elements).GetHashCode (), Length.GetHashCode ());

    public static bool operator == (ArrayPointer<T> lhs, ArrayPointer<T> rhs) => lhs.Equals (rhs);
    public static bool operator != (ArrayPointer<T> lhs, ArrayPointer<T> rhs) => !lhs.Equals (rhs);
}

public static partial class ES_Utils {
    public unsafe static ArrayPointer<T> GetArray<T> (this IMemoryManager manager, int count)
        where T : unmanaged {
        if (count < 1)
            throw new ArgumentOutOfRangeException (nameof (count), "Count must be greater than zero.");

        var mem = manager.GetMemory<T> (count);
        return new ArrayPointer<T> (mem, count);
    }

    public unsafe static ArrayPointer<T> GetArrayAligned<T> (this IMemoryManager manager, int count, int alignment)
        where T : unmanaged {
        if (count < 1)
            throw new ArgumentOutOfRangeException (nameof (count), "Count must be greater than zero.");

        var mem = manager.GetMemoryAligned<T> (alignment, count);
        return new ArrayPointer<T> (mem, count);
    }

    public unsafe static void ReturnMemory<T> (this IMemoryManager manager, ArrayPointer<T> mem)
        where T : unmanaged
        => manager.ReturnMemory (mem.Elements);
}
