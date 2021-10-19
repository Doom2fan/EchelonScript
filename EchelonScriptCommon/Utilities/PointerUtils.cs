/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Runtime.InteropServices;
using ChronosLib.Unmanaged;

namespace EchelonScriptCommon.Utilities {
    [StructLayout (LayoutKind.Sequential)]
    public unsafe struct Pointer<T>
        where T : unmanaged {
        public T* Address;

        public Pointer (T* address) {
            Address = address;
        }

        public static implicit operator T* (Pointer<T> pointer) {
            return pointer.Address;
        }

        public static implicit operator Pointer<T> (T* pointer) {
            return new Pointer<T> (pointer);
        }

        public static implicit operator IntPtr (Pointer<T> pointer) {
            return (IntPtr) pointer.Address;
        }
    }

    //[StructLayout (LayoutKind.Sequential, Pack = 1)]
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

        public override int GetHashCode ()
            => HashCode.Combine (((IntPtr) Elements).GetHashCode (), Length.GetHashCode ());
    }

    public static class ES_Utils {
        public unsafe static ArrayPointer<T> GetArray<T> (this IMemoryManager manager, int count)
            where T : unmanaged {
            if (count < 1)
                throw new ArgumentOutOfRangeException (nameof (count), "Count must be greater than zero.");

            var mem = manager.GetMemory<T> (count);
            return new ArrayPointer<T> (mem, count);
        }

        public unsafe static void ReturnMemory<T> (this IMemoryManager manager, ArrayPointer<T> mem)
            where T : unmanaged {
            manager.ReturnMemory (mem.Elements);
        }
    }
}
