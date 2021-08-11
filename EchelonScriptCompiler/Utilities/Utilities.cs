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
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Utilities {
    [StructLayout(LayoutKind.Sequential)]
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
