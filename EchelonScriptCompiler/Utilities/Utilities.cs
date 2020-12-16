/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Runtime.InteropServices;
using Microsoft.Toolkit.HighPerformance.Buffers;

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
        public static string GetPooledString (this Span<char> chars) {
            return StringPool.Shared.GetOrAdd (chars);
        }

        public static string GetPooledString (this ReadOnlySpan<char> chars) {
            return StringPool.Shared.GetOrAdd (chars);
        }

        public static string GetPooledString (this Memory<char> chars) {
            return StringPool.Shared.GetOrAdd (chars.Span);
        }

        public static string GetPooledString (this ReadOnlyMemory<char> chars) {
            return StringPool.Shared.GetOrAdd (chars.Span);
        }
    }
}
