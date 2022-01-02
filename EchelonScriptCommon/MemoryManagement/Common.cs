/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using EchelonScriptCommon.Data.Types;

using ES_ArrayIndexBase = System.Int32;

namespace EchelonScriptCommon {
    public enum ES_ObjectFlags : uint {
        /// <summary>Object is medium sized. Marks all lines covered explicitly, and triggers the overflow allocator.
        /// </summary>
        MediumObject = 1 << 0,
        /// <summary>Object is large sized. Allocated in the Large Object Heap.</summary>
        LargeObject = 1 << 1,
        /// <summary>Object is pinned and cannot be moved or evacuated.</summary>
        Pinned = 1 << 2,
        /// <summary>The object contains no references within itself.</summary>
        NoRefs = 1 << 3,

        /// <summary>Object has been marked by the garbage collector and is reachable from the roots.</summary>
        Marked = 1 << 4,
        /// <summary>Object has been forwarded by the garbage collector and has been replaced with a forwarding
        /// pointer.</summary>
        Forwarded = 1 << 5,
        /// <summary>Object has been marked for removal in the next collection, regardless of liveness.</summary>
        DoRemove = 1 << 6,
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public unsafe struct ES_ObjectHeader {
        public ES_ObjectFlags Flags;
        public ES_TypeInfo* TypeData;
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public struct ES_ArrayIndex {
        public ES_ArrayIndexBase Value;

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static implicit operator ES_ArrayIndexBase (ES_ArrayIndex idx) => idx.Value;
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static implicit operator ES_ArrayIndex (ES_ArrayIndexBase idx) => new ES_ArrayIndex { Value = idx };
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public unsafe struct ES_ArrayHeader {
        public ES_ArrayIndex Length;
        private short padding0;
        private byte padding1;
        public byte Rank;

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static void* GetArrayDataPointer ([NotNull] ES_ArrayHeader* arrayPointer)
            => (byte*) arrayPointer + sizeof (ES_ArrayHeader) + arrayPointer->Rank * sizeof (ES_ArrayIndex);

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static void* GetArrayDataPointer<T> ([NotNull] ES_ArrayHeader* arrayPointer) where T : unmanaged
            => (T*) GetArrayDataPointer (arrayPointer);

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static ES_ArrayIndex* GetArrayIndicesPointer ([NotNull] ES_ArrayHeader* arrayPointer)
            => (ES_ArrayIndex*) ((byte*) arrayPointer + sizeof (ES_ArrayIndex));
    }
}
