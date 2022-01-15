/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.GarbageCollection.Immix;
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
        /// <summary>The object is an array.</summary>
        IsArray = 1 << 3,
        /// <summary>The object contains no references within itself.</summary>
        NoRefs = 1 << 4,

        /// <summary>Object has been marked by the garbage collector and is reachable from the roots.</summary>
        Marked = 1 << 5,
        /// <summary>Object has been forwarded by the garbage collector and has been replaced with a forwarding
        /// pointer.</summary>
        Forwarded = 1 << 6,
        /// <summary>Object has been marked for removal in the next collection, regardless of liveness.</summary>
        DoRemove = 1 << 7,
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public unsafe struct ES_ObjectHeader {
        public ES_ObjectFlags Flags;
        public ES_TypeInfo* TypeData;
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public unsafe struct ES_ObjectAddress {
        public void* Address;

        public ES_ObjectHeader* Header {
            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            get => ((ES_ObjectHeader*) Address) - 1;
        }

        internal ImmixBlockHeader* ImmixBlock {
            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            get => (ImmixBlockHeader*) ((nint) Header & ImmixConstants.BlockStartMask);
        }

        internal int ImmixStartLine {
            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            get => (int) (((nint) Header & ImmixConstants.BlockMask) / ImmixConstants.LineSize - ImmixConstants.HeaderLines);
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public ES_ObjectAddress (void* addr) => Address = addr;

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static implicit operator ES_ObjectAddress (void* obj) => new (obj);

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        [DebuggerNonUserCode]
        [ES_ExcludeFromStackTrace]
        public static T* NullCheck<T> (T* ptr) where T : unmanaged {
            if (ptr == null)
                throw new EchelonScriptNullAccessException ();

            return ptr;
        }
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public struct ES_ArrayIndex {
        public ES_ArrayIndexBase Value;

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static implicit operator ES_ArrayIndexBase (ES_ArrayIndex idx) => idx.Value;
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static implicit operator ES_ArrayIndex (ES_ArrayIndexBase idx) => new ES_ArrayIndex { Value = idx };

        [ES_ExcludeFromStackTrace]
        [DebuggerNonUserCode]
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static void CheckBounds (int dimCount, int maxBounds, int val) {
            if (val < 0)
                throw new EchelonScriptOutOfBoundsException ($"Array access out of bounds. Negative index, index #{dimCount} = {val}.");
            else if (val >= maxBounds)
                throw new EchelonScriptOutOfBoundsException ($"Array access out of bounds. Index #{dimCount} = {val}, max = {maxBounds}.");

        }
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
            => (ES_ArrayIndex*) ((byte*) arrayPointer + sizeof (ES_ArrayHeader));

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static int GetArraySize ([NotNull] ES_ObjectAddress arrayPointer) {
            var arrayType = (ES_ArrayTypeData*) arrayPointer.Header->TypeData;
            var arrayHeader = (ES_ArrayHeader*) arrayPointer.Address;

            return (
                sizeof (ES_ObjectHeader) +
                GetArrayHeaderSize (arrayHeader->Rank) +
                arrayType->ElementType->RuntimeSize * arrayHeader->Length
            );
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static int GetArrayHeaderSize (int dimsCount)
            => sizeof (ES_ArrayHeader) + dimsCount * sizeof (ES_ArrayIndex);
    }
}
