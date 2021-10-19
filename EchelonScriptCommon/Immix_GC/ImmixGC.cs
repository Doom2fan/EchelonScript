/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using static TerraFX.Interop.Mimalloc;

namespace EchelonScriptCommon.Immix_GC {
    public enum ImmixBlockUsage : int {
        Empty = 0,
        Filled,
        Recyclable,
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public unsafe struct ImmixBlock {
        public ImmixBlockUsage UsageLevel;

        public readonly ArrayPointer<byte> LineMap;
        public readonly void* MemPointer;

        public ImmixBlock (void* blockStartPtr) {
            UsageLevel = ImmixBlockUsage.Empty;

            LineMap = new ArrayPointer<byte> ((byte*) blockStartPtr, ImmixGC.LinesCount);
            MemPointer = ((byte*) blockStartPtr + ImmixGC.LinesCount);
        }
    }

    public enum ImmixObjectFlags : short {
        /// <summary>Object is medium size. Marks all lines covered explicitly, and triggers the overflow allocator.
        /// </summary>
        MediumSize = 1 << 0,
        /// <summary>Object is pinned and cannot be moved or evacuated.</summary>
        Pinned = 1 << 1,

        /// <summary>Object has been marked by the garbage collector and is reachable from the roots.</summary>
        Marked = 1 << 2,
        /// <summary>Object has been forwarded by the garbage collector and has been replaced with a forwarding
        /// pointer.</summary>
        Forwarded = 1 << 3,
        /// <summary>Object has been marked for removal in the next collection, regardless of liveness.</summary>
        DoRemove = 1 << 4,
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    public unsafe struct ImmixObjectHeader {
        public ImmixObjectFlags Flags;
        public short Size;
        public ES_TypeInfo* TypeData;
    }

    public unsafe static class ImmixGC {
        #region ================== Constants

        // These shouldn't really be touched.
        // If you DO touch this for some reason, make sure BlockSize is < 64KB, and is a multiple of LineSize.
        public const int BlockSize = 32 * 1024;
        public const int LineSize = 128;
        public const int LinesCount = BlockSize / LineSize;

        // Make sure this is under BlockSize!
        public const int LargeObjectSize = 8 * 1024;

        #endregion

        #region ================== Structs

        private struct BlockList {
            private const int MinHeadroomCount = 5;

            #region ================== Instance fields

            private ImmixBlock [] blocks;

            #endregion

            #region ================== Instance properties

            public int Count { get; private set; }

            public Span<ImmixBlock> Span => blocks.AsSpan (0, Count);

            #endregion

            #region ================== Indexers

            public ref ImmixBlock this [int index] {
                get {
                    if (index >= Count)
                        throw new IndexOutOfRangeException (nameof (index));

                    return ref blocks! [index];
                }
            }

            #endregion

            #region ================== Static methods

            public static BlockList Create () {
                var ret = new BlockList ();

                ret.blocks = Array.Empty<ImmixBlock> ();
                ret.Count = 0;

                return ret;
            }

            #endregion

            #region ================== Instance methods

            private void EnsureHeadroom (int num) {
                if (blocks.Length >= Count + num)
                    return;

                var oldBlocks = blocks;

                blocks = new ImmixBlock [Count + num];
                Array.Copy (oldBlocks, blocks, Count);
            }

            public void Add (ImmixBlock block) {
                if (Count >= blocks.Length)
                    EnsureHeadroom (MinHeadroomCount);

                blocks [Count++] = block;
            }

            public void Remove (int pos) {
                if (pos < Count - 1)
                    Array.Copy (blocks, pos + 1, blocks, pos - 1, Count - pos);

                Count--;
            }

            #endregion

            #region ================== Casts

            public static implicit operator Span<ImmixBlock> (BlockList list) => list.Span;

            #endregion
        }

        private struct AllocData {
            #region ================== Instance fields

            public BlockList BlocksList;

            public byte* BumpPointer;
            public byte* BumpLimit;

            public int CurBlockIndex;

            #endregion

            #region ================== Instance properties

            public int BumpSpace => (int) (BumpLimit - BumpPointer);

            public ImmixBlock CurrentBlock => BlocksList! [CurBlockIndex];
            public ref ImmixBlock CurrentBlockRef => ref BlocksList! [CurBlockIndex];

            #endregion

            #region ================== Instance methods

            public void Initialize () {
                BlocksList = BlockList.Create ();

                var newBlock = ImmixGC_GlobalAllocator.GetBlock ();
                newBlock.UsageLevel = ImmixBlockUsage.Filled;
                BlocksList.Add (newBlock);

                CurBlockIndex = 0;
                SetNewAllocData (0, LinesCount);
            }

            private void SetNewAllocData (int holeOffs, int holeSize) {
                BumpPointer = (byte*) CurrentBlock.MemPointer + (holeOffs * LineSize);
                BumpLimit = BumpPointer + (holeSize * LineSize);
            }

            private bool ScanForHole (int minSize, out int holeOffs, out int holeSize) {
                // If the block is empty, return the whole block's length as a hole.
                if (CurrentBlock.UsageLevel == ImmixBlockUsage.Empty) {
                    holeOffs = 0;
                    holeSize = BlockSize;
                    return true;
                }

                int posOffs = (int) (BumpPointer - (byte*) CurrentBlock.MemPointer);
                int startPos = posOffs / LineSize;
                if (startPos * LineSize < posOffs)
                    startPos++;

                var lineMapSpan = CurrentBlock.LineMap.Span;
                bool lastLineMarked = false; // Used for skipping implicit marks
                for (int i = startPos; i < LinesCount; i++) {
                    int curHoleSize = 0;

                    if (lineMapSpan [i] != 0) {
                        lastLineMarked = true;
                        continue;
                    } else if (lastLineMarked) {
                        lastLineMarked = false;
                        continue;
                    }

                    for (int j = i + 1; j < LinesCount; j++) {
                        if (lineMapSpan [i] != 0)
                            break;

                        curHoleSize++;
                    }

                    if (curHoleSize > 0 && (curHoleSize * LineSize) >= minSize) {
                        holeOffs = LineSize * i;
                        holeSize = LineSize * curHoleSize;
                        return true;
                    }
                }

                holeOffs = 0;
                holeSize = 0;
                return false;
            }

            public void EnsureAllocatable (int minSize) {
                if (ScanForHole (minSize, out var holeOffs, out var holeSize)) {
                    SetNewAllocData (holeOffs, holeSize);
                    return;
                }

                // Try to find a block with a hole big enough.
                while (CurBlockIndex < BlocksList.Count) {
                    if (CurrentBlock.UsageLevel != ImmixBlockUsage.Recyclable) {
                        CurBlockIndex++;
                        continue;
                    }

                    if (ScanForHole (minSize, out holeOffs, out holeSize)) {
                        SetNewAllocData (holeOffs, holeSize);
                        CurrentBlockRef.UsageLevel = ImmixBlockUsage.Filled;
                        return;
                    }

                    SetNewAllocData (0, 0);
                    CurBlockIndex++;
                }

                // If we didn't find one, allocate a new one.
                BlocksList.Add (ImmixGC_GlobalAllocator.GetBlock ());
                CurBlockIndex = BlocksList.Count - 1;

                SetNewAllocData (0, LinesCount);
                CurrentBlockRef.UsageLevel = ImmixBlockUsage.Filled;
            }

            #endregion
        }

        #endregion

        #region ================== Static fields

        [ThreadStatic]
        private static bool initialized = false;

        [ThreadStatic]
        private static AllocData allocData;
        [ThreadStatic]
        private static AllocData overflowAllocData;

        #endregion

        #region ================== Static methods

        private static void EnsureInitialized () {
            if (!initialized) {
                allocData.Initialize ();
                overflowAllocData.Initialize ();

                initialized = true;
            }
        }

        [UnmanagedCallersOnly (CallConvs = new [] { typeof (CallConvCdecl) })]
        public static void* AllocObjectUnmanaged (ES_TypeInfo* type) => AllocObject (type);

        [UnmanagedCallersOnly (CallConvs = new [] { typeof (CallConvCdecl) })]
        public static void* AllocArrayUnmanaged (ES_ArrayTypeData* arrayType, int* dimSizesPtr, int dimsCount)
            => AllocArray (arrayType, dimSizesPtr, dimsCount);

        public static T* AllocObject<T> (ES_TypeInfo* type, T defaultVal = default) where T : unmanaged {
            var ptr = (T*) AllocObject (type);
            *ptr = defaultVal;

            return ptr;
        }

        public static void* AllocObject (ES_TypeInfo* type) {
            EnsureInitialized ();

            int objSize = Math.Max (type->RuntimeSize, sizeof (void*));
            int allocSize = sizeof (ImmixObjectHeader) + objSize;

            // Allocate large objects in the LOS heap.
            if (allocSize >= LargeObjectSize)
                throw new NotImplementedException ("LOS heap not implemented yet.");

            bool isMedium = false;
            if (allocSize > LineSize)
                isMedium = true;

            // Generate the object header.
            var headerVal = new ImmixObjectHeader {
                Flags = isMedium ? ImmixObjectFlags.MediumSize : 0,
                Size = (short) objSize,
                TypeData = type,
            };

            int spaceLeft = allocData.BumpSpace;
            void* objHeader;

            // If it's a medium object and there's not enough space to allocate in, use the overflow allocator.
            if (isMedium & (allocSize > spaceLeft)) {
                // Ensure there's enough space to allocate the object.
                if (allocSize > overflowAllocData.BumpSpace)
                    overflowAllocData.EnsureAllocatable (allocSize);

                // Grab the header's pointer and bump the allocation pointer.
                objHeader = overflowAllocData.BumpPointer;
                allocData.BumpPointer += allocSize;

                *((ImmixObjectHeader*) objHeader) = headerVal;

                return (byte*) objHeader + sizeof (ImmixObjectHeader);
            }

            // Ensure there's enough space to allocate the object.
            if (allocSize > spaceLeft)
                allocData.EnsureAllocatable (allocSize);

            // Grab the header's pointer and bump the allocation pointer.
            objHeader = allocData.BumpPointer;
            allocData.BumpPointer += allocSize;

            *(ImmixObjectHeader*) objHeader = headerVal;

            return (byte*) objHeader + sizeof (ImmixObjectHeader);
        }

        public static void* AllocArray (ES_ArrayTypeData* arrayType, int* dimSizesPtr, int dimsCount) {
            EnsureInitialized ();

            var elementType = arrayType->ElementType;
            var dimSizesSpan = new Span<int> (dimSizesPtr, dimsCount);

            int elemSize = elementType->RuntimeSize;

            // Calculate the total number of elements.
            int totalElemsCount = 1;

            foreach (var dim in dimSizesSpan)
                totalElemsCount *= dim;

            // Calculate the array object's sizes.
            int arrSize = elemSize * totalElemsCount;
            int arrHeaderSize = dimsCount * sizeof (int);

            int allocSize = sizeof (ImmixObjectHeader) + arrHeaderSize + arrSize;

            // Allocate large objects in the LOS heap.
            if (allocSize >= LargeObjectSize)
                throw new NotImplementedException ("LOS heap not implemented yet.");

            bool isMedium = false;
            if (allocSize > LineSize)
                isMedium = true;

            // Determine the object's flags.
            ImmixObjectFlags headerFlags = 0;

            if (isMedium)
                headerFlags = ImmixObjectFlags.MediumSize;

            // Generate the object header.
            var headerVal = new ImmixObjectHeader {
                Flags = headerFlags,
                Size = (short) (arrHeaderSize + arrSize),
                TypeData = &arrayType->TypeInfo,
            };

            int spaceLeft = allocData.BumpSpace;
            void* objHeader;

            // If it's a medium object and there's not enough space to allocate in, use the overflow allocator.
            if (isMedium & (allocSize > spaceLeft)) {
                // Ensure there's enough space to allocate the object.
                if (allocSize > overflowAllocData.BumpSpace)
                    overflowAllocData.EnsureAllocatable (allocSize);

                // Grab the header's pointer and bump the allocation pointer.
                objHeader = overflowAllocData.BumpPointer;
                allocData.BumpPointer += allocSize;

                *((ImmixObjectHeader*) objHeader) = headerVal;

                return (byte*) objHeader + sizeof (ImmixObjectHeader);
            }

            // Ensure there's enough space to allocate the object.
            if (allocSize > spaceLeft)
                allocData.EnsureAllocatable (allocSize);

            // Grab the header's pointer and bump the allocation pointer.
            objHeader = allocData.BumpPointer;
            allocData.BumpPointer += allocSize;

            *((ImmixObjectHeader*) objHeader) = headerVal;

            // Copy the array dimensions to the start of the array object.
            var arrHeader = (byte*) objHeader + sizeof (ImmixObjectHeader);
            dimSizesSpan.CopyTo (new Span<int> (arrHeader, dimsCount));

            return arrHeader + arrHeaderSize;
        }

        #endregion
    }

    internal unsafe static class ImmixGC_GlobalAllocator {
        private static List<ImmixBlock> freeBlocks;

        static ImmixGC_GlobalAllocator () {
            freeBlocks = new List<ImmixBlock> ();
        }

        internal static ImmixBlock GetBlock () {
            bool gotBlock = false;
            ImmixBlock block = default;

            lock (freeBlocks) {
                if (freeBlocks.Count > 0) {
                    block = freeBlocks [^1];
                    freeBlocks.RemoveAt (freeBlocks.Count - 1);

                    return block;
                }
            }

            if (!gotBlock) {
                var mem = mi_malloc (ImmixGC.LinesCount + ImmixGC.BlockSize);
                block = new ImmixBlock (mem);
                block.LineMap.Span.Fill (0);

                gotBlock = true;
            }

            block.UsageLevel = ImmixBlockUsage.Empty;

            return block;
        }
    }
}
