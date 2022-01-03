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
using System.Diagnostics;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Threading;
using ChronosLib.Unmanaged;
using EchelonScriptCommon.Utilities;
using static TerraFX.Interop.Mimalloc;

namespace EchelonScriptCommon.GarbageCollection.Immix {
    internal enum ImmixBlockUsage : int {
        Empty = 0,
        Filled,
        Recyclable,
    }

    public unsafe struct ImmixConstants {
        // Make sure BlockSize is < 64KB, and is a multiple of LineSize.
        public const int BlockSize = 32 * 1024;
        public const int LineSize = 128;
        public static int TrueBlockSize => LinesCount * LineSize;

        public const int BlocksPerChunk = 8;
        public static int ChunkMetadataSize => sizeof (ImmixChunkHeader) + LinesCount * BlocksPerChunk;
        public static int TotalChunkSize => ChunkMetadataSize + BlockSize * BlocksPerChunk;

        public const int BareLinesCount = BlockSize / LineSize;
        public static int HeaderLines => (sizeof (ImmixBlockHeader) + (LineSize - 1)) / LineSize;
        public static int LinesCount => BareLinesCount - HeaderLines;

        public const nint BlockStartMask = ~(BlockSize - 1);
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    internal unsafe struct ImmixChunkHeader {
        public int FreeBlocks;
        public fixed byte BlockUsed [ImmixConstants.BlocksPerChunk];

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Initialize () {
            FreeBlocks = ImmixConstants.BlocksPerChunk;

            for (int i = 0; i < ImmixConstants.BlocksPerChunk; i++)
                BlockUsed [i] = 0;
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static ImmixBlockHeader* GetBlock (ImmixChunkHeader* chunk, int idx)
            => (ImmixBlockHeader*) ((byte*) chunk + ImmixConstants.ChunkMetadataSize + ImmixConstants.BlockSize * idx);
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    internal unsafe struct ImmixBlockHeader {
        public ImmixBlockUsage UsageLevel;

        public readonly byte* LineMap;
        public Span<byte> LineMapSpan => new Span<byte> (LineMap, ImmixConstants.LinesCount);

        public ImmixBlockHeader (byte* lineMapPtr) {
            UsageLevel = ImmixBlockUsage.Empty;

            LineMap = lineMapPtr;
            LineMapSpan.Clear ();
        }
    }

    public struct ImmixDebugInfo {
        public int BlockCount;
        public int EmptyBlocks;
        public int FullBlocks;
        public int RecyclableBlocks;

        public int CurrentBlockIndex;
        public int BlockStride;
        public UnmanagedArray<byte> Blocks;
    }

    internal unsafe sealed class ImmixGC : IDisposable {
        #region ================== Structs

        private struct BlockList {
            private const int MinHeadroomCount = 5;

            #region ================== Instance fields

            private Pointer<ImmixBlockHeader> [] blocks;

            #endregion

            #region ================== Instance properties

            public int Count {
                [MethodImpl (MethodImplOptions.AggressiveInlining)]
                get;
                [MethodImpl (MethodImplOptions.AggressiveInlining)]
                private set;
            }

            public Span<Pointer<ImmixBlockHeader>> Span {
                [MethodImpl (MethodImplOptions.AggressiveInlining)]
                get => blocks.AsSpan (0, Count);
            }

            #endregion

            #region ================== Indexers

            public ref Pointer<ImmixBlockHeader> this [int index] {
                [MethodImpl (MethodImplOptions.AggressiveInlining)]
                get {
                    Debug.Assert (index < Count);
                    return ref blocks! [index];
                }
            }

            #endregion

            #region ================== Static methods

            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            public static BlockList Create () {
                var ret = new BlockList ();

                ret.blocks = Array.Empty<Pointer<ImmixBlockHeader>> ();
                ret.Count = 0;

                return ret;
            }

            #endregion

            #region ================== Instance methods

            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            private void EnsureHeadroom (int num) {
                if (blocks.Length >= Count + num)
                    return;

                var oldBlocks = blocks;
                var newCount = 1 << (sizeof (int) * 8 - BitOperations.LeadingZeroCount ((uint) (Count + num - 1)));

                blocks = new Pointer<ImmixBlockHeader> [newCount];
                Array.Copy (oldBlocks, blocks, Count);
            }

            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            public void Add (Pointer<ImmixBlockHeader> block) {
                if (Count >= blocks.Length)
                    EnsureHeadroom (MinHeadroomCount);

                blocks [Count++] = block;
            }

            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            public void Remove (int pos) {
                if (pos < Count - 1)
                    Array.Copy (blocks, pos + 1, blocks, pos - 1, Count - pos);

                Count--;
            }

            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            public void RemoveAll () => Count = 0;

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

            public int BumpSpace {
                [MethodImpl (MethodImplOptions.AggressiveInlining)]
                get => (int) (BumpLimit - BumpPointer);
            }

            public ImmixBlockHeader* CurrentBlock {
                [MethodImpl (MethodImplOptions.AggressiveInlining)]
                get => BlocksList! [CurBlockIndex];
            }
            public ref Pointer<ImmixBlockHeader> CurrentBlockRef {
                [MethodImpl (MethodImplOptions.AggressiveInlining)]
                get => ref BlocksList! [CurBlockIndex];
            }

            #endregion

            #region ================== Instance methods

            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            public void Initialize () {
                BlocksList = BlockList.Create ();

                var newBlock = ImmixGC_GlobalAllocator.GetBlock ();
                newBlock->UsageLevel = ImmixBlockUsage.Filled;
                BlocksList.Add (newBlock);

                CurBlockIndex = 0;
                SetNewAllocData (0, ImmixConstants.LinesCount);
            }

            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            private void SetNewAllocData (int holeOffs, int holeSize) {
                BumpPointer = (byte*) CurrentBlock + ((ImmixConstants.HeaderLines + holeOffs) * ImmixConstants.LineSize);
                BumpLimit = BumpPointer + (holeSize * ImmixConstants.LineSize);
            }

            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            private bool ScanForHole (int minSize, out int holeOffs, out int holeSize) {
                // If the block is empty, return the whole block's length as a hole.
                if (CurrentBlock->UsageLevel == ImmixBlockUsage.Empty) {
                    holeOffs = 0;
                    holeSize = ImmixConstants.TrueBlockSize;
                    return true;
                }

                int posOffs = (int) (BumpPointer - (byte*) CurrentBlock + ImmixConstants.HeaderLines);
                int startPos = posOffs / ImmixConstants.LineSize;
                if (startPos * ImmixConstants.LineSize < posOffs)
                    startPos++;

                var lineMapSpan = CurrentBlock->LineMapSpan;
                bool lastLineMarked = false; // Used for skipping implicit marks
                for (int i = startPos; i < ImmixConstants.LinesCount; i++) {
                    int curHoleSize = 0;

                    if (lineMapSpan [i] != 0) {
                        lastLineMarked = true;
                        continue;
                    } else if (lastLineMarked) {
                        lastLineMarked = false;
                        continue;
                    }

                    for (int j = i + 1; j < ImmixConstants.LinesCount; j++) {
                        if (lineMapSpan [i] != 0)
                            break;

                        curHoleSize++;
                    }

                    if (curHoleSize > 0 && (curHoleSize * ImmixConstants.LineSize) >= minSize) {
                        holeOffs = ImmixConstants.LineSize * i;
                        holeSize = ImmixConstants.LineSize * curHoleSize;
                        return true;
                    }
                }

                holeOffs = 0;
                holeSize = 0;
                return false;
            }

            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            public void EnsureAllocatable (int minSize) {
                if (ScanForHole (minSize, out var holeOffs, out var holeSize)) {
                    SetNewAllocData (holeOffs, holeSize);
                    return;
                }

                // Try to find a block with a hole big enough.
                while (CurBlockIndex < BlocksList.Count) {
                    if (CurrentBlock->UsageLevel != ImmixBlockUsage.Recyclable) {
                        CurBlockIndex++;
                        continue;
                    }

                    if (ScanForHole (minSize, out holeOffs, out holeSize)) {
                        SetNewAllocData (holeOffs, holeSize);
                        CurrentBlock->UsageLevel = ImmixBlockUsage.Filled;
                        return;
                    }

                    SetNewAllocData (0, 0);
                    CurBlockIndex++;
                }

                // If we didn't find one, allocate a new one.
                BlocksList.Add (ImmixGC_GlobalAllocator.GetBlock ());
                CurBlockIndex = BlocksList.Count - 1;

                SetNewAllocData (0, ImmixConstants.LinesCount);
                CurrentBlock->UsageLevel = ImmixBlockUsage.Filled;
            }

            public void Dispose () {
                BlocksList.RemoveAll ();
                BumpPointer = null;
                BumpLimit = null;

                CurBlockIndex = -1;
            }

            #endregion
        }

        #endregion

        #region ================== Instance fields

        private AllocData allocData;
        private AllocData overflowAllocData;

        #endregion

        #region ================== Constructors

        public ImmixGC () {
            allocData = new ();
            overflowAllocData = new ();

            allocData.Initialize ();
            overflowAllocData.Initialize ();
        }

        #endregion

        #region ================== Instance methods

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public void GetInfo (out ImmixDebugInfo info, out ImmixDebugInfo overflowInfo) {
            info = GetInfo (ref allocData);
            overflowInfo = GetInfo (ref overflowAllocData);
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private ImmixDebugInfo GetInfo (ref AllocData alloc) {
            int blockStride = ImmixConstants.LinesCount / 8;
            if (blockStride * 8 != ImmixConstants.LinesCount)
                blockStride++;

            var blocks = UnmanagedArray<byte>.GetArray (alloc.BlocksList.Count * blockStride);
            var blocksSpan = blocks.Span;

            int fullBlocks = 0;
            int emptyBlocks = 0;
            int recyclableBlocks = 0;

            int blockIdx = 0;
            foreach (var block in alloc.BlocksList.Span) {
                switch (block.Address->UsageLevel) {
                    case ImmixBlockUsage.Empty: emptyBlocks++; break;
                    case ImmixBlockUsage.Filled: fullBlocks++; break;
                    case ImmixBlockUsage.Recyclable: recyclableBlocks++; break;
                }

                var lineMapSpan = block.Address->LineMapSpan;
                var linesSpan = blocksSpan.Slice (blockStride * blockIdx, blockStride);
                for (int lineIdx = 0; lineIdx < ImmixConstants.LinesCount; lineIdx++) {
                    byte lineMapChunk = 0;

                    int chunkLen = Math.Min (8, ImmixConstants.LinesCount - lineIdx);
                    var lineMapMidSpan = lineMapSpan.Slice (lineIdx, chunkLen);
                    for (byte i = 0; i < chunkLen; i++)
                        lineMapChunk |= (byte) ((lineMapMidSpan [i] & 1) << i);

                    linesSpan [lineIdx / 8] = lineMapChunk;
                }

                blockIdx++;
            }

            return new ImmixDebugInfo {
                BlockCount = alloc.BlocksList.Count,
                EmptyBlocks = emptyBlocks,
                FullBlocks = fullBlocks,
                RecyclableBlocks = recyclableBlocks,

                CurrentBlockIndex = alloc.CurBlockIndex,
                BlockStride = blockStride,
                Blocks = blocks,
            };
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public void* AllocObject (ref ES_ObjectHeader headerData, int allocSize) {
            // Large objects cannot be allocated with Immix.
            Debug.Assert (allocSize < ES_GarbageCollector.LargeObjectSize, "Large objects must be allocated in the LOH.");

            ref var allocSpace = ref allocData;

            // If it's a medium object and there's not enough space to allocate in, use the overflow allocator.
            if (headerData.Flags.HasFlag (ES_ObjectFlags.MediumObject) & (allocSize > allocSpace.BumpSpace))
                allocSpace = ref overflowAllocData;

            // Ensure there's enough space to allocate the object.
            if (allocSize > allocSpace.BumpSpace)
                allocSpace.EnsureAllocatable (allocSize);

            // Grab the header's pointer and bump the allocation pointer.
            var allocStart = allocSpace.BumpPointer;
            allocSpace.BumpPointer += allocSize;

            return allocStart;
        }

        #endregion

        #region ================== IDisposable support

        private bool disposedValue;
        public bool IsDisposed => disposedValue;

        ~ImmixGC () {
            if (!disposedValue)
                DoDispose ();
        }

        private void DoDispose () {
            if (disposedValue)
                return;

            allocData.Dispose ();
            overflowAllocData.Dispose ();

            disposedValue = true;
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }

    internal unsafe static class ImmixGC_GlobalAllocator {
        private static List<Pointer<ImmixChunkHeader>> chunks;
        private static List<int> freeChunks;
        private static int totalChunks;

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        static ImmixGC_GlobalAllocator () {
            Debug.Assert ((ImmixConstants.BlockSize & (ImmixConstants.BlockSize - 1)) == 0,
                "The block size must be a power of two.");
            Debug.Assert (ES_GarbageCollector.LargeObjectSize <= ImmixConstants.TrueBlockSize,
                "The large object size must be smaller than the true block size.");
            Debug.Assert (ImmixConstants.ChunkMetadataSize <= ImmixConstants.BlockSize,
                "The size of the chunk header + the number of lines in a block must be smaller than BlockSize");

            chunks = new ();
            freeChunks = new ();
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private static ImmixChunkHeader* AllocateChunk () {
            var chunkMemPtr = (nint) mi_malloc_aligned_at (
                (nuint) ImmixConstants.TotalChunkSize,
                ImmixConstants.BlockSize,
                (nuint) ImmixConstants.ChunkMetadataSize);

            // Assign the chunk header.
            var chunkPtr = (ImmixChunkHeader*) chunkMemPtr;
            chunkPtr->Initialize ();

            // Initialize the blocks.
            var lineMapsStart = (byte*) chunkMemPtr + sizeof (ImmixChunkHeader);

            for (int i = 0; i < ImmixConstants.BlocksPerChunk; i++)
                *ImmixChunkHeader.GetBlock (chunkPtr, i) = new ImmixBlockHeader (lineMapsStart + (ImmixConstants.LinesCount * i));

            // Add the chunk to the lists and increment the total chunks counter.
            // TODO: Add these by address order.
            chunks.Add (chunkPtr);
            freeChunks.Add (chunks.Count - 1);
            Interlocked.Increment (ref totalChunks);

            return chunkPtr;
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        internal static ImmixBlockHeader* GetBlock () {
            ImmixBlockHeader* block = null;

            lock (chunks) {
                if (freeChunks.Count > 0) {
                    var chunkIdx = freeChunks [0];
                    var chunk = chunks [chunkIdx].Address;

                    Debug.Assert (chunk->FreeBlocks > 0);

                    for (int blockIdx = 0; blockIdx < ImmixConstants.BlocksPerChunk; blockIdx++) {
                        if (chunk->BlockUsed [blockIdx] != 0)
                            continue;

                        block = ImmixChunkHeader.GetBlock (chunk, blockIdx);
                        chunk->BlockUsed [blockIdx] = 0xFF;

                        break;
                    }

                    Debug.Assert (block != null, "Free chunks list contains a full chunk.");

                    if (--chunk->FreeBlocks < 1)
                        freeChunks.RemoveAt (0);
                } else {
                    var newChunk = AllocateChunk ();
                    newChunk->BlockUsed [0] = 0xFF;
                    newChunk->FreeBlocks--;

                    block = ImmixChunkHeader.GetBlock (newChunk, 0);
                }
            }

            if (block->UsageLevel != ImmixBlockUsage.Empty) {
                block->UsageLevel = ImmixBlockUsage.Empty;
                block->LineMapSpan.Clear ();
            }

            return block;
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        internal static void ReturnBlocks (Span<Pointer<ImmixBlockHeader>> block) {
            throw new NotImplementedException ("[TODO] Implement returning Immix blocks.");
            /*Debug.Assert (block.UsageLevel != ImmixBlockUsage.Empty);

            block.LineMap.Span.Clear ();
            lock (freeBlocks) {
                freeBlocks.BinarySearch ()
                freeBlocks.Add (block);
            }*/
        }

        internal static void DoDispose () {
            foreach (var chunkPtr in chunks)
                mi_free (chunkPtr.Address);

            chunks.Clear ();
            freeChunks.Clear ();
        }
    }
}
