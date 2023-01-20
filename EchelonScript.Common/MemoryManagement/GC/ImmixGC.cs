/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
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
using ChronosLib.Pooled;
using ChronosLib.Unmanaged;
using EchelonScript.Common.Utilities;
using static TerraFX.Interop.Mimalloc;

namespace EchelonScript.Common.GarbageCollection.Immix;

internal enum ImmixBlockUsage : short {
    Empty = 0,
    Filled,
    Recyclable,
}

[Flags]
internal enum ImmixBlockFlags : short {
    Evacuating = 1 << 0,
}

public unsafe struct ImmixConstants {
    // Make sure BlockSize is < 64KB, and is a multiple of LineSize.
    // Must be a power of two.
    public const int BlockSize = 32 * 1024;
    public const int LineSize = 128;
    public static int TrueBlockSize => LinesCount * LineSize;

    public const int BlocksPerChunk = 8;
    public static int ChunkMetadataSize => sizeof (ImmixChunkHeader) + LinesCount * BlocksPerChunk;
    public static int TotalChunkSize => ChunkMetadataSize + BlockSize * BlocksPerChunk;

    public const int BareLinesCount = BlockSize / LineSize;
    public static int HeaderLines => (sizeof (ImmixBlockHeader) + (LineSize - 1)) / LineSize;
    public static int LinesCount => BareLinesCount - HeaderLines;

    public const nint BlockMask = BlockSize - 1;
    public const nint BlockStartMask = ~(BlockSize - 1);
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
internal unsafe struct ImmixChunkHeader {
    public int ChunkIndex;
    public int FreeBlocks;
    public fixed byte BlockUsed [ImmixConstants.BlocksPerChunk];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Initialize (int idx) {
        ChunkIndex = idx;
        FreeBlocks = ImmixConstants.BlocksPerChunk;

        for (var i = 0; i < ImmixConstants.BlocksPerChunk; i++)
            BlockUsed [i] = 0;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static ImmixBlockAddress GetBlock (ImmixChunkHeader* chunk, int idx)
        => (ImmixBlockHeader*) ((byte*) chunk + ImmixConstants.ChunkMetadataSize + ImmixConstants.BlockSize * idx);
}

[StructLayout(LayoutKind.Sequential, Pack = 1)]
internal unsafe struct ImmixBlockHeader {
    public ImmixBlockUsage UsageLevel;
    public ImmixBlockFlags Flags;
    public int BlockNumber;

    public readonly byte* LineMap;
    public Span<byte> LineMapSpan => new (LineMap, ImmixConstants.LinesCount);

    public ImmixBlockHeader (int blockNum, byte* lineMapPtr) {
        UsageLevel = ImmixBlockUsage.Empty;
        Flags = 0;
        BlockNumber = blockNum;

        LineMap = lineMapPtr;
        LineMapSpan.Clear ();
    }

    internal void ClearMarks () => LineMapSpan.Clear ();
}

internal unsafe struct ImmixBlockAddress {
    public ImmixBlockHeader* Header;

    public void* Data => (byte*) Header + ImmixConstants.HeaderLines * ImmixConstants.LineSize;

    public ImmixChunkHeader* ChunkHeader {
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        get => (ImmixChunkHeader*) (
            (byte*) Header - ImmixConstants.BlockSize * Header->BlockNumber
            - ImmixConstants.ChunkMetadataSize
        );
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public ImmixBlockAddress (void* addr) => Header = (ImmixBlockHeader*) addr;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static implicit operator ImmixBlockAddress (void* obj) => new (obj);
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

        private ImmixBlockAddress [] blocks;

        #endregion

        #region ================== Instance properties

        public int Count {
            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            get;
            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            private set;
        }

        public Span<ImmixBlockAddress> Span {
            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            get => blocks.AsSpan (0, Count);
        }

        #endregion

        #region ================== Indexers

        public ref ImmixBlockAddress this [int index] {
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
            return new BlockList {
                blocks = Array.Empty<ImmixBlockAddress> (),
                Count = 0
            };
        }

        #endregion

        #region ================== Instance methods

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private void EnsureHeadroom (int num) {
            if (blocks.Length >= Count + num)
                return;

            var oldBlocks = blocks;
            var newCount = 1 << (sizeof (int) * 8 - BitOperations.LeadingZeroCount ((uint) (Count + num - 1)));

            blocks = new ImmixBlockAddress [newCount];
            Array.Copy (oldBlocks, blocks, Count);
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public void Add (ImmixBlockAddress block) {
            if (Count >= blocks.Length)
                EnsureHeadroom (MinHeadroomCount);

            blocks [Count++] = block;
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public void AddRange (ReadOnlySpan<ImmixBlockAddress> newBlocks) {
            if (Count - 1 + newBlocks.Length >= blocks.Length)
                EnsureHeadroom (newBlocks.Length + MinHeadroomCount);

            newBlocks.CopyTo (blocks.AsSpan (Count, newBlocks.Length));
            Count += newBlocks.Length;
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

        public ImmixBlockAddress CurrentBlock {
            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            get => BlocksList! [CurBlockIndex];
        }
        public ref ImmixBlockAddress CurrentBlockRef {
            [MethodImpl (MethodImplOptions.AggressiveInlining)]
            get => ref BlocksList! [CurBlockIndex];
        }

        #endregion

        #region ================== Instance methods

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public void Initialize () {
            BlocksList = BlockList.Create ();

            var newBlock = ImmixGC_GlobalAllocator.GetBlock ();
            newBlock.Header->UsageLevel = ImmixBlockUsage.Filled;
            BlocksList.Add (newBlock);

            CurBlockIndex = 0;
            SetNewAllocData (0, ImmixConstants.LinesCount);
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public void Reset () {
            CurBlockIndex = 0;
            SetNewAllocData (0, 0);
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private void SetNewAllocData (int holeOffs, int holeSize) {
            Debug.Assert (CurrentBlock.ChunkHeader != null);
            Debug.Assert (CurrentBlock.Header != null);
            Debug.Assert (CurrentBlock.Data != null);

            BumpPointer = (byte*) CurrentBlock.Data + (holeOffs * ImmixConstants.LineSize);
            BumpLimit = BumpPointer + (holeSize * ImmixConstants.LineSize);

            Debug.Assert (BumpPointer != null);
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private bool ScanForHole (int minSize, out int holeOffs, out int holeSize) {
            // If the block is empty, return the whole block's length as a hole.
            if (CurrentBlock.Header->UsageLevel == ImmixBlockUsage.Empty) {
                holeOffs = 0;
                holeSize = ImmixConstants.LinesCount;
                return true;
            }

            var posOffs = (int) (BumpLimit - (byte*) CurrentBlock.Data);
            var startPos = posOffs / ImmixConstants.LineSize;
            if (startPos * ImmixConstants.LineSize < posOffs)
                startPos++;

            var lineMapSpan = CurrentBlock.Header->LineMapSpan;
            var lastLineMarked = false; // Used for skipping implicit marks
            for (var i = startPos; i < ImmixConstants.LinesCount; i++) {
                var curHoleSize = 1;

                if (lineMapSpan [i] != 0) {
                    lastLineMarked = true;
                    continue;
                } else if (lastLineMarked) {
                    lastLineMarked = false;
                    continue;
                }

                for (var j = i + 1; j < ImmixConstants.LinesCount; j++) {
                    if (lineMapSpan [j] != 0)
                        break;

                    curHoleSize++;
                }

                if (curHoleSize > 0 && (curHoleSize * ImmixConstants.LineSize) >= minSize) {
                    holeOffs = i;
                    holeSize = curHoleSize;
                    return true;
                }
            }

            holeOffs = 0;
            holeSize = 0;
            return false;
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public void EnsureAllocatable (int minSize) {
            Debug.Assert (CurrentBlock.Header->UsageLevel != ImmixBlockUsage.Empty);

            if (ScanForHole (minSize, out var holeOffs, out var holeSize)) {
                SetNewAllocData (holeOffs, holeSize);
                return;
            }

            // Try to find a block with a hole big enough.
            while (CurBlockIndex < BlocksList.Count) {
                if (CurrentBlock.Header->UsageLevel == ImmixBlockUsage.Filled) {
                    CurBlockIndex++;
                    if (CurBlockIndex < BlocksList.Count)
                        SetNewAllocData (0, 0);
                    continue;
                }

                if (ScanForHole (minSize, out holeOffs, out holeSize)) {
                    SetNewAllocData (holeOffs, holeSize);
                    CurrentBlock.Header->UsageLevel = ImmixBlockUsage.Filled;
                    return;
                }

                CurBlockIndex++;
                SetNewAllocData (0, 0);
            }

            // If we didn't find one, allocate a new one.
            BlocksList.Add (ImmixGC_GlobalAllocator.GetBlock ());
            CurBlockIndex = BlocksList.Count - 1;
            CurrentBlock.Header->UsageLevel = ImmixBlockUsage.Filled;

            SetNewAllocData (0, ImmixConstants.LinesCount);
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

    public ImmixGC () {
        allocData = new ();
        overflowAllocData = new ();

        allocData.Initialize ();
        overflowAllocData.Initialize ();
    }

    #region ================== Instance methods

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public void GetInfo (out ImmixDebugInfo info, out ImmixDebugInfo overflowInfo) {
        info = GetInfo (ref allocData);
        overflowInfo = GetInfo (ref overflowAllocData);
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public void ClearMarks () {
        foreach (var block in allocData.BlocksList.Span)
            block.Header->ClearMarks ();
        foreach (var block in overflowAllocData.BlocksList.Span)
            block.Header->ClearMarks ();
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public void Sweep () {
        SweepBlocks (ref allocData);
        SweepBlocks (ref overflowAllocData);

        if (overflowAllocData.BlocksList.Count > 1) {
            var overflowBlock = overflowAllocData.BlocksList [^1];
            allocData.BlocksList.AddRange (overflowAllocData.BlocksList.Span [..^1]);

            overflowAllocData.BlocksList.RemoveAll ();
            overflowAllocData.BlocksList.Add (overflowBlock);
            overflowAllocData.Reset ();
        }

        allocData.Reset ();
        overflowAllocData.Reset ();
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private void SweepBlocks (ref AllocData allocData) {
        using var allocBlocks = new StructPooledList<ImmixBlockAddress> (CL_ClearMode.Auto);
        using var freeBlocks = new StructPooledList<ImmixBlockAddress> (CL_ClearMode.Auto);

        foreach (var block in allocData.BlocksList.Span) {
            SweepBlock (block);

            if (block.Header->UsageLevel == ImmixBlockUsage.Empty)
                freeBlocks.Add (block);
            else
                allocBlocks.Add (block);
        }

        if (allocBlocks.Count < 1) {
            Debug.Assert (freeBlocks.Count > 0);

            var block = freeBlocks [^1];
            block.Header->UsageLevel = ImmixBlockUsage.Filled;
            allocBlocks.Add (block);
            freeBlocks.RemoveEnd (1);
        }

        allocData.BlocksList.RemoveAll ();
        allocData.BlocksList.AddRange (allocBlocks.Span);
        allocData.Reset ();

        ImmixGC_GlobalAllocator.ReturnBlocks (freeBlocks.Span);
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private void SweepBlock (ImmixBlockAddress block) {
        var lineCount = 0;
        foreach (var line in block.Header->LineMapSpan)
            lineCount += (line != 0) ? 1 : 0;

        if (lineCount >= ImmixConstants.LinesCount)
            block.Header->UsageLevel = ImmixBlockUsage.Filled;
        else
            block.Header->UsageLevel = lineCount > 0 ? ImmixBlockUsage.Recyclable : ImmixBlockUsage.Empty;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private ImmixDebugInfo GetInfo (ref AllocData alloc) {
        var blockStride = ImmixConstants.LinesCount / sizeof (byte) * 8;
        if (blockStride * sizeof (byte) * 8 != ImmixConstants.LinesCount)
            blockStride++;

        var blocks = UnmanagedArray<byte>.GetArray (alloc.BlocksList.Count * blockStride);
        var blocksSpan = blocks.Span;

        var fullBlocks = 0;
        var emptyBlocks = 0;
        var recyclableBlocks = 0;

        var blockIdx = 0;
        foreach (var block in alloc.BlocksList.Span) {
            switch (block.Header->UsageLevel) {
                case ImmixBlockUsage.Empty: emptyBlocks++; break;
                case ImmixBlockUsage.Filled: fullBlocks++; break;
                case ImmixBlockUsage.Recyclable: recyclableBlocks++; break;
            }

            var lineMapSpan = block.Header->LineMapSpan;
            var linesSpan = blocksSpan.Slice (blockStride * blockIdx, blockStride);
            for (var lineIdx = 0; lineIdx < ImmixConstants.LinesCount;) {
                byte lineMapChunk = 0;

                var chunkLen = Math.Min (8, ImmixConstants.LinesCount - lineIdx);
                var lineMapMidSpan = lineMapSpan.Slice (lineIdx, chunkLen);
                for (byte i = 0; i < chunkLen; i++)
                    lineMapChunk |= (byte) ((lineMapMidSpan [i] != 0 ? 1 : 0) << i);

                linesSpan [lineIdx / 8] = lineMapChunk;

                lineIdx += chunkLen;
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
        Debug.Assert (allocSpace.BumpSpace >= allocSize);

        // Grab the header's pointer and bump the allocation pointer.
        Debug.Assert (allocData.BumpPointer != null);
        var allocStart = allocSpace.BumpPointer;
        allocSpace.BumpPointer += allocSize;

        return allocStart;
    }

    #endregion

    #region ================== IDisposable support

    public bool IsDisposed {
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        get;
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private set;
    }

    ~ImmixGC () {
        if (!IsDisposed)
            DoDispose ();
    }

    private void DoDispose () {
        if (IsDisposed)
            return;

        allocData.Dispose ();
        overflowAllocData.Dispose ();

        IsDisposed = true;
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
        chunkPtr->Initialize (totalChunks);

        // Initialize the blocks.
        var lineMapsStart = (byte*) chunkMemPtr + sizeof (ImmixChunkHeader);

        for (var i = 0; i < ImmixConstants.BlocksPerChunk; i++)
            *ImmixChunkHeader.GetBlock (chunkPtr, i).Header = new ImmixBlockHeader (i, lineMapsStart + (ImmixConstants.LinesCount * i));

        // Add the chunk to the lists and increment the total chunks counter.
        // TODO: Add these by address order.
        chunks.Add (chunkPtr);
        freeChunks.Add (totalChunks);
        Interlocked.Increment (ref totalChunks);

        return chunkPtr;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal static ImmixBlockAddress GetBlock () {
        ImmixBlockAddress block = null;

        lock (chunks) {
            if (freeChunks.Count > 0) {
                var chunkIdx = freeChunks [0];
                var chunk = chunks [chunkIdx].Address;

                Debug.Assert (chunk->FreeBlocks > 0);

                for (var blockIdx = 0; blockIdx < ImmixConstants.BlocksPerChunk; blockIdx++) {
                    if (chunk->BlockUsed [blockIdx] != 0)
                        continue;

                    block = ImmixChunkHeader.GetBlock (chunk, blockIdx);
                    chunk->BlockUsed [blockIdx] = 0xFF;

                    break;
                }

                Debug.Assert (block.Header != null, "Free chunks list contains a full chunk.");

                if (--chunk->FreeBlocks < 1)
                    freeChunks.RemoveAt (0);
            } else {
                var newChunk = AllocateChunk ();
                newChunk->BlockUsed [0] = 0xFF;
                newChunk->FreeBlocks--;

                block = ImmixChunkHeader.GetBlock (newChunk, 0);
            }
        }

        if (block.Header->UsageLevel != ImmixBlockUsage.Empty) {
            block.Header->UsageLevel = ImmixBlockUsage.Empty;
            block.Header->LineMapSpan.Clear ();
        }

        return block;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal static void ReturnBlocks (ReadOnlySpan<ImmixBlockAddress> blocks) {
        foreach (var block in blocks) {
            Debug.Assert (block.Header->UsageLevel == ImmixBlockUsage.Empty);
            block.Header->LineMapSpan.Clear ();
        }

        lock (chunks) {
            foreach (var block in blocks) {
                var chunk = block.ChunkHeader;

                if (chunk->FreeBlocks < 1)
                    freeChunks.Add (chunk->ChunkIndex);

                chunk->BlockUsed [block.Header->BlockNumber] = 0;
                chunk->FreeBlocks++;
            }
        }
    }

    internal static void DoDispose () {
        foreach (var chunkPtr in chunks)
            mi_free (chunkPtr.Address);

        chunks.Clear ();
        freeChunks.Clear ();
    }
}
