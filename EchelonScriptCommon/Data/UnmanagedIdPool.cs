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
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using ChronosLib.Pooled;
using ChronosLib.Unmanaged;
using CommunityToolkit.HighPerformance;
using EchelonScriptCommon.Utilities;
using static TerraFX.Interop.Mimalloc;

namespace EchelonScriptCommon.Data;

public unsafe struct ES_Identifier : IEquatable<ES_Identifier> {
    internal const int EmptyIndex = -1;

    public static ES_Identifier Empty {
        get => new (IntPtr.Zero, EmptyIndex);
    }

    #region ================== Instance properties

    internal readonly IntPtr idPoolHandle;
    internal readonly int Index;

    internal ES_IdentifierPool idPool {
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        get {
            var pool = GCHandle.FromIntPtr (idPoolHandle).Target as ES_IdentifierPool;
            Debug.Assert (pool is not null);
            return pool;
        }
    }

    #endregion

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal ES_Identifier (IntPtr pool, int id) {
        idPoolHandle = pool;
        Index = id;
    }

    #region ================== Instance methods

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public ReadOnlySpan<byte> GetBytesSpan ()
        => Index != EmptyIndex ? idPool.GetBytesSpan (this) : ReadOnlySpan<byte>.Empty;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public ReadOnlySpan<char> GetCharsSpan ()
        => Index != EmptyIndex ? idPool.GetCharsSpan (this) : ReadOnlySpan<char>.Empty;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public override bool Equals ([NotNullWhen (true)] object? obj) {
        if (obj is ES_Identifier other)
            return Equals (other);

        return false;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public override int GetHashCode () => HashCode.Combine (((nint) idPoolHandle).GetHashCode (), Index.GetHashCode ());

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public bool Equals (ES_Identifier other) {
        if (idPoolHandle == other.idPoolHandle)
            return Index == other.Index;

        var bytes = GetBytesSpan ();
        var otherBytes = other.GetBytesSpan ();

        return bytes.SequenceEqual (otherBytes);
    }

    #endregion

    #region ================== Operators

    public static bool operator == (ES_Identifier lhs, ES_Identifier rhs) => lhs.Equals (rhs);
    public static bool operator != (ES_Identifier lhs, ES_Identifier rhs) => !lhs.Equals (rhs);

    #endregion
}

public unsafe sealed class ES_IdentifierPool : IDisposable {
    #region ================== Constants

    private const int IdBlockSize = 1024 * 4;
    private const int BaseBlockSize = 1024 * 4;
    /// <summary>The lowest amount of memory a block can have before it's considered full.</summary>
    private const int MinimumBlockSize = 16;

    #endregion

    #region ================== Structs

    private struct IdData {
        public readonly ArrayPointer<byte> Bytes;
        public readonly ArrayPointer<char> Chars;
        public readonly ES_Identifier Identifier;

        public IdData (ArrayPointer<byte> bytes, ArrayPointer<char> chars, ES_Identifier id) {
            Bytes = bytes;
            Chars = chars;
            Identifier = id;
        }
    }

    private struct IdText : IEquatable<IdText> {
        public readonly ArrayPointer<char> ArrayPointer;
        public readonly PooledArray<char> PooledArray;

        public IdText (ArrayPointer<char> chars) {
            ArrayPointer = chars;
            PooledArray = PooledArray<char>.Empty ();
        }

        public IdText (PooledArray<char> chars) {
            PooledArray = chars;
            ArrayPointer = ArrayPointer<char>.Null;
        }

        #region ================== Instance methods

        private ReadOnlySpan<char> Span => ArrayPointer.Elements != null ? ArrayPointer.Span : PooledArray.Span;

        public override bool Equals ([NotNullWhen (true)] object? obj) {
            if (obj is IdText other)
                return Equals (other);

            return false;
        }

        public bool Equals (IdText other) => Span.SequenceEqual (other.Span);

        public override int GetHashCode ()
            => CultureInfo.InvariantCulture.CompareInfo.GetHashCode (Span, CompareOptions.Ordinal);

        #endregion
    }

    private struct MemoryArena : IDisposable {
        private struct MemoryArea {
            public readonly byte* MemArea;
            public readonly int Size;
            public int BytesUsed;

            public int BytesLeft => Size - BytesUsed;

            public MemoryArea (byte* ptr, int length) {
                MemArea = ptr;
                Size = length;

                BytesUsed = 0;
            }
        }

        private List<MemoryArea> memoryAreas;
        private List<MemoryArea> fullMemoryAreas;

        public void Dispose () {
            foreach (var memArea in memoryAreas)
                mi_free (memArea.MemArea);

            foreach (var memArea in fullMemoryAreas)
                mi_free (memArea.MemArea);

            memoryAreas.Clear ();
            fullMemoryAreas.Clear ();
        }

        public void Initialize () {
            memoryAreas = new List<MemoryArea> ();
            fullMemoryAreas = new List<MemoryArea> ();
        }

        public ArrayPointer<T> GetMemory<T> (int length) where T : unmanaged {
            if (length == 0)
                return ArrayPointer<T>.Null;

            var selectedAreaIdx = -1;
            var allocSize = length * sizeof (T);

            var areasSpan = memoryAreas.AsSpan ();
            for (var i = areasSpan.Length - 1; i >= 0; i--) {
                ref var memArea = ref areasSpan [i];

                if (memArea.BytesLeft >= allocSize) {
                    selectedAreaIdx = i;
                    break;
                }
            }

            if (selectedAreaIdx == -1) {
                var newBlockSize = (allocSize + BaseBlockSize - 1) / BaseBlockSize * BaseBlockSize;
                var newMemoryArea = new MemoryArea ((byte*) mi_malloc ((nuint) newBlockSize), newBlockSize);

                memoryAreas.Add (newMemoryArea);
                selectedAreaIdx = memoryAreas.Count - 1;
            }

            ref var selectedArea = ref memoryAreas.AsSpan () [selectedAreaIdx];

            Debug.Assert ((selectedArea.Size - selectedArea.BytesUsed) >= allocSize);

            var ret = new ArrayPointer<T> ((T*) (selectedArea.MemArea + selectedArea.BytesUsed), length);
            selectedArea.BytesUsed += allocSize;

            if (selectedArea.BytesLeft <= MinimumBlockSize) {
                fullMemoryAreas.Add (selectedArea);

                if (selectedAreaIdx < memoryAreas.Count - 1)
                    memoryAreas [selectedAreaIdx] = memoryAreas [^1];

                memoryAreas.RemoveAt (memoryAreas.Count - 1);
            }

            return ret;
        }

        public ArrayPointer<T> GetMemoryAndCopy<T> (ReadOnlySpan<T> data) where T : unmanaged {
            var mem = GetMemory<T> (data.Length);
            data.CopyTo (mem.Span);
            return mem;
        }

        public ArrayPointer<T> GetMemoryAndCopy<T> (Span<T> data) where T : unmanaged
            => GetMemoryAndCopy ((ReadOnlySpan<T>) data);
    }

    #endregion

    #region ================== Instance fields and properties

    private GCHandle thisHandle;

    private IdData [] idsList;
    private int idsCount;
    private Dictionary<IdText, int> idsMap;

    private MemoryArena memArena;

    public ES_Identifier IdEmpty {
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        get;
        private init;
    }

    #endregion

    public ES_IdentifierPool () {
        IsDisposed = false;

        thisHandle = GCHandle.Alloc (this, GCHandleType.Weak);

        idsList = Array.Empty<IdData> ();
        idsMap = new (1000);

        memArena = new ();
        memArena.Initialize ();

        IdEmpty = GetIdentifier (ArrayPointer<char>.Null);
    }

    #region ================== Instance methods

    #region Public

    public ES_Identifier GetIdentifier (ReadOnlySpan<char> chars) {
        CheckDisposed ();

        using var charsMem = UnmanagedArray<char>.GetArray (chars.Length);
        chars.CopyTo (charsMem);

        return GetIdentifier (charsMem);
    }

    public ES_Identifier GetIdentifier (ArrayPointer<char> chars) {
        CheckDisposed ();

        if (idsMap.TryGetValue (new (chars), out var val))
            return idsList [val].Identifier;

        EnsureIdsCapacity (idsCount + 1);

        var index = idsCount++;

        var id = new ES_Identifier (GCHandle.ToIntPtr (thisHandle), index);

        var enc = ES_Encodings.Identifier;
        var bytes = memArena.GetMemory<byte> (enc.GetByteCount (chars.Span));
        enc.GetBytes (chars.Span, bytes.Span);
        var charsMem = memArena.GetMemoryAndCopy (chars.Span);

        idsList [index] = new IdData (bytes, charsMem, id);
        idsMap [new (charsMem)] = index;

        return id;
    }

    public ES_Identifier GetIdentifier (UnmanagedArray<char> chars)
        => GetIdentifier (new ArrayPointer<char> (chars.Pointer, chars.Length));

    public ES_Identifier GetIdentifier (string str) => GetIdentifier (str.AsSpan ());

    public ReadOnlySpan<byte> GetBytesSpan (ES_Identifier id) {
        if (id.Index == ES_Identifier.EmptyIndex || id.idPoolHandle != GCHandle.ToIntPtr (thisHandle))
            return ReadOnlySpan<byte>.Empty;

        Debug.Assert (id.Index >= 0 && id.Index < idsList.Length);
        return idsList [id.Index].Bytes.Span;
    }

    public ReadOnlySpan<char> GetCharsSpan (ES_Identifier id) {
        if (id.Index == ES_Identifier.EmptyIndex || id.idPoolHandle != GCHandle.ToIntPtr (thisHandle))
            return ReadOnlySpan<char>.Empty;

        Debug.Assert (id.Index >= 0 && id.Index < idsList.Length);
        return idsList [id.Index].Chars.Span;
    }

    #endregion

    #region Private

    private void CheckDisposed () {
        if (IsDisposed)
            throw new ObjectDisposedException (nameof (ES_IdentifierPool));
    }

    private void EnsureIdsCapacity (int capacity) {
        if (idsList.Length >= capacity)
            return;

        var newSize = (capacity + (IdBlockSize - 1)) / IdBlockSize * IdBlockSize;
        Debug.Assert (newSize >= capacity);

        var oldList = idsList;
        idsList = new IdData [newSize];

        oldList.AsSpan () [..idsCount].CopyTo (idsList);
    }

    #endregion

    #endregion

    #region ================== IDisposable support

    public bool IsDisposed {
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        get;
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private set;
    }

    private void DoDispose () {
        if (IsDisposed)
            return;

        idsList = Array.Empty<IdData> ();
        idsCount = 0;
        idsMap.Clear ();

        memArena.Dispose ();

        thisHandle.Free ();

        IsDisposed = true;
    }

    ~ES_IdentifierPool () {
        if (!IsDisposed)
            Dispose ();
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion
}
