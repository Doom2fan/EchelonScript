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
using System.Linq;
using ChronosLib.Pooled;
using Collections.Pooled;
using CommunityToolkit.HighPerformance;
using EchelonScriptCommon;
using EchelonScriptCommon.Utilities;
using static TerraFX.Interop.Mimalloc;

namespace EchelonScriptCompiler.Data;

public class UnmanagedIdentifierComparer : IComparer<ArrayPointer<byte>> {
    public static UnmanagedIdentifierComparer Instance { get; } = new ();

    public unsafe int Compare ([DisallowNull] ArrayPointer<byte> x, [DisallowNull] ArrayPointer<byte> y) {
        var comp = ((IntPtr) x.Elements).CompareTo ((IntPtr) y.Elements);

        if (comp != 0)
            return comp;

        comp = x.Length.CompareTo (y.Length);

        if (comp != 0)
            return comp;

        return 0;
    }
}

public unsafe class UnmanagedIdentifierPool : IDisposable {
    #region ================== Constants

    public const int BaseBlockSize = 1024;

    #endregion

    #region ================== Structs

    protected struct IdData {
        public enum DataType {
            None,
            SingleId,
            IdArray,
        }

        #region ================== Instance fields

        public readonly DataType Type;
        private void* pointer;
        public readonly int Length;
        public readonly int HashCode;

        #endregion

        #region ================== Constructors

        public IdData (int hashCode) {
            Type = DataType.None;
            HashCode = hashCode;

            pointer = null;
            Length = 0;
        }

        public IdData (int hashCode, ArrayPointer<byte> arrPtr) {
            Type = DataType.SingleId;

            pointer = arrPtr.Elements;
            Length = arrPtr.Length;

            HashCode = hashCode;
        }

        public IdData (int hashCode, ArrayPointer<ArrayPointer<byte>> arrPtr) {
            Type = DataType.IdArray;

            pointer = arrPtr.Elements;
            Length = arrPtr.Length;

            HashCode = hashCode;
        }

        #endregion

        #region ================== Instance methods

        public ArrayPointer<byte> GetSingle () {
            if (Type != DataType.SingleId)
                throw new Exception ("Data does not match requested type.");

            return new ArrayPointer<byte> {
                Elements = (byte*) pointer,
                Length = Length,
            };
        }

        public ArrayPointer<ArrayPointer<byte>> GetArray () {
            if (Type != DataType.IdArray)
                throw new Exception ("Data does not match requested type.");

            return new ArrayPointer<ArrayPointer<byte>> {
                Elements = (ArrayPointer<byte>*) pointer,
                Length = Length,
            };
        }

        #endregion
    }

    protected struct MemoryArea<T>
        where T : unmanaged {
        #region ================== Instance fields

        public readonly T* MemArea;
        public readonly int Size;
        public int BytesUsed;

        #endregion

        #region ================== Constructors

        public MemoryArea (T* ptr, int length) {
            MemArea = ptr;
            Size = length;

            BytesUsed = 0;
        }

        #endregion
    }

    #endregion

    #region ================== Classes

    protected class IdDataHashCodeComparer : IComparer<IdData> {
        public int Compare ([DisallowNull] IdData x, [DisallowNull] IdData y) => x.HashCode.CompareTo (y.HashCode);
    }

    #endregion

    #region ================== Static fields

    protected static IdDataHashCodeComparer hashCodeComparer { get; } = new ();

    #endregion

    #region ================== Constructors

    public UnmanagedIdentifierPool () {
        idsList = new PooledList<IdData> ();
        bytesMemoryAreas = new StructPooledList<MemoryArea<byte>> (CL_ClearMode.Auto);
    }

    #endregion

    #region ================== Instance fields

    protected PooledList<IdData> idsList;
    protected StructPooledList<MemoryArea<byte>> bytesMemoryAreas;

    #endregion

    #region ================== Instance methods

    protected ArrayPointer<byte> GetBytesMemory (int length) {
        var selectedAreaIdx = -1;

        var areasSpan = bytesMemoryAreas.Span;
        for (var i = areasSpan.Length - 1; i >= 0; i--) {
            ref var memArea = ref areasSpan [i];

            if ((memArea.Size - memArea.BytesUsed) <= length) {
                selectedAreaIdx = i;
                break;
            }
        }

        if (selectedAreaIdx == -1) {
            var size = (int) (Math.Ceiling (length / (float) BaseBlockSize) * BaseBlockSize);
            var memPtr = (byte*) mi_malloc ((nuint) size);
            var newMemoryArea = new MemoryArea<byte> (memPtr, size);

            bytesMemoryAreas.Add (newMemoryArea);
            selectedAreaIdx = bytesMemoryAreas.Count - 1;
        }

        ref var selectedArea = ref bytesMemoryAreas.Span [selectedAreaIdx];

        Debug.Assert ((selectedArea.Size - selectedArea.BytesUsed) >= length);

        var ret = new ArrayPointer<byte> {
            Elements = selectedArea.MemArea + selectedArea.BytesUsed,
            Length = length,
        };
        selectedArea.BytesUsed += length;

        return ret;
    }

    protected ArrayPointer<byte> GetIdBytes (ReadOnlySpan<byte> bytes) {
        var newMem = GetBytesMemory (bytes.Length);

        bytes.CopyTo (newMem.Span);

        return newMem;
    }

    protected ref IdData HashCodeBinarySearch (int hashCode) {
        var hashCodeIdData = new IdData (hashCode);
        var idx = idsList.BinarySearch (hashCodeIdData, hashCodeComparer);

        if (idx < 0) {
            idx = ~idx;

            idsList.Insert (idx, hashCodeIdData);
        }

        return ref idsList.Span [idx];
    }

    public ArrayPointer<byte> GetIdentifier (ReadOnlySpan<byte> bytes) {
        CheckDisposed ();

        if (bytes.Length < 1)
            return ArrayPointer<byte>.Null;

        var hashCode = bytes.GetDjb2HashCode ();

        ArrayPointer<byte>? ret = null;
        ref var idData = ref HashCodeBinarySearch (hashCode);

        switch (idData.Type) {
            case IdData.DataType.None: {
                var idBytes = GetIdBytes (bytes);

                idData = new IdData (hashCode, idBytes);

                ret = idBytes;

                break;
            }

            case IdData.DataType.SingleId: {
                var singleId = idData.GetSingle ();

                if (singleId.Span.SequenceEqual (bytes))
                    ret = singleId;
                else {
                    var arrPtr = new ArrayPointer<ArrayPointer<byte>> {
                        Elements = (ArrayPointer<byte>*) mi_malloc ((nuint) (sizeof (ArrayPointer<byte>) * 2)),
                        Length = 2,
                    };

                    var idBytes = GetIdBytes (bytes);
                    var arrPtrSpan = arrPtr.Span;
                    arrPtrSpan [0] = singleId;
                    arrPtrSpan [1] = idBytes;

                    ret = idBytes;
                }

                break;
            }

            case IdData.DataType.IdArray: {
                var idsArr = idData.GetArray ();
                var idsArrSpan = idsArr.Span;

                foreach (var id in idsArrSpan) {
                    if (id.Span.SequenceEqual (bytes)) {
                        ret = id;
                        break;
                    }
                }

                if (ret == null) {
                    var newLen = idsArr.Length + 1;
                    var newArrPtr = new ArrayPointer<ArrayPointer<byte>> {
                        Elements = (ArrayPointer<byte>*) mi_malloc ((nuint) (sizeof (ArrayPointer<byte>) * newLen)),
                        Length = newLen,
                    };

                    var newArrPtrSpan = newArrPtr.Span;
                    var idBytes = GetIdBytes (bytes);

                    idsArrSpan.CopyTo (newArrPtrSpan);
                    newArrPtrSpan [^0] = idBytes;

                    idData = new IdData (hashCode, newArrPtr);
                    mi_free (idsArr.Elements);

                    ret = idBytes;
                }

                break;
            }
        }

        if (ret == null)
            throw new Exception ("This shouldn't happen.");

        return ret.Value;
    }

    public ArrayPointer<byte> GetIdentifier (ReadOnlySpan<char> chars) {
        CheckDisposed ();

        if (chars.Length < 1)
            return ArrayPointer<byte>.Null;

        Span<byte> nameBytes = stackalloc byte [chars.Length];
        ES_Encodings.Identifier.GetBytes (chars, nameBytes);

        return GetIdentifier (nameBytes);
    }

    protected void CheckDisposed () {
        if (disposedValue)
            throw new ObjectDisposedException (nameof (UnmanagedIdentifierPool));
    }

    #endregion

    #region ================== IDisposable support

    private bool disposedValue = false;

    ~UnmanagedIdentifierPool () {
        if (!disposedValue)
            DoDispose ();
    }

    protected virtual void DoDispose () {
        if (disposedValue)
            return;

        foreach (var idData in idsList) {
            if (idData.Type == IdData.DataType.IdArray)
                mi_free (idData.GetArray ().Elements);
        }

        // Memory areas
        foreach (var memArea in bytesMemoryAreas)
            mi_free (memArea.MemArea);
        bytesMemoryAreas.Dispose ();
        idsList.Dispose ();

        disposedValue = true;
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion
}
