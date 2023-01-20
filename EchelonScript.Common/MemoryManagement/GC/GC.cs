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
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using ChronosLib.Pooled;
using CommunityToolkit.HighPerformance;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.GarbageCollection.Immix;
using EchelonScript.Common.Utilities;

namespace EchelonScript.Common.GarbageCollection;

public unsafe sealed partial class ES_GarbageCollector : IDisposable {
    private struct RootCollectionPair {
        public ES_ObjectAddress** Roots;
        public int Count;
    }

    #region ================== Constants

    // Make sure this is under ImmixGC.BlockSize!
    public const int LargeObjectSize = 8 * 1024;

    #endregion

    #region ================== Enums

    public enum CollectionMode : int {
        Forced = 0,
        Optimized,

        __Count,

        Default = Forced,
    }

    #endregion

    #region ================== Static fields

    [ThreadStatic]
    private static ES_GarbageCollector? garbageCollector;

    #endregion

    #region ================== Instance fields

    private ImmixGC immixGC;

    private List<RootCollectionPair> gcRoots;
    private bool markFlipped;

    #endregion

    internal ES_GarbageCollector () {
        immixGC = new ImmixGC ();

        gcRoots = new List<RootCollectionPair> ();
    }

    #region ================== Static methods

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private static void EnsureInitialized () {
        if (garbageCollector is not null)
            return;

        garbageCollector = new ES_GarbageCollector ();
        ChronosLib.StaticDisposables.AddDisposable (garbageCollector);
    }

    public static void GetImmixInfo (out ImmixDebugInfo info, out ImmixDebugInfo overflowInfo) {
        EnsureInitialized ();

        garbageCollector!.CheckDisposed ();
        garbageCollector!.immixGC.GetInfo (out info, out overflowInfo);
    }

    #region Collection

    /// <summary>Performs a garbage collection for the specified generations.</summary>
    /// <param name="gen">What generations to collect. Use -1 to collect all generations.</param>
    /// <exception cref="ArgumentOutOfRangeException"></exception>
    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static void PerformCollection (int gen = -1, CollectionMode mode = CollectionMode.Default) {
        EnsureInitialized ();
        garbageCollector!.PerformCollectionInternal (gen, mode);
    }

    /// <summary>Performs a collection if it's deemed optimal or necessary to do so.
    /// Equivalent to PerformCollection with gen = -1 and collection mode "Optimized".</summary>
    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static void CheckCollection () {
        EnsureInitialized ();
        garbageCollector!.CheckCollectionInternal (-1);
    }

    /// <summary>Performs a collection if it's deemed optimal or necessary to do so.
    /// Equivalent to PerformCollection with collection mode "Optimized".</summary>
    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static void CheckCollection (int gen) {
        EnsureInitialized ();
        garbageCollector!.CheckCollectionInternal (gen);
    }

    #endregion

    #region Allocation

    [UnmanagedCallersOnly (CallConvs = new [] { typeof (CallConvCdecl) })]
    public static void* AllocObjectUnmanaged (ES_MethodTable* type, bool immutable, byte pinned) {
        EnsureInitialized ();
        return garbageCollector!.AllocateObject (type, immutable, pinned != 0);
    }

    [UnmanagedCallersOnly (CallConvs = new [] { typeof (CallConvCdecl) })]
    public static void* AllocArrayUnmanaged (ES_MethodTable* elemType, ES_ArrayIndex* dimSizesPtr, int rank, bool immutable, byte pinned) {
        EnsureInitialized ();
        return garbageCollector!.AllocateArray (elemType, new Span<ES_ArrayIndex> (dimSizesPtr, rank), immutable, pinned != 0, true);
    }

    #region Objects

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private static T* AllocObject_Internal<T> (ES_MethodTable* type, bool immutable, bool pinned, T defaultVal) where T : unmanaged {
        EnsureInitialized ();

        var ptr = (T*) garbageCollector!.AllocateObject (type, immutable, pinned);
        *ptr = defaultVal;

        return ptr;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static ES_Object<T> AllocObject<T> (ES_MethodTable* type, bool pinned, T defaultVal = default) where T : unmanaged
        => new (new (AllocObject_Internal (type, false, pinned, defaultVal)));

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static ES_ObjectImmut<T> AllocObjectImmut<T> (T value, ES_MethodTable* type, bool pinned) where T : unmanaged
        => new (new (AllocObject_Internal (type, true, pinned, value)));

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static void* AllocObject (ES_MethodTable* type, bool immutable, bool pinned) {
        EnsureInitialized ();
        return garbageCollector!.AllocateObject (type, immutable, pinned);
    }

    #endregion

    #region Arrays

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static ES_Array1D<T> AllocArray<T> (ES_MethodTable* elemType, ReadOnlySpan<ES_ArrayIndex> dimSizes, bool pinned, T defaultElemVal = default) where T : unmanaged {
        EnsureInitialized ();

        var ptr = garbageCollector!.AllocateArray (elemType, dimSizes, false, pinned, false);

        var arraySpan = new Span<T> (ES_ArrayHeader.GetArrayDataPointer (ptr), ptr->Length);
        arraySpan.Fill (defaultElemVal);

        return new (ptr);
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static ES_ImmutArray1D<T> AllocArrayImmut<T> (ES_MethodTable* elemType, ReadOnlySpan<T> values, bool pinned) where T : unmanaged {
        EnsureInitialized ();

        Span<ES_ArrayIndex> dimSizes = stackalloc ES_ArrayIndex [1] { values.Length, };

        return new (AllocArrayImmut_Internal (elemType, dimSizes, values, pinned));
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static ES_ImmutArray2D<T> AllocArrayImmut<T> (ES_MethodTable* elemType, ReadOnlySpan2D<T> values, bool pinned) where T : unmanaged {
        EnsureInitialized ();

        Span<ES_ArrayIndex> dimSizes = stackalloc ES_ArrayIndex [2] { values.Width, values.Height, };

        var ptr = garbageCollector!.AllocateArray (elemType, dimSizes, false, pinned, false);

        var arraySpan = new Span<T> (ES_ArrayHeader.GetArrayDataPointer (ptr), ptr->Length);
        values.CopyTo (arraySpan);

        return new (ptr);
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static ES_ImmutArray1D<T> AllocArrayImmut<T> (ES_MethodTable* elemType, ReadOnlySpan<ES_ArrayIndex> dimSizes, ReadOnlySpan<T> values, bool pinned) where T : unmanaged {
        if (dimSizes.Length < 0 || dimSizes.Length > byte.MaxValue)
            throw new ArgumentOutOfRangeException (nameof (dimSizes));

        var totalSize = dimSizes [0];
        foreach (var dimSize in dimSizes [1..])
            totalSize *= dimSize;

        if (totalSize != values.Length)
            throw new ArgumentException ("Values span must match the total length of the array");

        EnsureInitialized ();

        return new (AllocArrayImmut_Internal (elemType, dimSizes, values, pinned));
    }

    private static ES_ArrayHeader* AllocArrayImmut_Internal<T> (ES_MethodTable* elemType, ReadOnlySpan<ES_ArrayIndex> dimSizes, ReadOnlySpan<T> values, bool pinned) where T : unmanaged {
        var ptr = garbageCollector!.AllocateArray (elemType, dimSizes, false, pinned, false);

        var arraySpan = new Span<T> (ES_ArrayHeader.GetArrayDataPointer (ptr), ptr->Length);
        values.CopyTo (arraySpan);

        return ptr;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static ES_ArrayHeader* AllocArray (ES_MethodTable* elemType, ReadOnlySpan<ES_ArrayIndex> dimSizes, bool immutable, bool pinned) {
        EnsureInitialized ();
        return garbageCollector!.AllocateArray (elemType, dimSizes, immutable, pinned, true);
    }

    #endregion

    #endregion

    #region Roots

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static void AddRoots (ES_ObjectAddress** roots, int count) {
        EnsureInitialized ();
        garbageCollector!.AddRootsInternal (roots, count);
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static void RemoveRoots (ES_ObjectAddress** roots) {
        EnsureInitialized ();
        garbageCollector!.RemoveRootsInternal (roots);
    }

    #endregion

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private static ES_ArrayIndex CalculateTotalArrayLength (ReadOnlySpan<ES_ArrayIndex> dimSizes) {
        var totalElemsCount = dimSizes [0];

        foreach (var dim in dimSizes [1..])
            totalElemsCount *= dim;

        return totalElemsCount;
    }

    #endregion

    #region ================== Instance methods

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private void CheckDisposed () {
        Debug.Assert (!IsDisposed, "GC object was disposed.");
    }

    #region Collection

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private void CheckCollectionInternal (int gen) {
        throw new NotImplementedException ("[TODO] Garbage collection determination not implemented yet.");
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private void PerformCollectionInternal (int gen = -1, CollectionMode mode = CollectionMode.Default) {
        if (gen < -1 || gen > 2)
            throw new ArgumentOutOfRangeException (nameof (gen), $"Argument must be between -1 (all) and 2.");

        switch (mode) {
            case CollectionMode.Forced:
                CollectGarbage (gen);
                break;

            case CollectionMode.Optimized:
                CheckCollectionInternal (gen);
                break;

            default:
                throw new ArgumentException ($"Invalid collection mode.", nameof (gen));
        }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private void CollectGarbage (int gen) {
        using var rootsList = new StructPooledList<Pointer<ES_ObjectAddress>> (CL_ClearMode.Auto);

        foreach (var rootsSet in gcRoots) {
            var rootsSetSpan = new Span<Pointer<ES_ObjectAddress>> (rootsSet.Roots, rootsSet.Count);

            foreach (var root in rootsSetSpan) {
                if (root.Address->Address != null)
                    rootsList.Add (root);
            }
        }

        immixGC.ClearMarks ();

        DoMarking (gen, rootsList.Span);

        immixGC.Sweep ();
    }

    #endregion

    #region Roots

    private void AddRootsInternal (ES_ObjectAddress** roots, int count) {
        Debug.Assert (roots is not null);
        Debug.Assert (count >= 0);

        lock (gcRoots) {
            gcRoots.Add (new RootCollectionPair {
                Roots = roots,
                Count = count,
            });
        }
    }

    private void RemoveRootsInternal (ES_ObjectAddress** roots) {
        Debug.Assert (roots is not null);

        lock (gcRoots) {
            Debug.Assert (gcRoots.Count > 0);

            for (var i = 0; i < gcRoots.Count; i++) {
                if (gcRoots [i].Roots != roots)
                    continue;

                gcRoots.RemoveAt (i);
                return;
            }
        }

        Debug.Fail ("Roots set not present in the list.");
    }

    #endregion

    #region Allocation

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private void* AllocateObject (ES_MethodTable* type, bool immutable, bool pinned) {
        CheckDisposed ();

        var objSize = Math.Max (type->RuntimeSize, sizeof (void*));
        var allocSize = sizeof (ES_ObjectHeader) + objSize;

        // Determine the object's flags.
        var objFlags = (ES_ObjectFlags) 0;

        if (allocSize > LargeObjectSize)
            objFlags |= ES_ObjectFlags.LargeObject;
        else if (allocSize > ImmixConstants.LineSize)
            objFlags |= ES_ObjectFlags.MediumObject;

        if (markFlipped)
            objFlags |= ES_ObjectFlags.Marked;

        if (immutable)
            objFlags |= ES_ObjectFlags.Immutable;

        if (pinned)
            objFlags |= ES_ObjectFlags.Pinned;

        // Generate the object header.
        var headerData = new ES_ObjectHeader {
            Flags = objFlags,
            MethodTable = type,
        };

        // Allocate the object.
        ES_ObjectHeader* objHeader;
        if (objFlags.HasFlag (ES_ObjectFlags.LargeObject))
            throw new NotImplementedException ("[TODO] LOH not implemented yet.");
        else
            objHeader = (ES_ObjectHeader*) immixGC.AllocObject (ref headerData, allocSize);

        // Set the header.
        *objHeader = headerData;

        // Calculate the object's start address and clear its memory.
        var objPtr = (byte*) objHeader + sizeof (ES_ObjectHeader);
        var objMem = new Span<byte> (objPtr, objSize);
        objMem.Clear ();

        return objPtr;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private ES_ArrayHeader* AllocateArray (ES_MethodTable* elemType, ReadOnlySpan<ES_ArrayIndex> dimSizes, bool immutable, bool pinned, bool clearElems) {
        CheckDisposed ();

        Debug.Assert (dimSizes.Length > 0 && dimSizes.Length <= byte.MaxValue);

        var elemSize = elemType->RuntimeSize;

        // Calculate the total number of elements.
        var totalElemsCount = CalculateTotalArrayLength (dimSizes);

        // Calculate the array's sizes.
        var arrSize = elemSize * totalElemsCount;
        var arrHeaderSize = ES_ArrayHeader.GetArrayHeaderSize (dimSizes.Length);

        var objSize = Math.Max (arrHeaderSize + arrSize, sizeof (void*));
        var allocSize = sizeof (ES_ObjectHeader) + objSize;

        // Determine the array's flags.
        var objFlags = ES_ObjectFlags.IsArray;

        if (allocSize > LargeObjectSize)
            objFlags |= ES_ObjectFlags.LargeObject;
        else if (allocSize > ImmixConstants.LineSize)
            objFlags |= ES_ObjectFlags.MediumObject;

        if (markFlipped)
            objFlags |= ES_ObjectFlags.Marked;

        if (immutable)
            objFlags |= ES_ObjectFlags.Immutable;

        if (pinned)
            objFlags |= ES_ObjectFlags.Pinned;

        // Generate the object header.
        var headerData = new ES_ObjectHeader {
            Flags = objFlags,
            MethodTable = elemType,
        };

        // Allocate the array.
        ES_ObjectHeader* objHeader;
        if (objFlags.HasFlag (ES_ObjectFlags.LargeObject))
            throw new NotImplementedException ("[TODO] LOH not implemented yet.");
        else
            objHeader = (ES_ObjectHeader*) immixGC.AllocObject (ref headerData, allocSize);

        // Set the header.
        *objHeader = headerData;

        // Copy the array data to the start of the array object.
        var arrHeader = (ES_ArrayHeader*) ((byte*) objHeader + sizeof (ES_ObjectHeader));
        var arrDimsArea = ES_ArrayHeader.GetArrayRanksPointer (arrHeader);

        *arrHeader = new ES_ArrayHeader {
            Length = totalElemsCount,
            Rank = (byte) dimSizes.Length,
        };
        dimSizes.CopyTo (new Span<ES_ArrayIndex> (arrDimsArea, dimSizes.Length));

        // Clear the array's memory.
        var objMem = new Span<byte> (ES_ArrayHeader.GetArrayDataPointer (arrHeader), totalElemsCount);
        if (clearElems)
            objMem.Clear ();

        return arrHeader;
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

    ~ES_GarbageCollector () {
        if (!IsDisposed)
            DoDispose ();
    }

    private void DoDispose () {
        if (IsDisposed)
            return;

        immixGC.Dispose ();
        ImmixGC_GlobalAllocator.DoDispose ();

        IsDisposed = true;
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion
}
