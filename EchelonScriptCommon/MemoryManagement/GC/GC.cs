/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.GarbageCollection.Immix;

namespace EchelonScriptCommon.GarbageCollection {
    public unsafe sealed class ES_GarbageCollector : IDisposable {
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

        #endregion

        #region ================== Constructors

        internal ES_GarbageCollector () {
            immixGC = new ImmixGC ();
        }

        #endregion

        #region ================== Static methods

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private static void EnsureInitialized () {
            if (garbageCollector is null) {
                garbageCollector = new ES_GarbageCollector ();
                ChronosLib.StaticDisposables.AddDisposable (garbageCollector);
            }
        }

        public static void GetImmixInfo (out ImmixDebugInfo info, out ImmixDebugInfo overflowInfo) {
            EnsureInitialized ();

            garbageCollector!.CheckDisposed ();
            garbageCollector!.immixGC.GetInfo (out info, out overflowInfo);
        }

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

        [UnmanagedCallersOnly (CallConvs = new [] { typeof (CallConvCdecl) })]
        public static void* AllocObjectUnmanaged (ES_TypeInfo* type, byte pinned) {
            EnsureInitialized ();
            return garbageCollector!.AllocateObject (type, pinned != 0);
        }

        [UnmanagedCallersOnly (CallConvs = new [] { typeof (CallConvCdecl) })]
        public static void* AllocArrayUnmanaged (ES_ArrayTypeData* arrayType, ES_ArrayIndex* dimSizesPtr, int dimsCount, byte pinned) {
            EnsureInitialized ();
            return garbageCollector!.AllocateArray (arrayType, new Span<ES_ArrayIndex> (dimSizesPtr, dimsCount), pinned != 0, true);
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static T* AllocObject<T> (ES_TypeInfo* type, bool pinned, T defaultVal = default) where T : unmanaged {
            EnsureInitialized ();

            var ptr = (T*) garbageCollector!.AllocateObject (type, pinned);
            *ptr = defaultVal;

            return ptr;
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static ES_ArrayHeader* AllocArray<T> (ES_ArrayTypeData* arrayType, ReadOnlySpan<ES_ArrayIndex> dimSizes, bool pinned, T defaultElemVal = default) where T : unmanaged {
            EnsureInitialized ();

            var ptr = garbageCollector!.AllocateArray (arrayType, dimSizes, pinned, false);

            var arraySpan = new Span<T> (ES_ArrayHeader.GetArrayDataPointer (ptr), ptr->Length);
            arraySpan.Fill (defaultElemVal);

            return ptr;
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static void* AllocObject (ES_TypeInfo* type, bool pinned) {
            EnsureInitialized ();
            return garbageCollector!.AllocateObject (type, pinned);
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        public static ES_ArrayHeader* AllocArray (ES_ArrayTypeData* arrayType, ReadOnlySpan<ES_ArrayIndex> dimSizes, bool pinned) {
            EnsureInitialized ();
            return garbageCollector!.AllocateArray (arrayType, dimSizes, pinned, true);
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private static ES_ArrayIndex CalculateTotalArrayLength (ReadOnlySpan<ES_ArrayIndex> dimSizes) {
            var totalElemsCount = dimSizes [0];
            foreach (var dim in dimSizes.Slice (1))
                totalElemsCount *= dim;

            return totalElemsCount;
        }

        #endregion

        #region ================== Instance methods

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private void CheckDisposed () {
            Debug.Assert (!disposedValue, "GC object was disposed.");
        }

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
            throw new NotImplementedException ("[TODO] Garbage collection not implemented yet.");
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        private void* AllocateObject (ES_TypeInfo* type, bool pinned) {
            CheckDisposed ();

            int objSize = Math.Max (type->RuntimeSize, sizeof (void*));
            int allocSize = sizeof (ES_ObjectHeader) + objSize;

            // Determine the object's flags.
            var objFlags = (ES_ObjectFlags) 0;

            if (allocSize > LargeObjectSize)
                objFlags |= ES_ObjectFlags.LargeObject;
            else if (allocSize > ImmixConstants.LineSize)
                objFlags |= ES_ObjectFlags.MediumObject;

            if (pinned)
                objFlags |= ES_ObjectFlags.Pinned;

            // Generate the object header.
            var headerData = new ES_ObjectHeader {
                Flags = objFlags,
                TypeData = type,
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
        private ES_ArrayHeader* AllocateArray (ES_ArrayTypeData* arrayType, ReadOnlySpan<ES_ArrayIndex> dimSizes, bool pinned, bool clearElems) {
            CheckDisposed ();

            Debug.Assert (dimSizes.Length > 0 && dimSizes.Length <= byte.MaxValue);

            var elementType = arrayType->ElementType;
            var elemSize = elementType->RuntimeSize;

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

            if (pinned)
                objFlags |= ES_ObjectFlags.Pinned;

            // Generate the object header.
            var headerData = new ES_ObjectHeader {
                Flags = objFlags,
                TypeData = &arrayType->TypeInfo,
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
            var arrDimsArea = ES_ArrayHeader.GetArrayIndicesPointer (arrHeader);

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

        #region ================== IDisposable support

        private bool disposedValue;
        public bool IsDisposed => disposedValue;

        ~ES_GarbageCollector () {
            if (!disposedValue)
                DoDispose ();
        }

        private void DoDispose () {
            if (disposedValue)
                return;

            immixGC.Dispose ();
            ImmixGC_GlobalAllocator.DoDispose ();

            disposedValue = true;
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }
}
