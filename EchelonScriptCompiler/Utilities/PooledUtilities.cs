/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;

namespace EchelonScriptCompiler.Utilities {
    public enum ClearMode {
        /// <summary>Reference types and value types that contain reference types are cleared when the internal arrays
        /// are returned to the pool. Value types that do not contain reference types are not cleared when returned to
        /// the pool.</summary>
        Auto = 0,
        /// <summary>Collections are always cleared.</summary>
        Always = 1,
        /// <summary>Collections are never cleared.</summary>
        Never = 2
    }

    internal static class PooledUtils {
        public static bool IsCompatibleObject<T> (object value) {
            // Non-null values are fine.  Only accept nulls if T is a class or Nullable<U>.
            // Note that default(T) is not equal to null for value types except when T is Nullable<U>.
            return (value is T) || (value == null && default (T) == null);
        }

        public static void EnsureNotNull<T> (object value, string paramName) {
            // Note that default(T) is not equal to null for value types except when T is Nullable<U>.
            if (!(default (T) == null) && value == null)
                throw new ArgumentNullException (paramName);
        }
    }

    public struct StructPooledList<T> : IList<T>, IReadOnlyList<T>, IList, IDisposable {
        #region ================== Constants

        private const int maxArrayLength = 0x7FEFFFFF;
        private const int defaultCapacity = 4;
        private static readonly T [] emptyArray = Array.Empty<T> ();

        #endregion

        #region ================== Instance fields

        private ArrayPool<T> pool;
        private object syncRoot;

        private T [] items;
        private int size;
        private int version;
        private readonly bool clearOnFree;

        #endregion

        #region ================== Constructors

        public StructPooledList (ClearMode clearMode)
            : this (clearMode, null) {
        }

        public StructPooledList (ClearMode clearMode, ArrayPool<T> customPool)
            : this () {
            clearOnFree = ShouldClear (clearMode);
            pool = customPool ?? ArrayPool<T>.Shared;
            items = emptyArray;
        }

        #endregion

        #region ================== Indexers

        public T this [int idx] {
            get {
                if ((uint) idx >= (uint) size)
                    throw new IndexOutOfRangeException ();

                return items [idx];
            }

            set {
                if ((uint) idx >= (uint) size)
                    throw new IndexOutOfRangeException ();

                version++;
                items [idx] = value;
            }
        }

        object IList.this [int index] {
            get => this [index];

            set {
                PooledUtils.EnsureNotNull<T> (value, nameof (value));

                try {
                    this [index] = (T) value;
                } catch (InvalidCastException) {
                    throw new ArgumentException ($"Wrong value type. Expected {typeof (T).ToString ()}, got '{value.GetType ().Name}'");
                }
            }
        }

        #endregion

        #region ================== Instance properties

        /// <inheritdoc/>
        public int Capacity {
            get => items.Length;

            set {
                if (value < size)
                    throw new ArgumentOutOfRangeException (null, "The specified capacity is lower than the list's size");

                if (value != items.Length) {
                    if (value > 0) {
                        var newItems = pool.Rent (value);

                        if (size > 0)
                            Array.Copy (items, newItems, size);

                        ReturnArray ();

                        items = newItems;
                    } else {
                        ReturnArray ();
                        size = 0;
                    }
                }
            }
        }

        /// <inheritdoc/>
        public int Count => size;

        public Span<T> Span {
            get {
                if (items == null)
                    return emptyArray;

                return items [0 .. size];
            }
        }

        /// <summary>Returns the ClearMode behavior for the collection, denoting whether values are cleared from
        /// internal arrays before returning them to the pool.</summary>
        public ClearMode ClearMode => clearOnFree ? ClearMode.Always : ClearMode.Never;

        #endregion

        #region ================== Instance methods

        private static bool ShouldClear (ClearMode mode) {
            return mode == ClearMode.Always
                || (mode == ClearMode.Auto && RuntimeHelpers.IsReferenceOrContainsReferences <T> ());
        }

        private void ReturnArray () {
            if (items.Length == 0)
                return;

            try {
                // Clear the elements so that the gc can reclaim the references.
                pool?.Return (items, clearArray: clearOnFree);
            } catch (ArgumentException) {
                // oh well, the array pool didn't like our array
            }

            items = emptyArray;
        }

        private void EnsureCapacity (int min) {
            if (items.Length < min) {
                int newCapacity = items.Length == 0 ? defaultCapacity : items.Length * 2;
                // Allow the list to grow to maximum possible capacity (~2G elements) before encountering overflow.
                // Note that this check works even when _items.Length overflowed thanks to the (uint) cast

                if ((uint) newCapacity > maxArrayLength)
                    newCapacity = maxArrayLength;
                if (newCapacity < min)
                    newCapacity = min;

                Capacity = newCapacity;
            }
        }

        #region Item insertion

        /// <inheritdoc/>
        public void Add (T item) {
            EnsureCapacity (size + 1);

            items [size] = item;

            version++;
            size++;
        }

        /// <inheritdoc/>
        public void Insert (int index, T item) {
            // Note that insertions at the end are legal.
            if ((uint) index > (uint) size)
                throw new ArgumentOutOfRangeException (nameof (index), "Index is out of range.");

            if (size == items.Length)
                EnsureCapacity (size + 1);
            if (index < size)
                Array.Copy (items, index, items, index + 1, size - index);

            items [index] = item;
            size++;
            version++;
        }

        #endregion

        #region Item removal

        /// <inheritdoc/>
        public void Clear () {
            version++;

            if (size > 0 && clearOnFree) {
                // Clear the elements so that the gc can reclaim the references.
                Array.Clear (items, 0, size);
            }

            size = 0;
        }

        /// <inheritdoc/>
        public bool Remove (T item) {
            int index = IndexOf (item);
            if (index >= 0) {
                RemoveAt (index);
                return true;
            }

            return false;
        }

        /// <inheritdoc/>
        public void RemoveAt (int index) {
            if ((uint) index >= (uint) size)
                throw new ArgumentOutOfRangeException (nameof (index), "Index is out of range.");

            size--;
            if (index < size)
                Array.Copy (items, index + 1, items, index, size - index);
            version++;

            if (clearOnFree) {
                // Clear the removed element so that the gc can reclaim the reference.
                items [size] = default;
            }
        }

        #endregion

        #region Search

        /// <inheritdoc/>
        public int IndexOf (T item)
            => Array.IndexOf (items, item, 0, size);

        /// <inheritdoc/>
        public bool Contains (T item)
            => size != 0 && IndexOf (item) != -1;

        #endregion

        #region Copying

        /// <summary>Copies this list to the specified span.</summary>
        /// <param name="span">The span to copy to.</param>
        public void CopyTo (Span<T> span) {
            if (span.Length < Count)
                throw new ArgumentException ("Destination span is shorter than the list being copied.");

            Span.CopyTo (span);
        }

        /// <inheritdoc/>
        public void CopyTo (T [] array, int arrayIndex) {
            Array.Copy (items, 0, array, arrayIndex, size);
        }

        #endregion

        #region Enumeration

        public IEnumerator<T> GetEnumerator () {
            return new Enumerator (this);
        }

        IEnumerator IEnumerable.GetEnumerator () {
            return new Enumerator (this);
        }

        #endregion

        public T [] ToArray () {
            if (size == 0)
                return emptyArray;

            var arr = new T [size];
            CopyTo (arr, 0);

            return arr;
        }

        #endregion

        #region ================== Enumerator

        public struct Enumerator : IEnumerator<T>, IEnumerator {
            #region ================== Instance fields

            private readonly StructPooledList<T> list;
            private int index;
            private readonly int version;
            private T current;

            #endregion

            #region ================== Instance properties

            public T Current => current;

            object IEnumerator.Current {
                get {
                    if (index == 0 || index == list.size + 1)
                        throw new InvalidOperationException ("Invalid enumerator state: enumeration cannot proceed.");

                    return Current;
                }
            }

            #endregion

            #region ================== Constructors

            internal Enumerator (StructPooledList<T> list) {
                this.list = list;
                index = 0;
                version = list.version;
                current = default;
            }

            #endregion

            #region ================== Instance methods

            public bool MoveNext () {
                var localList = list;

                if (version == localList.version && ((uint) index < (uint) localList.size)) {
                    current = localList.items [index];
                    index++;
                    return true;
                }

                return MoveNextRare ();
            }

            private bool MoveNextRare () {
                if (version != list.version)
                    throw new InvalidOperationException ("Collection was modified during enumeration.");

                index = list.size + 1;
                current = default;
                return false;
            }

            void IEnumerator.Reset () {
                if (version != list.version)
                    throw new InvalidOperationException ("Collection was modified during enumeration.");

                index = 0;
                current = default;
            }

            public void Dispose () { }

            #endregion
        }

        #endregion

        #region ================== Interface implementations

        #region IList

        bool IList.IsFixedSize => false;

        bool IList.IsReadOnly => false;

        int IList.Add (object item) {
            if (PooledUtils.IsCompatibleObject<T> (item)) {
                Add ((T) item);
                return size - 1;
            }

            return -1;
        }

        void IList.Insert (int index, object item) {
            if (PooledUtils.IsCompatibleObject<T> (item))
                Insert (index, (T) item);
        }

        void IList.Remove (object item) {
            if (PooledUtils.IsCompatibleObject<T> (item))
                Remove ((T) item);
        }

        int IList.IndexOf (object item) {
            if (PooledUtils.IsCompatibleObject<T> (item))
                return IndexOf ((T) item);

            return -1;
        }

        bool IList.Contains (object item) {
            if (PooledUtils.IsCompatibleObject<T> (item))
                return Contains ((T) item);

            return false;
        }

        #endregion

        #region ICollection

        bool ICollection<T>.IsReadOnly => false;

        bool ICollection.IsSynchronized => false;

        // Synchronization root for this object.
        object ICollection.SyncRoot {
            get {
                if (syncRoot == null)
                    Interlocked.CompareExchange<object> (ref syncRoot, new object (), null);

                return syncRoot;
            }
        }

        void ICollection.CopyTo (Array array, int index) {
            if (array != null && array.Rank != 1)
                throw new ArgumentException ("Multidimensional arrays not supported.");

            try {
                // Array.Copy will check for NULL.
                Array.Copy (items, 0, array, index, size);
            } catch (ArrayTypeMismatchException) {
                throw new ArgumentException ("Invalid array type.");
            }
        }

        #endregion

        #endregion

        #region ================== IDisposable Support

        public bool IsDisposed { get; private set; }

        private void DoDispose () {
            if (!IsDisposed) {
                ReturnArray ();
                size = 0;

                IsDisposed = true;
            }
        }

        public void Dispose () {
            DoDispose ();
        }

        #endregion
    }
}
