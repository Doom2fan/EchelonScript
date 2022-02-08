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

namespace EchelonScriptCompiler.CompilerCommon.IR {
    public abstract class ESIR_Node {
        internal bool IsCached { get; set; } = false;

        public abstract ESIR_NodeKind Kind { get; }
        internal abstract int ChildrenCount { get; }

        internal abstract ESIR_Node? GetChild (int slot);
    }

    internal abstract class ESIR_NodeCacheBase {
        protected enum BucketState {
            Empty = 0,

            Single,
            Multi,
        }

        protected const int CacheSizeBits = 16;
        protected const int CacheSize = 1 << CacheSizeBits;
        protected const int CacheMask = CacheSize - 1;

        protected struct CacheBucket {
            public BucketState State;

            public CacheEntry Entry;
            public CacheEntry [] Entries;
            public int EntriesCount;

            public CacheBucket (CacheEntry entry) {
                State = BucketState.Single;

                Entry = entry;
                Entries = Array.Empty<CacheEntry> ();
                EntriesCount = default;
            }

            public CacheBucket (CacheEntry [] entries, int entriesCount) {
                State = BucketState.Multi;

                Entry = default;
                Entries = entries;
                EntriesCount = entriesCount;
            }

            public struct Enumerator {
                private CacheBucket bucket;
                private int idx;

                public CacheEntry Current {
                    get {
                        if (bucket.State == BucketState.Single && idx == 0)
                            return bucket.Entry;
                        else if (bucket.State == BucketState.Multi && idx < bucket.Entries.Length)
                            return bucket.Entries [idx];

                        Debug.Fail ("Enumeration error");
                        return default;
                    }
                }

                public Enumerator (CacheBucket newBucket) {
                    bucket = newBucket;
                    idx = -1;
                }

                private int Count () {
                    if (bucket.State == BucketState.Single)
                        return 1;
                    else if (bucket.State == BucketState.Multi)
                        return bucket.EntriesCount;

                    return 0;
                }

                public bool MoveNext () => ++idx < Count ();
            }

            public Enumerator GetEnumerator () => new (this);
        }

        protected struct CacheEntry {
            public int Hash;
            public ESIR_Node Node;

            public CacheEntry (int hash, ESIR_Node node) {
                Hash = hash;
                Node = node;
            }
        }

        protected CacheBucket [] buckets;

        internal ESIR_NodeCacheBase () {
            buckets = new CacheBucket [CacheSize];
        }

        protected static void AddToBucket (ref CacheBucket bucket, CacheEntry entry) {
            const int mulSize = 5;

            if (bucket.State == BucketState.Empty)
                bucket = new CacheBucket (entry);
            else if (bucket.State == BucketState.Single) {
                var arr = new CacheEntry [mulSize];

                arr [0] = bucket.Entry;
                arr [1] = entry;

                bucket = new CacheBucket (arr, 2);
            } else {
                var arr = bucket.Entries;
                var count = bucket.EntriesCount;

                if (count + 1 > arr.Length) {
                    var oldArr = arr;
                    arr = new CacheEntry [(count + 1 + mulSize - 1) / mulSize * mulSize];

                    oldArr.AsSpan (0, count).CopyTo (arr);
                }

                arr [count++] = entry;
                bucket = new CacheBucket (arr, count);
            }
        }
    }

    internal class ESIR_NodeCache : ESIR_NodeCacheBase {
        internal readonly static ESIR_NodeCache Shared = new ();

        private static bool IsEquivalent (ESIR_Node node, ESIR_NodeKind kind) => node.Kind == kind && node.ChildrenCount == 0;

        private static bool IsEquivalent (ESIR_Node node, ESIR_NodeKind kind, ESIR_Node? child1) {
            return (
                node.Kind == kind &&
                node.ChildrenCount == 1 &&
                node.GetChild (0) == child1
            );
        }

        private static bool IsEquivalent (ESIR_Node node, ESIR_NodeKind kind, ESIR_Node? child1, ESIR_Node? child2) {
            return (
                node.Kind == kind &&
                node.ChildrenCount == 2 &&
                node.GetChild (0) == child1 &&
                node.GetChild (1) == child2
            );
        }

        private static bool IsEquivalent (ESIR_Node node, ESIR_NodeKind kind, ESIR_Node? child1, ESIR_Node? child2, ESIR_Node? child3) {
            return (
                node.Kind == kind &&
                node.ChildrenCount == 3 &&
                node.GetChild (0) == child1 &&
                node.GetChild (1) == child2 &&
                node.GetChild (2) == child3
            );
        }

        private static bool IsNodeCached (ESIR_Node? node) {
            if (node is null)
                return true;

            return node.IsCached;
        }

        private static bool AreChildrenCached (ESIR_Node node) {
            var childrenCount = node.ChildrenCount;
            for (int i = 0; i < childrenCount; i++) {
                var child = node.GetChild (i);
                if (child is not null && !child.IsCached)
                    return false;
            }

            return true;
        }

        internal void AddNode (ESIR_Node node, int hash) {
            Debug.Assert (hash >= 0 && AreChildrenCached (node));

            var bucketIdx = hash & CacheMask;
            ref var bucket = ref buckets [bucketIdx];

            AddToBucket (ref bucket, new CacheEntry (hash, node));
        }

        internal ESIR_Node? TryGetNode (ESIR_NodeKind kind, out int hash) {
            hash = GetCacheHash (kind);
            var bucketIdx = hash & CacheMask;

            ref var bucket = ref buckets [bucketIdx];
            foreach (var entry in bucket) {
                if (entry.Hash != hash)
                    continue;

                if (IsEquivalent (entry.Node, kind))
                    return entry.Node;
            }

            return null;
        }

        internal ESIR_Node? TryGetNode (ESIR_NodeKind kind, ESIR_Node? child1, out int hash) {
            if (!IsNodeCached (child1)) {
                hash = -1;
                return null;
            }

            hash = GetCacheHash (kind, child1);
            var bucketIdx = hash & CacheMask;

            ref var bucket = ref buckets [bucketIdx];
            foreach (var entry in bucket) {
                if (entry.Hash != hash)
                    continue;

                if (IsEquivalent (entry.Node, kind, child1))
                    return entry.Node;
            }

            return null;
        }

        internal ESIR_Node? TryGetNode (ESIR_NodeKind kind, ESIR_Node? child1, ESIR_Node? child2, out int hash) {
            if (!IsNodeCached (child1) || !IsNodeCached (child2)) {
                hash = -1;
                return null;
            }

            hash = GetCacheHash (kind, child1, child2);
            var bucketIdx = hash & CacheMask;

            ref var bucket = ref buckets [bucketIdx];
            foreach (var entry in bucket) {
                if (entry.Hash != hash)
                    continue;

                if (IsEquivalent (entry.Node, kind, child1, child2))
                    return entry.Node;
            }

            return null;
        }

        internal ESIR_Node? TryGetNode (ESIR_NodeKind kind, ESIR_Node? child1, ESIR_Node? child2, ESIR_Node? child3, out int hash) {
            if (!IsNodeCached (child1) || !IsNodeCached (child2) || !IsNodeCached (child3)) {
                hash = -1;
                return null;
            }

            hash = GetCacheHash (kind, child1, child2, child3);
            var bucketIdx = hash & CacheMask;

            ref var bucket = ref buckets [bucketIdx];
            foreach (var entry in bucket) {
                if (entry.Hash != hash)
                    continue;

                if (IsEquivalent (entry.Node, kind, child1, child2, child3))
                    return entry.Node;
            }

            return null;
        }

        private static int GetCacheHash (ESIR_NodeKind kind) {
            var code = (int) kind;
            return code & 0x7FFFFFFF;
        }

        private static int GetCacheHash (ESIR_NodeKind kind, ESIR_Node? child1) {
            var code = (int) kind;

            if (child1 is not null)
                code = HashCode.Combine (code, GetCacheHash (child1));

            return code & 0x7FFFFFFF;
        }

        private static int GetCacheHash (ESIR_NodeKind kind, ESIR_Node? child1, ESIR_Node? child2) {
            var code = (int) kind;

            if (child1 is not null)
                code = HashCode.Combine (code, GetCacheHash (child1));
            if (child2 is not null)
                code = HashCode.Combine (code, GetCacheHash (child2));

            return code & 0x7FFFFFFF;
        }

        private static int GetCacheHash (ESIR_NodeKind kind, ESIR_Node? child1, ESIR_Node? child2, ESIR_Node? child3) {
            var code = (int) kind;

            if (child1 is not null)
                code = HashCode.Combine (code, GetCacheHash (child1));
            if (child2 is not null)
                code = HashCode.Combine (code, GetCacheHash (child2));
            if (child3 is not null)
                code = HashCode.Combine (code, GetCacheHash (child3));

            return code & 0x7FFFFFFF;
        }

        private static int GetCacheHash (ESIR_Node node) {
            var code = (int) node.Kind;

            for (int i = 0; i < node.ChildrenCount; i++) {
                var child = node.GetChild (i);

                if (child is not null)
                    code = HashCode.Combine (code, GetCacheHash (child));
            }

            return code & 0x7FFFFFFF;
        }
    }
}
