/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using ChronosLib.Pooled;
using EchelonScript.Common.GarbageCollection.Immix;
using EchelonScript.Common.Utilities;

namespace EchelonScript.Common.GarbageCollection;

unsafe partial class ES_GarbageCollector {
    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private void DoMarking (int gen, Span<Pointer<ES_ObjectAddress>> roots) {
        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        static bool IsMarked (ES_ObjectFlags flags, bool flipMark) => flags.HasFlag (ES_ObjectFlags.Marked) ^ flipMark;

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        static bool MarkObject (ES_ObjectAddress obj, bool flipMark) {
            var curFlags = obj.Header->Flags;

            var isMarked = !IsMarked (curFlags, flipMark);

            obj.Header->Flags = flipMark
                ? (curFlags & ~ES_ObjectFlags.Marked)
                : (curFlags | ES_ObjectFlags.Marked);

            return isMarked;
        }

        [MethodImpl (MethodImplOptions.AggressiveInlining)]
        static bool HandleForwarding (ref ES_ObjectAddress obj) {
            var objFlags = obj.Header->Flags;

            if (objFlags.HasFlag (ES_ObjectFlags.Forwarded)) {
                obj.Address = *(void**) obj.Address;
                return true;
            } else if (objFlags.HasFlag (ES_ObjectFlags.DoRemove)) {
                obj.Address = null;
                return true;
            }

            return false;
        }

        var flipMark = markFlipped;
        using var greySet = new StructPooledList<ES_ObjectAddress> (CL_ClearMode.Auto);

        foreach (var root in roots) {
            ref var objAddress = ref *root.Address;
            Debug.Assert (objAddress.Address != null);

            if (!MarkObject (objAddress, flipMark) || HandleForwarding (ref objAddress))
                continue;

            greySet.Add (objAddress);
        }

        while (greySet.Count > 0) {
            using var greyObjs = greySet.MoveToArray ();
            greySet.Reinit ();

            foreach (var obj in greyObjs.Span) {
                var objHeader = obj.Header;

                var isMedium = obj.Header->Flags.HasFlag (ES_ObjectFlags.MediumObject);
                var isLarge = obj.Header->Flags.HasFlag (ES_ObjectFlags.LargeObject);
                var isArray = obj.Header->Flags.HasFlag (ES_ObjectFlags.IsArray);

                if (!isLarge) {
                    var immixBlock = obj.ImmixBlock;
                    var immixLinemap = immixBlock.Header->LineMapSpan;
                    var linesStart = obj.ImmixStartLine;

                    if (isMedium) {
                        int allocSize;
                        if (!isArray)
                            allocSize = sizeof (ES_ObjectHeader) + obj.Header->MethodTable->RuntimeSize;
                        else {
                            var arrayHeader = (ES_ArrayHeader*) obj.Address;
                            allocSize = ES_ArrayHeader.GetArraySize (obj);
                        }

                        var linesCount = (allocSize + (ImmixConstants.LineSize - 1)) / ImmixConstants.LineSize;

                        immixLinemap.Slice (linesStart, linesCount).Fill (0xFF);
                    } else
                        immixLinemap [linesStart] = 0xFF;
                }

                if (!isArray) {
                    foreach (var refOffs in obj.Header->MethodTable->GetRefsList ()) {
                        var refAddr = new ES_ObjectAddress (*(void**) ((byte*) obj.Address + refOffs));
                        ref var objAddress = ref refAddr;

                        if (objAddress.Address == null)
                            continue;

                        if (!MarkObject (objAddress, flipMark) || HandleForwarding (ref objAddress))
                            continue;

                        greySet.Add (objAddress);
                    }
                } else {
                    var elemType = objHeader->MethodTable;
                    var arrayHeader = (ES_ArrayHeader*) obj.Address;
                    var arrayData = (byte*) ES_ArrayHeader.GetArrayDataPointer (arrayHeader);

                    var elemLength = elemType->RuntimeSize;
                    var elemRefs = elemType->GetRefsList ();

                    for (var i = 0; i < arrayHeader->Length; i++) {
                        var elemStartAddr = arrayData + i * elemLength;

                        foreach (var refOffs in elemRefs) {
                            var refAddr = new ES_ObjectAddress (elemStartAddr + refOffs);
                            ref var objAddress = ref refAddr;

                            if (objAddress.Address == null)
                                continue;

                            if (!MarkObject (objAddress, flipMark) || HandleForwarding (ref objAddress))
                                continue;

                            greySet.Add (objAddress);
                        }
                    }
                }
            }
        }

        markFlipped = !flipMark;
    }
}
