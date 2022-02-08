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
    public class ESIR_List<T> : ESIR_Node where T : ESIR_Node {
        public readonly static ESIR_List<T> Empty = ESIR_Factory.List<T> ();

        public override ESIR_NodeKind Kind => ESIR_NodeKind.List;
        internal override int ChildrenCount => elems.Length;

        private T [] elems;

        public ReadOnlySpan<T> Elements => elems;

        internal ESIR_List () => elems = Array.Empty<T> ();

        internal ESIR_List (T node) {
            elems = new T [1];
            elems [0] = node;
        }

        internal ESIR_List (ReadOnlySpan<T> elemsSpan) {
            elems = new T [elemsSpan.Length];
            elemsSpan.CopyTo (elems);
        }

        internal override ESIR_Node? GetChild (int slot) {
            Debug.Assert (slot >= 0 && slot < elems.Length);

            if (slot < 0 || slot >= elems.Length)
                return null;

            return elems [slot];
        }
    }

    public static partial class ESIR_Factory {
        public static ESIR_List<T> List<T> () where T : ESIR_Node => new ();
        public static ESIR_List<T> List<T> (T node) where T : ESIR_Node => new (node);
        public static ESIR_List<T> List<T> (Span<T> nodes) where T : ESIR_Node => new (nodes);
        public static ESIR_List<T> List<T> (ReadOnlySpan<T> nodes) where T : ESIR_Node => new (nodes);
        public static ESIR_List<T> List<T> (params T [] nodes) where T : ESIR_Node => new (nodes);
    }
}
