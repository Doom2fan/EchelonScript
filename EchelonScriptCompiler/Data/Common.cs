/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;

namespace EchelonScriptCompiler.Data {
    public unsafe struct ArrayPointer<T>
        : IEquatable<ArrayPointer<T>>
        where T : unmanaged {
        public static ArrayPointer<T> Null = new ArrayPointer<T> (null, 0);

        public int Length;
        public T* Elements;

        public ArrayPointer (T* ptr, int len) {
            Elements = ptr;
            Length = len;
        }

        public Span<T> Span => new Span<T> (Elements, Length);

        public bool Equals (ArrayPointer<T> other) {
            if (Elements == null && other.Elements == null)
                return true;
            else if (Elements == null || other.Elements == null)
                return false;
            else if (Elements == other.Elements && Length == other.Length)
                return true;
            else
                return false;
        }

        public override int GetHashCode () {
            return HashCode.Combine (((IntPtr) Elements).GetHashCode (), Length.GetHashCode ());
        }
    }
}
