/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScriptCompiler.Utilities {
    public unsafe struct Pointer<T>
        where T : unmanaged {
        public T* Address;

        public Pointer (T* address) {
            Address = address;
        }

        public static implicit operator T* (Pointer<T> pointer) {
            return pointer.Address;
        }

        public static implicit operator Pointer<T> (T* pointer) {
            return new Pointer<T> (pointer);
        }
    }
}
