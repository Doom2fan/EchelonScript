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
        where T : unmanaged {
        public int Length;
        public T* Elements;

        public Span<T> Span => new Span<T> (Elements, Length);
    }
}
