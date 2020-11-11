/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScriptCompiler.Data {
    public unsafe interface ES_TypeData {
        /// <summary>The type's name.</summary>
        public string TypeName { get; }

        /// <summary>Whether data like the size of the type is valid.</summary>
        public bool Valid { get; }

        /// <summary>The type's size in bytes.</summary>
        public int Size { get; }
    }
}
