/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;

namespace EchelonScriptCompiler {
    public class CompilationException : Exception {
        public CompilationException () { }

        public CompilationException (string message)
            : base (message) {
        }

        public CompilationException (string message, Exception innerException)
            : base (message, innerException) {
        }
    }
}
