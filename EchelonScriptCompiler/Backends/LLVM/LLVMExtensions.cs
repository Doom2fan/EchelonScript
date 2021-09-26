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
using LLVMSharp.Interop;

namespace EchelonScriptCompiler.Backends.LLVMBackend {
    internal static class LLVMExtensions {
        public static void AddIncoming (this LLVMValueRef phi, LLVMValueRef value, LLVMBasicBlockRef block) {
            Debug.Assert (phi != null);
            Debug.Assert (value != null);
            Debug.Assert (block != null);

            Span<LLVMValueRef> phiValue = stackalloc LLVMValueRef [1];
            Span<LLVMBasicBlockRef> phiBlock = stackalloc LLVMBasicBlockRef [1];

            phiValue [0] = value;
            phiBlock [0] = block;
            phi.AddIncoming (phiValue, phiBlock, 1);
        }

        public static bool IsPointerType (this LLVMTypeRef type) {
            Debug.Assert (type != null);

            return type.Kind == LLVMTypeKind.LLVMPointerTypeKind;
        }

        public static bool IsPointer (this LLVMValueRef val) {
            Debug.Assert (val != null);
            Debug.Assert (val.TypeOf != null);

            return val.TypeOf.IsPointerType ();
        }

        public static bool IsNonRefPointer (this LLVMValueRef val) {
            Debug.Assert (val != null);
            Debug.Assert (val.TypeOf != null);

            if (!val.TypeOf.IsPointerType ())
                return false;

            return (
                val.IsAAllocaInst != null ||
                val.IsAGetElementPtrInst != null ||
                val.IsAGlobalValue != null ||
                val.IsALoadInst != null
            );
        }
    }
}
