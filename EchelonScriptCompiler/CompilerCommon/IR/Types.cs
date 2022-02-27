/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.CompilerCommon.IR;

public unsafe class ESIR_TypeNode : ESIR_Node {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.TypeNode;
    internal override int ChildrenCount => 1;

    private readonly ESIR_ValueNode ptrNode;
    public ES_TypeInfo* Pointer => (ES_TypeInfo*) ptrNode.GetPointer ()!.Value;

    internal ESIR_TypeNode (ESIR_ValueNode ptr) => ptrNode = ptr;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return ptrNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public static partial class ESIR_Factory {
    public unsafe static ESIR_TypeNode TypeNode (ES_TypeInfo* ptr) {
        Debug.Assert (ptr is not null);
        return TypeNode (ValueNode (ptr));
    }
    private static ESIR_TypeNode TypeNode (ESIR_ValueNode ptr) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.TypeNode, ptr, out var hash);
        if (node is not null)
            return (ESIR_TypeNode) node;

        var ret = new ESIR_TypeNode (ptr);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }
}
