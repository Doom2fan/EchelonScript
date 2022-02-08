/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.CompilerCommon.IR {
    public unsafe class ESIR_Struct : ESIR_Node {
        public override ESIR_NodeKind Kind => ESIR_NodeKind.Struct;
        internal override int ChildrenCount => 2;

        private readonly ESIR_ValueNode typeNode;
        private readonly ESIR_List<ESIR_MemberNode> membersNode;

        public ES_TypeInfo* Type => (ES_TypeInfo*) typeNode.GetPointer ()!.Value;
        public ESIR_List<ESIR_MemberNode> Members => membersNode;

        internal ESIR_Struct (ESIR_ValueNode type, ESIR_List<ESIR_MemberNode> members) {
            typeNode = type;
            membersNode = members;
        }

        internal override ESIR_Node? GetChild (int slot) {
            switch (slot) {
                case 0: return typeNode;
                case 1: return membersNode;

                default:
                    Debug.Fail ("Invalid slot num");
                    return null;
            }
        }
    }

    public unsafe static partial class ESIR_Factory {
        public static ESIR_Struct Struct (ES_TypeInfo* type, ESIR_List<ESIR_MemberNode> members)
            => Struct (ValueNode (type), members);
        private static ESIR_Struct Struct (ESIR_ValueNode type, ESIR_List<ESIR_MemberNode> members)
            => new (type, members);
    }
}
