/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCompiler.CompilerCommon.IR;

public abstract class ESIR_MemberNode : ESIR_Node { }

public class ESIR_Field : ESIR_MemberNode {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.Field;
    internal override int ChildrenCount => 3;

    private readonly ESIR_TypeNode typeNode;
    private readonly ESIR_ValueNode offsetNode;
    private readonly ESIR_ValueNode nameNode;

    public ESIR_TypeNode Type => typeNode;
    public int Offset => (int) offsetNode.GetInt ()!.Value;
    public ES_Identifier Name => nameNode.GetIdentifier ()!.Value;

    internal ESIR_Field (ESIR_TypeNode type, ESIR_ValueNode offset, ESIR_ValueNode name) {
        typeNode = type;
        offsetNode = offset;
        nameNode = name;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return typeNode;
            case 1: return offsetNode;
            case 2: return nameNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_StaticField : ESIR_MemberNode {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.StaticField;
    internal override int ChildrenCount => 2;

    private readonly ESIR_ValueNode nameNode;
    private readonly ESIR_ValueNode staticVariableNode;

    public ES_Identifier Name => nameNode.GetIdentifier ()!.Value;
    public ES_Identifier StaticVariable => staticVariableNode.GetIdentifier ()!.Value;

    internal ESIR_StaticField (ESIR_ValueNode name, ESIR_ValueNode staticVar) {
        nameNode = name;
        staticVariableNode = staticVar;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return nameNode;
            case 1: return staticVariableNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public static partial class ESIR_Factory {
    public static ESIR_Field Field (ESIR_TypeNode type, int offset, ES_Identifier name)
        => Field (type, ValueNode (offset), ValueNode (name));
    private static ESIR_Field Field (ESIR_TypeNode type, ESIR_ValueNode offset, ESIR_ValueNode name)
        => new (type, offset, name);

    public static ESIR_StaticField StaticField (ES_Identifier name, ES_Identifier staticVar)
        => StaticField (ValueNode (name), ValueNode (staticVar));
    private static ESIR_StaticField StaticField (ESIR_ValueNode name, ESIR_ValueNode staticVar)
        => new (name, staticVar);
}
