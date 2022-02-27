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

public class ESIR_ArgumentDefinition : ESIR_Node {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ArgumentDefinition;
    internal override int ChildrenCount => 2;

    private readonly ESIR_ValueNode argTypeNode;
    private readonly ESIR_TypeNode valueTypeNode;

    public ES_ArgumentType ArgType => (ES_ArgumentType) argTypeNode.GetInt ()!.Value;
    public ESIR_TypeNode ValueType => valueTypeNode;

    internal ESIR_ArgumentDefinition (ESIR_ValueNode argType, ESIR_TypeNode valueType) {
        argTypeNode = argType;
        valueTypeNode = valueType;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return argTypeNode;
            case 1: return valueTypeNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_ArgumentValue : ESIR_Node {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ArgumentValue;
    internal override int ChildrenCount => 2;

    private readonly ESIR_ValueNode argTypeNode;
    private readonly ESIR_Expression expressionNode;

    public ES_ArgumentType ArgType => (ES_ArgumentType) argTypeNode.GetInt ()!.Value;
    public ESIR_Expression Expression => expressionNode;

    internal ESIR_ArgumentValue (ESIR_ValueNode argType, ESIR_Expression expression) {
        argTypeNode = argType;
        expressionNode = expression;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return argTypeNode;
            case 1: return expressionNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public static partial class ESIR_Factory {
    public static ESIR_ArgumentDefinition ArgumentDefinition (ES_ArgumentType argType, ESIR_TypeNode valueType)
        => ArgumentDefinition (ValueNode ((int) argType), valueType);
    private static ESIR_ArgumentDefinition ArgumentDefinition (ESIR_ValueNode argType, ESIR_TypeNode valueType) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ArgumentDefinition, argType, valueType, out var hash);
        if (node is not null)
            return (ESIR_ArgumentDefinition) node;

        var ret = new ESIR_ArgumentDefinition (argType, valueType);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_ArgumentValue ArgumentValue (ES_ArgumentType argType, ESIR_Expression expression)
        => ArgumentValue (ValueNode ((int) argType), expression);
    private static ESIR_ArgumentValue ArgumentValue (ESIR_ValueNode argType, ESIR_Expression expression) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ArgumentValue, argType, expression, out var hash);
        if (node is not null)
            return (ESIR_ArgumentValue) node;

        var ret = new ESIR_ArgumentValue (argType, expression);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }
}
