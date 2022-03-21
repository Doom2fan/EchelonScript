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

namespace EchelonScriptCompiler.CompilerCommon.IR;

public class ESIR_StaticVariable : ESIR_Node {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.StaticVariable;
    internal override int ChildrenCount => 2;

    private readonly ESIR_TypeNode typeNode;
    private readonly ESIR_ValueNode nameNode;

    public ESIR_TypeNode Type => typeNode;
    public ES_Identifier Name => nameNode.GetIdentifier ()!.Value;

    internal ESIR_StaticVariable (ESIR_TypeNode type, ESIR_ValueNode name) {
        typeNode = type;
        nameNode = name;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return typeNode;
            case 1: return nameNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_Function : ESIR_Node {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.Function;
    internal override int ChildrenCount => 6;

    private readonly ESIR_ValueNode nameNode;
    private readonly ESIR_List<ESIR_Attribute> attributesNode;
    private readonly ESIR_TypeNode returnTypeNode;
    private readonly ESIR_List<ESIR_ArgumentDefinition> argumentsNode;
    private readonly ESIR_List<ESIR_TypeNode> localValuesNode;
    private readonly ESIR_List<ESIR_Statement> statementsNode;

    public ES_Identifier Name => nameNode.GetIdentifier ()!.Value;
    public ESIR_List<ESIR_Attribute> Attributes => attributesNode;
    public ESIR_TypeNode ReturnType => returnTypeNode;
    public ESIR_List<ESIR_ArgumentDefinition> Arguments => argumentsNode;
    public ESIR_List<ESIR_TypeNode> LocalValues => localValuesNode;
    public ESIR_List<ESIR_Statement> Statements => statementsNode;

    internal ESIR_Function (
        ESIR_ValueNode name,
        ESIR_List<ESIR_Attribute> attributes,
        ESIR_TypeNode returnType,
        ESIR_List<ESIR_ArgumentDefinition> arguments,
        ESIR_List<ESIR_TypeNode> localValues,
        ESIR_List<ESIR_Statement> statements
    ) {
        nameNode = name;
        attributesNode = attributes;
        returnTypeNode = returnType;
        argumentsNode = arguments;
        localValuesNode = localValues;
        statementsNode = statements;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return nameNode;
            case 1: return attributesNode;
            case 2: return returnTypeNode;
            case 3: return argumentsNode;
            case 4: return localValuesNode;
            case 5: return statementsNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public static partial class ESIR_Factory {
    public static ESIR_StaticVariable StaticVariable (ESIR_TypeNode type, ES_Identifier id)
        => StaticVariable (type, ValueNode (id));
    private static ESIR_StaticVariable StaticVariable (ESIR_TypeNode type, ESIR_ValueNode id) => new (type, id);

    public static ESIR_Function Function (
        ES_Identifier name,
        ESIR_List<ESIR_Attribute> attributes,
        ESIR_TypeNode type,
        ESIR_List<ESIR_ArgumentDefinition> args,
        ESIR_List<ESIR_TypeNode> localValues,
        ESIR_List<ESIR_Statement> statements
    ) => Function (ValueNode (name), attributes, type, args, localValues, statements);
    private static ESIR_Function Function (
        ESIR_ValueNode name,
        ESIR_List<ESIR_Attribute> attributes,
        ESIR_TypeNode type,
        ESIR_List<ESIR_ArgumentDefinition> args,
        ESIR_List<ESIR_TypeNode> localValues,
        ESIR_List<ESIR_Statement> statements
    ) => new (name, attributes, type, args, localValues, statements);
}
