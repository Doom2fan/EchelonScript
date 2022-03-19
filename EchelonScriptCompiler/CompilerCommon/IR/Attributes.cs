/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCompiler.CompilerCommon.IR;

public abstract class ESIR_Attribute : ESIR_Node { }

public unsafe class ESIR_TraceDataAttribute : ESIR_Attribute {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.TraceDataAttribute;
    internal override int ChildrenCount => 4;

    private readonly ESIR_ValueNode namespaceNode;
    private readonly ESIR_ValueNode nameNode;
    private readonly ESIR_ValueNode? parentTypeNode;
    private readonly ESIR_ValueNode? fileNameNode;

    public ES_Identifier Namespace => namespaceNode.GetIdentifier ()!.Value;
    public ES_Identifier Name => nameNode.GetIdentifier ()!.Value;
    public ES_Identifier? ParentType => parentTypeNode?.GetIdentifier ();
    public string? FileName => fileNameNode?.GetString (out _, null);

    internal ESIR_TraceDataAttribute (
        ESIR_ValueNode nsName,
        ESIR_ValueNode name,
        ESIR_ValueNode? parentType,
        ESIR_ValueNode? fileName
    ) {
        namespaceNode = nsName;
        nameNode = name;
        parentTypeNode = parentType;
        fileNameNode = fileName;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return namespaceNode;
            case 1: return nameNode;
            case 2: return parentTypeNode;
            case 3: return fileNameNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public unsafe class ESIR_FunctionDataAttribute : ESIR_Attribute {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.FunctionDataAttribute;
    internal override int ChildrenCount => 2;

    private readonly ESIR_ValueNode funcDataNode;
    private readonly ESIR_ValueNode? parentTypeNode;

    public ES_FunctionData* FunctionData => (ES_FunctionData*) (funcDataNode.GetPointer () ?? 0);
    public ES_TypeInfo* ParentType => (ES_TypeInfo*) (parentTypeNode?.GetPointer () ?? 0);

    internal ESIR_FunctionDataAttribute (
        ESIR_ValueNode funcData,
        ESIR_ValueNode? parentType
    ) {
        funcDataNode = funcData;
        parentTypeNode = parentType;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return funcDataNode;
            case 1: return parentTypeNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public unsafe static partial class ESIR_Factory {
    public static ESIR_TraceDataAttribute TraceDataAttribute (ES_Identifier ns, ES_Identifier name, string? fileName)
        => TraceDataAttribute (ValueNode (ns), ValueNode (name), null, fileName is not null ? ValueNode (fileName) : null);
    public static ESIR_TraceDataAttribute TraceDataAttribute (ES_Identifier ns, ES_Identifier name, ES_Identifier parentType, string? fileName)
        => TraceDataAttribute (ValueNode (ns), ValueNode (name), ValueNode (parentType), fileName is not null ? ValueNode (fileName) : null);
    private static ESIR_TraceDataAttribute TraceDataAttribute (ESIR_ValueNode ns, ESIR_ValueNode name, ESIR_ValueNode? parentType, ESIR_ValueNode? fileName)
        => new (ns, name, parentType, fileName);

    public static ESIR_FunctionDataAttribute FunctionDataAttribute ([NotNull] ES_FunctionData* funcData)
        => FunctionDataAttribute (ValueNode (funcData), null);
    public static ESIR_FunctionDataAttribute FunctionDataAttribute ([NotNull] ES_FunctionData* funcData, [NotNull] ES_TypeInfo* parentType)
        => FunctionDataAttribute (ValueNode (funcData), ValueNode (parentType));
    private static ESIR_FunctionDataAttribute FunctionDataAttribute (ESIR_ValueNode funcData, ESIR_ValueNode? parentType)
        => new (funcData, parentType);
}
