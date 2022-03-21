/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;
using EchelonScriptCommon.Data;

namespace EchelonScriptCompiler.CompilerCommon.IR;

internal enum ESIR_ValueType {
    None,

    Int,
    Float32,
    Float64,
    Char,

    Identifier,
    String,
    Pointer,
}

internal unsafe struct ESIR_NodeValue {
    public ESIR_ValueType Type { get; private init; }

    internal ulong ValueInt { get; init; }
    internal ES_Identifier ValueId { get; init; }
    internal string? ValueString { get; init; }
    internal void* ValuePointer { get; init; }

    #region ================== Constructors

    public ESIR_NodeValue (long value) {
        Type = ESIR_ValueType.Int;

        ValueInt = (ulong) value;
        ValueId = ES_Identifier.Empty;
        ValueString = null;
        ValuePointer = null;
    }

    public ESIR_NodeValue (ulong value) {
        Type = ESIR_ValueType.Int;

        ValueInt = value;
        ValueId = ES_Identifier.Empty;
        ValueString = null;
        ValuePointer = null;
    }

    public ESIR_NodeValue (float value) {
        Type = ESIR_ValueType.Float32;

        ValueInt = Unsafe.As<float, uint> (ref value);
        ValueId = ES_Identifier.Empty;
        ValueString = null;
        ValuePointer = null;
    }

    public ESIR_NodeValue (double value) {
        Type = ESIR_ValueType.Float64;

        ValueInt = Unsafe.As<double, ulong> (ref value);
        ValueId = ES_Identifier.Empty;
        ValueString = null;
        ValuePointer = null;
    }

    public ESIR_NodeValue (ES_Identifier id) {
        Type = ESIR_ValueType.Identifier;

        ValueId = id;
        ValueInt = 0;
        ValueString = null;
        ValuePointer = null;
    }

    public ESIR_NodeValue (string value) {
        Type = ESIR_ValueType.String;

        ValueString = value;
        ValueInt = 0;
        ValueId = ES_Identifier.Empty;
        ValuePointer = null;
    }

    public ESIR_NodeValue (Rune value) {
        Type = ESIR_ValueType.Char;

        ValueInt = (ulong) value.Value;
        ValueId = ES_Identifier.Empty;
        ValueString = null;
        ValuePointer = null;
    }

    public ESIR_NodeValue (void* value) {
        Type = ESIR_ValueType.Pointer;

        ValuePointer = value;
        ValueInt = 0;
        ValueId = ES_Identifier.Empty;
        ValueString = null;
    }

    #endregion

    public bool Equals (ESIR_NodeValue other) {
        if (Type != other.Type)
            return false;

        switch (Type) {
            case ESIR_ValueType.Int:
            case ESIR_ValueType.Float32:
            case ESIR_ValueType.Float64:
            case ESIR_ValueType.Char:
                return ValueInt == other.ValueInt;

            case ESIR_ValueType.Identifier:
                return ValueId.Equals (other.ValueId);

            case ESIR_ValueType.String:
                if (ValueString is null || other.ValueString is null)
                    return ValueString == other.ValueString;

                return ValueString.Equals (other.ValueString, StringComparison.Ordinal);

            case ESIR_ValueType.Pointer:
                return ValuePointer == other.ValuePointer;

            default:
                Debug.Fail ("Invalid value type");
                return false;
        }
    }

    public override int GetHashCode () {
        var code = (int) Type;

        switch (Type) {
            case ESIR_ValueType.Int:
            case ESIR_ValueType.Float32:
            case ESIR_ValueType.Float64:
            case ESIR_ValueType.Char:
                code = HashCode.Combine (code, ValueInt.GetHashCode ());
                break;

            case ESIR_ValueType.Identifier:
                code = HashCode.Combine (code, ValueId.GetHashCode ());
                break;

            case ESIR_ValueType.String:
                if (ValueString is not null)
                    code = HashCode.Combine (code, ValueString.GetHashCode (StringComparison.Ordinal));
                break;

            case ESIR_ValueType.Pointer:
                code = HashCode.Combine (code, ((nint) ValuePointer).GetHashCode ());
                break;

            default:
                Debug.Fail ("Invalid value type");
                break;
        }

        return code;
    }
}

public unsafe class ESIR_ValueNode : ESIR_Node {
    private ESIR_NodeKind nodeKind;
    public override ESIR_NodeKind Kind => nodeKind;

    private ESIR_NodeValue nodeValue;

    internal override int ChildrenCount => 0;
    internal override ESIR_Node? GetChild (int slot) => null;

    internal ESIR_ValueNode (ESIR_NodeKind kind, ESIR_NodeValue value) {
        nodeKind = kind;
        nodeValue = value;
    }

    internal bool IsEquivalent (ESIR_NodeKind kind, ESIR_NodeValue value) {
        if (kind != nodeKind)
            return false;

        return nodeValue.Equals (value);
    }

    public bool TryGetInt (out long value) {
        if (nodeKind != ESIR_NodeKind.ValueInt) {
            value = 0;
            return false;
        }

        value = (long) nodeValue.ValueInt;
        return true;
    }

    public bool TryGetUInt (out ulong value) {
        if (nodeKind != ESIR_NodeKind.ValueUInt) {
            value = 0;
            return false;
        }

        value = nodeValue.ValueInt;
        return true;
    }

    public bool TryGetFloat32 (out float value) {
        if (nodeKind != ESIR_NodeKind.ValueFloat32) {
            value = 0;
            return false;
        }

        var valInt = (int) nodeValue.ValueInt;
        value = Unsafe.As<int, float> (ref valInt);
        return true;
    }

    public bool TryGetFloat64 (out double value) {
        if (nodeKind != ESIR_NodeKind.ValueFloat64) {
            value = 0;
            return false;
        }

        var valLong = nodeValue.ValueInt;
        value = Unsafe.As<ulong, double> (ref valLong);
        return true;
    }

    public bool TryGetIdentifier (out ES_Identifier value) {
        if (nodeKind != ESIR_NodeKind.ValueIdentifier) {
            value = ES_Identifier.Empty;
            return false;
        }

        value = nodeValue.ValueId;
        return true;
    }

    public bool TryGetString (out string? value) {
        if (nodeKind != ESIR_NodeKind.ValueString) {
            value = null;
            return false;
        }

        value = nodeValue.ValueString;
        return true;
    }

    public bool TryGetPointer (out nint? value) {
        if (nodeKind != ESIR_NodeKind.ValuePointer) {
            value = null;
            return false;
        }

        value = (nint) nodeValue.ValuePointer;
        return true;
    }

    public long? GetInt () => Kind == ESIR_NodeKind.ValueInt ? (long) nodeValue.ValueInt : null;

    public ulong? GetUInt () => Kind == ESIR_NodeKind.ValueUInt ? nodeValue.ValueInt : null;

    public float? GetFloat32 () {
        if (Kind != ESIR_NodeKind.ValueFloat32)
            return null;

        var valInt = (int) nodeValue.ValueInt;
        return Unsafe.As<int, float> (ref valInt);
    }

    public double? GetFloat64 () {
        if (Kind != ESIR_NodeKind.ValueFloat64)
            return null;

        var valLong = nodeValue.ValueInt;
        return Unsafe.As<ulong, double> (ref valLong);
    }

    public ES_Identifier? GetIdentifier () => Kind == ESIR_NodeKind.ValueIdentifier ? nodeValue.ValueId : null;

    public string? GetString (out bool isValid, string? errorVal = null) {
        if (nodeKind != ESIR_NodeKind.ValueString) {
            isValid = false;
            return errorVal;
        }

        isValid = true;
        return nodeValue.ValueString;
    }

    public nint? GetPointer () => Kind == ESIR_NodeKind.ValuePointer ? (nint) nodeValue.ValuePointer : null;
}

internal sealed class ESIR_ValueNodeCache : ESIR_NodeCacheBase {
    internal readonly static ESIR_ValueNodeCache Shared = new ();

    private ESIR_ValueNodeCache () : base () { }

    internal ESIR_ValueNode GetNode (ESIR_NodeKind kind, ESIR_NodeValue value) {
        switch (value.Type) {
            case ESIR_ValueType.String:
            case ESIR_ValueType.Identifier:
            case ESIR_ValueType.Pointer:
                return new (kind, value);
        }

        var nodeHash = GetCacheHash (kind, value);
        var bucketIdx = nodeHash & CacheMask;

        ref var bucket = ref buckets [bucketIdx];
        foreach (var entry in bucket) {
            if (entry.Hash != nodeHash)
                continue;

            Debug.Assert (entry.Node is ESIR_ValueNode);
            var other = (entry.Node as ESIR_ValueNode)!;

            if (other.IsEquivalent (kind, value))
                return other;
        }

        var newNode = new ESIR_ValueNode (kind, value) {
            IsCached = true
        };
        AddToBucket (ref bucket, new CacheEntry (nodeHash, newNode));

        return newNode;
    }

    private static int GetCacheHash (ESIR_NodeKind kind, ESIR_NodeValue value) {
        var code = HashCode.Combine ((int) kind, value.GetHashCode ());
        return code & 0x7FFFFFFF;
    }
}

public unsafe static partial class ESIR_Factory {
    private static ESIR_ValueNode ValueNode (ESIR_NodeKind kind, ESIR_NodeValue value)
        => ESIR_ValueNodeCache.Shared.GetNode (kind, value);

    public static ESIR_ValueNode ValueNode (long value) => ValueNode (ESIR_NodeKind.ValueInt, new (value));
    public static ESIR_ValueNode ValueNode (ulong value) => ValueNode (ESIR_NodeKind.ValueUInt, new (value));
    public static ESIR_ValueNode ValueNode (float value) => ValueNode (ESIR_NodeKind.ValueFloat32, new (value));
    public static ESIR_ValueNode ValueNode (double value) => ValueNode (ESIR_NodeKind.ValueFloat64, new (value));
    public static ESIR_ValueNode ValueNode (string value) => ValueNode (ESIR_NodeKind.ValueString, new (value));
    public static ESIR_ValueNode ValueNode (void* value) => ValueNode (ESIR_NodeKind.ValuePointer, new (value));
    public static ESIR_ValueNode ValueNode (ES_Identifier value)
        => ValueNode (ESIR_NodeKind.ValueIdentifier, new (value));
}
