/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Common.Utilities;

namespace EchelonScript.Common.Data.Types;

public enum ES_IntSize : byte {
    Int8  = 1,
    Int16 = 2,
    Int32 = 3,
    Int64 = 4,
}

public enum ES_FloatSize : byte {
    Single = 1,
    Double = 2,
}

public unsafe struct ES_IntInfo {
    private const int SizeMask = 0b0000_1111;
    private const int UnsignedBit = 0b1000_0000;

    private nint data;

    private ES_IntInfo (nint value) => data = value;
    public ES_IntInfo (ES_IntSize size, bool unsigned) {
        data = ((int) size & SizeMask) | (unsigned ? UnsignedBit : 0);
    }

    public ES_IntSize Size => (ES_IntSize) (data & SizeMask);

    public bool Unsigned => (data & UnsignedBit) == UnsignedBit;

    public bool IsValid () {
        if ((data & ~(SizeMask & UnsignedBit)) != 0)
            return false;

        return Size >= ES_IntSize.Int8 && Size <= ES_IntSize.Int64;
    }

    public static explicit operator ES_IntInfo (void* value) => new ((nint) value);
    public static explicit operator ES_IntInfo (nint value) => new (value);

    public static explicit operator void* (ES_IntInfo value) => (void*) value.data;
    public static explicit operator nint (ES_IntInfo value) => value.data;
}

public unsafe struct ES_FloatInfo {
    private const int SizeMask = 0b0000_1111;

    private nint data;

    private ES_FloatInfo (nint value) => data = value;
    public ES_FloatInfo (ES_FloatSize size) => data = ((int) size & SizeMask);

    public ES_FloatSize Size => (ES_FloatSize) (data & SizeMask);

    public bool IsValid () {
        if ((data & ~SizeMask) != 0)
            return false;

        return Size >= ES_FloatSize.Single && Size <= ES_FloatSize.Double;
    }

    public static explicit operator ES_FloatInfo (void* value) => new ((nint) value);
    public static explicit operator ES_FloatInfo (nint value) => new (value);

    public static explicit operator void* (ES_FloatInfo value) => (void*) value.data;
    public static explicit operator nint (ES_FloatInfo value) => value.data;
}

public readonly unsafe struct ES_ArrayInfo {
    public readonly int Rank;
    public readonly ES_Constness ElementConstness;
    public readonly ES_MethodTable* ElementType;

    public ES_ArrayInfo (int rank, ES_Constness elemConst, ES_MethodTable* elemType) {
        Rank = rank;
        ElementConstness = elemConst;
        ElementType = elemType;
    }
}

public readonly unsafe struct ES_ReferenceInfo {
    public readonly ES_Constness PointedConstness;
    public readonly ES_MethodTable* PointedType;

    public ES_ReferenceInfo (ES_Constness pointedConst, ES_MethodTable* pointedType) {
        PointedConstness = pointedConst;
        PointedType = pointedType;
    }
}

public readonly unsafe struct ES_FunctionPrototypeInfo {
    #region ================== Instance fields

    public readonly ES_MethodTable* ReturnType;
    public readonly ArrayPointer<ES_FunctionProtoArgInfo> ArgumentsList;

    #endregion

    public ES_FunctionPrototypeInfo (ES_MethodTable* retType, ArrayPointer<ES_FunctionProtoArgInfo> argsList) {
        ReturnType = retType;
        ArgumentsList = argsList;
    }
}

public readonly unsafe struct ES_FunctionProtoArgInfo {
    #region ================== Instance fields

    public readonly ES_ArgumentKind ArgKind;
    public readonly ES_MethodTable* ArgType;

    #endregion

    public ES_FunctionProtoArgInfo (ES_ArgumentKind kind, ES_MethodTable* type) {
        ArgKind = kind;
        ArgType = type;
    }
}
