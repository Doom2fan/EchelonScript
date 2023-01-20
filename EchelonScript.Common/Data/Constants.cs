/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;

namespace EchelonScript.Common.Data.Types;

public static class ES_PrimitiveTypeConsts {
    public const string Void = "void";
    public const string Object = "object";
    public const string Bool = "bool";

    public const string Int64 = "int64";
    public const string Int32 = "int32";
    public const string Int16 = "int16";
    public const string Int8  = "int8";

    public const string UInt64 = "uint64";
    public const string UInt32 = "uint32";
    public const string UInt16 = "uint16";
    public const string UInt8  = "uint8";

    public const string Float32 = "float32";
    public const string Float64 = "float64";

    public const string String = "string";
    public const string Char = "char";

    public static string GetIntName (ES_IntSize size, bool unsigned) {
        return size switch {
            ES_IntSize.Int8 => unsigned ? UInt8 : Int8,
            ES_IntSize.Int16 => unsigned ? UInt16 : Int16,
            ES_IntSize.Int32 => unsigned ? UInt32 : Int32,
            ES_IntSize.Int64 => unsigned ? UInt64 : Int64,

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }

    public static int GetIntMemorySize (ES_IntSize size) {
        return size switch {
            ES_IntSize.Int8 => 1,
            ES_IntSize.Int16 => 2,
            ES_IntSize.Int32 => 4,
            ES_IntSize.Int64 => 8,

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }

    public static string GetFloatName (ES_FloatSize size) {
        return size switch {
            ES_FloatSize.Single => Float32,
            ES_FloatSize.Double => Float64,

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }

    public static int GetFloatMemorySize (ES_FloatSize size) {
        return size switch {
            ES_FloatSize.Single => 4,
            ES_FloatSize.Double => 8,

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }
}
