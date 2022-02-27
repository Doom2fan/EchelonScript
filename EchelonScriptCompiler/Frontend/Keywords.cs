/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.Frontend;

public static class ES_PrimitiveTypes {
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

    public static string GetFloatName (ES_FloatSize size) {
        return size switch {
            ES_FloatSize.Single => Float32,
            ES_FloatSize.Double => Float64,

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }
}

public static class ES_Keywords {
    public const string Using = "using";
    public const string Alias = "alias";
    public const string Namespace = "namespace";

    public const string Var = "var";
    public const string New = "new";
    public const string Cast = "cast";
    public const string This = "this";

    public const string Void = "void";
    public const string Const = "const";
    public const string Immutable = "immutable";

    public const string Public = "public";
    public const string Protected = "protected";
    public const string Internal = "internal";
    public const string Private = "private";

    public const string Static = "static";

    public const string Virtual = "virtual";
    public const string Abstract = "abstract";
    public const string Override = "override";

    public const string Class = "class";
    public const string Struct = "struct";
    public const string Enum = "enum";

    public const string Ref = "ref";
    public const string In = "in";
    public const string Out = "out";

    public const string If = "if";
    public const string Else = "else";
    public const string Switch = "switch";
    public const string Break = "break";
    public const string Continue = "continue";
    public const string Goto = "goto";
    public const string Return = "return";

    public const string While = "while";
    public const string Do = "do";
    public const string For = "for";

    public const string Case = "case";
    public const string Default = "default";

    public const string True = "true";
    public const string False = "false";
    public const string Null = "null";
}
