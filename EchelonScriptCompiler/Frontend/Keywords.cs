/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScriptCompiler.Frontend {
    public static class ES_PrimitiveTypes {
        public const string Object = "object";
        public const string Bool = "bool";

        public const string Int64 = "int64";
        public const string Int32 = "int32";
        public const string Int16 = "int16";
        public const string Int8  = "int8";

        public const string UInt64 = "int64";
        public const string UInt32 = "int32";
        public const string UInt16 = "int16";
        public const string UInt8  = "int8";

        public const string Float32 = "float32";
        public const string Float64 = "float64";

        public const string String = "string";
        public const string Char = "char";
    }

    public static class ES_Keywords {
        public const string Using = "using";
        public const string Alias = "alias";
        public const string Namespace = "namespace";

        public const string Var = "var";
        public const string New = "new";
        public const string Cast = "cast";

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
    }
}
