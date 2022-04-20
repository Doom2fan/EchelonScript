/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.Frontend.Data;

internal struct ESC_FunctionArg {
    public ES_Identifier Name { get; private init; }
    public ES_AstExpression? DefaultValue { get; private init; }

    public ESC_FunctionArg (ES_Identifier name, ES_AstExpression? defVal) {
        Name = name;
        DefaultValue = defVal;
    }
}

internal struct ESC_PrototypeArg {
    public ES_ArgumentType ArgType { get; private init; }
    public ESC_TypeRef ValueType { get; private init; }

    public ESC_PrototypeArg (ES_ArgumentType argType, ESC_TypeRef valueType) {
        ArgType = argType;
        ValueType = valueType;
    }
}

internal struct ESC_FunctionParent {
    public ESC_TypeData? Type { get; private init; }
    public ES_Identifier NamespaceName { get; private init; }

    public ESC_FunctionParent (ESC_TypeData parentType) {
        Type = parentType;
        NamespaceName = ES_Identifier.Empty;
    }

    public ESC_FunctionParent (ES_Identifier nsName) {
        NamespaceName = nsName;
        Type = null;
    }
}

internal class ESC_Function {
    public ESC_FunctionParent Parent { get; private init; }
    public ES_Identifier Name { get; private init; }

    public ES_AccessModifier AccessModifier { get; private init; }
    public ES_Identifier SourceUnit { get; private init; }

    public ESC_TypePrototype Prototype { get; private init; }
    public ESC_FunctionArg [] Arguments { get; private init; }
    public int OptionalArgsCount { get; private init; }

    public ESC_Function (
        ESC_FunctionParent parent, ES_Identifier name,
        ES_AccessModifier accessMod, ES_Identifier sourceUnit,
        ESC_TypePrototype proto, ESC_FunctionArg [] args, int optArgsCount
    ) {
        Parent = parent;
        Name = name;

        AccessModifier = accessMod;
        SourceUnit = sourceUnit;

        Prototype = proto;
        Arguments = args;
        OptionalArgsCount = optArgsCount;
    }
}
