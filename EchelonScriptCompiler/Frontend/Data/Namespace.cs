/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Collections.Generic;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.Frontend.Data;

internal sealed class ESC_Namespace {
    #region ================== Instance properties

    public ES_Identifier NamespaceName { get; private init; }

    public Dictionary<ES_Identifier, ESC_Function> Functions { get; private init; }
    public Dictionary<ES_Identifier, ESC_TypeData> Types { get; private init; }

    #endregion

    public ESC_Namespace (ES_Identifier name) {
        NamespaceName = name;

        Functions = new ();
        Types = new ();
    }

    #region ================== Instance methods

    public bool AddType (ESC_TypeData type) => Types.TryAdd (type.Name.TypeName, type);

    public ES_TypeTag? CheckTypeExists (ES_Identifier name, ES_TypeTag? ignoredType, out ESC_TypeData? value) {
        if (!Types.TryGetValue (name, out value))
            return null;

        ES_TypeTag? retType = value switch {
            ESC_TypeClass => ES_TypeTag.Class,
            ESC_TypeStruct => ES_TypeTag.Struct,
            ESC_TypeEnum => ES_TypeTag.Enum,
            ESC_TypePrototype => ES_TypeTag.FuncPrototype,

            _ => null,
        };

        return retType != ignoredType ? retType : null;
    }

    public ESC_TypeClass GetOrCreateClass (ES_AccessModifier accessMod,
        ES_Identifier name, ES_Identifier sourceUnit
    ) {
        if (CheckTypeExists (name, ES_TypeTag.Class, out var typeData) != null)
            throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

        if (typeData is ESC_TypeClass classData)
            return classData;

        classData = new ESC_TypeClass (new ES_FullyQualifiedName (NamespaceName, name)) {
            AccessModifier = accessMod,
            SourceUnit = sourceUnit,
        };
        AddType (classData);

        return classData;
    }

    public ESC_TypeStruct GetOrCreateStruct (ES_AccessModifier accessMod,
        ES_Identifier name, ES_Identifier sourceUnit
    ) {
        if (CheckTypeExists (name, ES_TypeTag.Struct, out var typeData) != null)
            throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

        if (typeData is ESC_TypeStruct structData)
            return structData;

        structData = new ESC_TypeStruct (new ES_FullyQualifiedName (NamespaceName, name)) {
            AccessModifier = accessMod,
            SourceUnit = sourceUnit,
        };
        AddType (structData);

        return structData;
    }

    public ESC_TypeEnum GetOrCreateEnum (ES_AccessModifier accessMod,
        ES_Identifier name, ES_Identifier sourceUnit
    ) {
        if (CheckTypeExists (name, ES_TypeTag.Struct, out var typeData) != null)
            throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

        if (typeData is ESC_TypeEnum enumData)
            return enumData;

        enumData = new ESC_TypeEnum (new ES_FullyQualifiedName (NamespaceName, name), null) {
            AccessModifier = accessMod,
            SourceUnit = sourceUnit,
        };
        AddType (enumData);

        return enumData;
    }

    public ESC_TypeClass? GetClass (ES_Identifier name) {
        if (CheckTypeExists (name, ES_TypeTag.Struct, out var typeData) != null)
            throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

        if (typeData is ESC_TypeClass classData)
            return classData;

        return null;
    }

    public ESC_TypeStruct? GetStruct (ES_Identifier name) {
        if (CheckTypeExists (name, ES_TypeTag.Struct, out var typeData) != null)
            throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

        if (typeData is ESC_TypeStruct structData)
            return structData;

        return null;
    }

    public ESC_TypeEnum? GetEnum (ES_Identifier name) {
        if (CheckTypeExists (name, ES_TypeTag.Struct, out var typeData) != null)
            throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

        if (typeData is ESC_TypeEnum enumData)
            return enumData;

        return null;
    }

    #endregion
}
