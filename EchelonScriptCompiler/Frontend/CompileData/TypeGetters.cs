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
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.Frontend.Data;

namespace EchelonScriptCompiler.Frontend;

internal ref partial struct CompileData {
    /*
     * Generic getters
     */

    public ESC_TypeRef GetFullyQualifiedType (ES_Identifier namespaceName, ES_Identifier typeName, ESC_Constness constness = ESC_Constness.Mutable)
        => GetFullyQualifiedType (new ES_FullyQualifiedName (namespaceName, typeName), constness);

    public ESC_TypeRef GetFullyQualifiedType (ES_FullyQualifiedName fullyQualifiedName, ESC_Constness constness = ESC_Constness.Mutable) {
        if (!Namespaces.TryGetValue (fullyQualifiedName.NamespaceName, out var namespaceData))
            return ESC_TypeRef.Null (constness);

        var typesList = namespaceData.Types;

        foreach (var type in typesList) {
            if (type.Value.Name.TypeName.Equals (fullyQualifiedName.TypeName))
                return new (constness, type.Value);
        }

        return ESC_TypeRef.Null (constness);
    }

    public ESC_TypeRef GetGlobalType (ES_Identifier name, ESC_Constness constness = ESC_Constness.Mutable)
        => GetFullyQualifiedType (Env.GlobalsNamespace, name, constness);

    public ESC_TypeRef GetGeneratedType (ES_Identifier name, ESC_Constness constness = ESC_Constness.Mutable)
        => GetFullyQualifiedType (Env.GeneratedNamespace, name, constness);

    /*
     * Typed getters
     */

    #region Primitive types

    public ESC_TypeRef GetUnknownType (ESC_Constness constness) => new (constness, TypeUnknown);

    public ESC_TypeRef GetNullType (ESC_Constness constness) => new (constness, TypeNull);

    public ESC_TypeRef GetVoidType (ESC_Constness constness)
        => GetGlobalType (IdPool.GetIdentifier (ES_PrimitiveTypes.Void), constness);

    public ESC_TypeRef GetBoolType (ESC_Constness constness)
        => GetGlobalType (IdPool.GetIdentifier (ES_PrimitiveTypes.Bool), constness);

    public ESC_TypeRef GetIntType (ES_IntSize size, bool unsigned, ESC_Constness constness = ESC_Constness.Mutable) {
        var intName = IdPool.GetIdentifier (ES_PrimitiveTypes.GetIntName (size, unsigned));
        var intType = GetFullyQualifiedType (Env.GlobalsNamespace, intName, constness);
        Debug.Assert (intType.Type is ESC_TypeInt);

        return intType;
    }

    public ESC_TypeRef GetFloat32Type (ESC_Constness constness)
        => GetGlobalType (IdPool.GetIdentifier (ES_PrimitiveTypes.Float32), constness);

    public ESC_TypeRef GetFloat64Type (ESC_Constness constness)
        => GetGlobalType (IdPool.GetIdentifier (ES_PrimitiveTypes.Float64), constness);

    public ESC_TypeRef GetArrayIndexType () => GetIntType (ES_IntSize.Int32, false, ESC_Constness.Const);

    #endregion

    public ESC_TypePrototype? GetPrototype (ESC_TypeRef returnType, ReadOnlySpan<ESC_PrototypeArg> args, bool doAdd) {
        var name = GeneratePrototypeName (returnType, args);
        var fqn = new ES_FullyQualifiedName (Env.GeneratedNamespace, name);

        var funcType = GetFullyQualifiedType (fqn);

        if (funcType.Type is not ESC_TypePrototype && doAdd) {
            funcType = funcType.WithType (new ESC_TypePrototype (fqn, returnType, args.ToArray ()));
            GetOrCreateNamespace (Env.GeneratedNamespace).AddType (funcType.Type!);
        }

        return funcType.Type as ESC_TypePrototype;
    }

    #region Derived types

    public ESC_TypeRef GetReferenceType (ESC_TypeRef pointedType, ESC_Constness constness) {
        // Format sample: "@generated::NamespaceName__TypeName&"
        using var charsList = GetGeneratedTypeName (pointedType, "", "&");

        var refId = IdPool.GetIdentifier (charsList.Span);
        var refName = new ES_FullyQualifiedName (Env.GeneratedNamespace, refId);

        var refType = GetFullyQualifiedType (refName, constness);
        if (refType.Type is not null)
            return refType;

        refType = new (constness, new ESC_TypeReference (refName, pointedType));
        GetOrCreateNamespace (Env.GeneratedNamespace).AddType (refType.Type!);

        return refType;
    }

    public ESC_TypeRef GetArrayType (ESC_TypeRef elementType, int rank, ESC_Constness constness) {
        // Format sample: "@generated::NamespaceName__TypeName[,,]"
        using var charsList = GetGeneratedTypeName (elementType, "", "");
        charsList.Add ('[');
        charsList.Add (',', rank - 1);
        charsList.Add (']');

        var arrId = IdPool.GetIdentifier (charsList.Span);
        var arrName = new ES_FullyQualifiedName (Env.GeneratedNamespace, arrId);

        var arrTypeRef = GetFullyQualifiedType (arrName, constness);
        if (arrTypeRef.Type is not null)
            return arrTypeRef;

        var typeIndex = GetArrayIndexType ();
        var arrType = new ESC_TypeArray (arrName, elementType, rank);
        arrTypeRef = new (constness, arrType);

        arrType.Members.Add (new ESC_TypeMember_Field {
            Name = IdPool.GetIdentifier (rank < 2 ? "Length" : "TotalLength"),
            AccessModifier = ES_AccessModifier.Public,
            FieldType = typeIndex.WithConst (ESC_Constness.Const),
        });

        arrType.Members.Add (new ESC_TypeMember_Field {
            Name = IdPool.GetIdentifier ("Rank"),
            AccessModifier = ES_AccessModifier.Public,
            FieldType = GetIntType (ES_IntSize.Int8, true, ESC_Constness.Const),
        });

        for (var i = 0; i < rank; i++) {
            arrType.Members.Add (new ESC_TypeMember_Field {
                Name = IdPool.GetIdentifier ($"LengthD{i}"),
                AccessModifier = ES_AccessModifier.Public,
                FieldType = typeIndex.WithConst (ESC_Constness.Const),
            });
        }

        GetOrCreateNamespace (Env.GeneratedNamespace).AddType (arrTypeRef.Type!);

        return arrTypeRef;
    }

    public ESC_TypeRef GetNullableType (ESC_TypeRef innerType, ESC_Constness constness) {
        throw new NotImplementedException ("[TODO] Nullable types not implemented yet.");
    }

    #endregion
}
