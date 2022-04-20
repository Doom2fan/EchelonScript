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
using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.Frontend.Data;

namespace EchelonScriptCompiler.Frontend;

internal ref partial struct CompileData {
    private unsafe ES_TypeInfo* GetOrCreateTypeInfo<T> (ES_FullyQualifiedName name, ES_TypeTag? typeTag, out bool created) where T : unmanaged {
        var ret = Env.GetFullyQualifiedType (name);
        if (ret is not null) {
            Debug.Assert (typeTag is null || ret->TypeTag == typeTag);
            created = false;
            return ret;
        }

        ret = (ES_TypeInfo*) EnvBuilder.MemoryManager.GetMemoryAligned<T> (sizeof (void*));
        *ret = new (typeTag ?? ES_TypeTag.UNKNOWN, ES_AccessModifier.Public, ES_TypeFlag.None, ES_Identifier.Empty, name);

        var nsData = EnvBuilder.GetOrCreateNamespace (name.NamespaceName);
        nsData.Types.Add (ret);
        created = true;

        return ret;
    }

    private unsafe ES_TypeInfo* GetOrCreateTypeInfo<T> (ES_FullyQualifiedName name, ES_TypeTag? typeTag) where T : unmanaged
        => GetOrCreateTypeInfo<T> (name, typeTag, out _);

    private unsafe ES_TypeInfo* GetTypeInfo_Reference (ES_FullyQualifiedName name, ES_TypeInfo* pointedType) {
        var refData = (ES_ReferenceData*) GetOrCreateTypeInfo<ES_ReferenceData> (name, ES_TypeTag.Reference, out var typeCreated);
        if (typeCreated) {
            *refData = new (name, pointedType);
            refData->TypeInfo.RefsList = EnvBuilder.ReferenceTypeRefList;
        }

        return &refData->TypeInfo;
    }

    private unsafe ES_TypeInfo* GetTypeInfo_Array (ES_FullyQualifiedName name, ES_TypeInfo* elemType, int rank) {
        var arrayData = (ES_ArrayData*) GetOrCreateTypeInfo<ES_ArrayData> (name, ES_TypeTag.Array, out var typeCreated);
        if (typeCreated) {
            *arrayData = new (name, elemType, rank);
            arrayData->TypeInfo.RefsList = EnvBuilder.ReferenceTypeRefList;
        }

        return &arrayData->TypeInfo;
    }

    private unsafe ES_TypeInfo* GetTypeInfo_Const (ES_TypeInfo* innerType, bool immutable) {
        // Format sample: "@generated::const(NamespaceName__TypeName)" or "@generated::immutable(NamespaceName__TypeName)"
        var constPrefix = immutable ? "immutable(" : "const(";
        using var constChars = GetGeneratedFQN (innerType->Name, constPrefix, ")");

        var constId = IdPool.GetIdentifier (constChars.Span);
        var constName = new ES_FullyQualifiedName (Env.GeneratedNamespace, constId);

        var refData = (ES_ConstData*) GetOrCreateTypeInfo<ES_ConstData> (constName, null, out var typeCreated);
        if (typeCreated)
            *refData = new (constName, innerType, immutable);
        else
            Debug.Assert (refData->TypeInfo.TypeTag == (!immutable ? ES_TypeTag.Const : ES_TypeTag.Immutable));

        return &refData->TypeInfo;
    }

    public unsafe ES_TypeInfo* ToTypeInfo (ESC_TypeRef type) {
        Debug.Assert (type.Type is not null);

        ES_TypeInfo* retType;

        retType = type.Type switch {
            ESC_TypeUnknown typeUnkn => GetOrCreateTypeInfo<ES_TypeInfo> (typeUnkn.Name, ES_TypeTag.UNKNOWN),
            ESC_TypeNull typeNull => GetOrCreateTypeInfo<ES_TypeInfo> (typeNull.Name, ES_TypeTag.Null),

            ESC_TypeVoid typeVoid => GetOrCreateTypeInfo<ES_TypeInfo> (typeVoid.Name, ES_TypeTag.Void),
            ESC_TypeBool typeBool => GetOrCreateTypeInfo<ES_TypeInfo> (typeBool.Name, ES_TypeTag.Bool),
            ESC_TypeInt typeInt => GetOrCreateTypeInfo<ES_IntData> (typeInt.Name, ES_TypeTag.Int),
            ESC_TypeFloat typeFloat => GetOrCreateTypeInfo<ES_FloatData> (typeFloat.Name, ES_TypeTag.Float),
            ESC_TypeEnum typeEnum => GetOrCreateTypeInfo<ES_EnumData> (typeEnum.Name, ES_TypeTag.Enum),

            ESC_TypeStruct typeStruct => GetOrCreateTypeInfo<ES_StructData> (typeStruct.Name, ES_TypeTag.Struct),
            ESC_TypeClass typeClass => GetOrCreateTypeInfo<ES_ClassData> (typeClass.Name, ES_TypeTag.Class),
            ESC_TypeInterface typeInterface => GetOrCreateTypeInfo<ES_InterfaceData> (typeInterface.Name, ES_TypeTag.Interface),

            ESC_TypePrototype typePrototype => GetOrCreateTypeInfo<ES_FunctionPrototypeData> (typePrototype.Name, ES_TypeTag.FuncPrototype),

            ESC_TypeReference typeRef => GetTypeInfo_Reference (typeRef.Name, ToTypeInfo (typeRef.PointedType)),
            ESC_TypeArray typeArray => GetTypeInfo_Array (typeArray.Name, ToTypeInfo (typeArray.ElementType), typeArray.Rank),

            _ => throw new NotImplementedException ("Type not implemented."),
        };


        retType = type.Constness switch {
            ESC_Constness.Mutable => retType,
            ESC_Constness.Const => GetTypeInfo_Const (retType, false),
            ESC_Constness.Immutable => GetTypeInfo_Const (retType, true),

            _ => throw new NotImplementedException ("Constness not implemented."),
        };

        return retType;
    }

    public unsafe ES_TypeInfo* ToTypeInfo (ESC_TypeData type) => ToTypeInfo (new ESC_TypeRef (ESC_Constness.Mutable, type));

    public unsafe ES_FunctionData* ToFunctionInfo (ESC_Function func) {
        ES_FullyQualifiedName fqn;
        if (func.Parent.Type is not null) {
            using var funcNameChars = new StructPooledList<char> (CL_ClearMode.Auto);

            funcNameChars.AddRange (func.Parent.Type.Name.TypeName.GetCharsSpan ());
            funcNameChars.Add ('.');
            funcNameChars.AddRange (func.Name.GetCharsSpan ());

            var funcName = IdPool.GetIdentifier (funcNameChars.Span);
            fqn = new (func.Parent.Type.Name.NamespaceName, funcName);
        } else
            fqn = new (func.Parent.NamespaceName, func.Name);

        var nsBuilder = EnvBuilder.GetOrCreateNamespace (fqn.NamespaceName);
        if (nsBuilder.Functions.TryGetValue (fqn.TypeName, out var funcInfoPtr))
            return funcInfoPtr.Address;

        var protoInfo = (ES_FunctionPrototypeData*) ToTypeInfo (func.Prototype);
        Debug.Assert (protoInfo->TypeInfo.TypeTag == ES_TypeTag.FuncPrototype);

        var args = EnvBuilder.MemoryManager.GetArray<ES_FunctionArg> (func.Arguments.Length);
        var argIdx = 0;
        foreach (var arg in func.Arguments)
            args.Elements [argIdx++] = new (arg.Name);

        var funcInfo = EnvBuilder.MemoryManager.GetMemory<ES_FunctionData> ();
        *funcInfo = new (fqn, func.AccessModifier, func.SourceUnit, protoInfo, args, func.OptionalArgsCount);

        nsBuilder.Functions.Add (fqn.TypeName, funcInfo);

        return funcInfo;
    }
}
