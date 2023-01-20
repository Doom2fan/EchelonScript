/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.Utilities;

namespace EchelonScript.Compiler.Frontend;

internal unsafe static class Compiler_TypeInfoGen {
    public static void GenerateTypes (ref CompileData compileData) {
        /*HandleType (ref compileData, compileData.TypeUnknown);
        HandleType (ref compileData, compileData.TypeNull);

        foreach (var nsData in compileData.Namespaces.Values) {
            foreach (var type in nsData.Types.Values)
                HandleType (ref compileData, type);
        }

        foreach (var nsData in compileData.Env.Namespaces.Values) {
            foreach (var typeInfo in nsData.Types) {
                Debug.Assert (typeInfo.Address is not null);

                RecurseTypeInfo (ref compileData, typeInfo.Address);
            }
        }*/
    }
#if false
    private static void HandleType (ref CompileData compileData, ESC_TypeData type) {
        var typeInfo = compileData.ToTypeInfo (type);
        switch (type) {
            case ESC_TypeUnknown:
            case ESC_TypeNull:
                break;

            case ESC_TypeVoid:
            case ESC_TypeBool:
                break;

            case ESC_TypeInt typeInt: {
                var intInfo = (ES_IntData*) typeInfo;
                *intInfo = new (intInfo->TypeInfo.Name, typeInt.Size, typeInt.Unsigned);
                break;
            }

            case ESC_TypeFloat typeFloat: {
                var floatInfo = (ES_FloatData*) typeInfo;
                *floatInfo = new (floatInfo->TypeInfo.Name, typeFloat.Size);
                break;
            }

            case ESC_TypeStruct typeStruct:
            case ESC_TypeClass typeClass:
            case ESC_TypeInterface:

            case ESC_TypeReference typeReference:
            case ESC_TypeArray typeArray:
                break;

            case ESC_TypePrototype typeProto: {
                var protoInfo = (ES_FunctionPrototypeData*) typeInfo;
                Debug.Assert (protoInfo->TypeInfo.TypeTag == ES_TypeTag.FuncPrototype);

                var retType = compileData.ToTypeInfo (typeProto.ReturnType);
                var argsList = compileData.MemoryManager.GetArray<ES_FunctionPrototypeArg> (typeProto.Arguments.Length);

                var argIdx = 0;
                foreach (var arg in typeProto.Arguments) {
                    var argInfo = new ES_FunctionPrototypeArg (arg.ArgType, compileData.ToTypeInfo (arg.ValueType));
                    argsList.Span [argIdx++] = argInfo;
                }

                *protoInfo = new (protoInfo->TypeInfo.Name, retType, argsList);

                break;
            }

            default:
                throw new NotImplementedException ("Type not implemented.");
        }

        typeInfo->RuntimeSize = type.GetRuntimeSize ();
        typeInfo->RefsList = GetGCRefsArray (ref compileData, type.GetGCRefs ());

        if (type.Flags.HasFlag (ESC_TypeFlag.NoNew))
            typeInfo->Flags |= ES_TypeFlag.NoNew;
        if (type.Flags.HasFlag (ESC_TypeFlag.NoRefs))
            typeInfo->Flags |= ES_TypeFlag.NoRefs;

        typeInfo->Flags |= ES_TypeFlag.Analyzed;
    }

    private static ArrayPointer<nint> GetGCRefsArray (ref CompileData compileData, IEnumerable<nint> gcRefs) {
        var len = 0;
        foreach (var _ in gcRefs)
            len++;

        if (len < 1)
            return ArrayPointer<nint>.Null;
        else if (len == compileData.EnvBuilder.ReferenceTypeRefList.Length)
            return compileData.EnvBuilder.ReferenceTypeRefList;

        var ret = compileData.EnvBuilder.MemoryManager.GetArrayAligned<nint> (len, sizeof (void*));
        var refIdx = 0;
        foreach (var gcRef in gcRefs)
            ret.Span [refIdx++] = gcRef;

        return ret;
    }

    private static void RecurseTypeInfo (ref CompileData compileData, [NotNull] ES_TypeInfo* typeInfo) {
        if (typeInfo->Flags.HasFlag (ES_TypeFlag.Analyzed))
            return;

        switch (typeInfo->TypeTag) {
            case ES_TypeTag.UNKNOWN:
            case ES_TypeTag.Null:

            case ES_TypeTag.Void:
            case ES_TypeTag.Bool:
            case ES_TypeTag.Int:
            case ES_TypeTag.Float:

            case ES_TypeTag.Struct:
            case ES_TypeTag.Class:
            case ES_TypeTag.Interface:

            case ES_TypeTag.FuncPrototype:
                Debug.Assert (typeInfo->Flags.HasFlag (ES_TypeFlag.Analyzed));
                return;

            case ES_TypeTag.Enum: {
                var enumData = (ES_EnumData*) typeInfo;

                Debug.Assert (enumData->BaseType is not null);
                RecurseTypeInfo (ref compileData, enumData->BaseType);

                enumData->TypeInfo.RuntimeSize = enumData->BaseType->RuntimeSize;
                enumData->TypeInfo.RefsList = enumData->BaseType->RefsList;
                enumData->TypeInfo.Flags |= ES_TypeFlag.Analyzed;
                if (enumData->TypeInfo.RefsList.Length > 0 || enumData->BaseType->Flags.HasFlag (ES_TypeFlag.NoRefs))
                    enumData->TypeInfo.Flags |= ES_TypeFlag.NoRefs;

                return;
            }

            case ES_TypeTag.Reference: {
                var refData = (ES_ReferenceData*) typeInfo;

                Debug.Assert (refData->PointedType is not null);
                RecurseTypeInfo (ref compileData, refData->PointedType);

                refData->TypeInfo.Flags |= ES_TypeFlag.Analyzed;

                return;
            }

            case ES_TypeTag.Array: {
                var arrayData = (ES_ArrayData*) typeInfo;

                Debug.Assert (arrayData->ElementType is not null);
                RecurseTypeInfo (ref compileData, arrayData->ElementType);

                arrayData->TypeInfo.Flags |= ES_TypeFlag.Analyzed;

                return;
            }

            case ES_TypeTag.Const:
            case ES_TypeTag.Immutable: {
                var constData = (ES_ConstData*) typeInfo;

                Debug.Assert (constData->InnerType is not null);
                RecurseTypeInfo (ref compileData, constData->InnerType);

                constData->TypeInfo.RuntimeSize = constData->InnerType->RuntimeSize;
                constData->TypeInfo.RefsList = constData->InnerType->RefsList;
                constData->TypeInfo.Flags |= constData->InnerType->Flags | ES_TypeFlag.Analyzed;

                return;
            }
        }
    }

#endif
}
