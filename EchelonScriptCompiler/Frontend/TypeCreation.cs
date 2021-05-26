/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using ChronosLib.Pooled;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Frontend {
    public unsafe partial class CompilerFrontend {
        protected void CreateTypes (ref TranslationUnitData transUnit) {
            var idPool = Environment!.IdPool;

            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                foreach (var nm in astUnit.Ast.Namespaces) {
                    ArrayPointer<byte> namespaceName;
                    using (var nameArr = nm.NamespaceName.ToPooledChars ())
                        namespaceName = idPool.GetIdentifier (nameArr);

                    var namespaceBuilder = EnvironmentBuilder!.GetOrCreateNamespace (namespaceName);

                    foreach (var type in nm.Contents) {
                        switch (type) {
                            case ES_AstClassDefinition classDef:
                                CreateTypes_Aggregate (ref transUnit, namespaceBuilder, ES_TypeTag.Class, classDef);
                                break;

                            case ES_AstStructDefinition structDef:
                                CreateTypes_Aggregate (ref transUnit, namespaceBuilder, ES_TypeTag.Struct, structDef);
                                break;

                            case ES_AstEnumDefinition enumDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (enumDef.Name.Text.Span);

                                if (namespaceBuilder.CheckTypeExists (typeName, null) != null) {
                                    errorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                                        namespaceBuilder.NamespaceData.NamespaceNameString,
                                        enumDef.Name.Text.Span.GetPooledString (),
                                        enumDef.Name
                                    ));
                                    break;
                                }

                                var builder = namespaceBuilder.GetOrCreateEnum (enumDef.AccessModifier, typeName, transUnit.Name);
                                EnvironmentBuilder!.PointerAstMap.Add ((IntPtr) builder.EnumData, enumDef);

                                break;
                            }

                            case ES_AstFunctionDefinition:
                                // Functions are handled in a pass of their own.
                                break;

                            default:
                                throw new NotImplementedException ("Node type not implemented yet.");
                        }
                    }
                }
            }
        }

        protected void CreateTypes_Aggregate (
            ref TranslationUnitData transUnit, ES_NamespaceData.Builder namespaceBuilder,
            ES_TypeTag type, ES_AstAggregateDefinition typeDef
        ) {
            var namespaceName = namespaceBuilder.NamespaceData.NamespaceName;
            var typeName = Environment!.IdPool.GetIdentifier (typeDef.Name.Text.Span);

            if (namespaceBuilder.CheckTypeExists (typeName, null) != null) {
                errorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                    namespaceBuilder.NamespaceData.NamespaceNameString,
                    typeDef.Name.Text.Span.GetPooledString (),
                    typeDef.Name
                ));
                return;
            }

            ES_TypeInfo* typeData = null;
            if (type == ES_TypeTag.Class) {
                var classBuilder = namespaceBuilder.GetOrCreateClass (typeDef.AccessModifier, typeName, transUnit.Name);
                typeData = &classBuilder.ClassData->TypeInfo;
            } else if (type == ES_TypeTag.Struct) {
                var structBuilder = namespaceBuilder.GetOrCreateStruct (typeDef.AccessModifier, typeName, transUnit.Name);
                typeData = &structBuilder.StructData->TypeInfo;
            } else
                Debug.Fail ("Not implemented/supported.");

            EnvironmentBuilder!.PointerAstMap.Add ((IntPtr) typeData, typeDef);
        }

        protected void GenerateTypesList () {
            foreach (var nmKVP in EnvironmentBuilder!.NamespaceBuilders) {
                var namespaceName = nmKVP.Key;
                var namespaceBuilder = nmKVP.Value;
                var namespaceData = namespaceBuilder.NamespaceData;

                using var newTypesList = new StructPooledList<Pointer<ES_TypeInfo>> (CL_ClearMode.Auto);

                newTypesList.EnsureCapacity (
                    namespaceBuilder.ClassBuilders.Count +
                    namespaceBuilder.StructBuilders.Count +
                    namespaceBuilder.EnumBuilders.Count
                );

                foreach (var classKVP in namespaceBuilder.ClassBuilders) {
                    var classBuilder = classKVP.Value;
                    newTypesList.Add (&classBuilder.ClassData->TypeInfo);
                }
                foreach (var structKVP in namespaceBuilder.StructBuilders) {
                    var structBuilder = structKVP.Value;
                    newTypesList.Add (&structBuilder.StructData->TypeInfo);
                }
                foreach (var enumKVP in namespaceBuilder.EnumBuilders) {
                    var enumBuilder = enumKVP.Value;
                    newTypesList.Add (&enumBuilder.EnumData->TypeInfo);
                }
            }
        }

        protected void GenerateBuiltinTypes () {
            Debug.Assert (Environment is not null);
            Debug.Assert (EnvironmentBuilder is not null);

            var globalTypesList = EnvironmentBuilder.GetOrCreateNamespace (Environment.GlobalTypesNamespace).NamespaceData.Types;

            globalTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int8, false));
            globalTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int16, false));
            globalTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int32, false));
            globalTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int64, false));

            globalTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int8, true));
            globalTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int16, true));
            globalTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int32, true));
            globalTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int64, true));

            var floatType = GenerateBuiltinTypes_Float (ES_FloatSize.Single);
            var doubleType = GenerateBuiltinTypes_Float (ES_FloatSize.Double);
            EnvironmentBuilder.TypeFloat32 = floatType;
            EnvironmentBuilder.TypeFloat64 = doubleType;
            globalTypesList.Add (floatType);
            globalTypesList.Add (doubleType);

            var voidType = GenerateBuiltinTypes_Simple (ES_PrimitiveTypes.Void, ES_TypeTag.Void, 0);
            var boolType = GenerateBuiltinTypes_Simple (ES_PrimitiveTypes.Bool, ES_TypeTag.Bool, 1);
            EnvironmentBuilder.TypeVoid = voidType;
            EnvironmentBuilder.TypeBool = boolType;
            globalTypesList.Add (voidType);
            globalTypesList.Add (boolType);
        }

        protected Pointer<ES_TypeInfo> GenerateBuiltinTypes_Int (ES_IntSize size, bool unsigned) {
            var intDataPtr = EnvironmentBuilder!.MemoryManager.GetMemory<ES_IntTypeData> ();
            var namePtr = Environment!.IdPool.GetIdentifier (ES_PrimitiveTypes.GetIntName (size, unsigned));
            var fqn = new ES_FullyQualifiedName (Environment.GlobalTypesNamespace, namePtr);

            *intDataPtr = new ES_IntTypeData (ES_AccessModifier.Public, ArrayPointer<byte>.Null, fqn, size, unsigned);

            return new Pointer<ES_TypeInfo> (&intDataPtr->TypeInfo);
        }

        protected Pointer<ES_TypeInfo> GenerateBuiltinTypes_Float (ES_FloatSize size) {
            var floatDataPtr = EnvironmentBuilder!.MemoryManager.GetMemory<ES_FloatTypeData> ();
            var namePtr = Environment!.IdPool.GetIdentifier (ES_PrimitiveTypes.GetFloatName (size));
            var fqn = new ES_FullyQualifiedName (Environment.GlobalTypesNamespace, namePtr);

            *floatDataPtr = new ES_FloatTypeData (ES_AccessModifier.Public, ArrayPointer<byte>.Null, fqn, size);

            return new Pointer<ES_TypeInfo> (&floatDataPtr->TypeInfo);
        }

        protected Pointer<ES_TypeInfo> GenerateBuiltinTypes_Simple (ReadOnlySpan<char> name, ES_TypeTag tag, int runtimeSize) {
            var voidDataPtr = EnvironmentBuilder!.MemoryManager.GetMemory<ES_TypeInfo> ();
            var namePtr = Environment!.IdPool.GetIdentifier (name);
            var fqn = new ES_FullyQualifiedName (Environment.GlobalTypesNamespace, namePtr);

            *voidDataPtr = new ES_TypeInfo (tag, ES_AccessModifier.Public, ArrayPointer<byte>.Null, fqn);
            voidDataPtr->RuntimeSize = runtimeSize;

            return new Pointer<ES_TypeInfo> (voidDataPtr);
        }
    }
}
