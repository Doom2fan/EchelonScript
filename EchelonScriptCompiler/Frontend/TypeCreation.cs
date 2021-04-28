/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using ChronosLib.Pooled;
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
                                var fullyQualifiedName = Environment.GetFullyQualifiedName (namespaceName, typeName);

                                if (namespaceBuilder.CheckTypeExists (typeName, null) != null) {
                                    errorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                                        namespaceBuilder.NamespaceData.NamespaceNameString,
                                        enumDef.Name.Text.Span.GetPooledString (),
                                        enumDef.Name
                                    ));
                                    break;
                                }

                                namespaceBuilder.GetOrCreateEnum (
                                    enumDef.AccessModifier,
                                    typeName, fullyQualifiedName, transUnit.Name
                                );

                                break;
                            }

                            case ES_AstFunctionDefinition:
                                break;

                            default:
                                throw new NotImplementedException ();
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
            var fullyQualifiedName = Environment.GetFullyQualifiedName (namespaceName, typeName);

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
                var classBuilder = namespaceBuilder.GetOrCreateClass (
                    typeDef.AccessModifier,
                    typeName, fullyQualifiedName, transUnit.Name
                );

                typeData = &classBuilder.ClassData->TypeInfo;
            } else if (type == ES_TypeTag.Struct) {
                var structBuilder = namespaceBuilder.GetOrCreateStruct (
                    typeDef.AccessModifier,
                    typeName, fullyQualifiedName, transUnit.Name
                );

                typeData = &structBuilder.StructData->TypeInfo;
            } else
                Debug.Fail ("Not implemented/supported.");
        }

        protected void GenerateTypesList () {
            var typesList = EnvironmentBuilder!.TypesList;

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

                var typesStartIdx = typesList.Count;
                var typesCount = newTypesList.Count;

                var typesSpan = typesList.AddSpan (typesCount);
                newTypesList.Span.CopyTo (typesSpan);

                namespaceBuilder.TypesStartIdx = typesStartIdx;
                namespaceBuilder.TypesLength = typesCount;
            }
        }

        protected void GenerateBuiltinTypes () {
            var typesList = EnvironmentBuilder!.TypesList;
            using var newTypesList = new StructPooledList<Pointer<ES_TypeInfo>> (CL_ClearMode.Auto);

            newTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int8, false));
            newTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int16, false));
            newTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int32, false));
            newTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int64, false));

            newTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int8, true));
            newTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int16, true));
            newTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int32, true));
            newTypesList.Add (GenerateBuiltinTypes_Int (ES_IntSize.Int64, true));

            var floatType = GenerateBuiltinTypes_Float (ES_FloatSize.Single);
            var doubleType = GenerateBuiltinTypes_Float (ES_FloatSize.Double);
            EnvironmentBuilder.TypeFloat32 = floatType;
            EnvironmentBuilder.TypeFloat64 = doubleType;
            newTypesList.Add (floatType);
            newTypesList.Add (doubleType);

            var voidType = GenerateBuiltinTypes_Simple (ES_PrimitiveTypes.Void, ES_TypeTag.Void, 0);
            var boolType = GenerateBuiltinTypes_Simple (ES_PrimitiveTypes.Bool, ES_TypeTag.Bool, 1);
            EnvironmentBuilder.TypeVoid = voidType;
            EnvironmentBuilder.TypeBool = boolType;
            newTypesList.Add (voidType);
            newTypesList.Add (boolType);

            // Save type data
            var typesStartIdx = typesList.Count;
            var typesCount = newTypesList.Count;
            var builtinTypesList = EnvironmentBuilder.MemoryManager.GetArray<Pointer<ES_TypeInfo>> (typesCount);
            EnvironmentBuilder.BuiltinTypesList = builtinTypesList;

            newTypesList.Span.CopyTo (typesList.AddSpan (typesCount));
            newTypesList.Span.CopyTo (builtinTypesList.Span);
        }

        protected Pointer<ES_TypeInfo> GenerateBuiltinTypes_Int (ES_IntSize size, bool unsigned) {
            var intDataPtr = EnvironmentBuilder!.MemoryManager.GetMemory<ES_IntTypeData> ();
            var namePtr = Environment!.IdPool.GetIdentifier (ES_PrimitiveTypes.GetIntName (size, unsigned));
            var fqnPtr = Environment.GetFullyQualifiedName (ArrayPointer<byte>.Null, namePtr);

            *intDataPtr = new ES_IntTypeData (ES_AccessModifier.Public, ArrayPointer<byte>.Null, namePtr, fqnPtr, size, unsigned);

            return new Pointer<ES_TypeInfo> (&intDataPtr->TypeInfo);
        }

        protected Pointer<ES_TypeInfo> GenerateBuiltinTypes_Float (ES_FloatSize size) {
            var floatDataPtr = EnvironmentBuilder!.MemoryManager.GetMemory<ES_FloatTypeData> ();
            var namePtr = Environment!.IdPool.GetIdentifier (ES_PrimitiveTypes.GetFloatName (size));
            var fqnPtr = Environment.GetFullyQualifiedName (ArrayPointer<byte>.Null, namePtr);

            *floatDataPtr = new ES_FloatTypeData (ES_AccessModifier.Public, ArrayPointer<byte>.Null, namePtr, fqnPtr, size);

            return new Pointer<ES_TypeInfo> (&floatDataPtr->TypeInfo);
        }

        protected Pointer<ES_TypeInfo> GenerateBuiltinTypes_Simple (ReadOnlySpan<char> name, ES_TypeTag tag, int runtimeSize) {
            var voidDataPtr = EnvironmentBuilder!.MemoryManager.GetMemory<ES_TypeInfo> ();
            var namePtr = Environment!.IdPool.GetIdentifier (name);
            var fqnPtr = Environment.GetFullyQualifiedName (ArrayPointer<byte>.Null, namePtr);

            *voidDataPtr = new ES_TypeInfo (tag, ES_AccessModifier.Public, ArrayPointer<byte>.Null, namePtr, fqnPtr);
            voidDataPtr->RuntimeSize = runtimeSize;

            return new Pointer<ES_TypeInfo> (voidDataPtr);
        }
    }
}
