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
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Utilities;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Frontend {
    public unsafe partial class CompilerFrontend {
        protected void GatherTypes (ref TranslationUnitData transUnit) {
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
                                GatherTypes_Aggregate (ref transUnit, namespaceBuilder, ES_TypeTag.Class, classDef);
                                break;

                            case ES_AstStructDefinition structDef:
                                GatherTypes_Aggregate (ref transUnit, namespaceBuilder, ES_TypeTag.Struct, structDef);
                                break;

                            case ES_AstEnumDefinition enumDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (enumDef.Name.Text.Span);
                                var fullyQualifiedName = GetFullyQualifiedName (namespaceName, typeName);

                                if (namespaceBuilder.CheckTypeExists (typeName, null) != null) {
                                    errorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                                        namespaceBuilder.NamespaceData.NamespaceNameString,
                                        StringPool.Shared.GetOrAdd (enumDef.Name.Text.Span),
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

                            case ES_AstFunctionDefinition funcDef:
                                GatherTypes_Functions (ref transUnit, namespaceBuilder, null, funcDef);
                                break;

                            default:
                                throw new NotImplementedException ();
                        }
                    }
                }
            }
        }

        protected void GatherTypes_Aggregate (
            ref TranslationUnitData transUnit, ES_NamespaceData.Builder namespaceBuilder,
            ES_TypeTag type, ES_AstAggregateDefinition typeDef
        ) {
            var namespaceName = namespaceBuilder.NamespaceData.NamespaceName;
            var typeName = Environment!.IdPool.GetIdentifier (typeDef.Name.Text.Span);
            var fullyQualifiedName = GetFullyQualifiedName (namespaceName, typeName);

            if (namespaceBuilder.CheckTypeExists (typeName, null) != null) {
                errorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                    namespaceBuilder.NamespaceData.NamespaceNameString,
                    StringPool.Shared.GetOrAdd (typeDef.Name.Text.Span),
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

            foreach (ES_AstFunctionDefinition? funcDef in typeDef.Contents) {
                if (funcDef is null)
                    continue;

                GatherTypes_Functions (ref transUnit, namespaceBuilder, typeData, funcDef!);
            }
        }

        protected void GatherTypes_Functions (
            ref TranslationUnitData transUnit, ES_NamespaceData.Builder namespaceBuilder,
            ES_TypeInfo* parentType, ES_AstFunctionDefinition funcDef
        ) {
            var namespaceName = namespaceBuilder.NamespaceData.NamespaceName;
            var funcName = Environment!.IdPool.GetIdentifier (funcDef.Name.Text.Span);
            ArrayPointer<byte> fullyQualifiedName;

            if (parentType == null) {
                fullyQualifiedName = GetFullyQualifiedName (namespaceName, funcName);
            } else {
                Span<ArrayPointer<byte>> parts = stackalloc ArrayPointer<byte> [2];
                parts [0] = parentType->TypeName;
                parts [1] = funcName;
                fullyQualifiedName = GetFullyQualifiedName (namespaceName, funcName);
            }

            if (namespaceBuilder.CheckTypeExists (funcName, null) != null) {
                errorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                    namespaceBuilder.NamespaceData.NamespaceNameString,
                    StringPool.Shared.GetOrAdd (funcDef.Name.Text.Span),
                    funcDef.Name
                ));
                return;
            }

            var funcBuilder = namespaceBuilder.GetOrCreateFunction (
                funcDef.AccessModifier,
                funcName, fullyQualifiedName, transUnit.Name
            );

            funcBuilder.ParentType = parentType;
            funcBuilder.Flags = 0 | (parentType != null ? ES_FunctionFlags.Method : 0);
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
                    namespaceBuilder.EnumBuilders.Count +
                    namespaceBuilder.FunctionBuilders.Count
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
                foreach (var functionKVP in namespaceBuilder.FunctionBuilders) {
                    var functionBuilder = functionKVP.Value;
                    newTypesList.Add (&functionBuilder.FunctionData->TypeInfo);
                }

                var typesStartIdx = typesList.Count;
                var typesCount = newTypesList.Count;

                var typesSpan = typesList.AddSpan (typesCount);
                newTypesList.Span.CopyTo (typesSpan);

                namespaceBuilder.TypesStartIdx = typesStartIdx;
                namespaceBuilder.TypesLength = typesCount;
            }
        }

        protected void GatherGlobalImports (ref TranslationUnitData transUnit) {
            var idPool = Environment!.IdPool;
            var namespaces = Environment!.Namespaces;

            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                var ast = astUnit.Ast;
                ref var symbols = ref astUnit.Symbols;
                symbols.Push ();

                foreach (var import in ast.ImportStatements) {
                    using var nmNameString = import.NamespaceName.ToPooledChars ();
                    var namespaceName = idPool.GetIdentifier (nmNameString.Span);

                    if (!Environment.Namespaces.TryGetValue (namespaceName, out var namespaceData)) {
                        var err = ES_FrontendErrors.GenNamespaceDoesntExist (
                            StringPool.Shared.GetOrAdd (nmNameString.Span),
                            astUnit.Ast.Source.Span,
                            import.NamespaceName.NodeBounds
                        );
                        errorList.Add (err);
                        continue;
                    }

                    if (import.ImportedNames is null || import.ImportedNames.Length == 0)
                        symbols.AddTypes (namespaceData.TypeSpan);
                    else {
                        var typesSpan = namespaceData.TypeSpan;

                        foreach (var importTk in import.ImportedNames) {
                            var importId = idPool.GetIdentifier (importTk.Text.Span);
                            ES_TypeInfo* type = null;

                            foreach (var typeInfo in namespaceData.TypeSpan) {
                                var typeId = typeInfo.Address->TypeName;

                                if (importId.Equals (typeId)) {
                                    type = typeInfo;
                                    break;
                                }
                            }

                            if (type == null) {
                                var err = ES_FrontendErrors.GenCantFindSymbol (importTk.Text.GetPooledString (), importTk);
                                errorList.Add (err);
                                continue;
                            }

                            symbols.AddType (type);
                        }
                    }
                }
            }
        }
    }
}
