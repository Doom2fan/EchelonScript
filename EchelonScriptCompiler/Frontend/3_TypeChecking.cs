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

namespace EchelonScriptCompiler.Frontend {
    public unsafe partial class CompilerFrontend {
        protected ES_TypeInfo* GetType (ref SymbolsStack symbols, ES_AstTypeDeclaration_TypeName typeName, ReadOnlySpan<char> src) {
            var idPool = Environment!.IdPool;

            if (typeName.Namespace != null) {
                var fqnChars = PooledArray<char>.GetArray (typeName.GetStringLength ());
                var fqnId = idPool.GetIdentifier (fqnChars.Span);

                var type = Environment!.GetFullyQualifiedType (fqnId);

                if (type == null) {
                    var err = ES_FrontendErrors.GenCantFindSymbol (fqnChars.Span.GetPooledString (), src, typeName.NodeBounds);
                    errorList.Add (err);
                }

                return type;
            } else {
                var typeParts = typeName.TypeName.Parts;
                var typeId = idPool.GetIdentifier (typeParts [0].Text.Span);
                var type = symbols.GetType (typeId);

                if (type == null) {
                    var err = ES_FrontendErrors.GenCantFindSymbol (typeParts [0].Text.Span.GetPooledString (), typeParts [0]);
                    errorList.Add (err);

                    return null;
                }

                var namespaceName = type->FullyQualifiedName;
                using var partsArr = PooledArray<ArrayPointer<byte>>.GetArray (typeParts.Length);
                for (int i = 0; i < typeParts.Length; i++) {
                    var part = typeParts [i];
                    using var partBytes = PooledArray<byte>.GetArray (part.Text.Length);

                    Encoding.ASCII.GetBytes (part.Text.Span, partBytes.Span);

                    partsArr.Span [i] = idPool.GetIdentifier (partBytes.Span);
                }

                var fqnId = GetFullyQualifiedName (namespaceName, partsArr.Span);
                type = Environment!.GetFullyQualifiedType (fqnId);

                if (type == null) {
                    for (int i = 1; i < typeParts.Length; i++) {
                        fqnId = GetFullyQualifiedName (namespaceName, partsArr.Span.Slice (0, i));
                        type = Environment!.GetFullyQualifiedType (fqnId);

                        if (type == null) {
                            var err = ES_FrontendErrors.GenCantFindSymbol (typeParts [0].Text.Span.GetPooledString (), typeParts [0]);
                            errorList.Add (err);

                            return null;
                        }
                    }

                    Debug.Fail ("This should never be reached.");
                    return null;
                }

                return type;
            }
        }

        protected void CheckTypes (ref TranslationUnitData transUnit) {
            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                foreach (var nm in astUnit.Ast.Namespaces) {
                    ArrayPointer<byte> namespaceName;
                    using (var nameArr = nm.NamespaceName.ToPooledChars ())
                        namespaceName = Environment!.IdPool.GetIdentifier (nameArr);

                    var namespaceBuilder = EnvironmentBuilder!.GetOrCreateNamespace (namespaceName);
                    var namespaceData = namespaceBuilder.NamespaceData;

                    astUnit.Symbols.Push ();
                    astUnit.Symbols.AddTypes (namespaceData.TypeSpan);

                    foreach (var type in nm.Contents) {
                        switch (type) {
                            case ES_AstClassDefinition classDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (classDef.Name.Text.Span);
                                var classBuilder = namespaceBuilder.GetClass (typeName);
                                Debug.Assert (classBuilder != null);

                                CheckTypes_Class (ref transUnit, ref astUnit, classDef, classBuilder);

                                break;
                            }

                            case ES_AstStructDefinition structDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (structDef.Name.Text.Span);
                                var structBuilder = namespaceBuilder.GetStruct (typeName);
                                Debug.Assert (structBuilder != null);

                                CheckTypes_Struct (ref transUnit, ref astUnit, structDef, structBuilder);

                                break;
                            }

                            case ES_AstEnumDefinition enumDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (enumDef.Name.Text.Span);
                                var enumBuilder = namespaceBuilder.GetEnum (typeName);
                                Debug.Assert (enumBuilder != null);

                                CheckTypes_Enum (ref transUnit, ref astUnit, enumDef, enumBuilder);

                                break;
                            }

                            default:
                                throw new NotImplementedException ();
                        }
                    }

                    astUnit.Symbols.Pop ();
                }
            }
        }

        protected void CheckTypes_InheritanceList (
            ref TranslationUnitData transUnit, ref AstUnitData astUnit,
            ReadOnlySpan<ES_AstTypeDeclaration_TypeName> inheritanceList, bool isClass,
            out ArrayPointer<Pointer<ES_InterfaceData>> interfacesListMem, out ES_ClassData* baseClass
        ) {
            baseClass = null;

            var srcCode = astUnit.Ast.Source; ;
            var idPool = Environment!.IdPool;
            ref var symbols = ref astUnit.Symbols;

            using var interfacesList = new StructPooledList<Pointer<ES_InterfaceData>> (CL_ClearMode.Auto);

            foreach (var inheritId in inheritanceList) {
                var type = GetType (ref symbols, inheritId, srcCode.Span);

                if (type == null)
                    continue;

                var typeTag = type->TypeTag;
                if (isClass && typeTag == ES_TypeTag.Class) {
                    if (baseClass != null) {
                        var err = new EchelonScriptErrorMessage (srcCode.Span, inheritId.NodeBounds, ES_FrontendErrors.MultipleBaseClasses);
                        errorList.Add (err);
                        continue;
                    }

                    baseClass = (ES_ClassData*) type;
                } else if (typeTag == ES_TypeTag.Interface) {
                    bool interfaceInList = false;
                    foreach (var interfaceData in interfacesList) {
                        if (interfaceData == type)
                            interfaceInList = true;
                    }

                    if (interfaceInList) {
                        var interfaceFqnStr = type->FullyQualifiedNameString;
                        errorList.Add (ES_FrontendErrors.GenRepeatedInterfaceInList (interfaceFqnStr, srcCode.Span, inheritId.NodeBounds));
                        continue;
                    }

                    interfacesList.Add ((ES_InterfaceData*) type);
                } else {
                    using var charsPool = PooledArray<char>.GetArray (inheritId.GetStringLength ());
                    inheritId.ToString (charsPool.Span);

                    var symbolStr = charsPool.Span.GetPooledString ();
                    errorList.Add (ES_FrontendErrors.GenInvalidInheritance (symbolStr, srcCode.Span, inheritId.NodeBounds));
                    continue;
                }
            }

            interfacesListMem = new ArrayPointer<Pointer<ES_InterfaceData>> (
                (Pointer<ES_InterfaceData>*) EnvironmentBuilder!.GetUnmanagedMemory (sizeof (ES_InterfaceData*)),
                interfacesList.Count
            );
            interfacesList.CopyTo (interfacesListMem.Span);
        }

        protected void CheckTypes_Class (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstClassDefinition classDef, ES_ClassData.Builder builder) {
            if (builder.ClassData->TypeInfo.RuntimeSize > -1)
                return;

            var srcCode = astUnit.Ast.Source;
            var idPool = Environment!.IdPool;
            ref var symbols = ref astUnit.Symbols;

            CheckTypes_InheritanceList (ref transUnit, ref astUnit, classDef.InheritanceList, true, out var interfacesList, out var baseClass);
            builder.BaseClass = baseClass;
            builder.InterfacesList = interfacesList;

            throw new NotImplementedException ();
        }

        protected void CheckTypes_Struct (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstStructDefinition structDef, ES_StructData.Builder builder) {
            if (builder.StructData->TypeInfo.RuntimeSize > -1)
                return;

            var srcCode = astUnit.Ast.Source;
            var idPool = Environment!.IdPool;
            ref var symbols = ref astUnit.Symbols;

            CheckTypes_InheritanceList (ref transUnit, ref astUnit, structDef.InterfacesList, true, out var interfacesList, out _);
            builder.InterfacesList = interfacesList;
        }

        protected void CheckTypes_Enum (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstEnumDefinition enumDef, ES_EnumData.Builder builder) {
            //Debug.Fail ("Not implemented yet");
        }
    }
}
