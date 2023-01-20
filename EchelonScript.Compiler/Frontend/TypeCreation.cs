/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using ChronosLib.Pooled;
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.Utilities;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;

namespace EchelonScript.Compiler.Frontend;

internal unsafe static class Compiler_TypeCreation {
    public static void CreateTypes (ref CompileData compileData) { }

#if false
    private ref struct PassData {
        public ES_Identifier TransUnitName;
        public SourceData Source;
    }

    public static void CreateTypes (ref CompileData compileData) {
        GenerateBuiltinTypes (ref compileData);

        foreach (ref var transUnit in compileData.TranslationUnits) {
            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                var passData = new PassData {
                    TransUnitName = transUnit.Name,
                    Source = astUnit.SourceData,
                };

                CheckAst (ref compileData, ref passData, astUnit.Ast);
            }
        }
    }

    private static void CheckAst (ref CompileData compileData, ref PassData passData, ES_AbstractSyntaxTree ast) {
        foreach (var nm in ast.Namespaces) {
            var nsName = nm.NamespaceName.ToIdentifier (compileData.IdPool);
            var nsData = compileData.GetOrCreateNamespace (nsName);

            foreach (var type in nm.Contents) {
                switch (type) {
                    case ES_AstClassDefinition classDef:
                        CreateTypes_Aggregate (ref compileData, ref passData, nsData, ES_TypeTag.Class, classDef);
                        break;

                    case ES_AstStructDefinition structDef:
                        CreateTypes_Aggregate (ref compileData, ref passData, nsData, ES_TypeTag.Struct, structDef);
                        break;

                    case ES_AstEnumDefinition enumDef: {
                        var typeName = compileData.IdPool.GetIdentifier (enumDef.Name.Text.Span);

                        if (nsData.CheckTypeExists (typeName, null, out _) != null) {
                            compileData.ErrorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                                nsData.NamespaceName.GetCharsSpan ().GetPooledString (),
                                enumDef.Name.Text.Span.GetPooledString (),
                                enumDef.Name
                            ));
                            break;
                        }

                        var enumData = nsData.GetOrCreateEnum (enumDef.AccessModifier, typeName, passData.TransUnitName);
                        enumData.AccessModifier = enumDef.AccessModifier;

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

    private static void CreateTypes_Aggregate (
        ref CompileData compileData, ref PassData passData, ESC_Namespace nsData,
        ES_TypeTag type, ES_AstAggregateDefinition typeDef
    ) {
        var typeName = compileData.IdPool.GetIdentifier (typeDef.Name.Text.Span);

        if (nsData.CheckTypeExists (typeName, null, out _) != null) {
            compileData.ErrorList.Add (ES_FrontendErrors.GenTypeAlreadyDefined (
                nsData.NamespaceName.GetCharsSpan ().GetPooledString (),
                typeDef.Name.Text.Span.GetPooledString (),
                typeDef.Name
            ));
            return;
        }

        ESC_TypeAggregate typeData;
        switch (type) {
            case ES_TypeTag.Class:
                typeData = nsData.GetOrCreateClass (typeDef.AccessModifier, typeName, passData.TransUnitName);
                break;
            case ES_TypeTag.Struct:
                typeData = nsData.GetOrCreateStruct (typeDef.AccessModifier, typeName, passData.TransUnitName);
                break;

            default:
                throw new NotImplementedException ("Not implemented/supported.");
        };

        typeData.AccessModifier = typeDef.AccessModifier;
    }

    private static void GenerateBuiltinTypes (ref CompileData compileData) {
        var idPool = compileData.IdPool;
        var globalNSId = compileData.Env.GlobalsNamespace;
        var globalNS = compileData.GetOrCreateNamespace (globalNSId);

        globalNS.AddType (GenerateBuiltinTypes_Int (ref compileData, ES_IntSize.Int8, false));
        globalNS.AddType (GenerateBuiltinTypes_Int (ref compileData, ES_IntSize.Int16, false));
        globalNS.AddType (GenerateBuiltinTypes_Int (ref compileData, ES_IntSize.Int32, false));
        globalNS.AddType (GenerateBuiltinTypes_Int (ref compileData, ES_IntSize.Int64, false));

        globalNS.AddType (GenerateBuiltinTypes_Int (ref compileData, ES_IntSize.Int8, true));
        globalNS.AddType (GenerateBuiltinTypes_Int (ref compileData, ES_IntSize.Int16, true));
        globalNS.AddType (GenerateBuiltinTypes_Int (ref compileData, ES_IntSize.Int32, true));
        globalNS.AddType (GenerateBuiltinTypes_Int (ref compileData, ES_IntSize.Int64, true));

        var floatType = GenerateBuiltinTypes_Float (ref compileData, ES_FloatSize.Single);
        var doubleType = GenerateBuiltinTypes_Float (ref compileData, ES_FloatSize.Double);
        globalNS.AddType (floatType);
        globalNS.AddType (doubleType);

        globalNS.AddType (new ESC_TypeVoid (new (globalNSId, idPool.GetIdentifier (ES_PrimitiveTypes.Void))));
        globalNS.AddType (new ESC_TypeBool (new (globalNSId, idPool.GetIdentifier (ES_PrimitiveTypes.Bool))));

        compileData.EnvBuilder.TypeUnknown = compileData.ToTypeInfo (compileData.TypeUnknown);
        compileData.EnvBuilder.TypeNull = compileData.ToTypeInfo (compileData.TypeNull);
        compileData.EnvBuilder.TypeVoid = compileData.ToTypeInfo (compileData.GetVoidType (ESC_Constness.Mutable));
        compileData.EnvBuilder.TypeBool = compileData.ToTypeInfo (compileData.GetBoolType (ESC_Constness.Mutable));
        compileData.EnvBuilder.TypeFloat32 = compileData.ToTypeInfo (compileData.GetFloat32Type (ESC_Constness.Mutable));
        compileData.EnvBuilder.TypeFloat64 = compileData.ToTypeInfo (compileData.GetFloat64Type (ESC_Constness.Mutable));
    }

    private static ESC_TypeInt GenerateBuiltinTypes_Int (ref CompileData compileData, ES_IntSize size, bool unsigned)  {
        var nameId = compileData.IdPool.GetIdentifier (ES_PrimitiveTypes.GetIntName (size, unsigned));
        return new (new (compileData.Env.GlobalsNamespace, nameId), size, unsigned);
    }

    private static ESC_TypeFloat GenerateBuiltinTypes_Float (ref CompileData compileData, ES_FloatSize size) {
        var nameId = compileData.IdPool.GetIdentifier (ES_PrimitiveTypes.GetFloatName (size));
        return new (new (compileData.Env.GlobalsNamespace, nameId), size);
    }
#endif

}
