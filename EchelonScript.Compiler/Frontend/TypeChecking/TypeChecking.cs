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
using EchelonScript.Common;
using EchelonScript.Common.Data;
using EchelonScript.Compiler.CompilerCommon.MIR;
using EchelonScript.Compiler.Data;

namespace EchelonScript.Compiler.Frontend;

internal unsafe static partial class Compiler_TypeChecking {
#if false
    private ref struct PassData {
        public ES_Identifier TransUnitName { get; set; }
        public SourceData Source { get; set; }

        public ESIR_Writer IRWriter { get; init; }
    }

    private static ESC_TypeRef GetTypeRef (ES_AstTypeDeclaration? typeDecl) {
        Debug.Assert (typeDecl is not null);

        var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;
        Debug.Assert (typeRef is not null);

        return typeRef.Reference;
    }

    public static ESIR_Tree CheckTypes (ref CompileData compileData) {
        using var irWriter = new ESIR_Writer ();
        var passData = new PassData {
            IRWriter = irWriter,
        };

        Debug.Assert (compileData.Symbols.ScopesCount == 0);
        compileData.Symbols.Push ();

        compileData.GatherGlobalImports ();

        foreach (ref var transUnit in compileData.TranslationUnits) {
            passData.TransUnitName = transUnit.Name;

            foreach (ref var astUnit in transUnit.AstUnits.Span)
                CheckAstUnit (ref compileData, ref passData, ref astUnit);
        }

        GenerateGlobalStaticConstructor (ref compileData, ref passData);

        compileData.Symbols.Pop ();
        Debug.Assert (compileData.Symbols.ScopesCount == 0);

        return irWriter.FinishTree ();
    }

    private static void CheckAstUnit (ref CompileData compileData, ref PassData passData, ref AstUnitData astUnit) {
        var idPool = compileData.IdPool;

        passData.Source = astUnit.SourceData;
        compileData.Symbols.Push ();

        compileData.GatherAstImports (ref astUnit);

        foreach (var nm in astUnit.Ast.Namespaces) {
            var namespaceName = nm.NamespaceName.ToIdentifier (idPool);
            var namespaceData = compileData.GetOrCreateNamespace (namespaceName);

            compileData.Symbols.Push ();

            compileData.ImportNamespaceSymbols (namespaceData);

            foreach (var type in nm.Contents) {
                switch (type) {
                    case ES_AstClassDefinition classDef: {
                        var typeName = idPool.GetIdentifier (classDef.Name.Text.Span);
                        var classData = namespaceData.GetClass (typeName);
                        Debug.Assert (classData is not null);

                        // TODO: CheckTypes_Class (ref compileData, ref passData, classDef, classData);
                        throw new NotImplementedException ("[TODO] Classes not implemented yet.");
                    }

                    case ES_AstStructDefinition structDef: {
                        var typeName = idPool.GetIdentifier (structDef.Name.Text.Span);
                        var structData = namespaceData.GetStruct (typeName);
                        Debug.Assert (structData is not null);

                        CheckStruct (ref compileData, ref passData, structDef, structData);
                        break;
                    }

                    case ES_AstEnumDefinition enumDef: {
                        var typeName = idPool.GetIdentifier (enumDef.Name.Text.Span);
                        var enumData = namespaceData.GetEnum (typeName);
                        Debug.Assert (enumData is not null);

                        // TODO: CheckTypes_Enum (ref compileData, ref passData, enumDef, enumData);
                        throw new NotImplementedException ("[TODO] Enums not implemented yet.");
                    }

                    case ES_AstFunctionDefinition funcDef:
                        CheckFunction (ref compileData, ref passData, null, namespaceData, funcDef);
                        break;

                    default:
                        throw new NotImplementedException ("Node type not implemented.");
                }
            }

            compileData.Symbols.Pop ();
        }

        compileData.Symbols.Pop ();
    }

    private static ESC_Namespace? GetNamespace (ref CompileData compileData, ReadOnlySpan<char> namespaceStr) {
        var namespaceName = compileData.IdPool.GetIdentifier (namespaceStr);

        if (!compileData.Namespaces.TryGetValue (namespaceName, out var namespaceData)) {
            Debug.Fail ("We shouldn't get here.");
            return null;
        }

        return namespaceData;
    }

    private static void CheckAggregate (
        ref CompileData compileData, ref PassData passData,
        ESC_TypeAggregate type,
        ES_AstAggregateDefinition typeDef, bool isClass
    ) {
        var idPool = compileData.IdPool;
        var irWriter = passData.IRWriter;
        var irTypeVoid = TypeNode (ref compileData, compileData.GetVoidType (ESC_Constness.Mutable));

        using var defStaticConsBody = new StructPooledList<ESIR_Statement> (CL_ClearMode.Auto);

        foreach (var content in typeDef.Contents) {
            switch (content) {
                case ES_AstMemberVarDefinition varDef: {
                    Debug.Assert (varDef.ValueType is not null);

                    var varId = idPool.GetIdentifier (varDef.Name.Text.Span);
                    var varType = GetTypeRef (varDef.ValueType);

                    if (varType.Type!.Flags.HasFlag (ESC_TypeFlag.NoNew)) {
                        compileData.ErrorList.Add (ES_FrontendErrors.GenNoTypeNew (
                            compileData.GetNiceNameString (varType, true), passData.Source, varDef.ValueType.NodeBounds
                        ));
                    }

                    ESIR_Expression initValue;
                    if (varDef.InitializationExpression is not null) {
                        if (!isClass && !varDef.Static)
                            compileData.ErrorList.Add (new (varDef.Name, ES_FrontendErrors.InstDefValOutsideClass));

                        var defExpr = CheckExpression (ref compileData, ref passData, varDef.InitializationExpression, varType);
                        var compat = EnsureCompat (ref compileData, ref passData, ref defExpr, varType, defExpr.Expr.NodeBounds);

                        if (!defExpr.CompileTimeConst) {
                            compileData.ErrorList.Add (new (
                                passData.Source, defExpr.Expr.NodeBounds, ES_FrontendErrors.ConstantExprExpected
                            ));
                        }

                        initValue = defExpr.Value;
                    } else
                        initValue = DefaultValueExpression (TypeNode (ref compileData, varType));

                    if (varDef.Static) {
                        var mangledStaticVar = MangleStaticVar (ref compileData, type.Name, varId);
                        passData.IRWriter.AddStaticVar (StaticVariable (TypeNode (ref compileData, varType), mangledStaticVar));

                        var staticVarName = MangleStaticVar (ref compileData, type.Name, varId);

                        defStaticConsBody.Add (
                            ExpressionStatement (
                                AssignmentExpression (
                                    StaticVariableExpression (staticVarName),
                                    initValue
                                )
                            )
                        );
                    }

                    break;
                }

                case ES_AstFunctionDefinition funcDef:
                    throw new NotImplementedException ("[TODO] Member functions not implemented yet.");

                default:
                    throw new NotImplementedException ("Node type not implemented.");
            }
        }

        var defStaticConsName = MangleDefStaticConstructor (ref compileData, type.Name);
        irWriter.StartFunction (defStaticConsName, irTypeVoid);
        irWriter.AddStatements (defStaticConsBody.Span);
        irWriter.EndFunction (null);
    }

    private static void EnsureInterfaces (
        ref CompileData compileData, ref PassData passData,
        ReadOnlySpan<ESC_TypeInterface> interfaces, ESC_TypeAggregate type
    ) {
        if (interfaces.Length > 0)
            throw new NotImplementedException ("[TODO] Interfaces not implemented yet.");
    }

    private static void CheckStruct (
        ref CompileData compileData, ref PassData passData,
        ES_AstStructDefinition structDef, ESC_TypeStruct structData
    ) {
        CheckAggregate (ref compileData, ref passData, structData, structDef, false);

        EnsureInterfaces (ref compileData, ref passData, structData.Interfaces, structData);

        using var membersList = new StructPooledList<ESIR_MemberNode> (CL_ClearMode.Auto);
        foreach (var member in structData.GetMembers ()) {
            var memberName = member.Name;

            switch (member) {
                case ESC_TypeMember_Field memberField: {
                    ESIR_MemberNode irMember;
                    if (memberField.Flags.HasFlag (ESC_MemberFlags.Static)) {
                        var staticVarName = MangleStaticVar (ref compileData, structData.Name, memberName);
                        irMember = StaticField (memberName, staticVarName);
                    } else
                        irMember = Field (TypeNode (ref compileData, memberField.FieldType), memberField.Offset, memberName);

                    membersList.Add (irMember);

                    break;
                }

                case ESC_TypeMember_Function:
                    break;

                default:
                    throw new NotImplementedException ("Member type not implemented.");
            }
        }

        passData.IRWriter.AddStruct (Struct (compileData.ToTypeInfo (structData), List (membersList.Span)));
    }

    private static bool IsNullable (ref CompileData compileData, ESC_TypeRef destType, out ESC_TypeRef retType) {
        Debug.Assert (destType.Type is not null);

        var typeUnkn = compileData.GetUnknownType (ESC_Constness.Mutable);

        switch (destType.Type) {
            case ESC_TypeUnknown:
                retType = typeUnkn.WithConst (destType.Constness);
                return true;

            case ESC_TypeInterface:
            case ESC_TypeReference:
            case ESC_TypeArray:
                retType = destType;
                return true;

            default:
                retType = typeUnkn.WithConst (destType.Constness);
                return false;
        }
    }

    private static ESIR_TypeNode TypeNode (ref CompileData compileData, ESC_TypeRef type)
        => ESIR_Factory.TypeNode (compileData.ToTypeInfo (type.WithConst (ESC_Constness.Mutable)));

    private static void GenerateGlobalStaticConstructor (ref CompileData compileData, ref PassData passData) {
        var irWriter = passData.IRWriter;

        using var globalStaticConsBody = new StructPooledList<ESIR_Statement> (CL_ClearMode.Auto);

        foreach (var nsKVP in compileData.Namespaces) {
            foreach (var typeKVP in nsKVP.Value.Types) {
                switch (typeKVP.Value) {
                    case ESC_TypeNull:
                    case ESC_TypeVoid:
                    case ESC_TypeBool:
                    case ESC_TypeInt:
                    case ESC_TypeFloat:
                    case ESC_TypePrototype:
                    case ESC_TypeEnum:
                    case ESC_TypeInterface:
                    case ESC_TypeReference:
                    case ESC_TypeArray:
                        continue;

                    case ESC_TypeStruct:
                    case ESC_TypeClass:
                        break;

                    default:
                        throw new NotImplementedException ("Type not implemented yet.");
                }

                var defStaticConsName = MangleDefStaticConstructor (ref compileData, typeKVP.Value.Name);
                globalStaticConsBody.Add (
                    ExpressionStatement (
                        FunctionCallExpression (defStaticConsName, List<ESIR_ArgumentValue> ())
                    )
                );
            }
        }

        var globalStaticConsName = compileData.IdPool.GetIdentifier (ES_Constants.GlobalStaticConstructorName);
        irWriter.StartFunction (globalStaticConsName, TypeNode (ref compileData, compileData.GetVoidType (ESC_Constness.Mutable)));
        irWriter.AddStatements (globalStaticConsBody.Span);
        irWriter.EndFunction (null);
    }
#endif
}
