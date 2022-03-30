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
using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Data;

using static EchelonScriptCompiler.CompilerCommon.IR.ESIR_Factory;

namespace EchelonScriptCompiler.Frontend;

internal unsafe static partial class Compiler_TypeChecking {
    private ref struct PassData {
        public EchelonScriptEnvironment Env { get; init; }
        public EchelonScriptEnvironment.Builder EnvBuilder { get; init; }

        public List<EchelonScriptErrorMessage> ErrorList { get; init; }
        public List<EchelonScriptErrorMessage> WarnList { get; init; }
        public List<EchelonScriptErrorMessage> InfoList { get; init; }

        public ES_Identifier TransUnitName { get; set; }
        public SourceData Source { get; set; }

        public SymbolStack<TCSymbol> Symbols { get; set; }
        public ESIR_Writer IRWriter { get; init; }
        public ES_IdentifierPool IdPool => Env.IdPool;

        public TypeData GetArrayIndexType () => new (Constness.Const, Env.GetArrayIndexType ());
        public TypeData GetUnknownType (Constness constness) => new (constness, Env.TypeUnknownValue);
        public TypeData GetFloat32Type (Constness constness) => new (constness, Env.TypeFloat32);
        public TypeData GetFloat64Type (Constness constness) => new (constness, Env.TypeFloat64);
        public TypeData GetBoolType (Constness constness) => new (constness, Env.TypeBool);
    }

    private enum SymbolType {
        None,
        Type,
        Variable,
        Function,
    }

    private enum SymbolFlags {
        UsingVar            = 1,
        RefVar              = 1 << 1,
        OutVar              = 1 << 2,
        CompileTimeConstant = 1 << 3,
        Writable            = 1 << 4,
    }

    private unsafe struct TCSymbol {
        #region ================== Instance fields

        private readonly void* valuePtr;
        private readonly TCVariable valueVar;

        public readonly SymbolType Tag;
        public readonly SymbolFlags Flags;

        #endregion

        #region ================== Constructors

        internal TCSymbol (SymbolType tag, void* value, SymbolFlags flags) {
            Tag = tag;
            valuePtr = value;
            valueVar = default;
            Flags = flags;
        }

        internal TCSymbol (TCVariable value, SymbolFlags flags) {
            Tag = SymbolType.Variable;
            valueVar = value;
            valuePtr = default;
            Flags = flags;
        }

        #endregion

        #region ================== Instance methods

        public ES_TypeInfo* MatchType () {
            if (Tag != SymbolType.Type)
                throw new CompilationException ();

            return (ES_TypeInfo*) valuePtr;
        }

        public TCVariable MatchVar () {
            if (Tag != SymbolType.Variable)
                throw new CompilationException ();

            return valueVar;
        }

        public ES_FunctionData* MatchFunction () {
            if (Tag != SymbolType.Function)
                throw new CompilationException ();

            return (ES_FunctionData*) valuePtr;
        }

        #endregion

        public static TCSymbol NewVariable (TypeData type, ESIR_Expression expr, SymbolFlags flags = 0)
            => new (new (type, expr), flags);

        public static TCSymbol NewType (ES_TypeInfo* type) => new (SymbolType.Type, type, 0);

        public static TCSymbol NewFunction (ES_FunctionData* data) => new (SymbolType.Function, data, 0);
    }

    private unsafe struct TCVariable {
        public readonly TypeData Type;
        public readonly ESIR_Expression IRExpression;

        internal TCVariable (TypeData type, ESIR_Expression irExpr) {
            Type = type;
            IRExpression = irExpr;
        }
    }

    private static ES_TypeInfo* GetTypeRef (ES_AstTypeDeclaration? typeDecl) {
        Debug.Assert (typeDecl is not null);

        var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;
        Debug.Assert (typeRef is not null);

        return typeRef.Reference;
    }

    public static ESIR_Tree CheckTypes (
        EchelonScriptEnvironment env,
        EchelonScriptEnvironment.Builder envBuilder,

        List<EchelonScriptErrorMessage> errList,
        List<EchelonScriptErrorMessage> warnList,
        List<EchelonScriptErrorMessage> infoList,

        Span<TranslationUnitData> transUnits
    ) {
        using var irWriter = new ESIR_Writer ();
        using var symbols = new SymbolStack<TCSymbol> (new TCSymbol (SymbolType.None, null, 0));
        var passData = new PassData {
            Env = env,
            EnvBuilder = envBuilder,

            ErrorList = errList,
            WarnList = warnList,
            InfoList = infoList,

            Symbols = symbols,
            IRWriter = irWriter,
        };

        passData.Symbols.Push ();

        GatherGlobalImports (ref passData);

        foreach (ref var transUnit in transUnits) {
            passData.TransUnitName = transUnit.Name;

            foreach (ref var astUnit in transUnit.AstUnits.Span)
                CheckAstUnit (ref passData, ref astUnit);
        }

        GenerateGlobalStaticConstructor (ref passData);

        passData.Symbols.Pop ();

        return irWriter.FinishTree ();
    }

    private static void GatherGlobalImports (ref PassData passData) {
        var idPool = passData.Env.IdPool;
        var globalsNS = passData.EnvBuilder.GetOrCreateNamespace (passData.Env.GlobalsNamespace);
        var globalsList = globalsNS.NamespaceData.Types;

        // Add built-in symbols.
        foreach (var type in globalsList)
            passData.Symbols.AddSymbol (type.Address->Name.TypeName, TCSymbol.NewType (type));
    }

    private static void GatherAstImports (ref PassData passData, ref AstUnitData astUnit) {
        // Add imported symbols.
        foreach (var import in astUnit.Ast.ImportStatements)
            HandleImport (ref passData, import);

        foreach (var alias in astUnit.Ast.TypeAliases)
            HandleAlias (ref passData, alias);
    }

    private static void CheckAstUnit (ref PassData passData, ref AstUnitData astUnit) {
        var idPool = passData.Env.IdPool;

        passData.Source = astUnit.SourceData;
        passData.Symbols.Push ();

        GatherAstImports (ref passData, ref astUnit);

        foreach (var nm in astUnit.Ast.Namespaces) {
            ES_Identifier namespaceName;
            using (var nameArr = nm.NamespaceName.ToPooledChars ())
                namespaceName = idPool.GetIdentifier (nameArr);

            var namespaceBuilder = passData.EnvBuilder.GetOrCreateNamespace (namespaceName);
            var namespaceData = namespaceBuilder.NamespaceData;

            passData.Symbols.Push ();

            ImportNamespaceSymbols (ref passData, namespaceData);

            foreach (var type in nm.Contents) {
                switch (type) {
                    case ES_AstClassDefinition classDef: {
                        var typeName = idPool.GetIdentifier (classDef.Name.Text.Span);
                        var classBuilder = namespaceBuilder.GetClass (typeName);
                        Debug.Assert (classBuilder is not null);

                        // TODO: CheckTypes_Class (ref transUnit, ref astUnit, classDef, classBuilder);
                        throw new NotImplementedException ("[TODO] Classes not implemented yet.");
                    }

                    case ES_AstStructDefinition structDef: {
                        var typeName = idPool.GetIdentifier (structDef.Name.Text.Span);
                        var structBuilder = namespaceBuilder.GetStruct (typeName);
                        Debug.Assert (structBuilder is not null);

                        CheckStruct (ref passData, structDef, structBuilder);
                        break;
                    }

                    case ES_AstEnumDefinition enumDef: {
                        var typeName = idPool.GetIdentifier (enumDef.Name.Text.Span);
                        var enumBuilder = namespaceBuilder.GetEnum (typeName);
                        Debug.Assert (enumBuilder is not null);

                        // TODO: CheckTypes_Enum (ref transUnit, ref astUnit, enumDef, enumBuilder);
                        throw new NotImplementedException ("[TODO] Enums not implemented yet.");
                    }

                    case ES_AstFunctionDefinition funcDef:
                        CheckFunction (ref passData, null, namespaceData, funcDef);
                        break;

                    default:
                        throw new NotImplementedException ("Node type not implemented.");
                }
            }

            passData.Symbols.Pop ();
        }

        passData.Symbols.Pop ();
    }

    private static ES_NamespaceData? GetNamespace (ref PassData passData, ReadOnlySpan<char> namespaceStr) {
        var namespaceName = passData.Env.IdPool.GetIdentifier (namespaceStr);

        if (!passData.Env.Namespaces.TryGetValue (namespaceName, out var namespaceData)) {
            Debug.Fail ("We shouldn't get here.");
            return null;
        }

        return namespaceData;
    }

    private static void ImportNamespaceSymbols (
        ref PassData passData,
        ES_NamespaceData namespaceData
    ) {
        foreach (var type in namespaceData.Types)
            passData.Symbols.AddSymbol (type.Address->Name.TypeName, TCSymbol.NewType (type));
        foreach (var funcKVP in namespaceData.Functions)
            passData.Symbols.AddSymbol (funcKVP.Key, TCSymbol.NewFunction (funcKVP.Value));
    }

    private static void HandleImport (
        ref PassData passData,
        ES_AstImportStatement import
    ) {
        var symbols = passData.Symbols;

        using var nmNameString = import.NamespaceName.ToPooledChars ();
        var namespaceData = GetNamespace (ref passData, nmNameString);

        if (namespaceData is null)
            return;

        if (import.ImportedNames is null || import.ImportedNames.Length == 0) {
            foreach (var type in namespaceData.Types) {
                if (!symbols.AddSymbol (type.Address->Name.TypeName, TCSymbol.NewType (type)))
                    Debug.Fail ("This shouldn't be reachable.");
            }

            foreach (var funcKVP in namespaceData.Functions) {
                if (!symbols.AddSymbol (funcKVP.Key, TCSymbol.NewFunction (funcKVP.Value)))
                    Debug.Fail ("This shouldn't be reachable.");
            }

            return;
        }

        foreach (var importTk in import.ImportedNames) {
            var name = passData.Env.IdPool.GetIdentifier (importTk.Text.Span);

            var symbolFound = false;
            var isDuplicate = false;

            if (!symbolFound) {
                foreach (var typeData in namespaceData.Types) {
                    if (!typeData.Address->Name.TypeName.Equals (name))
                        continue;

                    isDuplicate = !symbols.AddSymbol (name, TCSymbol.NewType (typeData));
                    symbolFound = true;
                    break;
                }
            }

            if (!symbolFound) {
                foreach (var funcKVP in namespaceData.Functions) {
                    if (!funcKVP.Key.Equals (name))
                        continue;

                    isDuplicate = !symbols.AddSymbol (name, TCSymbol.NewFunction (funcKVP.Value.Address));
                    symbolFound = true;
                    break;
                }
            }

            if (!symbolFound)
                Debug.Fail ("This shouldn't be reachable.");

            if (isDuplicate)
                Debug.Fail ("This shouldn't be reachable.");
        }
    }

    private static void HandleAlias (
        ref PassData passData,
        ES_AstTypeAlias alias
    ) {
        var idPool = passData.Env.IdPool;

        var aliasName = alias.AliasName.Text.Span;
        var aliasId = idPool.GetIdentifier (aliasName);

        Debug.Assert (alias.OriginalName is ES_AstTypeDeclaration_TypeReference);
        var origType = GetTypeRef (alias.OriginalName);

        if (!passData.Symbols.AddSymbol (aliasId, TCSymbol.NewType (origType)))
            Debug.Fail ("This shouldn't be reachable.");
    }

    private static void CheckAggregate (
        ref PassData passData,
        ES_FullyQualifiedName typeFQN,
        ES_AstAggregateDefinition typeDef, bool isClass
    ) {
        var idPool = passData.Env.IdPool;
        var irWriter = passData.IRWriter;
        var irTypeVoid = TypeNode (ref passData, passData.Env.TypeVoid);

        using var defStaticConsBody = new StructPooledList<ESIR_Statement> (CL_ClearMode.Auto);

        foreach (var content in typeDef.Contents) {
            switch (content) {
                case ES_AstMemberVarDefinition varDef: {
                    Debug.Assert (varDef.ValueType is not null);

                    var varId = idPool.GetIdentifier (varDef.Name.Text.Span);
                    var varType = UnpackFirstConst (GetTypeRef (varDef.ValueType));
                    var flags = (ES_MemberFlags) 0;

                    if (varType.Type->Flags.HasFlag (ES_TypeFlag.NoNew)) {
                        passData.ErrorList.Add (ES_FrontendErrors.GenNoTypeNew (
                            passData.Env.GetNiceTypeNameString (varType.Type, true), passData.Source, varDef.ValueType.NodeBounds
                        ));
                    }

                    if (varDef.Static)
                        flags |= ES_MemberFlags.Static;

                    ESIR_Expression initValue;
                    if (varDef.InitializationExpression is not null) {
                        if (!isClass && !varDef.Static)
                            passData.ErrorList.Add (new (varDef.Name, ES_FrontendErrors.InstDefValOutsideClass));

                        var defExpr = CheckExpression (ref passData, varDef.InitializationExpression, varType);
                        var compat = EnsureCompat (ref defExpr, varType, ref passData, defExpr.Expr.NodeBounds);

                        if (!defExpr.CompileTimeConst) {
                            passData.ErrorList.Add (new (
                                passData.Source, defExpr.Expr.NodeBounds, ES_FrontendErrors.ConstantExprExpected
                            ));
                        }

                        initValue = defExpr.Value;
                    } else
                        initValue = DefaultValueExpression (TypeNode (ref passData, varType));

                    if (varDef.Static) {
                        var mangledStaticVar = MangleStaticVar (ref passData, typeFQN, varId);
                        passData.IRWriter.AddStaticVar (StaticVariable (TypeNode (ref passData, varType), mangledStaticVar));

                        var staticVarName = MangleStaticVar (ref passData, typeFQN, varId);

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

        var defStaticConsName = MangleDefStaticConstructor (ref passData, typeFQN);
        irWriter.StartFunction (defStaticConsName, irTypeVoid);
        irWriter.AddStatements (defStaticConsBody.Span);
        irWriter.EndFunction (null);
    }

    private static void EnsureInterfaces (
        ref PassData passData,
        ReadOnlySpan<Pointer<ES_InterfaceData>> interfaces, ES_TypeMembers* members
    ) {
        if (interfaces.Length > 0)
            throw new NotImplementedException ("[TODO] Interfaces not implemented yet.");
    }

    private static void CheckStruct (
        ref PassData passData,
        ES_AstStructDefinition structDef, ES_StructData.Builder structBuilder
    ) {
        var structType = structBuilder.StructData;

        CheckAggregate (ref passData, structType->TypeInfo.Name, structDef, false);

        EnsureInterfaces (ref passData, structBuilder.InterfacesList.Span, structBuilder.MembersList);

        using var membersList = new StructPooledList<ESIR_MemberNode> (CL_ClearMode.Auto);
        foreach (var memberAddr in structType->TypeInfo.MembersList.MembersList.Span) {
            var member = memberAddr.Address;
            var memberName = member->Name;

            if (member->MemberType != ES_MemberType.Field)
                continue;

            var field = (ES_MemberData_Variable*) member;

            ESIR_MemberNode irMember;
            if (member->Flags.HasFlag (ES_MemberFlags.Static)) {
                var staticVarName = MangleStaticVar (ref passData, structType->TypeInfo.Name, memberName);
                irMember = StaticField (memberName, staticVarName);
            } else
                irMember = Field (TypeNode (ref passData, field->Type), field->Offset, memberName);

            membersList.Add (irMember);
        }

        passData.IRWriter.AddStruct (Struct (&structType->TypeInfo, List (membersList.Span)));
    }

    private static bool IsNullable (
        ref PassData passData,
        TypeData destType, out TypeData retType
    ) {
        Debug.Assert (destType.Type is not null);

        var typeUnkn = passData.Env.TypeUnknownValue;

        switch (destType.Type->TypeTag) {
            case ES_TypeTag.UNKNOWN:
                retType = destType.WithType (typeUnkn);
                return true;

            case ES_TypeTag.Interface:
            case ES_TypeTag.Reference:
            case ES_TypeTag.Array:
                retType = destType;
                return true;

            default:
                retType = destType.WithType (typeUnkn);
                return false;
        }
    }

    private static ESIR_TypeNode TypeNode (ref PassData passData, ES_TypeInfo* type)
        => ESIR_Factory.TypeNode (StripFirstConst (type, out _));

    private static ESIR_TypeNode TypeNode (ref PassData passData, TypeData type) => TypeNode (ref passData, type.Type);

    private static void GenerateGlobalStaticConstructor (ref PassData passData) {
        var irWriter = passData.IRWriter;

        using var globalStaticConsBody = new StructPooledList<ESIR_Statement> (CL_ClearMode.Auto);

        foreach (var nsKVP in passData.Env.Namespaces) {
            foreach (var typePtr in nsKVP.Value.Types) {
                var type = typePtr.Address;

                switch (type->TypeTag) {
                    case ES_TypeTag.Null:
                    case ES_TypeTag.Void:
                    case ES_TypeTag.Bool:
                    case ES_TypeTag.Int:
                    case ES_TypeTag.Float:
                    case ES_TypeTag.FuncPrototype:
                    case ES_TypeTag.Enum:
                    case ES_TypeTag.Interface:
                    case ES_TypeTag.Reference:
                    case ES_TypeTag.Const:
                    case ES_TypeTag.Immutable:
                    case ES_TypeTag.Array:
                        continue;

                    case ES_TypeTag.Struct:
                    case ES_TypeTag.Class:
                        break;

                    default:
                        throw new NotImplementedException ("Type not implemented yet.");
                }

                var defStaticConsName = MangleDefStaticConstructor (ref passData, type->Name);
                globalStaticConsBody.Add (
                    ExpressionStatement (
                        FunctionCallExpression (defStaticConsName, List<ESIR_ArgumentValue> ())
                    )
                );
            }
        }

        var globalStaticConsName = passData.Env.IdPool.GetIdentifier (ES_Constants.GlobalStaticConstructorName);
        irWriter.StartFunction (globalStaticConsName, TypeNode (ref passData, passData.Env.TypeVoid));
        irWriter.AddStatements (globalStaticConsBody.Span);
        irWriter.EndFunction (null);
    }
}
