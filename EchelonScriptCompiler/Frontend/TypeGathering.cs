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
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Frontend {
    public unsafe partial class CompilerFrontend {
        protected ES_TypeInfo* GetType (SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src, ES_AstTypeDeclaration_TypeName typeName) {
            var idPool = Environment!.IdPool;

            Debug.Assert (typeName.TypeName.Parts.Length > 0);

            if (typeName.Namespace is not null) {
                using var namespaceChars = typeName.Namespace.ToPooledChars ();
                using var nameChars = typeName.TypeName.ToPooledChars ();
                var namespaceId = idPool.GetIdentifier (namespaceChars);
                var nameId = idPool.GetIdentifier (nameChars);

                var type = Environment!.GetFullyQualifiedType (namespaceId, nameId);

                if (type == null) {
                    var err = ES_FrontendErrors.GenCantFindSymbol (typeName.ToString (), src, typeName.NodeBounds);
                    errorList.Add (err);

                    return Environment.TypeUnknownValue;
                }

                return type;
            } else {
                var typeParts = typeName.TypeName.Parts;
                var typeId = idPool.GetIdentifier (typeParts [0].Text.Span);

                var symbol = symbols.GetSymbol (typeId);

                if (symbol.Tag == FrontendSymbolType.None) {
                    var err = ES_FrontendErrors.GenCantFindSymbol (typeParts [0].Text.Span.GetPooledString (), typeParts [0]);
                    errorList.Add (err);

                    return Environment.TypeUnknownValue;
                } else if (symbol.Tag == FrontendSymbolType.Variable) {
                    var err = ES_FrontendErrors.GenVarUsedAsType (typeParts [0].Text.Span.GetPooledString (), typeParts [0]);
                    errorList.Add (err);

                    return Environment.TypeUnknownValue;
                } else if (symbol.Tag == FrontendSymbolType.Function) {
                    var err = ES_FrontendErrors.GenFuncUsedAsType (typeParts [0].Text.Span.GetPooledString (), typeParts [0]);
                    errorList.Add (err);

                    return Environment.TypeUnknownValue;
                } else if (symbol.Tag != FrontendSymbolType.Type)
                    throw new NotImplementedException ("Not implemented?");

                if (typeParts.Length > 1)
                    throw new NotImplementedException ("[TODO] Nested types not implemented yet.");

                return symbol.MatchType ();
            }
        }

        protected ES_TypeInfo* ResolveTypeDeclaration (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src,
            ES_AstTypeDeclaration typeDecl
        ) {
            Debug.Assert (EnvironmentBuilder is not null);

            switch (typeDecl) {
                case ES_AstTypeDeclaration_TypeName typeName: {
                    var type = GetType (symbols, src, typeName);

                    switch (type->AccessModifier) {
                        case ES_AccessModifier.Public: break;

                        case ES_AccessModifier.Internal:
                            if (!type->SourceUnit.Equals (transUnit.Name)) {
                                using var symbolName = PooledArray<char>.GetArray (typeName.GetStringLength ());
                                typeName.ToString (symbolName);

                                var err = ES_FrontendErrors.GenInaccessibleProtectionLevel (
                                    symbolName.Span.GetPooledString (), src, typeName.NodeBounds
                                );
                                errorList.Add (err);
                            }
                            break;

                        default:
                            throw new NotImplementedException ("Access modifier not implemented yet.");
                    }

                    return type;
                }

                case ES_AstTypeDeclaration_TypeReference typeRef:
                    return typeRef.Reference;

                case ES_AstTypeDeclaration_Array arrayDecl: {
                    var elemType = ResolveTypeDeclaration (ref transUnit, symbols, src, arrayDecl.ElementType);
                    return EnvironmentBuilder.CreateArrayType (elemType, arrayDecl.Dimensions);
                }

                case ES_AstTypeDeclaration_Basic basicDecl: {
                    var innerType = ResolveTypeDeclaration (ref transUnit, symbols, src, basicDecl.Inner!);

                    switch (basicDecl.Type) {
                        case ES_AstTypeDeclaration_Basic.DeclType.Const:
                            return EnvironmentBuilder.CreateConstType (innerType);

                        case ES_AstTypeDeclaration_Basic.DeclType.Immutable:
                            return EnvironmentBuilder.CreateImmutableType (innerType);

                        case ES_AstTypeDeclaration_Basic.DeclType.Nullable:
                            return EnvironmentBuilder.CreateNullableType (innerType);

                        case ES_AstTypeDeclaration_Basic.DeclType.Reference:
                            return EnvironmentBuilder.CreateReferenceType (innerType);

                        default:
                            throw new NotImplementedException ("Basic declaration type not implemented.");
                    }
                }

                default:
                    throw new NotImplementedException ("Declaration type not implemented.");
            }
        }

        protected ES_AstTypeDeclaration_TypeReference GenerateASTTypeRef (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src,
            ES_AstTypeDeclaration typeDecl
        ) {
            Debug.Assert (typeDecl is not null);

            if (typeDecl is ES_AstTypeDeclaration_TypeReference)
                return (typeDecl as ES_AstTypeDeclaration_TypeReference)!;

            var type = ResolveTypeDeclaration (ref transUnit, symbols, src, typeDecl);

            if (type == null)
                type = Environment!.TypeUnknownValue;

            return new ES_AstTypeDeclaration_TypeReference (typeDecl, type);
        }

        protected ES_NamespaceData? GetNamespace (ReadOnlySpan<char> src, ES_AstNodeBounds nodeBounds, ReadOnlySpan<char> namespaceStr) {
            var namespaceName = Environment!.IdPool.GetIdentifier (namespaceStr);

            if (!Environment!.Namespaces.TryGetValue (namespaceName, out var namespaceData)) {
                var err = ES_FrontendErrors.GenNamespaceDoesntExist (
                    namespaceStr.GetPooledString (),
                    src,
                    nodeBounds
                );
                errorList.Add (err);
                return null;
            }

            return namespaceData;
        }

        protected void ImportNamespaceSymbols (SymbolStack<FrontendSymbol> symbols, ES_NamespaceData namespaceData) {
            foreach (var type in namespaceData.Types)
                symbols.AddSymbol (type.Address->Name.TypeName, NewSymbolType (type));
            foreach (var funcKVP in namespaceData.Functions)
                symbols.AddSymbol (funcKVP.Key, NewSymbolFunction (funcKVP.Value));
        }

        protected void AST_HandleImport (SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src, ES_AstImportStatement import) {
            using var nmNameString = import.NamespaceName.ToPooledChars ();
            var namespaceData = GetNamespace (src, import.NamespaceName.NodeBounds, nmNameString);

            if (namespaceData is null)
                return;

            if (import.ImportedNames is null || import.ImportedNames.Length == 0) {
                foreach (var type in namespaceData.Types) {
                    if (!symbols.AddSymbol (type.Address->Name.TypeName, NewSymbolVariable (type))) {
                        var err = ES_FrontendErrors.GenDuplicateSymbolDef (
                            type.Address->Name.TypeNameString,
                            src, import.NamespaceName.NodeBounds
                        );
                        errorList.Add (err);
                    }
                }
                foreach (var funcKVP in namespaceData.Functions) {
                    if (!symbols.AddSymbol (funcKVP.Key, NewSymbolFunction (funcKVP.Value))) {
                        var err = ES_FrontendErrors.GenDuplicateSymbolDef (
                            funcKVP.Value.Address->Name.TypeNameString,
                            src, import.NamespaceName.NodeBounds
                        );
                        errorList.Add (err);
                    }
                }
            } else {
                foreach (var importTk in import.ImportedNames) {
                    var name = Environment!.IdPool.GetIdentifier (importTk.Text.Span);

                    bool symbolFound = false;
                    bool isDuplicate = false;

                    if (!symbolFound) {
                        foreach (var typeData in namespaceData.Types) {
                            if (typeData.Address->Name.TypeName.Equals (name)) {
                                isDuplicate = !symbols.AddSymbol (name, NewSymbolType (typeData));
                                symbolFound = true;
                                break;
                            }
                        }
                    }

                    if (!symbolFound) {
                        foreach (var funcKVP in namespaceData.Functions) {
                            if (funcKVP.Key.Equals (name)) {
                                isDuplicate = !symbols.AddSymbol (name, NewSymbolFunction (funcKVP.Value.Address));
                                symbolFound = true;
                                break;
                            }
                        }
                    }

                    if (!symbolFound)
                        errorList.Add (ES_FrontendErrors.GenCantFindSymbol (importTk.Text.Span.GetPooledString (), importTk));

                    if (isDuplicate) {
                        errorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (
                            importTk.Text.Span.GetPooledString (),
                            importTk
                        ));
                    }
                }
            }
        }

        protected void AST_HandleAlias (ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src, ES_AstTypeAlias alias) {
            var aliasName = alias.AliasName.Text.Span;
            var aliasId = Environment!.IdPool.GetIdentifier (aliasName);

            if (alias.OriginalName is ES_AstTypeDeclaration_TypeName typeName) {
                ES_FunctionData* func = null;

                using var origNameChars = typeName.TypeName.ToPooledChars ();
                var origName = Environment.IdPool.GetIdentifier (origNameChars);
                origNameChars.Dispose ();

                if (typeName.Namespace is not null) {
                    using var namespaceName = typeName.Namespace!.ToPooledChars ();
                    var namespaceData = GetNamespace (src, typeName.Namespace.NodeBounds, namespaceName);

                    if (namespaceData is not null && namespaceData.Functions.TryGetValue (origName, out var newFunc))
                        func = newFunc;
                } else {
                    var symbol = symbols.GetSymbol (origName);

                    if (symbol.Tag == FrontendSymbolType.Function)
                        func = symbol.MatchFunction ();
                }

                if (func != null) {
                    if (!symbols.AddSymbol (aliasId, NewSymbolFunction (func)))
                        errorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (aliasName.GetPooledString (), alias.AliasName));

                    return;
                }
            }

            alias.OriginalName = GenerateASTTypeRef (ref transUnit, symbols, src, alias.OriginalName!);

            var origType = GetTypeRef (alias.OriginalName);

            if (!symbols.AddSymbol (aliasId, NewSymbolType (origType)))
                errorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (aliasName.GetPooledString (), alias.AliasName));
        }

        protected void GatherGlobalImports (ref TranslationUnitData transUnit) {
            var idPool = Environment!.IdPool;
            var namespaces = Environment.Namespaces;
            var globalTypesList = EnvironmentBuilder!.GetOrCreateNamespace (Environment.GlobalTypesNamespace).NamespaceData.Types;

            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                var ast = astUnit.Ast;
                var symbols = astUnit.Symbols;
                var src = ast.Source.Span;

                // Add built-in symbols
                symbols.Push ();
                foreach (var type in globalTypesList)
                    symbols.AddSymbol (type.Address->Name.TypeName, NewSymbolType (type));

                // Add imported symbols
                symbols.Push ();

                foreach (var import in ast.ImportStatements)
                    AST_HandleImport (symbols, src, import);

                foreach (var alias in ast.TypeAliases)
                    AST_HandleAlias (ref transUnit, symbols, src, alias);
            }
        }

        protected void GatherTypes (ref TranslationUnitData transUnit) {
            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                foreach (var nm in astUnit.Ast.Namespaces) {
                    ArrayPointer<byte> namespaceName;
                    using (var nameArr = nm.NamespaceName.ToPooledChars ())
                        namespaceName = Environment!.IdPool.GetIdentifier (nameArr);

                    var namespaceBuilder = EnvironmentBuilder!.GetOrCreateNamespace (namespaceName);
                    var namespaceData = namespaceBuilder.NamespaceData;

                    astUnit.Symbols.Push ();
                    ImportNamespaceSymbols (astUnit.Symbols, namespaceData);

                    foreach (var type in nm.Contents) {
                        switch (type) {
                            case ES_AstClassDefinition classDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (classDef.Name.Text.Span);
                                var classBuilder = namespaceBuilder.GetClass (typeName);
                                Debug.Assert (classBuilder is not null);

                                GatherTypes_Class (ref transUnit, ref astUnit, classDef, classBuilder);
                                break;
                            }

                            case ES_AstStructDefinition structDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (structDef.Name.Text.Span);
                                var structBuilder = namespaceBuilder.GetStruct (typeName);
                                Debug.Assert (structBuilder is not null);

                                GatherTypes_Struct (ref transUnit, ref astUnit, structDef, structBuilder);
                                break;
                            }

                            case ES_AstEnumDefinition enumDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (enumDef.Name.Text.Span);
                                var enumBuilder = namespaceBuilder.GetEnum (typeName);
                                Debug.Assert (enumBuilder is not null);

                                GatherTypes_Enum (ref transUnit, ref astUnit, enumDef, enumBuilder);
                                break;
                            }

                            case ES_AstFunctionDefinition funcDef:
                                GatherTypes_Function (ref transUnit, ref astUnit, funcDef);
                                break;

                            default:
                                throw new NotImplementedException ("Node type not implemented.");
                        }
                    }

                    astUnit.Symbols.Pop ();
                }
            }
        }

        protected void GatherTypes_InheritanceList (
            ref TranslationUnitData transUnit, ref AstUnitData astUnit,
            ReadOnlySpan<ES_AstTypeDeclaration_TypeName> inheritanceList, bool isClass,
            out ArrayPointer<Pointer<ES_InterfaceData>> interfacesListMem, out ES_ClassData* baseClass
        ) {
            baseClass = null;

            var srcCode = astUnit.Ast.Source;
            var idPool = Environment!.IdPool;
            var symbols = astUnit.Symbols;

            using var interfacesList = new StructPooledList<Pointer<ES_InterfaceData>> (CL_ClearMode.Auto);

            foreach (var inheritId in inheritanceList) {
                var type = ResolveTypeDeclaration (ref transUnit, symbols, srcCode.Span, inheritId);

                if (type == null)
                    continue;

                var typeTag = type->TypeTag;
                if (isClass && typeTag == ES_TypeTag.Class) {
                    if (baseClass is not null) {
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
                        var interfaceFqnStr = type->Name.GetNameAsTypeString ();
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

            if (interfacesList.Count > 0) {
                interfacesListMem = EnvironmentBuilder!.MemoryManager.GetArray<Pointer<ES_InterfaceData>> (interfacesList.Count);
                interfacesList.CopyTo (interfacesListMem.Span);
            } else
                interfacesListMem = ArrayPointer<Pointer<ES_InterfaceData>>.Null;
        }

        protected unsafe void ResolveAggregate (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_TypeMembers.Builder membersBuilder, ES_AstAggregateDefinition typeDef, bool isClass) {
            static bool IsNameUsed (ArrayPointer<byte> id, ReadOnlySpan<ES_MemberData_Variable> vars, ReadOnlySpan<ES_MemberData_Function> funcs) {
                foreach (var memberVar in vars) {
                    if (memberVar.Info.Name.Equals (id))
                        return true;
                }

                foreach (var memberFunc in funcs) {
                    if (memberFunc.Info.Name.Equals (id))
                        return true;
                }

                return false;
            }

            static ES_MemberData* FindMember (ArrayPointer<byte> id, ReadOnlySpan<Pointer<ES_MemberData>> members) {
                foreach (var member in members) {
                    if (member.Address->Name.Equals (id))
                        return member.Address;
                }

                Debug.Fail ("This should never be reached.");
                return null;
            }

            Debug.Assert (Environment is not null);
            Debug.Assert (EnvironmentBuilder is not null);

            var srcCode = astUnit.Ast.Source.Span;
            var idPool = Environment.IdPool;
            var symbols = astUnit.Symbols;

            using var varsList = new StructPooledList<ES_MemberData_Variable> (CL_ClearMode.Auto);
            using var funcsList = new StructPooledList<ES_MemberData_Function> (CL_ClearMode.Auto);

            foreach (var content in typeDef.Contents) {
                switch (content) {
                    case ES_AstMemberVarDefinition varDef: {
                        Debug.Assert (varDef.ValueType is not null);

                        varDef.ValueType = GenerateASTTypeRef (ref transUnit, symbols, srcCode, varDef.ValueType);
                        if (varDef.InitializationExpression is not null)
                            GatherTypes_Expression (ref transUnit, symbols, srcCode, varDef.InitializationExpression);

                        var varId = idPool.GetIdentifier (varDef.Name.Text.Span);
                        var varType = GetTypeRef (varDef.ValueType);
                        var flags = (ES_MemberFlags) 0;

                        if (varDef.Static)
                            flags |= ES_MemberFlags.Static;

                        if (IsNameUsed (varId, varsList.Span, funcsList.Span)) {
                            errorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (
                                varDef.Name.Text.Span.GetPooledString (), varDef.Name
                            ));
                        }

                        varsList.Add (new ES_MemberData_Variable (varId, transUnit.Name, varDef.AccessModifier, flags, -1, varType));

                        break;
                    }

                    case ES_AstFunctionDefinition funcDef:
                        throw new NotImplementedException ("[TODO] Member functions not implemented yet.");

                    default:
                        throw new NotImplementedException ("Node type not implemented.");
                }
            }

            EnvironmentBuilder.GenerateMembersList (membersBuilder, varsList.Span, funcsList.Span);
            var membersSpan = membersBuilder.MembersList.Span;

            // Add the AST nodes to the Pointer->AST map.
            foreach (var content in typeDef.Contents) {
                switch (content) {
                    case ES_AstMemberVarDefinition varDef: {
                        var varPtr = FindMember (idPool.GetIdentifier (varDef.Name.Text.Span), membersSpan);
                        EnvironmentBuilder.PointerAstMap.Add ((IntPtr) varPtr, varDef);
                        break;
                    }

                    case ES_AstFunctionDefinition funcDef: {
                        var funcPtr = FindMember (idPool.GetIdentifier (funcDef.Name.Text.Span), membersSpan);
                        EnvironmentBuilder.PointerAstMap.Add ((IntPtr) funcPtr, funcDef);
                        break;
                    }

                    default:
                        throw new NotImplementedException ("Node type not implemented.");
                }
            }
        }

        protected void GatherTypes_Class (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstClassDefinition classDef, ES_ClassData.Builder builder) {
            if (builder.ClassData->TypeInfo.RuntimeSize > -1)
                return;

            var srcCode = astUnit.Ast.Source;
            var idPool = Environment!.IdPool;
            ref var symbols = ref astUnit.Symbols;

            GatherTypes_InheritanceList (ref transUnit, ref astUnit, classDef.InheritanceList, true, out var interfacesList, out var baseClass);
            builder.BaseClass = baseClass;
            builder.InterfacesList = interfacesList;

            throw new NotImplementedException ("[TODO] Classes not implemented yet.");
        }

        protected void GatherTypes_Struct (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstStructDefinition structDef, ES_StructData.Builder builder) {
            GatherTypes_InheritanceList (ref transUnit, ref astUnit, structDef.InterfacesList, false, out var interfacesList, out _);
            builder.InterfacesList = interfacesList;

            ResolveAggregate (ref transUnit, ref astUnit, builder.MembersListBuilder, structDef, false);
        }

        protected void GatherTypes_Enum (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstEnumDefinition enumDef, ES_EnumData.Builder builder) {
            throw new NotImplementedException ("[TODO] Enums not implemented yet.");
        }

        protected void GatherTypes_Function (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstFunctionDefinition funcDef) {
            var symbols = astUnit.Symbols;
            var unitSrc = astUnit.Ast.Source.Span;

            // Guarantee some conditions.
            // These should have been validated and resolved by the function creation pass.
            Debug.Assert (funcDef.ReturnType is ES_AstTypeDeclaration_TypeReference);
            Debug.Assert (funcDef.ArgumentsList is not null);

            Debug.Assert (funcDef.Statement is not null);
            Debug.Assert (funcDef.Statement.Endpoint is null);
            // Expression-body functions must contain exactly a single expression statement.
            Debug.Assert (
                (funcDef.ExpressionBody && funcDef.Statement is ES_AstExpressionStatement) ||
                !funcDef.ExpressionBody
            );

            symbols.Push ();

            foreach (var arg in funcDef.ArgumentsList) {
                if (arg.DefaultExpression is null)
                    continue;

                GatherTypes_Expression (ref transUnit, symbols, unitSrc, arg.DefaultExpression);
            }

            GatherTypes_Statement (ref transUnit, symbols, unitSrc, funcDef.Statement);

            symbols.Pop ();
        }

        protected void GatherTypes_Statement (ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src, ES_AstStatement stmt) {
            Debug.Assert (stmt is not null);

            switch (stmt) {
                case ES_AstEmptyStatement:
                case ES_AstLabeledStatement:
                    break;

                case ES_AstBlockStatement blockStmt: {
                    symbols.Push ();

                    ES_AstStatement? curStatement = blockStmt.Statement;
                    while (curStatement is not null) {
                        GatherTypes_Statement (ref transUnit, symbols, src, curStatement);
                        curStatement = curStatement.Endpoint;
                    }

                    symbols.Pop ();
                    break;
                }

                #region Symbol definition

                case ES_AstImportStatement importStmt: {
                    AST_HandleImport (symbols, src, importStmt);
                    break;
                }

                case ES_AstTypeAlias aliasStmt: {
                    AST_HandleAlias (ref transUnit, symbols, src, aliasStmt);
                    break;
                }

                case ES_AstLocalVarDefinition varDef: {
                    if (varDef.ValueType is not null)
                        varDef.ValueType = GenerateASTTypeRef (ref transUnit, symbols, src, varDef.ValueType);

                    foreach (var variable in varDef.Variables) {
                        if (variable.InitializationExpression is not null)
                            GatherTypes_Expression (ref transUnit, symbols, src, variable.InitializationExpression);
                    }

                    break;
                }

                #endregion

                #region Jumps

                case ES_AstConditionalStatement condStmt:
                    GatherTypes_Expression (ref transUnit, symbols, src, condStmt.ConditionExpression);
                    GatherTypes_Statement (ref transUnit, symbols, src, condStmt.ThenStatement);
                    if (condStmt.ElseStatement is not null)
                        GatherTypes_Statement (ref transUnit, symbols, src, condStmt.ElseStatement);
                    break;

                case ES_AstSwitchStatement switchStmt: {
                    GatherTypes_Expression (ref transUnit, symbols, src, switchStmt.ValueExpression);

                    foreach (var section in switchStmt.Sections) {
                        foreach (var expr in section.Expressions) {
                            if (expr is not null)
                                GatherTypes_Expression (ref transUnit, symbols, src, expr);
                        }

                        ES_AstStatement? curStatement = section.StatementsBlock;
                        while (curStatement is not null) {
                            GatherTypes_Statement (ref transUnit, symbols, src, curStatement);
                            curStatement = curStatement.Endpoint;
                        }
                    }

                    break;
                }

                case ES_AstGotoLabelStatement:
                    break;

                case ES_AstGotoCaseStatement gotoCaseStmt:
                    if (gotoCaseStmt.CaseExpression is not null)
                        GatherTypes_Expression (ref transUnit, symbols, src, gotoCaseStmt.CaseExpression);
                    break;

                case ES_AstReturnStatement retStmt:
                    if (retStmt.ReturnExpression is not null)
                        GatherTypes_Expression (ref transUnit, symbols, src, retStmt.ReturnExpression);
                    break;

                #endregion

                #region Loops

                case ES_AstLoopStatement loopStmt: {
                    symbols.Push ();

                    if (loopStmt.InitializationStatement is not null)
                        GatherTypes_Statement (ref transUnit, symbols, src, loopStmt.InitializationStatement);

                    if (loopStmt.ConditionExpression is not null)
                        GatherTypes_Expression (ref transUnit, symbols, src, loopStmt.ConditionExpression);

                    if (loopStmt.IterationExpressions is not null) {
                        foreach (var expr in loopStmt.IterationExpressions) {
                            if (expr is not null)
                                GatherTypes_Expression (ref transUnit, symbols, src, expr);
                        }
                    }

                    Debug.Assert (loopStmt.LoopBody is not null);
                    Debug.Assert (loopStmt.LoopBody.Endpoint is null);

                    GatherTypes_Statement (ref transUnit, symbols, src, loopStmt.LoopBody);

                    symbols.Pop ();

                    break;
                }

                #endregion

                case ES_AstExpressionStatement exprStmt:
                    GatherTypes_Expression (ref transUnit, symbols, src, exprStmt.Expression);
                    break;

                case ES_AstExpressionListStatement exprListStmt: {
                    foreach (var expr in exprListStmt.Expressions)
                        GatherTypes_Expression (ref transUnit, symbols, src, expr);
                    break;
                }

                default:
                    throw new NotImplementedException ("Statement type not implemented.");
            }
        }

        protected void GatherTypes_Expression (ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, ReadOnlySpan<char> src, ES_AstExpression expr) {
            Debug.Assert (expr is not null);

            switch (expr) {
                case ES_AstParenthesisExpression parenExpr:
                    GatherTypes_Expression (ref transUnit, symbols, src, parenExpr.Inner);
                    break;

                #region Primary expressions

                case ES_AstFunctionCallExpression funcCallExpr: {
                    GatherTypes_Expression (ref transUnit, symbols, src, funcCallExpr.FunctionExpression);
                    foreach (var args in funcCallExpr.Arguments)
                        GatherTypes_Expression (ref transUnit, symbols, src, args.ValueExpression);
                    break;
                }

                case ES_AstIndexingExpression indexExpr: {
                    GatherTypes_Expression (ref transUnit, symbols, src, indexExpr.IndexedExpression);
                    foreach (var rank in indexExpr.RankExpressions) {
                        if (rank is not null)
                            GatherTypes_Expression (ref transUnit, symbols, src, rank);
                    }
                    break;
                }

                case ES_AstNewObjectExpression newObjExpr: {
                    if (newObjExpr.TypeDeclaration is not null)
                        newObjExpr.TypeDeclaration = GenerateASTTypeRef (ref transUnit, symbols, src, newObjExpr.TypeDeclaration);

                    foreach (var args in newObjExpr.Arguments)
                        GatherTypes_Expression (ref transUnit, symbols, src, args.ValueExpression);

                    break;
                }

                case ES_AstNewArrayExpression newArrayExpr: {
                    if (newArrayExpr.ElementType is not null)
                        newArrayExpr.ElementType = GenerateASTTypeRef (ref transUnit, symbols, src, newArrayExpr.ElementType);

                    foreach (var rank in newArrayExpr.Ranks) {
                        Debug.Assert (rank is not null);
                        GatherTypes_Expression (ref transUnit, symbols, src, rank);
                    }

                    break;
                }

                // We don't need to do anything for these
                case ES_AstIntegerLiteralExpression:
                case ES_AstBooleanLiteralExpression:
                case ES_AstFloatLiteralExpression:
                case ES_AstStringLiteralExpression:
                case ES_AstCharLiteralExpression:
                case ES_AstNullLiteralExpression:
                case ES_AstNameExpression:
                    break;

                case ES_AstMemberAccessExpression memberAccessExpr: {
                    GatherTypes_Expression (ref transUnit, symbols, src, memberAccessExpr.Parent);
                    break;
                }

                #endregion

                case ES_AstIncDecExpression incDecExpr:
                    GatherTypes_Expression (ref transUnit, symbols, src, incDecExpr.Inner);
                    break;

                #region Unary expressions

                case ES_AstSimpleUnaryExpression unaryExpr:
                    GatherTypes_Expression (ref transUnit, symbols, src, unaryExpr.Inner);
                    break;

                case ES_AstCastExpression castExpr:
                    if (castExpr.DestinationType is not null)
                        castExpr.DestinationType = GenerateASTTypeRef (ref transUnit, symbols, src, castExpr.DestinationType);
                    GatherTypes_Expression (ref transUnit, symbols, src, castExpr.InnerExpression);
                    break;

                #endregion

                case ES_AstSimpleBinaryExpression simpleBinaryExpr:
                    GatherTypes_Expression (ref transUnit, symbols, src, simpleBinaryExpr.Left);
                    GatherTypes_Expression (ref transUnit, symbols, src, simpleBinaryExpr.Right);
                    break;

                case ES_AstConditionalExpression condExpr:
                    GatherTypes_Expression (ref transUnit, symbols, src, condExpr.Condition);
                    GatherTypes_Expression (ref transUnit, symbols, src, condExpr.Then);
                    GatherTypes_Expression (ref transUnit, symbols, src, condExpr.Else);
                    break;

                default:
                    throw new NotImplementedException ("Expression type not implemented.");
            }
        }
    }
}
