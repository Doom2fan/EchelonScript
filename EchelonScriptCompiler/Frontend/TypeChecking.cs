﻿/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Text;
using ChronosLib.Pooled;
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Frontend {
    public unsafe partial class CompilerFrontend {
        public struct ExpressionData {
            public ES_AstExpression Expr;
            public ES_TypeInfo* Type;
            public ES_TypeInfo* TypeInfo;
            public ES_FunctionData* Function;
            public bool Constant;
            public bool Addressable;

            public bool TypeIsPointer (ES_TypeInfo* baseType = null) {
                if (Type is null)
                    return false;

                if (Type->TypeTag != ES_TypeTag.Reference)
                    return false;

                var refType = (ES_ReferenceData*) Type;

                return (baseType is null || refType->PointedType == baseType);
            }
        }

        public struct StatementData {
            public bool AlwaysReturns;
        }

        protected ES_TypeInfo* GetTypeRef (ES_AstTypeDeclaration? typeDecl) {
            Debug.Assert (typeDecl is not null);

            var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;
            Debug.Assert (typeRef is not null);

            return typeRef.Reference;
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
                    ImportNamespaceSymbols (astUnit.Symbols, namespaceData);

                    foreach (var type in nm.Contents) {
                        switch (type) {
                            case ES_AstClassDefinition classDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (classDef.Name.Text.Span);
                                var classBuilder = namespaceBuilder.GetClass (typeName);
                                Debug.Assert (classBuilder is not null);

                                //CheckTypes_Class (ref transUnit, ref astUnit, classDef, classBuilder);
                                throw new NotImplementedException ("[TODO] Classes not implemented yet.");
                            }

                            case ES_AstStructDefinition structDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (structDef.Name.Text.Span);
                                var structBuilder = namespaceBuilder.GetStruct (typeName);
                                Debug.Assert (structBuilder is not null);

                                CheckTypes_Struct (ref transUnit, ref astUnit, structDef, structBuilder);
                                break;
                            }

                            case ES_AstEnumDefinition enumDef: {
                                var typeName = Environment!.IdPool.GetIdentifier (enumDef.Name.Text.Span);
                                var enumBuilder = namespaceBuilder.GetEnum (typeName);
                                Debug.Assert (enumBuilder is not null);

                                //CheckTypes_Enum (ref transUnit, ref astUnit, enumDef, enumBuilder);
                                throw new NotImplementedException ("[TODO] Enums not implemented yet.");
                            }

                            case ES_AstFunctionDefinition funcDef:
                                CheckTypes_Function (ref transUnit, ref astUnit, funcDef);
                                break;

                            default:
                                throw new NotImplementedException ("Node type not implemented.");
                        }
                    }

                    astUnit.Symbols.Pop ();
                }
            }
        }

        protected void CheckTypes_Aggregate (
            ref TranslationUnitData transUnit, ref AstUnitData astUnit,
            ES_TypeMembers.Builder membersBuilder, ES_AstAggregateDefinition typeDef, bool isClass
        ) {
            var srcCode = astUnit.SourceData;
            var idPool = Environment!.IdPool;
            var symbols = astUnit.Symbols;

            foreach (var content in typeDef.Contents) {
                switch (content) {
                    case ES_AstMemberVarDefinition varDef: {
                        Debug.Assert (varDef.ValueType is not null);

                        var varId = idPool.GetIdentifier (varDef.Name.Text.Span);
                        var varType = GetTypeRef (varDef.ValueType);
                        var flags = (ES_MemberFlags) 0;

                        if (varType->Flags.HasFlag (ES_TypeFlag.NoNew)) {
                            errorList.Add (ES_FrontendErrors.GenNoTypeNew (
                                varType->Name.GetNameAsTypeString (), srcCode, varDef.ValueType.NodeBounds
                            ));
                        }

                        if (varDef.Static)
                            flags |= ES_MemberFlags.Static;

                        if (varDef.InitializationExpression is not null) {
                            if (!isClass && !varDef.Static)
                                errorList.Add (new EchelonScriptErrorMessage (varDef.Name, ES_FrontendErrors.InstDefValOutsideClass));

                            var defExpr = CheckTypes_Expression (ref transUnit, symbols, srcCode, varDef.InitializationExpression, varType);
                            bool compat = CheckTypes_EnsureCompat (varType, defExpr.Type, srcCode, defExpr.Expr.NodeBounds, out bool castIsConstant);

                            if (!defExpr.Constant || (compat && !castIsConstant)) {
                                errorList.Add (new EchelonScriptErrorMessage (
                                    srcCode, defExpr.Expr.NodeBounds, ES_FrontendErrors.ConstantExprExpected
                                ));
                            }
                        }

                        break;
                    }

                    case ES_AstFunctionDefinition funcDef:
                        throw new NotImplementedException ("[TODO] Member functions not implemented yet.");

                    default:
                        throw new NotImplementedException ("Node type not implemented.");
                }
            }
        }

        protected void CheckTypes_EnsureInterfaces (
            ref TranslationUnitData transUnit, ref AstUnitData astUnit,
            ReadOnlySpan<Pointer<ES_InterfaceData>> interfaces, ES_TypeMembers* members
        ) {
            if (interfaces.Length > 0)
                throw new NotImplementedException ("[TODO] Interfaces not implemented yet.");
        }

        protected void CheckTypes_Struct (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstStructDefinition structDef, ES_StructData.Builder structBuilder) {
            CheckTypes_Aggregate (ref transUnit, ref astUnit, structBuilder.MembersListBuilder, structDef, false);

            CheckTypes_EnsureInterfaces (ref transUnit, ref astUnit, structBuilder.InterfacesList.Span, structBuilder.MembersList);
        }

        protected void CheckTypes_Function (ref TranslationUnitData transUnit, ref AstUnitData astUnit, ES_AstFunctionDefinition funcDef) {
            var idPool = Environment!.IdPool;
            var unitSrc = astUnit.SourceData;
            var symbols = astUnit.Symbols;

            symbols.Push ();

            var retType = GetTypeRef (funcDef.ReturnType);

            Debug.Assert (retType is not null);

            foreach (var arg in funcDef.ArgumentsList) {
                var argName = idPool.GetIdentifier (arg.Name.Text.Span);
                var argValType = GetTypeRef (arg.ValueType);

                var flags = (FrontendSymbolFlags) 0;

                switch (arg.ArgType) {
                    case ES_ArgumentType.Normal:
                        break;

                    case ES_ArgumentType.Ref:
                        flags |= FrontendSymbolFlags.RefVar;
                        break;

                    case ES_ArgumentType.In:
                    case ES_ArgumentType.Out:
                        throw new NotImplementedException ("[TODO] Argument type not implemented yet.");

                    default:
                        throw new NotImplementedException ("Argument type not implemented yet.");
                }

                symbols.AddSymbol (argName, NewSymbolVariable (argValType, flags));

                if (arg.DefaultExpression is not null) {
                    var argDefExpr = CheckTypes_Expression (ref transUnit, symbols, unitSrc, arg.DefaultExpression, argValType);
                    CheckTypes_EnsureCompat (argValType, argDefExpr.Type, unitSrc, arg.DefaultExpression.NodeBounds, out _);
                }
            }

            Debug.Assert (funcDef.Statement is not null);
            Debug.Assert (funcDef.Statement.Endpoint is null);
            if (funcDef.ExpressionBody) {
                Debug.Assert (funcDef.Statement is ES_AstExpressionStatement);

                var exprExpType = retType->TypeTag != ES_TypeTag.Void ? retType : Environment.TypeUnknownValue;
                var exprStmt = (funcDef.Statement as ES_AstExpressionStatement)!;
                var exprVal = CheckTypes_Expression (ref transUnit, symbols, unitSrc, exprStmt.Expression, exprExpType);

                if (retType->TypeTag != ES_TypeTag.Void) {
                    if (!CheckTypes_EnsureCompat (retType, exprVal.Type, unitSrc, exprVal.Expr.NodeBounds, out _))
                        errorList.Add (new EchelonScriptErrorMessage (funcDef.Name, ES_FrontendErrors.MissingReturnStatement));
                }
            } else {
                var stmtData = CheckTypes_Statement (ref transUnit, symbols, unitSrc, retType, funcDef.Statement);

                if (!stmtData.AlwaysReturns && retType->TypeTag != ES_TypeTag.Void)
                    errorList.Add (new EchelonScriptErrorMessage (funcDef.Name, ES_FrontendErrors.MissingReturnStatement));
            }

            symbols.Pop ();
        }

        protected bool CheckTypes_MustBeCompat (ES_TypeInfo* destType, ES_TypeInfo* givenType, out bool isConstant) {
            // We don't need to do any checks here if they're *literally* the same type.
            if (destType == givenType) {
                isConstant = true;
                return true;
            }

            bool cantConvert;

            if (destType->TypeTag == ES_TypeTag.UNKNOWN || givenType->TypeTag == ES_TypeTag.UNKNOWN) {
                isConstant = false;
                return true;
            } else if (givenType->TypeTag == ES_TypeTag.Null) {
                cantConvert = !CheckTypes_IsNullable (destType, out _);
                isConstant = true;
            } else if (destType->TypeTag == ES_TypeTag.Int && givenType->TypeTag == ES_TypeTag.Int) {
                var destIntType = (ES_IntTypeData*) destType;
                var givenIntType = (ES_IntTypeData*) givenType;

                if (givenIntType->Unsigned == destIntType->Unsigned && givenIntType->IntSize <= destIntType->IntSize)
                    cantConvert = false;
                else
                    cantConvert = true;

                isConstant = true;
            } else {
                isConstant = false;
                cantConvert = true;
            }

            return !cantConvert;
        }

        protected bool CheckTypes_ExplicitCast (ES_TypeInfo* castType, ES_TypeInfo* exprType, out bool castRedundant, out bool isConstant) {
            if (CheckTypes_MustBeCompat (castType, exprType, out isConstant)) {
                castRedundant = true;
                return true;
            }

            if (castType->TypeTag == ES_TypeTag.Int && exprType->TypeTag == ES_TypeTag.Int) {
                var castIntType = (ES_IntTypeData*) castType;
                var exprIntType = (ES_IntTypeData*) exprType;

                castRedundant = (
                    castIntType->IntSize == exprIntType->IntSize &&
                    castIntType->Unsigned == exprIntType->Unsigned
                );

                isConstant = true;
                return true;
            } else if (castType->TypeTag == ES_TypeTag.Float && exprType->TypeTag == ES_TypeTag.Int) {
                castRedundant = false;
                isConstant = true;
                return true;
            } else if (castType->TypeTag == ES_TypeTag.Int && exprType->TypeTag == ES_TypeTag.Float) {
                castRedundant = false;
                isConstant = true;
                return true;
            } else if (castType->TypeTag == ES_TypeTag.Float && exprType->TypeTag == ES_TypeTag.Float) {
                var castFloatType = (ES_FloatTypeData*) castType;
                var exprFloatType = (ES_FloatTypeData*) exprType;

                castRedundant = castFloatType->FloatSize == exprFloatType->FloatSize;
                isConstant = true;
                return true;
            }

            isConstant = false;
            castRedundant = false;
            return false;
        }

        protected bool CheckTypes_EnsureCompat (ES_TypeInfo* destType, ES_TypeInfo* givenType, SourceData src, ES_AstNodeBounds bounds, out bool isConstant) {
            if (givenType->TypeTag == ES_TypeTag.Null) {
                if (!CheckTypes_IsNullable (destType, out var retType)) {
                    errorList.Add (ES_FrontendErrors.GenTypeNotNullable (
                        destType->Name.GetNameAsTypeString (), src, bounds
                    ));

                    isConstant = false;
                    return false;
                }

                isConstant = false;
                return true;
            } else if (!CheckTypes_MustBeCompat (destType, givenType, out isConstant)) {
                if (CheckTypes_ExplicitCast (destType, givenType, out _, out _)) {
                    errorList.Add (ES_FrontendErrors.GenNoImplicitCast (
                        destType->Name.GetNameAsTypeString (), givenType->Name.GetNameAsTypeString (),
                        src, bounds
                    ));
                } else {
                    errorList.Add (ES_FrontendErrors.GenNoCast (
                        destType->Name.GetNameAsTypeString (), givenType->Name.GetNameAsTypeString (),
                        src, bounds
                    ));
                }

                return false;
            }

            return true;
        }

        protected bool CheckTypes_IsNullable (ES_TypeInfo* destType, out ES_TypeInfo* retType) {
            Debug.Assert (Environment is not null);
            Debug.Assert (destType is not null);

            var typeUnkn = Environment.TypeUnknownValue;

            switch (destType->TypeTag) {
                case ES_TypeTag.UNKNOWN:
                    retType = typeUnkn;
                    return true;

                case ES_TypeTag.Interface:
                case ES_TypeTag.Reference:
                case ES_TypeTag.Array:
                    retType = destType;
                    return true;

                default: {
                    retType = typeUnkn;
                    return false;
                }
            }
        }

        protected ES_FunctionData* CheckTypes_FindConstructor (ES_TypeInfo* objType, ReadOnlySpan<ES_FunctionPrototypeArgData> arguments) {
            return null;
        }

        protected ES_TypeInfo* CheckTypes_Dereference (ES_TypeInfo* refType) {
            if (refType->TypeTag != ES_TypeTag.Reference)
                return refType;

            var refTypeData = (ES_ReferenceData*) refType;
            return refTypeData->PointedType;
        }

        protected StatementData CheckTypes_Statement (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, SourceData src,
            ES_TypeInfo* retType, ES_AstStatement stmt
        ) {
            Debug.Assert (stmt is not null);
            var idPool = Environment!.IdPool!;
            var typeUnkn = Environment.TypeUnknownValue;

            switch (stmt) {
                case ES_AstEmptyStatement:
                    return new StatementData { AlwaysReturns = false };

                case ES_AstLabeledStatement labelStmt:
                    throw new NotImplementedException ("[TODO] Labels not implemented yet.");

                case ES_AstBlockStatement blockStmt: {
                    symbols.Push ();

                    bool alwaysReturns = false;
                    bool reportedUnreachable = false;

                    ES_AstStatement? subStmt = blockStmt.Statement;
                    while (subStmt is not null) {
                        Debug.Assert (subStmt is not null);

                        if (alwaysReturns && !reportedUnreachable) {
                            warningList.Add (new EchelonScriptErrorMessage (
                                src, subStmt.NodeBounds, ES_FrontendWarnings.UnreachableCode
                            ));

                            reportedUnreachable = true;
                        }

                        var subStmtData = CheckTypes_Statement (ref transUnit, symbols, src, retType, subStmt);

                        alwaysReturns |= subStmtData.AlwaysReturns;

                        subStmt = subStmt.Endpoint;
                    }

                    symbols.Pop ();
                    return new StatementData { AlwaysReturns = alwaysReturns };
                }

                #region Symbol definition

                case ES_AstImportStatement importStmt:
                    AST_HandleImport (symbols, src, importStmt);
                    return new StatementData { AlwaysReturns = false };

                case ES_AstTypeAlias aliasStmt: {
                    AST_HandleAlias (ref transUnit, symbols, src, aliasStmt);
                    return new StatementData { AlwaysReturns = false };
                }

                case ES_AstLocalVarDefinition varDef: {
                    bool implicitType = varDef.ValueType is null;
                    var varType = !implicitType ? GetTypeRef (varDef.ValueType) : null;
                    var symbolFlags = (FrontendSymbolFlags) 0;

                    if (varDef.UsingVar)
                        symbolFlags |= FrontendSymbolFlags.UsingVar;

                    foreach (var variable in varDef.Variables) {
                        var varName = variable.Name.Text.Span;
                        var varNameId = idPool.GetIdentifier (varName);

                        if (!implicitType) {
                            if (!symbols.AddSymbol (varNameId, NewSymbolVariable (varType, symbolFlags))) {
                                errorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (
                                    varName.GetPooledString (), variable.Name
                                ));
                            }

                            if (variable.InitializationExpression is not null) {
                                var exprData = CheckTypes_Expression (ref transUnit, symbols, src, variable.InitializationExpression, varType);
                                CheckTypes_EnsureCompat (varType, exprData.Type, src, exprData.Expr.NodeBounds, out _);
                            }
                        } else {
                            Debug.Assert (variable.InitializationExpression is not null);
                            var exprData = CheckTypes_Expression (ref transUnit, symbols, src, variable.InitializationExpression, typeUnkn);

                            if (exprData.Type is not null && exprData.Type->TypeTag == ES_TypeTag.Null) {
                                exprData.Type = typeUnkn;
                                errorList.Add (new (src, exprData.Expr.NodeBounds, ES_FrontendErrors.NoImplicitNullVars));
                            }

                            if (!symbols.AddSymbol (varNameId, NewSymbolVariable (exprData.Type, symbolFlags))) {
                                errorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (
                                    varName.GetPooledString (), variable.Name
                                ));
                            }
                        }
                    }

                    return new StatementData { AlwaysReturns = false };
                }

                #endregion

                #region Jumps

                case ES_AstConditionalStatement condStmt: {
                    var boolType = Environment.TypeBool;

                    var condExprData = CheckTypes_Expression (ref transUnit, symbols, src, condStmt.ConditionExpression, boolType);
                    CheckTypes_EnsureCompat (boolType, condExprData.Type, src, condExprData.Expr.NodeBounds, out _);

                    bool alwaysReturns = false;

                    var thenStmtData = CheckTypes_Statement (ref transUnit, symbols, src, retType, condStmt.ThenStatement);
                    if (condStmt.ElseStatement is not null) {
                        var elseStmtData = CheckTypes_Statement (ref transUnit, symbols, src, retType, condStmt.ElseStatement);

                        if (thenStmtData.AlwaysReturns && elseStmtData.AlwaysReturns)
                            alwaysReturns = true;
                    }

                    return new StatementData { AlwaysReturns = alwaysReturns };
                }

                case ES_AstSwitchStatement switchStmt: {
                    var exprTypeData = CheckTypes_Expression (ref transUnit, symbols, src, switchStmt.ValueExpression, typeUnkn);

                    foreach (var section in switchStmt.Sections) {
                        foreach (var expr in section.Expressions) {
                            if (expr is not null) {
                                var sectionTypeData = CheckTypes_Expression (ref transUnit, symbols, src, expr, exprTypeData.Type);

                                if (!sectionTypeData.Constant) {
                                    errorList.Add (new EchelonScriptErrorMessage (
                                        src, expr.NodeBounds, ES_FrontendErrors.ConstantExprExpected
                                    ));
                                }

                                if (!CheckTypes_MustBeCompat (exprTypeData.Type, sectionTypeData.Type, out var sectionConst)) {
                                    errorList.Add (ES_FrontendErrors.GenNoCast (
                                        exprTypeData.Type->Name.GetNameAsTypeString (), sectionTypeData.Type->Name.GetNameAsTypeString (),
                                        src, sectionTypeData.Expr.NodeBounds
                                    ));
                                }

                                if (!sectionTypeData.Constant || !sectionConst) {
                                    errorList.Add (new EchelonScriptErrorMessage (
                                        src, expr.NodeBounds, ES_FrontendErrors.ConstantExprExpected
                                    ));
                                }
                            }
                        }

                        ES_AstStatement? subStmt = section.StatementsBlock;
                        while (subStmt is not null) {
                            CheckTypes_Statement (ref transUnit, symbols, src, retType, subStmt);

                            subStmt = subStmt.Endpoint;
                        }
                    }

                    // TODO: Change Switch statements to check if they always return.
                    return new StatementData { AlwaysReturns = false };
                }

                case ES_AstGotoCaseStatement gotoCaseStmt:
                    throw new NotImplementedException ("[TODO] 'goto case' not implemented yet.");

                case ES_AstReturnStatement retStmt: {
                    if (retStmt.ReturnExpression is not null) {
                        var exprData = CheckTypes_Expression (ref transUnit, symbols, src, retStmt.ReturnExpression, retType);

                        CheckTypes_EnsureCompat (retType, exprData.Type, src, exprData.Expr.NodeBounds, out _);
                    } else if (retType->TypeTag != ES_TypeTag.Void) {
                        errorList.Add (ES_FrontendErrors.GenMissingReturnValue (
                            retType->Name.GetNameAsTypeString (), src, retStmt.NodeBounds
                        ));
                    }

                    return new StatementData { AlwaysReturns = true };
                }

                #endregion

                #region Loops

                case ES_AstLoopStatement loopStmt: {
                    var boolType = Environment.TypeBool;

                    symbols.Push ();

                    if (loopStmt.InitializationStatement is not null)
                        CheckTypes_Statement (ref transUnit, symbols, src, retType, loopStmt.InitializationStatement);

                    if (loopStmt.ConditionExpression is not null) {
                        var condExprData = CheckTypes_Expression (ref transUnit, symbols, src, loopStmt.ConditionExpression, boolType);
                        CheckTypes_EnsureCompat (boolType, condExprData.Type, src, condExprData.Expr.NodeBounds, out _);
                    }

                    if (loopStmt.IterationExpressions is not null) {
                        foreach (var expr in loopStmt.IterationExpressions) {
                            if (expr is not null)
                                CheckTypes_Expression (ref transUnit, symbols, src, expr, typeUnkn);
                        }
                    }

                    Debug.Assert (loopStmt.LoopBody is not null);
                    Debug.Assert (loopStmt.LoopBody.Endpoint is null);

                    CheckTypes_Statement (ref transUnit, symbols, src, retType, loopStmt.LoopBody);

                    symbols.Pop ();

                    return new StatementData { AlwaysReturns = false };
                }

                #endregion

                case ES_AstExpressionStatement exprStmt:
                    CheckTypes_Expression (ref transUnit, symbols, src, exprStmt.Expression, typeUnkn);
                    return new StatementData { AlwaysReturns = false };

                case ES_AstExpressionListStatement exprListStmt: {
                    foreach (var expr in exprListStmt.Expressions)
                        CheckTypes_Expression (ref transUnit, symbols, src, expr, typeUnkn);

                    return new StatementData { AlwaysReturns = false };
                }

                default:
                    throw new NotImplementedException ("Statement type not implemented.");
            }
        }

        protected ExpressionData CheckTypes_Expression (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, SourceData src,
            ES_AstExpression expr, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (expr is not null);

            var typeUnkn = Environment!.TypeUnknownValue;
            var idPool = Environment.IdPool;

            switch (expr) {
                case ES_AstParenthesisExpression parenExpr:
                    return CheckTypes_Expression (ref transUnit, symbols, src, parenExpr.Inner, expectedType);

                #region Primary expressions

                case ES_AstFunctionCallExpression funcCallExpr:
                    return CheckTypes_Expression_FunctionCall (ref transUnit, symbols, src, funcCallExpr, expectedType);

                case ES_AstIndexingExpression indexExpr: {
                    var typeIndex = Environment.GetArrayIndexType ();

                    var rankCount = indexExpr.RankExpressions.Length;
                    var indexedExprData = CheckTypes_Expression (ref transUnit, symbols, src, indexExpr.IndexedExpression, typeUnkn);

                    foreach (var rank in indexExpr.RankExpressions) {
                        Debug.Assert (rank is not null);

                        var rankExprData = CheckTypes_Expression (ref transUnit, symbols, src, rank, typeIndex);
                        CheckTypes_EnsureCompat (typeIndex, rankExprData.Type, src, rank.NodeBounds, out _);
                    }

                    var badRankCount = false;
                    var returnType = typeUnkn;
                    if (indexedExprData.Type is not null) {
                        var indexedType = indexedExprData.Type;
                        var indexedTypeTag = indexedType->TypeTag;

                        if (indexedTypeTag == ES_TypeTag.Array) {
                            var arrayData = (ES_ArrayTypeData*) indexedExprData.Type;

                            badRankCount = rankCount != arrayData->DimensionsCount;
                            returnType = arrayData->ElementType;
                        } else {
                            errorList.Add (ES_FrontendErrors.GenCantApplyIndexingToType (
                                indexedExprData.Type->Name.GetNameAsTypeString (),
                                src, indexedExprData.Expr.NodeBounds
                            ));
                        }
                    } else {
                        errorList.Add (new EchelonScriptErrorMessage (
                            src, indexedExprData.Expr.NodeBounds, ES_FrontendErrors.CantApplyIndexing
                        ));
                    }

                    if (badRankCount) {
                        errorList.Add (new EchelonScriptErrorMessage (
                            src, indexedExprData.Expr.NodeBounds, ES_FrontendErrors.IndexingBadRankCount
                        ));
                    }

                    return new ExpressionData { Expr = expr, Type = returnType, Constant = false, Addressable = true };
                }

                case ES_AstNewObjectExpression newObjExpr:
                    return CheckTypes_Expression_NewObject (ref transUnit, symbols, src, newObjExpr, expectedType);

                case ES_AstNewArrayExpression newArrayExpr: {
                    var indexType = Environment!.GetArrayIndexType ();
                    var elemType = GetTypeRef (newArrayExpr.ElementType);
                    var arrType = EnvironmentBuilder!.CreateArrayType (elemType, newArrayExpr.Ranks.Length);

                    if (elemType->Flags.HasFlag (ES_TypeFlag.NoNew)) {
                        errorList.Add (ES_FrontendErrors.GenNoTypeNew (
                            elemType->Name.GetNameAsTypeString (), src, newArrayExpr.NodeBounds
                        ));
                    }

                    foreach (var rank in newArrayExpr.Ranks) {
                        Debug.Assert (rank is not null);
                        var rankExpr = CheckTypes_Expression (ref transUnit, symbols, src, rank, indexType);

                        CheckTypes_EnsureCompat (indexType, rankExpr.Type, src, rankExpr.Expr.NodeBounds, out _);
                    }

                    return new ExpressionData { Expr = expr, Type = arrType, Constant = false, Addressable = false };
                }

                case ES_AstIntegerLiteralExpression:
                case ES_AstBooleanLiteralExpression:
                case ES_AstFloatLiteralExpression:
                    throw new CompilationException (ES_FrontendErrors.ConstFoldFailure);

                case ES_AstStringLiteralExpression:
                    throw new NotImplementedException ("[TODO] String literals not implemented yet.");

                case ES_AstCharLiteralExpression:
                    throw new NotImplementedException ("[TODO] Character literals not implemented yet.");

                case ES_AstIntegerConstantExpression intConstExpr:
                    return new ExpressionData { Expr = expr, Type = intConstExpr.IntType, Constant = true, Addressable = false };

                case ES_AstBooleanConstantExpression:
                    return new ExpressionData { Expr = expr, Type = Environment.TypeBool, Constant = true, Addressable = false };

                case ES_AstFloat32ConstantExpression:
                    return new ExpressionData { Expr = expr, Type = Environment.TypeFloat32, Constant = true, Addressable = false };

                case ES_AstFloat64ConstantExpression:
                    return new ExpressionData { Expr = expr, Type = Environment.TypeFloat64, Constant = true, Addressable = false };

                case ES_AstNullLiteralExpression nullLitExpr:
                    return new ExpressionData { Expr = expr, Type = Environment.TypeNull, Constant = true, Addressable = false };

                case ES_AstNameExpression nameExpr: {
                    var id = idPool.GetIdentifier (nameExpr.Value.Text.Span);
                    var symbol = symbols.GetSymbol (id);

                    switch (symbol.Tag) {
                        case FrontendSymbolType.None: {
                            var symbolName = nameExpr.Value.Text.Span.GetPooledString ();
                            errorList.Add (ES_FrontendErrors.GenCantFindSymbol (symbolName, nameExpr.Value));

                            return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = false, Addressable = false };
                        }

                        case FrontendSymbolType.Variable:
                            return new ExpressionData { Expr = expr, Type = symbol.MatchVar (), Constant = false, Addressable = true };

                        case FrontendSymbolType.Type: {
                            if (expectedType is not null) {
                                errorList.Add (ES_FrontendErrors.GenInvalidExprTerm (
                                    nameExpr.Value.Text.Span.GetPooledString (),
                                    nameExpr.Value
                                ));

                                return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = true, Addressable = true };
                            }

                            return new ExpressionData { Expr = expr, TypeInfo = symbol.MatchType (), Constant = true, Addressable = true };
                        }

                        case FrontendSymbolType.Function: {
                            var func = symbol.MatchFunction ();
                            var type = (ES_TypeInfo*) func->FunctionType;
                            return new ExpressionData { Expr = expr, Type = type, Function = func, Constant = true, Addressable = true };
                        }

                        default:
                            throw new NotImplementedException ("Symbol type not implemented.");
                    }
                }

                case ES_AstMemberAccessExpression memberAccessExpr:
                    return CheckTypes_Expression_MemberAccess (ref transUnit, symbols, src, memberAccessExpr, expectedType);

                #endregion

                case ES_AstIncDecExpression incDecExpr: {
                    var exprType = CheckTypes_Expression (ref transUnit, symbols, src, incDecExpr.Inner, expectedType);

                    if (exprType.Type->TypeTag == ES_TypeTag.UNKNOWN)
                        return new ExpressionData { Expr = expr, Type = exprType.Type, Constant = false, Addressable = false };

                    if (!exprType.Addressable) {
                        errorList.Add (new EchelonScriptErrorMessage (
                            src, incDecExpr.Inner.NodeBounds, ES_FrontendErrors.TempValueInIncDecOp
                        ));
                    }

                    if (exprType.Type->TypeTag == ES_TypeTag.Int || exprType.Type->TypeTag == ES_TypeTag.Float)
                        return new ExpressionData { Expr = expr, Type = exprType.Type, Constant = false, Addressable = false };
                    else
                        throw new NotImplementedException ("[TODO] ?");
                }

                #region Unary expressions

                case ES_AstSimpleUnaryExpression unaryExpr: {
                    var exprData = CheckTypes_Expression (ref transUnit, symbols, src, unaryExpr.Inner, expectedType);

                    if (!EnvironmentBuilder!.UnaryOpCompat (exprData.Type, unaryExpr.ExpressionType, out var finalType, out var opConst)) {
                        errorList.Add (ES_FrontendErrors.GenCantApplyUnaryOp (
                            unaryExpr.OperatorToken.Text.Span.GetPooledString (), exprData.Type->Name.GetNameAsTypeString (),
                            src, exprData.Expr.NodeBounds
                        ));

                        return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = opConst, Addressable = false };
                    }

                    bool isAddressable = false;

                    if (exprData.Type->TypeTag == ES_TypeTag.Reference && unaryExpr.ExpressionType == SimpleUnaryExprType.Dereference)
                        isAddressable = true;

                    return new ExpressionData { Expr = expr, Type = finalType, Constant = opConst, Addressable = isAddressable };
                }

                case ES_AstCastExpression castExpr: {
                    var destType = GetTypeRef (castExpr.DestinationType);
                    var exprType = CheckTypes_Expression (ref transUnit, symbols, src, castExpr.InnerExpression, null);

                    if (!CheckTypes_ExplicitCast (destType, exprType.Type, out bool castRedundant, out var isConstant)) {
                        errorList.Add (ES_FrontendErrors.GenNoExplicitCast (
                            destType->Name.GetNameAsTypeString (), exprType.Type->Name.GetNameAsTypeString (),
                            src, castExpr.NodeBounds
                        ));

                        return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = isConstant, Addressable = false };
                    }

                    if (castRedundant) {
                        infoList.Add (new EchelonScriptErrorMessage (
                            src, castExpr.CastBounds, ES_FrontendInfoMsg.RedundantCast
                        ));
                    }

                    return new ExpressionData { Expr = expr, Type = destType, Constant = isConstant, Addressable = false };
                }

                #endregion

                case ES_AstSimpleBinaryExpression simpleBinaryExpr: {
                    var expectedRightType = expectedType;

                    var leftType = CheckTypes_Expression (ref transUnit, symbols, src, simpleBinaryExpr.Left, expectedType);

                    if (simpleBinaryExpr.ExpressionType.IsBitShift () && leftType.Type->TypeTag == ES_TypeTag.Int)
                        expectedRightType = Environment.GetIntType (((ES_IntTypeData*) expectedType)->IntSize, true);

                    var rightType = CheckTypes_Expression (ref transUnit, symbols, src, simpleBinaryExpr.Right, expectedRightType);

                    var constant = leftType.Constant & rightType.Constant;

                    if (leftType.Type is null || rightType.Type is null)
                        return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = constant, Addressable = false };

                    if (!EnvironmentBuilder!.BinaryOpCompat (leftType.Type, rightType.Type, simpleBinaryExpr.ExpressionType, out var finalType, out var opConst)) {
                        errorList.Add (ES_FrontendErrors.GenCantApplyBinaryOp (
                            simpleBinaryExpr.OperatorToken.Text.Span.GetPooledString (),
                            leftType.Type->Name.GetNameAsTypeString (), rightType.Type->Name.GetNameAsTypeString (),
                            src, simpleBinaryExpr.NodeBounds
                        ));

                        return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = constant, Addressable = false };
                    }
                    constant &= opConst;

                    if (simpleBinaryExpr.ExpressionType.IsAssignment () && !leftType.Addressable) {
                        errorList.Add (new EchelonScriptErrorMessage (
                            src, leftType.Expr.NodeBounds, ES_FrontendErrors.CannotAssignExpr
                        ));
                    }

                    return new ExpressionData { Expr = expr, Type = finalType, Constant = constant, Addressable = false };
                }

                case ES_AstConditionalExpression condExpr: {
                    var condType = CheckTypes_Expression (ref transUnit, symbols, src, condExpr.Condition, Environment.TypeBool);

                    CheckTypes_EnsureCompat (Environment.TypeBool, condType.Type, src, condType.Expr.NodeBounds, out var condConst);

                    var leftExpr = CheckTypes_Expression (ref transUnit, symbols, src, condExpr.Then, expectedType);
                    var rightExpr = CheckTypes_Expression (ref transUnit, symbols, src, condExpr.Else, expectedType);

                    bool isCompat = (
                        CheckTypes_EnsureCompat (expectedType, leftExpr.Type, src, leftExpr.Expr.NodeBounds, out var leftConst) &
                        CheckTypes_EnsureCompat (expectedType, rightExpr.Type, src, rightExpr.Expr.NodeBounds, out var rightConst)
                    );
                    bool constant = condType.Constant & leftExpr.Constant & rightExpr.Constant & condConst & leftConst & rightConst;

                    var finalType = isCompat ? expectedType : typeUnkn;

                    return new ExpressionData { Expr = expr, Type = finalType, Constant = constant, Addressable = false };
                }

                default:
                    throw new NotImplementedException ("Expression type not implemented.");
            }
        }

        protected ExpressionData CheckTypes_Expression_NewObject (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, SourceData src,
            ES_AstNewObjectExpression newObjExpr, ES_TypeInfo* expectedType
        ) {
            var typeUnkn = Environment!.TypeUnknownValue;

            var objType = GetTypeRef (newObjExpr.TypeDeclaration);
            var refType = EnvironmentBuilder!.CreateReferenceType (objType);
            var retType = refType;

            var errorFound = false;

            if (objType->Flags.HasFlag (ES_TypeFlag.NoNew)) {
                errorList.Add (ES_FrontendErrors.GenNoTypeNew (
                    objType->Name.GetNameAsTypeString (), src, newObjExpr.NodeBounds
                ));

                errorFound = true;
            }

            // Get the arg types.
            var argIdx = 0;
            Span<ES_FunctionPrototypeArgData> argsList = stackalloc ES_FunctionPrototypeArgData [newObjExpr.Arguments.Length];
            foreach (var arg in newObjExpr.Arguments) {
                var argValueExpr = CheckTypes_Expression (ref transUnit, symbols, src, arg.ValueExpression, typeUnkn);

                var argType = arg.ArgType;
                var argValueType = argValueExpr.Type;

                if (argValueExpr.Type is null || argValueExpr.Type->TypeTag == ES_TypeTag.UNKNOWN)
                    errorFound = true;

                var argProtoData = new ES_FunctionPrototypeArgData (argType, argValueType);

                argsList [argIdx++] = argProtoData;
            }

            if (errorFound)
                return new ExpressionData { Expr = newObjExpr, Type = typeUnkn, Constant = false, Addressable = false };

            // Get the constructor.
            var constructor = CheckTypes_FindConstructor (objType, argsList);

            // Error out if the constructor is null.
            if (constructor is null && newObjExpr.Arguments.Length > 0) {
                errorList.Add (ES_FrontendErrors.GenNoSuchConstructor (
                    objType->Name.GetNameAsTypeString (), Environment.GetFunctionSignatureString (argsList),
                    src, newObjExpr.NodeBounds
                ));
                retType = typeUnkn;
            }

            return new ExpressionData { Expr = newObjExpr, Type = retType, Constant = false, Addressable = false };
        }

        protected ExpressionData CheckTypes_Expression_MemberAccess (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, SourceData src,
            ES_AstMemberAccessExpression expr, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (Environment is not null);
            Debug.Assert (expr.Member is not null);

            var idPool = Environment.IdPool;
            var typeUnkn = Environment.TypeUnknownValue;

            var parentExpr = CheckTypes_Expression (ref transUnit, symbols, src, expr.Parent, null);
            var memberId = idPool.GetIdentifier (expr.Member.Value.Text.Span);

            if (parentExpr.Type is not null) {
                var type = parentExpr.Type;

                type = CheckTypes_Dereference (type);

                switch (type->TypeTag) {
                    case ES_TypeTag.UNKNOWN:
                        return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = false, Addressable = false };

                    case ES_TypeTag.Struct: {
                        return CheckTypes_Expression_MemberAccess_Basic (
                            ref transUnit, symbols, src,
                            expr, type, memberId, expectedType
                        );
                    }

                    case ES_TypeTag.Array: {
                        var ret = CheckTypes_Expression_MemberAccess_Basic (
                            ref transUnit, symbols, src,
                            expr, type, memberId, expectedType
                        );
                        ret.Addressable = false;
                        return ret;
                    }

                    default:
                        throw new NotImplementedException ("Type not implemented yet.");
                }
            } else if (parentExpr.TypeInfo is not null) {
                var type = parentExpr.TypeInfo;

                switch (type->TypeTag) {
                    case ES_TypeTag.UNKNOWN:
                        return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = false, Addressable = false };

                    case ES_TypeTag.Struct: {
                        return CheckTypes_Expression_MemberAccessStatic_Aggregate (
                            ref transUnit, symbols, src,
                            expr, type, memberId, expectedType
                        );
                    }

                    case ES_TypeTag.Array: {
                        errorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
                            type->Name.GetNameAsTypeString (),
                            expr.Member.Value.Text.Span.GetPooledString (),
                            expr.Member.Value
                        ));

                        return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = false, Addressable = false };
                    }

                    default:
                        throw new NotImplementedException ("Type not implemented yet.");
                }
            } else if (parentExpr.Function is not null)
                throw new NotImplementedException ("Not supported. (yet?)");
            else
                throw new CompilationException ("<<Unknown expression type in CheckTypes_Expression_MemberAccess>>");
        }

        protected ExpressionData CheckTypes_Expression_MemberAccess_Basic (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, SourceData src,
            ES_AstMemberAccessExpression expr, ES_TypeInfo* type, ArrayPointer<byte> memberId, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (Environment is not null);
            Debug.Assert (expr.Member is not null);

            var typeUnkn = Environment.TypeUnknownValue;
            var membersArr = type->MembersList.MembersList;

            foreach (var memberAddr in membersArr.Span) {
                var memberPtr = memberAddr.Address;

                if (!memberPtr->Name.Equals (memberId))
                    continue;

                switch (memberPtr->MemberType) {
                    case ES_MemberType.Field: {
                        var memberVar = (ES_MemberData_Variable*) memberPtr;
                        bool addressable = true;

                        if (memberVar->Info.Flags.HasFlag (ES_MemberFlags.Static)) {
                            errorList.Add (ES_FrontendErrors.GenStaticAccessOnInst (
                                type->Name.GetNameAsTypeString (),
                                expr.Member.Value.Text.Span.GetPooledString (),
                                expr.Member.Value
                            ));
                        }

                        return new ExpressionData { Expr = expr, Type = memberVar->Type, Constant = false, Addressable = addressable };
                    }

                    case ES_MemberType.Function:
                        throw new NotImplementedException ("[TODO] Member function access not implemented yet.");

                    default:
                        throw new NotImplementedException ("Member type not implemented yet.");
                }
            }

            errorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
                type->Name.GetNameAsTypeString (),
                expr.Member.Value.Text.Span.GetPooledString (),
                expr.Member.Value
            ));

            return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = false, Addressable = false };
        }

        protected ExpressionData CheckTypes_Expression_MemberAccessStatic_Aggregate (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, SourceData src,
            ES_AstMemberAccessExpression expr, ES_TypeInfo* type, ArrayPointer<byte> memberId, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (Environment is not null);
            Debug.Assert (expr.Member is not null);

            var typeUnkn = Environment.TypeUnknownValue;
            var membersArr = type->MembersList.MembersList;

            foreach (var memberAddr in membersArr.Span) {
                var memberPtr = memberAddr.Address;

                if (!memberPtr->Name.Equals (memberId))
                    continue;

                switch (memberPtr->MemberType) {
                    case ES_MemberType.Field: {
                        var memberVar = (ES_MemberData_Variable*) memberPtr;
                        bool addressable = true;

                        if (!memberVar->Info.Flags.HasFlag (ES_MemberFlags.Static)) {
                            errorList.Add (ES_FrontendErrors.GenInstAccessOnStatic (
                                expr.Member.Value.Text.Span.GetPooledString (),
                                expr.Member.Value
                            ));
                        }

                        return new ExpressionData { Expr = expr, Type = memberVar->Type, Constant = false, Addressable = addressable };
                    }

                    case ES_MemberType.Function:
                        throw new NotImplementedException ("[TODO] Static member function access not implemented yet.");

                    default:
                        throw new NotImplementedException ("Member type not implemented yet.");
                }
            }

            errorList.Add (ES_FrontendErrors.GenMemberDoesntExist (
                type->Name.GetNameAsTypeString (),
                expr.Member.Value.Text.Span.GetPooledString (),
                expr.Member.Value
            ));

            return new ExpressionData { Expr = expr, Type = typeUnkn, Constant = false, Addressable = false };
        }

        protected ExpressionData CheckTypes_Expression_FunctionCall (
            ref TranslationUnitData transUnit, SymbolStack<FrontendSymbol> symbols, SourceData src,
            ES_AstFunctionCallExpression funcCallExpr, ES_TypeInfo* expectedType
        ) {
            Debug.Assert (Environment is not null);
            var typeUnkn = Environment.TypeUnknownValue;

            var funcExpr = CheckTypes_Expression (ref transUnit, symbols, src, funcCallExpr.FunctionExpression, typeUnkn);
            ES_FunctionData* func = null;
            ES_FunctionPrototypeData* funcType = null;

            if (funcExpr.Function is not null) {
                func = funcExpr.Function;
                funcType = func->FunctionType;
            } else {
                if (funcExpr.TypeInfo is not null) {
                    errorList.Add (ES_FrontendErrors.GenCantInvokeType (
                        funcExpr.TypeInfo->Name.GetNameAsTypeString (), src, funcExpr.Expr.NodeBounds
                    ));
                    return new ExpressionData { Expr = funcCallExpr, Type = typeUnkn, Constant = false, Addressable = false };
                } else if (funcExpr.Type is not null) {
                    // TODO: Some types might be allowed to have `()` overrides too in the future. But not now.
                    if (funcExpr.Type->TypeTag != ES_TypeTag.UNKNOWN) {
                        errorList.Add (new EchelonScriptErrorMessage (
                            src, funcExpr.Expr.NodeBounds, ES_FrontendErrors.CantInvokeExpr
                        ));
                    }
                    return new ExpressionData { Expr = funcCallExpr, Type = typeUnkn, Constant = false, Addressable = false };
                } else
                    Debug.Fail ("???");
            }

            var funcName = funcType->TypeInfo.Name.TypeName;
            int funcArgCount = funcType->ArgumentsList.Length;
            int callArgCount = funcCallExpr.Arguments.Length;
            int reqArgCount = 0;

            if (func is not null) {
                funcName = func->Name.TypeName;
                reqArgCount = funcArgCount - func->OptionalArgsCount;
            } else
                reqArgCount = funcArgCount;

            if (callArgCount < reqArgCount) {
                var errBounds = funcCallExpr.FunctionExpression.NodeBounds;
                if (func is not null) {
                    var arg = func->Arguments.Span [callArgCount];
                    errorList.Add (ES_FrontendErrors.GenMissingFuncArg (
                        StringPool.Shared.GetOrAdd (arg.Name.Span, Encoding.ASCII),
                        StringPool.Shared.GetOrAdd (funcName.Span, Encoding.ASCII),
                        src, errBounds
                    ));
                } else {
                    errorList.Add (ES_FrontendErrors.GenNotEnoughFuncArgs (
                        StringPool.Shared.GetOrAdd (funcName.Span, Encoding.ASCII), src, errBounds
                    ));
                }
            }

            for (int argIdx = 0; argIdx < callArgCount; argIdx++) {
                var arg = funcCallExpr.Arguments [argIdx];
                ES_FunctionArgData* argData = null;
                ES_FunctionPrototypeArgData* argTypeData = null;

                if (argIdx < funcArgCount) {
                    argData = func->Arguments.Elements + argIdx;
                    argTypeData = funcType->ArgumentsList.Elements + argIdx;
                } else if (argIdx == funcArgCount) {
                    errorList.Add (ES_FrontendErrors.GenTooManyFuncArgs (
                        StringPool.Shared.GetOrAdd (funcName.Span, Encoding.ASCII), src,
                        arg.ValueExpression.NodeBounds
                    ));
                }

                ES_TypeInfo* argValType = null;
                if (argTypeData is not null) {
                    if (argTypeData->ArgType == ES_ArgumentType.Normal && arg.ArgType != ES_ArgumentType.Normal) {
                        if (argData is not null) {
                            errorList.Add (ES_FrontendErrors.GenWrongArgType (
                                StringPool.Shared.GetOrAdd (argData->Name.Span, Encoding.ASCII),
                                arg.ArgType.ToString (), src, arg.ValueExpression.NodeBounds
                            ));
                        } else {
                            errorList.Add (ES_FrontendErrors.GenWrongArgType (
                                argIdx, arg.ArgType.ToString (), src, arg.ValueExpression.NodeBounds
                            ));
                        }
                    }

                    if (argTypeData->ArgType != ES_ArgumentType.Normal && arg.ArgType != argTypeData->ArgType) {
                        if (argData is not null) {
                            errorList.Add (ES_FrontendErrors.GenWrongArgType (
                                StringPool.Shared.GetOrAdd (argData->Name.Span, Encoding.ASCII),
                                arg.ArgType.ToString (), src, arg.ValueExpression.NodeBounds
                            ));
                        } else {
                            errorList.Add (ES_FrontendErrors.GenWrongArgType (
                                argIdx, arg.ArgType.ToString (), src, arg.ValueExpression.NodeBounds
                            ));
                        }
                    }

                    argValType = argTypeData->ValueType;
                }

                var argExprData = CheckTypes_Expression (ref transUnit, symbols, src, arg.ValueExpression, argValType);

                if (argValType is not null)
                    CheckTypes_EnsureCompat (argValType, argExprData.Type, src, argExprData.Expr.NodeBounds, out _);
            }

            return new ExpressionData { Expr = funcCallExpr, Type = funcType->ReturnType, Constant = false, Addressable = false };
        }
    }
}