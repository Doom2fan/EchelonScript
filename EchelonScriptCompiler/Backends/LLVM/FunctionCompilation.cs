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
using System.Diagnostics.CodeAnalysis;
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Frontend;
using LLVMSharp.Interop;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Backends.LLVMBackend {
    public unsafe sealed partial class LLVMCompilerBackend {
        public struct StatementData {
            public bool AlwaysReturns;
        }

        private PooledArray<char> MangleFunctionType (ES_FunctionPrototypeData* type) {
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // Add the return type.
            mangleChars.AddRange ("Ret");
            mangleChars.AddRange (type->ReturnType->FullyQualifiedNameString);

            // Add the arg types.
            foreach (var arg in type->ArgumentsList.Span) {
                mangleChars.AddRange ("_");

                switch (arg.ArgType) {
                    case ES_ArgumentType.Normal: break;
                    case ES_ArgumentType.In: break;
                    case ES_ArgumentType.Out: mangleChars.AddRange ("out"); break;
                    case ES_ArgumentType.Ref: mangleChars.AddRange ("ref"); break;
                }

                mangleChars.AddRange (arg.ValueType->FullyQualifiedNameString);
            }

            return mangleChars.ToPooledArray ();
        }

        private PooledArray<char> MangleFunctionName ([DisallowNull] ES_TypeInfo* parentType, [DisallowNull] ES_FunctionData* func) {
            // Sample name: "System.Math__FMath.Sin"

            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            var parentFQN = parentType->FullyQualifiedNameString.AsSpan ();

            // Add the namespace.
            mangleChars.AddRange (parentFQN.Slice (0, parentFQN.IndexOf ("::")));

            // Namespace separator.
            mangleChars.AddRange ("__");

            // Add the parent type's name.
            mangleChars.AddRange (parentType->TypeNameString);

            // Member separator.
            mangleChars.Add ('.');

            // Add the method's name.
            Encoding.ASCII.GetChars (func->Name.Span, mangleChars.AddSpan (func->Name.Length));

            /*// Add the type's mangled name.
            mangleChars.AddRange ("_");
            using var typeMangle = MangleFunctionType (func->FunctionType);
            mangleChars.AddRange (typeMangle);*/

            return mangleChars.ToPooledArray ();
        }

        private PooledArray<char> MangleFunctionName ([DisallowNull] ES_NamespaceData namespaceData, [DisallowNull] ES_FunctionData* func) {
            // Sample name: "System.Math__SinF"

            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // Add the namespace.
            mangleChars.AddRange (namespaceData.NamespaceNameString);

            // Namespace separator.
            mangleChars.AddRange ("__");

            // Add the function's name.
            Encoding.ASCII.GetChars (func->Name.Span, mangleChars.AddSpan (func->Name.Length));

            /*// Add the type's mangled name.
            mangleChars.AddRange ("_");
            using var typeMangle = MangleFunctionType (func->FunctionType);
            mangleChars.AddRange (typeMangle);*/

            return mangleChars.ToPooledArray ();
        }

        private bool GenerateCode_Function (
            ref TranslationUnitData transUnit, ES_NamespaceData namespaceData,
            SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_TypeInfo* parentType, ES_AstFunctionDefinition funcDef
        ) {
            Debug.Assert (env is not null);

            symbols.Push ();

            ES_FunctionData* funcData;

            if (namespaceData is not null) {
                if (!namespaceData.Functions.TryGetValue (env.IdPool.GetIdentifier (funcDef.Name.Text.Span), out var ptr))
                    throw new CompilationException (Error_LLVMError);

                funcData = ptr;
            } else if (parentType is not null) {
                throw new NotImplementedException ();
            } else
                throw new CompilationException (Error_LLVMError);

            var funcType = funcData->FunctionType;

            using var argsArr = PooledArray<LLVMTypeRef>.GetArray (funcDef.ArgumentsList.Length);
            int argNum = 0;
            foreach (ref var arg in funcType->ArgumentsList.Span) {
                if (arg.ArgType != ES_ArgumentType.Normal)
                    throw new NotImplementedException ();

                argsArr.Span [argNum++] = GetLLVMType (arg.ValueType);
            }

            var retType = funcData->FunctionType->ReturnType;
            var funcTypeRef = LLVMTypeRef.CreateFunction (GetLLVMType (retType), argsArr, false);

            LLVMValueRef def;
            if (namespaceData is not null) {
                using var funcName = MangleFunctionName (namespaceData, funcData);
                def = moduleRef.AddFunction (funcName, funcTypeRef);
            } else if (parentType is not null) {
                throw new NotImplementedException ();
            } else
                throw new CompilationException (Error_LLVMError);

            argsArr.Dispose ();

            def.Linkage = LLVMLinkage.LLVMExternalLinkage;

            def.AppendBasicBlock ("entry");
            var entryBlock = def.EntryBasicBlock;
            builderRef.PositionAtEnd (entryBlock);

            for (uint i = 0; i < def.ParamsCount; i++) {
                var param = def.GetParam (i);
                ref var argTypeInfo = ref funcType->ArgumentsList.Span [(int) i];
                ref var argData = ref funcData->Arguments.Span [(int) i];

                var argFlags = (VariableFlags) 0;

                switch (argTypeInfo.ArgType) {
                    case ES_ArgumentType.Normal:
                        var paramAlloca = builderRef.BuildAlloca (param.TypeOf, param.Name);
                        builderRef.BuildStore (param, paramAlloca);
                        param = paramAlloca;
                        break;

                    default:
                        throw new NotImplementedException ();
                }

                symbols.AddSymbol (argData.Name, new Symbol (
                    new VariableData {
                        Flags = argFlags,
                        Type = argTypeInfo.ValueType,
                        LLVMValue = param,
                    }
                ));

                param.Name = StringPool.Shared.GetOrAdd (argData.Name.Span, Encoding.ASCII);
            }

            bool alwaysReturns = false;
            foreach (var stmt in funcDef.StatementsList) {
                Debug.Assert (stmt is not null);

                var stmtData = GenerateCode_Statement (ref transUnit, symbols, src, def, retType, stmt!);

                alwaysReturns |= stmtData.AlwaysReturns;
            }

            if (!alwaysReturns) {
                if (retType == env.TypeVoid) {
                    builderRef.PositionAtEnd (entryBlock);
                    builderRef.BuildRetVoid ();
                } else
                    throw new CompilationException (ES_BackendErrors.FrontendError);
            }

            return true;
        }

        private StatementData GenerateCode_Statement (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            LLVMValueRef funcLLVM, ES_TypeInfo* retType, ES_AstStatement stmt
        ) {
            Debug.Assert (stmt is not null);
            var idPool = env!.IdPool;
            var typeUnkn = env.TypeUnknownValue;

            switch (stmt) {
                case ES_AstEmptyStatement:
                    return new StatementData { AlwaysReturns = false };

                case ES_AstLabeledStatement labelStmt: {
                    throw new NotImplementedException ();
                    //return GenerateCode_Statement (ref transUnit, symbols, src, retType, labelStmt.Statement);
                }

                case ES_AstBlockStatement blockStmt: {
                    throw new NotImplementedException ();
                    /*Debug.Assert (blockStmt.Statements is not null);
                    symbols.Push ();

                    bool alwaysReturns = false;
                    foreach (var subStmt in blockStmt.Statements) {
                        Debug.Assert (subStmt is not null);

                        var subStmtData = GenerateCode_Statement (ref transUnit, symbols, src, retType, subStmt);

                        alwaysReturns |= subStmtData.AlwaysReturns;
                    }

                    symbols.Pop ();
                    return new StatementData { AlwaysReturns = alwaysReturns };*/
                }

                #region Symbol definition

                case ES_AstImportStatement importStmt:
                    AST_HandleImport (symbols, src, importStmt);
                    return new StatementData { AlwaysReturns = false };

                case ES_AstTypeAlias aliasStmt:
                    AST_HandleAlias (symbols, src, aliasStmt);
                    return new StatementData { AlwaysReturns = false };

                case ES_AstLocalVarDefinition varDef: {
                    bool implicitType = varDef.ValueType is null;

                    var baseVarFlags = (VariableFlags) 0;
                    if (varDef.UsingVar)
                        baseVarFlags |= VariableFlags.Using;

                    foreach (var variable in varDef.Variables) {
                        var varName = variable.Name.Text.Span;
                        var varNameId = idPool.GetIdentifier (varName);

                        var varFlags = baseVarFlags;
                        var varData = new VariableData {
                            Flags = varFlags,
                        };
                        bool hasInit;
                        ExpressionData initExprData;

                        if (!implicitType) {
                            varData.Type = GetTypeRef (varDef.ValueType);

                            if (variable.InitializationExpression is not null) {
                                hasInit = true;
                                initExprData = GenerateCode_Expression (ref transUnit, symbols, src, variable.InitializationExpression, varData.Type);
                                //TODO: CheckTypes_EnsureCompat (varType, exprData.Type, src, exprData.Expr.NodeBounds);
                            } else {
                                hasInit = false;
                                initExprData = default;
                            }
                        } else {
                            Debug.Assert (variable.InitializationExpression is not null);
                            hasInit = true;
                            initExprData = GenerateCode_Expression (ref transUnit, symbols, src, variable.InitializationExpression, typeUnkn);

                            varData.Type = initExprData.Type;
                        }

                        varData.LLVMValue = builderRef.BuildAlloca (GetLLVMType (varData.Type), varName);

                        if (!symbols.AddSymbol (varNameId, new Symbol (varData)))
                            throw new CompilationException (ES_BackendErrors.FrontendError);

                        if (hasInit)
                            builderRef.BuildStore (initExprData.Value, varData.LLVMValue);
                    }

                    return new StatementData { AlwaysReturns = false };
                }

                #endregion

                #region Jumps

                case ES_AstConditionalStatement condStmt: {
                    throw new NotImplementedException ();
                    /*var boolType = env!.TypeBool;

                    var condExprData = GenerateCode_Expression (ref transUnit, symbols, src, condStmt.ConditionExpression, boolType);
                    CheckTypes_EnsureCompat (boolType, condExprData.Type, src, condExprData.Expr.NodeBounds);

                    bool alwaysReturns = false;

                    var thenStmtData = CheckTypes_Statement (ref transUnit, symbols, src, retType, condStmt.ThenStatement);
                    if (condStmt.ElseStatement is not null) {
                        var elseStmtData = CheckTypes_Statement (ref transUnit, symbols, src, retType, condStmt.ElseStatement);

                        if (thenStmtData.AlwaysReturns && elseStmtData.AlwaysReturns)
                            alwaysReturns = true;
                    }

                    return new StatementData { AlwaysReturns = alwaysReturns };*/
                }

                case ES_AstSwitchStatement switchStmt: {
                    throw new NotImplementedException ();
                    /*var exprTypeData = GenerateCode_Expression (ref transUnit, symbols, src, switchStmt.ValueExpression, null);

                    foreach (var section in switchStmt.Sections) {
                        foreach (var expr in section.Expressions) {
                            if (expr is not null) {
                                var sectionTypeData = GenerateCode_Expression (ref transUnit, symbols, src, expr, exprTypeData.Type);

                                if (!sectionTypeData.Constant) {
                                    errorList.Add (new EchelonScriptErrorMessage (
                                        src, expr.NodeBounds, ES_FrontendErrors.ConstantExprExpected
                                    ));
                                }

                                if (!CheckTypes_MustBeCompat (exprTypeData.Type, sectionTypeData.Type)) {
                                    errorList.Add (ES_FrontendErrors.GenNoCast (
                                        exprTypeData.Type->FullyQualifiedNameString, sectionTypeData.Type->FullyQualifiedNameString,
                                        src, sectionTypeData.Expr.NodeBounds
                                    ));
                                }
                            }
                        }

                        foreach (var subStmt in section.StatementsBlock)
                            GenerateCode_Statement (ref transUnit, symbols, src, retType, subStmt);
                    }

                    // TODO: Change Switch statements to check if they always return.
                    return new StatementData { AlwaysReturns = false };*/
                }

                case ES_AstGotoCaseStatement gotoCaseStmt:
                    throw new NotImplementedException ();

                case ES_AstReturnStatement retStmt: {
                    if (retType == env!.TypeVoid) {
                        if (retStmt.ReturnExpression is not null)
                            throw new CompilationException (ES_BackendErrors.FrontendError);

                        builderRef.BuildRetVoid ();
                    } else if (retStmt.ReturnExpression is not null) {
                        var exprData = GenerateCode_Expression (ref transUnit, symbols, src, retStmt.ReturnExpression, retType);

                        //TODO: CheckTypes_EnsureCompat (retType, exprData.Type, src, exprData.Expr.NodeBounds);

                        builderRef.BuildRet (GetLLVMValue (exprData.Value));
                    } else if (retType != env.TypeVoid)
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    return new StatementData { AlwaysReturns = true };
                }

                #endregion

                #region Loops

                case ES_AstLoopStatement loopStmt: {
                    throw new NotImplementedException ();
                    /*var boolType = env!.TypeBool;

                    if (loopStmt.InitializationStatement is not null)
                        GenerateCode_Statement (ref transUnit, symbols, src, retType, loopStmt.InitializationStatement);

                    if (loopStmt.ConditionExpression is not null) {
                        var condExprData = GenerateCode_Expression (ref transUnit, symbols, src, loopStmt.ConditionExpression, boolType);
                        CheckTypes_EnsureCompat (boolType, condExprData.Type, src, condExprData.Expr.NodeBounds);
                    }

                    if (loopStmt.IterationExpressions is not null) {
                        foreach (var expr in loopStmt.IterationExpressions) {
                            if (expr is not null)
                                GenerateCode_Expression (ref transUnit, symbols, src, expr, null);
                        }
                    }

                    GenerateCode_Statement (ref transUnit, symbols, src, retType, loopStmt.LoopBody);

                    return new StatementData { AlwaysReturns = false };*/
                }

                #endregion

                case ES_AstExpressionStatement exprStmt:
                    GenerateCode_Expression (ref transUnit, symbols, src, exprStmt.Expression, typeUnkn);
                    return new StatementData { AlwaysReturns = false };

                case ES_AstExpressionListStatement exprListStmt: {
                    foreach (var expr in exprListStmt.Expressions)
                        GenerateCode_Expression (ref transUnit, symbols, src, expr, typeUnkn);

                    return new StatementData { AlwaysReturns = false };
                }

                default:
                    throw new NotImplementedException ();
            }
        }
    }
}
