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
using System.Diagnostics.CodeAnalysis;
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Frontend;
using LLVMSharp.Interop;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Backends.LLVMBackend {
    public unsafe sealed partial class LLVMCompilerBackend {
        public ref struct FunctionInfo {
            public ES_FunctionData* Data;
            public ES_FunctionPrototypeData* Type;

            public LLVMValueRef Definition;

            public LLVMValueRef RetValue;
            public LLVMBasicBlockRef RetBlock;

            public LLVMBasicBlockRef? ContinueBlock;
            public LLVMBasicBlockRef? BreakBlock;
        }

        public struct StatementData {
            public bool AlwaysReturns;
        }

        private PooledArray<char> MangleFunctionType (ES_FunctionPrototypeData* type) {
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // Add the return type.
            mangleChars.AddRange ("Ret");
            mangleChars.AddRange (type->ReturnType->Name.NamespaceNameString);
            mangleChars.AddRange ("__");
            mangleChars.AddRange (type->ReturnType->Name.TypeNameString);

            // Add the arg types.
            foreach (var arg in type->ArgumentsList.Span) {
                mangleChars.AddRange ("_");

                switch (arg.ArgType) {
                    case ES_ArgumentType.Normal: break;
                    case ES_ArgumentType.In: break;
                    case ES_ArgumentType.Out: mangleChars.AddRange ("out"); break;
                    case ES_ArgumentType.Ref: mangleChars.AddRange ("ref"); break;
                }

                mangleChars.AddRange (arg.ValueType->Name.NamespaceNameString);
                mangleChars.AddRange ("__");
                mangleChars.AddRange (arg.ValueType->Name.TypeNameString);
            }

            return mangleChars.ToPooledArray ();
        }

        private PooledArray<char> MangleFunctionName ([DisallowNull] ES_FunctionData* func) {
            // Sample name: "System.Math__FMath.Sin"
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // The namespace.
            mangleChars.AddRange (func->Name.NamespaceNameString);

            // The mangled namespace separator.
            mangleChars.AddRange ("__");

            // The function name.
            mangleChars.AddRange (func->Name.TypeNameString);

            /*// Add the type's mangled name.
            mangleChars.AddRange ("_");
            using var typeMangle = MangleFunctionType (func->FunctionType);
            mangleChars.AddRange (typeMangle);*/

            return mangleChars.ToPooledArray ();
        }

        private void InitializeVariable (ES_TypeInfo* varType, LLVMValueRef variable) {
            Debug.Assert (variable.IsPointer ());

            switch (varType->TypeTag) {
                case ES_TypeTag.Bool:
                case ES_TypeTag.Int: {
                    var val = LLVMValueRef.CreateConstInt (variable.TypeOf.ElementType, 0);
                    builderRef.BuildStore (val, variable);
                    break;
                }

                case ES_TypeTag.Float: {
                    var val = LLVMValueRef.CreateConstReal (variable.TypeOf.ElementType, 0);
                    builderRef.BuildStore (val, variable);
                    break;
                }

                default:
                    throw new NotImplementedException ("Variable type not implemented.");
            }
        }

        private void GetOrGenerateFunction (
            ES_NamespaceData? namespaceData, ES_TypeInfo* parentType, ArrayPointer<byte> funcId,
            out ES_FunctionData* funcData, out LLVMValueRef funcDef
        ) {
            if (namespaceData is not null) {
                if (!namespaceData.Functions.TryGetValue (funcId, out var ptr))
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                funcData = ptr;
            } else if (parentType is not null) {
                throw new NotImplementedException ("[TODO] Member functions not implemented yet.");
            } else
                throw new CompilationException (ES_BackendErrors.FrontendError);

            Debug.Assert (funcData is not null);
            var funcType = funcData->FunctionType;

            using var argsArr = PooledArray<LLVMTypeRef>.GetArray (funcType->ArgumentsList.Length);
            int argNum = 0;
            foreach (ref var arg in funcType->ArgumentsList.Span) {
                if (arg.ArgType != ES_ArgumentType.Normal)
                    throw new NotImplementedException ("[TODO] Argument type not implemented yet.");

                argsArr.Span [argNum++] = GetLLVMType (arg.ValueType);
            }

            var retType = funcData->FunctionType->ReturnType;
            var funcTypeRef = LLVMTypeRef.CreateFunction (GetLLVMType (retType), argsArr, false);
            argsArr.Dispose ();

            if (namespaceData is not null) {
                using var funcName = MangleFunctionName (funcData);

                funcDef = moduleRef.GetNamedFunction (funcName);
                if (funcDef == null)
                    funcDef = moduleRef.AddFunction (funcName, funcTypeRef);
            } else if (parentType is not null) {
                throw new NotImplementedException ("[TODO] Member functions not implemented yet.");
            } else
                throw new CompilationException (ES_BackendErrors.FrontendError);
        }

        private bool GenerateCode_Function (
            ref TranslationUnitData transUnit, ES_NamespaceData? namespaceData,
            SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            ES_TypeInfo* parentType, ES_AstFunctionDefinition funcDef
        ) {
            Debug.Assert (env is not null);
            Debug.Assert (namespaceData is not null || parentType is not null);

            symbols.Push ();

            var funcInfo = new FunctionInfo ();

            GetOrGenerateFunction (
                namespaceData, parentType, env.IdPool.GetIdentifier (funcDef.Name.Text.Span),
                out funcInfo.Data, out funcInfo.Definition
            );

            funcInfo.Type = funcInfo.Data->FunctionType;
            var retType = funcInfo.Type->ReturnType;

            funcInfo.Definition.Linkage = LLVMLinkage.LLVMExternalLinkage;

            funcInfo.Definition.AppendBasicBlock ("entry");
            funcInfo.RetBlock = funcInfo.Definition.AppendBasicBlock ("_retValBlock");

            builderRef.PositionAtEnd (funcInfo.Definition.EntryBasicBlock);
            if (retType->TypeTag != ES_TypeTag.Void)
                funcInfo.RetValue = builderRef.BuildAlloca (GetLLVMType (retType), "_retVal");

            builderRef.PositionAtEnd (funcInfo.RetBlock);
            if (retType->TypeTag != ES_TypeTag.Void)
                builderRef.BuildRet (GetLLVMValue (funcInfo.RetValue));
            else
                builderRef.BuildRetVoid ();

            builderRef.PositionAtEnd (funcInfo.Definition.EntryBasicBlock);
            for (uint i = 0; i < funcInfo.Definition.ParamsCount; i++) {
                var param = funcInfo.Definition.GetParam (i);
                ref var argTypeInfo = ref funcInfo.Type->ArgumentsList.Span [(int) i];
                ref var argData = ref funcInfo.Data->Arguments.Span [(int) i];

                var argFlags = (VariableFlags) 0;

                switch (argTypeInfo.ArgType) {
                    case ES_ArgumentType.Normal:
                        var paramAlloca = builderRef.BuildAlloca (param.TypeOf, param.Name);
                        builderRef.BuildStore (param, paramAlloca);
                        param = paramAlloca;
                        break;

                    default:
                        throw new NotImplementedException ("Argument type not implemented.");
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

            Debug.Assert (funcDef.Statement is not null);
            Debug.Assert (funcDef.Statement.Endpoint is null);
            if (funcDef.ExpressionBody) {
                Debug.Assert (funcDef.Statement is ES_AstExpressionStatement);

                var exprExpType = retType->TypeTag != ES_TypeTag.Void ? retType : env.TypeUnknownValue;
                var exprStmt = (funcDef.Statement as ES_AstExpressionStatement)!;
                var exprData = GenerateCode_Expression (ref transUnit, symbols, src, exprStmt.Expression, exprExpType);

                if (exprData.TypeInfo is not null || exprData.Function is not null)
                    throw new CompilationException (ES_BackendErrors.FrontendError);

                if (retType->TypeTag != ES_TypeTag.Void) {
                    GenerateCode_EnsureImplicitCompat (ref exprData, retType);
                    builderRef.BuildStore (GetLLVMValue (exprData.Value), funcInfo.RetValue);
                }

                builderRef.BuildBr (funcInfo.RetBlock);
            } else {
                var stmtData = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, funcDef.Statement);

                if (!stmtData.AlwaysReturns) {
                    if (retType->TypeTag == ES_TypeTag.Void)
                        builderRef.BuildBr (funcInfo.RetBlock);
                    else
                        throw new CompilationException (ES_BackendErrors.FrontendError);
                }
            }

            symbols.Pop ();

            return true;
        }

        private StatementData GenerateCode_Statement (
            ref TranslationUnitData transUnit, SymbolStack<Symbol> symbols, ReadOnlySpan<char> src,
            FunctionInfo funcInfo, ES_TypeInfo* retType, ES_AstStatement stmt
        ) {
            Debug.Assert (stmt is not null);
            var idPool = env!.IdPool;
            var typeUnkn = env.TypeUnknownValue;

            switch (stmt) {
                case ES_AstEmptyStatement:
                    return new StatementData { AlwaysReturns = false };

                case ES_AstLabeledStatement labelStmt: {
                    throw new NotImplementedException ("[TODO] Labels not implemented yet.");
                    //return GenerateCode_Statement (ref transUnit, symbols, src, retType, labelStmt.Statement);
                }

                case ES_AstBlockStatement blockStmt: {
                    symbols.Push ();

                    bool alwaysReturns = false;

                    ES_AstStatement? subStmt = blockStmt.Statement;
                    while (subStmt is not null) {
                        var subStmtData = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, subStmt);

                        if (subStmtData.AlwaysReturns) {
                            alwaysReturns = true;
                            break;
                        }

                        subStmt = subStmt.Endpoint;
                    }

                    symbols.Pop ();
                    return new StatementData { AlwaysReturns = alwaysReturns };
                }

                #region Symbol definition

                case ES_AstImportStatement importStmt:
                    AST_HandleImport (symbols, importStmt);
                    return new StatementData { AlwaysReturns = false };

                case ES_AstTypeAlias aliasStmt:
                    AST_HandleAlias (symbols, aliasStmt);
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
                                GenerateCode_EnsureImplicitCompat (ref initExprData, varData.Type);
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
                            builderRef.BuildStore (GetLLVMValue (initExprData.Value), varData.LLVMValue);
                        else
                            InitializeVariable (varData.Type, varData.LLVMValue);
                    }

                    return new StatementData { AlwaysReturns = false };
                }

                #endregion

                #region Jumps

                case ES_AstConditionalStatement condStmt: {
                    var boolType = env!.TypeBool;

                    bool hasElse = condStmt.ElseStatement is not null;
                    bool alwaysReturns = false;

                    // Generate the condition.
                    var condExpr = GenerateCode_Expression (ref transUnit, symbols, src, condStmt.ConditionExpression, boolType);
                    GenerateCode_EnsureImplicitCompat (ref condExpr, boolType);

                    // Create the blocks.
                    var thenBlock = funcInfo.Definition.AppendBasicBlock ("if_then");
                    var endBlock = funcInfo.Definition.AppendBasicBlock ("if_end");
                    var elseBlock = hasElse ? funcInfo.Definition.AppendBasicBlock ("if_else") : endBlock;

                    builderRef.BuildCondBr (condExpr.Value, thenBlock, elseBlock);

                    // Generate the then block.
                    builderRef.PositionAtEnd (thenBlock);
                    var thenStmtData = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, condStmt.ThenStatement);
                    if (!thenStmtData.AlwaysReturns)
                        builderRef.BuildBr (endBlock);

                    // Generate the else block (if any).
                    if (hasElse) {
                        builderRef.PositionAtEnd (elseBlock);
                        var elseStmtData = GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, condStmt.ElseStatement!);

                        if (!elseStmtData.AlwaysReturns)
                            builderRef.BuildBr (endBlock);

                        alwaysReturns = thenStmtData.AlwaysReturns && elseStmtData.AlwaysReturns;
                    }

                    // Generate the merge/end block.
                    if (!alwaysReturns)
                        builderRef.PositionAtEnd (endBlock);
                    else
                        endBlock.Delete ();

                    return new StatementData { AlwaysReturns = alwaysReturns };
                }

                case ES_AstSwitchStatement switchStmt: {
                    throw new NotImplementedException ("[TODO] Switches not implemented yet.");
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
                    throw new NotImplementedException ("[TODO] 'goto case' not implemented yet.");

                case ES_AstReturnStatement retStmt: {
                    if (retType->TypeTag == ES_TypeTag.Void) {
                        if (retStmt.ReturnExpression is not null)
                            throw new CompilationException (ES_BackendErrors.FrontendError);

                        builderRef.BuildBr (funcInfo.RetBlock);
                    } else if (retStmt.ReturnExpression is not null) {
                        var exprData = GenerateCode_Expression (ref transUnit, symbols, src, retStmt.ReturnExpression, retType);

                        GenerateCode_EnsureImplicitCompat (ref exprData, retType);

                        if (retType->TypeTag != ES_TypeTag.Void)
                            builderRef.BuildStore (GetLLVMValue (exprData.Value), funcInfo.RetValue);
                        builderRef.BuildBr (funcInfo.RetBlock);
                    } else if (retType->TypeTag != ES_TypeTag.Void)
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    return new StatementData { AlwaysReturns = true };
                }

                #endregion

                #region Loops

                case ES_AstLoopStatement loopStmt: {
                    var boolType = env!.TypeBool;

                    symbols.Push ();

                    var initBlock = funcInfo.Definition.AppendBasicBlock ("loop_init");
                    var condBlock = funcInfo.Definition.AppendBasicBlock ("loop_cond");
                    var bodyBlock = funcInfo.Definition.AppendBasicBlock ("loop_body");
                    var iterBlock = funcInfo.Definition.AppendBasicBlock ("loop_iter");
                    var endBlock  = funcInfo.Definition.AppendBasicBlock ("loop_end");

                    /* Emit the jump to the init block. */
                    builderRef.BuildBr (initBlock);

                    /* Emit the init block. (emit variable inits -> jump to condition block) */
                    builderRef.PositionAtEnd (initBlock);

                    if (loopStmt.InitializationStatement is not null)
                        GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, loopStmt.InitializationStatement);

                    builderRef.BuildBr (condBlock);

                    /* Emit the condition block. */
                    builderRef.PositionAtEnd (condBlock);

                    if (loopStmt.ConditionExpression is not null) {
                        var condExprData = GenerateCode_Expression (ref transUnit, symbols, src, loopStmt.ConditionExpression, boolType);
                        GenerateCode_EnsureImplicitCompat (ref condExprData, boolType);

                        builderRef.BuildCondBr (condExprData.Value, bodyBlock, endBlock);
                    } else
                        builderRef.BuildBr (bodyBlock);

                    /* Emit the iteration block. */
                    builderRef.PositionAtEnd (iterBlock);

                    if (loopStmt.IterationExpressions is not null) {
                        foreach (var expr in loopStmt.IterationExpressions) {
                            if (expr is not null)
                                GenerateCode_Expression (ref transUnit, symbols, src, expr, null);
                        }
                    }

                    builderRef.BuildBr (condBlock);

                    /* Emit the body block. */
                    builderRef.PositionAtEnd (bodyBlock);

                    funcInfo.ContinueBlock = condBlock;
                    funcInfo.BreakBlock = endBlock;

                    Debug.Assert (loopStmt.LoopBody is not null);
                    Debug.Assert (loopStmt.LoopBody.Endpoint is null);

                    GenerateCode_Statement (ref transUnit, symbols, src, funcInfo, retType, loopStmt.LoopBody);

                    builderRef.BuildBr (iterBlock);

                    builderRef.PositionAtEnd (endBlock);

                    symbols.Pop ();

                    return new StatementData { AlwaysReturns = false };
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
                    throw new NotImplementedException ("Expression type not implemented.");
            }
        }
    }
}
