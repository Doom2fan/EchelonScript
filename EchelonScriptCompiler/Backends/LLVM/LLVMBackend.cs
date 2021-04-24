﻿/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Frontend;
using LLVMSharp.Interop;

namespace EchelonScriptCompiler.Backends.LLVMBackend {
    public sealed class LLVMBackendData : IBackendData {
        #region ================== Instance fields

        private LLVMExecutionEngineRef engine;
        private LLVMModuleRef module;

        #endregion

        #region ================== Instance properties

        public bool IsDisposed { get; private set; }

        #endregion

        #region ================== Constructor

        private LLVMBackendData () {
            engine = null;
            module = null;
        }

        public LLVMBackendData (LLVMExecutionEngineRef eng, LLVMModuleRef mod) {
            engine = eng;
            module = mod;
        }

        #endregion

        #region ================== IDisposable support

        ~LLVMBackendData () {
            if (!IsDisposed)
                DoDispose ();
        }

        private void DoDispose () {
            if (!IsDisposed) {
                if (engine != null) {
                    engine.Dispose ();
                    module = null;
                    engine = null;
                }

                if (module != null) {
                    module.Dispose ();
                    module = null;
                }

                IsDisposed = true;
            }
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }

    internal static class LLVMExtensions {
        public static void AddIncoming (this LLVMValueRef phi, LLVMValueRef value, LLVMBasicBlockRef block) {
            Span<LLVMValueRef> phiValue = stackalloc LLVMValueRef [1];
            Span<LLVMBasicBlockRef> phiBlock = stackalloc LLVMBasicBlockRef [1];

            phiValue [0] = value;
            phiBlock [0] = block;
            phi.AddIncoming (phiValue, phiBlock, 1);
        }
    }

    public unsafe sealed partial class LLVMCompilerBackend : ICompilerBackend, IDisposable {
        #region ================== Enums

        private enum SymbolType : int {
            None,
            Type,
            Variable,
            Function,
        }

        private enum VariableFlags {
            Using,
            Ref,
            Out,
        }

        #endregion

        #region ================== Structs

        private struct Symbol {
            #region ================== Instance fields

            public readonly SymbolType Tag;

            private readonly void* ptrData;
            private readonly VariableData varData;

            #endregion

            #region ================== Constructors

            public Symbol (ES_TypeInfo* type) {
                Tag = SymbolType.Type;

                ptrData = type;

                varData = default;
            }

            public Symbol (VariableData data) {
                Tag = SymbolType.Variable;

                varData = data;

                ptrData = null;
            }

            public Symbol (ES_FunctionData* data) {
                Tag = SymbolType.Function;

                ptrData = data;

                varData = default;
            }

            #endregion

            #region ================== Instance methods

            public ES_TypeInfo* MatchType () {
                Debug.Assert (Tag == SymbolType.Type);

                return (ES_TypeInfo*) ptrData;
            }

            public VariableData MatchVariable () {
                Debug.Assert (Tag == SymbolType.Variable);

                return varData;
            }

            public ES_FunctionData* MatchFunction () {
                Debug.Assert (Tag == SymbolType.Function);

                return (ES_FunctionData*) ptrData;
            }

            #endregion
        }

        private struct VariableData {
            public VariableFlags Flags;
            public ES_TypeInfo* Type;
            public LLVMValueRef LLVMValue;
        }

        #endregion

        public const string Error_LLVMError = "LLVM detected an error when verifying.";

        #region ================== Instance fields

        private List<EchelonScriptErrorMessage> errorList;
        private List<EchelonScriptErrorMessage> warningList;
        private List<EchelonScriptErrorMessage> infoList;

        private LLVMContextRef contextRef;
        private LLVMBuilderRef builderRef;
        private LLVMPassManagerRef funcPassManagerRef;

        private EchelonScriptEnvironment? env;
        private EchelonScriptEnvironment.Builder? envBuilder;

        private LLVMModuleRef moduleRef;

        #endregion

        #region ================== Instance properties

        public bool IsDisposed { get; private set; }

        #endregion

        #region ================== Constructors

        public LLVMCompilerBackend () {
            errorList = null!;
            warningList = null!;
            infoList = null!;

            contextRef = null;
            builderRef = null;
            funcPassManagerRef = null;

            env = null;
            envBuilder = null;

            moduleRef = null;
        }

        #endregion

        #region ================== Instance methods

        #region ICompilerBackend implementation

        public void Initialize (
            List<EchelonScriptErrorMessage> errList,
            List<EchelonScriptErrorMessage> warnList,
            List<EchelonScriptErrorMessage> infoMsgList
        ) {
            errorList = errList;
            warningList = warnList;
            infoList = infoMsgList;

            InitializeLLVM ();

            env = null;

            moduleRef = null;
        }

        public bool CompileEnvironment (EchelonScriptEnvironment environment, EchelonScriptEnvironment.Builder builder, Span<TranslationUnitData> transUnits) {
            CheckDisposed ();

            if (errorList.Count > 0)
                return false;

            try {
                BeginEnvironment (environment, builder);

                CompileCode (transUnits);

                EndEnvironment ();
            } catch {
                if (moduleRef != null) {
                    moduleRef.Dispose ();
                    moduleRef = null;
                }

                env = null;
                envBuilder = null;

                throw;
            }

            return true;
        }

        #endregion

        #region LLVM initialization

        private void InitializeLLVM () {
            contextRef = LLVM.ContextCreate ();
            builderRef = LLVM.CreateBuilderInContext (contextRef);

            InitFuncPassManager ();

            LLVM.LinkInMCJIT ();

            LLVM.InitializeNativeAsmParser ();
            LLVM.InitializeNativeAsmPrinter ();
            LLVM.InitializeNativeTarget ();
        }

        private void InitFuncPassManager () {
            funcPassManagerRef = LLVMPassManagerRef.Create ();

            funcPassManagerRef.AddVerifierPass ();

            // "Memory to registers" pass. Mandatory.
            funcPassManagerRef.AddPromoteMemoryToRegisterPass ();

            // Do simple "peephole" optimizations.
            funcPassManagerRef.AddInstructionCombiningPass ();

            // Reassociate expressions.
            funcPassManagerRef.AddReassociatePass ();

            // Eliminate common subexpressions.
            funcPassManagerRef.AddGVNPass ();

            // Simplify the control flow graph. (Deleting unreachable blocks, etc)
            funcPassManagerRef.AddCFGSimplificationPass ();
        }

        #endregion

        #region Compilation functions

        private void BeginEnvironment (EchelonScriptEnvironment environment, EchelonScriptEnvironment.Builder builder) {
            CheckDisposed ();

            if (env is not null || moduleRef != null)
                throw new CompilationException (ES_BackendErrors.EnvStarted);

            env = environment;
            envBuilder = builder;

            moduleRef = contextRef.CreateModuleWithName (System.IO.Path.GetRandomFileName ());
        }

        private void EndEnvironment () {
            CheckDisposed ();
            CheckEnvironment ();

            Debug.Assert (moduleRef != null);
            Debug.Assert (envBuilder != null);

            if (!moduleRef.TryVerify (LLVMVerifierFailureAction.LLVMPrintMessageAction, out var msg))
                throw new CompilationException ($"{Error_LLVMError}\n{msg}");

            string irTextUnopt = moduleRef.PrintToString ();

            funcPassManagerRef.Run (moduleRef);

            string irText = moduleRef.PrintToString ();

            var compilerOpts = new LLVMMCJITCompilerOptions ();
            LLVM.InitializeMCJITCompilerOptions (&compilerOpts, (UIntPtr) sizeof (LLVMMCJITCompilerOptions));
            compilerOpts.OptLevel = 0;
            compilerOpts.NoFramePointerElim = 1;

            if (!moduleRef.TryCreateMCJITCompiler (out var engine, ref compilerOpts, out msg))
                throw new CompilationException ($"{Error_LLVMError}\n${msg}");

            foreach (var namespaceData in env!.Namespaces.Values) {
                foreach (var type in namespaceData.TypeSpan) {
                    // TODO: Handle types.
                }

                foreach (var funcKVP in namespaceData.Functions) {
                    var funcId = funcKVP.Key;
                    var func = funcKVP.Value.Address;

                    using var funcNameMangled = MangleFunctionName (namespaceData, func);
                    var funcDef = moduleRef.GetNamedFunction (funcNameMangled);

                    var asd = engine.FindFunction (funcNameMangled);

                    *func = new ES_FunctionData (*func, (void*) engine.GetPointerToGlobal (funcDef));
                }
            }

            envBuilder.BackendData = new LLVMBackendData (engine, moduleRef);
            moduleRef = null;

            env = null;
            envBuilder = null;
        }

        private void BeginTranslationUnit (ref TranslationUnitData transUnit) {
            CheckDisposed ();
            CheckEnvironment ();
        }

        private void EndTranslationUnit () {
            CheckDisposed ();
            CheckEnvironment ();
            CheckTranslationUnit ();
        }

        private bool CompileCode (Span<TranslationUnitData> transUnits) {
            using var symbols = new SymbolStack<Symbol> (new Symbol ());

            foreach (ref var transUnit in transUnits) {
                BeginTranslationUnit (ref transUnit);

                foreach (ref var astUnit in transUnit.AstUnits.Span) {
                    if (!astUnit.Ast.Valid)
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    var src = astUnit.Ast.Source.Span;
                    int astStackCount = 0;

                    // Copy the AST unit's symbols
                    foreach (var scope in astUnit.Symbols) {
                        symbols.Push ();
                        astStackCount++;

                        foreach (var symbolKVP in scope) {
                            var symbolName = symbolKVP.Key;
                            var symbol = symbolKVP.Value;

                            switch (symbol.Tag) {
                                case FrontendSymbolType.Type:
                                    symbols.AddSymbol (symbolName, new Symbol (symbol.MatchType ()));
                                    break;

                                case FrontendSymbolType.Function:
                                    symbols.AddSymbol (symbolName, new Symbol (symbol.MatchFunction ()));
                                    break;

                                case FrontendSymbolType.Variable:
                                    throw new NotImplementedException ();
                            }
                        }
                    }

                    symbols.Push ();

                    foreach (var nmDef in astUnit.Ast.Namespaces) {
                        using var namespaceNameChars = nmDef.NamespaceName.ToPooledChars ();
                        var namespaceData = GetNamespace (src, astUnit.Ast.NodeBounds, namespaceNameChars);

                        Debug.Assert (namespaceData is not null);

                        ImportNamespaceSymbols (symbols, namespaceData);

                        foreach (var def in nmDef.Contents) {
                            switch (def) {
                                case ES_AstClassDefinition classDef:
                                    throw new NotImplementedException ();
                                case ES_AstStructDefinition structDef:
                                    throw new NotImplementedException ();
                                case ES_AstEnumDefinition enumDef:
                                    throw new NotImplementedException ();

                                case ES_AstFunctionDefinition funcDef: {
                                    GenerateCode_Function (ref transUnit, namespaceData, symbols, src, null, funcDef);
                                    break;
                                }
                            }
                        }
                    }

                    symbols.Pop ();

                    for (; astStackCount > 0; astStackCount--)
                        symbols.Pop ();
                }

                EndTranslationUnit ();
            }

            return true;
        }

        #endregion

        #region LLVM utils

        private LLVMTypeRef GetLLVMType (ES_TypeInfo* type) {
            switch (type->TypeTag) {
                case ES_TypeTag.Void: return contextRef.VoidType;

                case ES_TypeTag.Bool: return contextRef.Int1Type;
                case ES_TypeTag.Int: {
                    var intType = (ES_IntTypeData*) type;
                    switch (intType->IntSize) {
                        case ES_IntSize.Int8: return contextRef.Int8Type;
                        case ES_IntSize.Int16: return contextRef.Int16Type;
                        case ES_IntSize.Int32: return contextRef.Int32Type;
                        case ES_IntSize.Int64: return contextRef.Int64Type;

                        default:
                            throw new NotImplementedException ();
                    }
                }

                case ES_TypeTag.Float: {
                    var floatType = (ES_FloatTypeData*) type;

                    switch (floatType->FloatSize) {
                        case ES_FloatSize.Single: return contextRef.FloatType;
                        case ES_FloatSize.Double: return contextRef.DoubleType;

                        default:
                            throw new NotImplementedException ();
                    }
                }

                case ES_TypeTag.Function:
                case ES_TypeTag.Struct:
                case ES_TypeTag.Class:
                case ES_TypeTag.Enum:
                case ES_TypeTag.Interface:
                case ES_TypeTag.Pointer:
                case ES_TypeTag.Const:
                case ES_TypeTag.Immutable:
                case ES_TypeTag.Array:
                    throw new NotImplementedException ();

                default:
                    throw new NotImplementedException ();
            }
        }

        private LLVMTypeRef GetIntType (ES_IntSize size, bool unsigned) {
            switch (size) {
                case ES_IntSize.Int8: return contextRef.Int8Type;
                case ES_IntSize.Int16: return contextRef.Int16Type;
                case ES_IntSize.Int32: return contextRef.Int32Type;
                case ES_IntSize.Int64: return contextRef.Int64Type;

                default:
                    throw new NotImplementedException ();
            }
        }

        private LLVMTypeRef GetFloatType (ES_FloatSize size) {
            switch (size) {
                case ES_FloatSize.Single: return contextRef.FloatType;
                case ES_FloatSize.Double: return contextRef.DoubleType;

                default:
                    throw new NotImplementedException ();
            }
        }

        private LLVMValueRef GetLLVMValue (LLVMValueRef val) {
            if (val.IsAAllocaInst != null)
                return builderRef.BuildLoad (val);

            return val;
        }

        #endregion

        #region AST/Language utils

        private ES_NamespaceData? GetNamespace (ReadOnlySpan<char> src, ES_AstNodeBounds nodeBounds, ReadOnlySpan<char> namespaceStr) {
            var namespaceName = env!.IdPool.GetIdentifier (namespaceStr);

            if (!env.Namespaces.TryGetValue (namespaceName, out var namespaceData)) {
                errorList.Add (new EchelonScriptErrorMessage (src, nodeBounds, ES_BackendErrors.FrontendError));
                return null;
            }

            return namespaceData;
        }

        private void ImportNamespaceSymbols (SymbolStack<Symbol> symbols, ES_NamespaceData namespaceData) {
            foreach (var type in namespaceData.TypeSpan)
                symbols.AddSymbol (type.Address->TypeName, new Symbol (type));
            foreach (var funcKVP in namespaceData.Functions)
                symbols.AddSymbol (funcKVP.Key, new Symbol (funcKVP.Value));
        }

        private void AST_HandleImport (SymbolStack<Symbol> symbols, ReadOnlySpan<char> src, ES_AstImportStatement import) {
            using var nmNameString = import.NamespaceName.ToPooledChars ();
            var namespaceData = GetNamespace (src, import.NamespaceName.NodeBounds, nmNameString);

            if (namespaceData is null)
                return;

            if (import.ImportedNames is null || import.ImportedNames.Length == 0) {
                foreach (var type in namespaceData.TypeSpan) {
                    if (!symbols.AddSymbol (type.Address->TypeName, new Symbol (type))) {
                        errorList.Add (new EchelonScriptErrorMessage (
                            src, import.NamespaceName.NodeBounds, ES_BackendErrors.FrontendError
                        ));
                    }
                }
                foreach (var funcKVP in namespaceData.Functions) {
                    if (!symbols.AddSymbol (funcKVP.Key, new Symbol (funcKVP.Value))) {
                        errorList.Add (new EchelonScriptErrorMessage (
                            src, import.NamespaceName.NodeBounds, ES_BackendErrors.FrontendError
                        ));
                    }
                }
            } else {
                foreach (var importTk in import.ImportedNames) {
                    var name = env!.IdPool.GetIdentifier (importTk.Text.Span);

                    bool symbolFound = false;
                    bool isDuplicate = false;

                    if (!symbolFound) {
                        foreach (var typeData in namespaceData.TypeSpan) {
                            if (typeData.Address->TypeName.Equals (name)) {
                                isDuplicate = !symbols.AddSymbol (name, new Symbol (typeData));
                                symbolFound = true;
                                break;
                            }
                        }
                    }

                    if (!symbolFound) {
                        foreach (var funcKVP in namespaceData.Functions) {
                            if (funcKVP.Key.Equals (name)) {
                                isDuplicate = !symbols.AddSymbol (name, new Symbol (funcKVP.Value.Address));
                                symbolFound = true;
                                break;
                            }
                        }
                    }

                    if (!symbolFound || isDuplicate)
                        errorList.Add (new EchelonScriptErrorMessage (importTk, ES_BackendErrors.FrontendError));
                }
            }
        }

        private void AST_HandleAlias (SymbolStack<Symbol> symbols, ReadOnlySpan<char> src, ES_AstTypeAlias alias) {
            var aliasName = alias.AliasName.Text.Span;
            var aliasId = env!.IdPool.GetIdentifier (aliasName);

            if (alias.OriginalName is ES_AstTypeDeclaration_TypeName typeName) {
                ES_FunctionData* func = null;

                using var origNameChars = typeName.TypeName.ToPooledChars ();
                var origName = env.IdPool.GetIdentifier (origNameChars);
                origNameChars.Dispose ();

                if (typeName.Namespace is not null) {
                    using var namespaceName = typeName.Namespace!.ToPooledChars ();
                    var namespaceData = GetNamespace (src, typeName.Namespace.NodeBounds, namespaceName);

                    if (namespaceData is not null && namespaceData.Functions.TryGetValue (origName, out var newFunc))
                        func = newFunc;
                } else {
                    var symbol = symbols.GetSymbol (origName);

                    if (symbol.Tag == SymbolType.Function)
                        func = symbol.MatchFunction ();
                }

                if (func != null) {
                    if (!symbols.AddSymbol (aliasId, new Symbol (func)))
                        errorList.Add (new EchelonScriptErrorMessage (alias.AliasName, ES_BackendErrors.FrontendError));

                    return;
                }
            }

            var origType = GetTypeRef (alias.OriginalName);

            if (!symbols.AddSymbol (aliasId, new Symbol (origType)))
                errorList.Add (new EchelonScriptErrorMessage (alias.AliasName, ES_BackendErrors.FrontendError));
        }

        private ES_TypeInfo* GetTypeRef (ES_AstTypeDeclaration? typeDecl) {
            Debug.Assert (typeDecl is not null);

            var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;
            Debug.Assert (typeRef is not null);

            return typeRef.Reference;
        }

        #endregion

        #region Misc utils

        private void AddVariable (SymbolStack<Symbol> stack, ArrayPointer<byte> name, VariableData data) {
            stack.AddSymbol (name, new Symbol (data));
        }

        private void CheckDisposed () {
            if (IsDisposed)
                throw new ObjectDisposedException (GetType ().Name);
        }

        private void CheckEnvironment () {
            if (env is null)
                throw new CompilationException (ES_BackendErrors.EnvIsNull);
        }

        private void CheckTranslationUnit () {
            if (moduleRef.Handle == IntPtr.Zero)
                throw new CompilationException (ES_BackendErrors.TransUnitIsNull);
        }

        #endregion

        #endregion

        #region ================== IDisposable support

        ~LLVMCompilerBackend () {
            if (!IsDisposed)
                DoDispose ();
        }

        private void DoDispose () {
            if (!IsDisposed) {
                if (moduleRef != null)
                    moduleRef.Dispose ();

                if (funcPassManagerRef != null)
                    funcPassManagerRef.Dispose ();

                if (builderRef != null)
                    builderRef.Dispose ();

                if (contextRef != null)
                    contextRef.Dispose ();

                env = null;

                IsDisposed = true;
            }
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }
}