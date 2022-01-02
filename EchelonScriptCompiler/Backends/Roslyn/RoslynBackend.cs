/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Loader;
using System.Text;
using ChronosLib.Pooled;
using Collections.Pooled;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Utilities;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Emit;
using Microsoft.CodeAnalysis.Text;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed class RoslynBackendData : IBackendData {
        #region ================== Instance fields

        private EchelonScriptEnvironment environment;

        private Stream dllStream;
        private Stream symbolsStream;
        private AssemblyLoadContext? asmLoadContext;
        private Assembly? assembly;

        private Dictionary<Pointer<ES_FunctionData>, MethodInfo> functionMethodMappings;
        private Dictionary<(MethodInfo, Type), Delegate> methodDelegateMappings;

        #endregion

        #region ================== Instance properties

        public bool IsDisposed { get; private set; }

        #endregion

        #region ================== Constructors

        public RoslynBackendData (EchelonScriptEnvironment env, Stream peStream, Stream pdbStream, string name) {
            environment = env;

            dllStream = peStream;
            symbolsStream = pdbStream;

            asmLoadContext = new AssemblyLoadContext ($"{name} load context", true);
            assembly = asmLoadContext.LoadFromStream (dllStream, symbolsStream);

            SizeTypes ();

            functionMethodMappings = new Dictionary<Pointer<ES_FunctionData>, MethodInfo> ();
            methodDelegateMappings = new Dictionary<(MethodInfo, Type), Delegate> ();

            var globalStaticConsMethodInfo = GetGlobalFunction (RoslynCompilerBackend.GlobalStaticConsName);
            globalStaticConsMethodInfo.Invoke (null, null);
        }

        #endregion

        #region ================== Instance methods

        private void SizeTypes () {
            Debug.Assert (assembly is not null);

            const string namespacePrefix = RoslynCompilerBackend.NamespaceName + ".";
            foreach (var nmData in environment.Namespaces.Values) {
                foreach (var typeAddr in nmData.Types) {
                    var type = typeAddr.Address;

                    if (type->RuntimeSize > -1)
                        continue;

                    switch (type->TypeTag) {
                        case ES_TypeTag.Void:
                        case ES_TypeTag.Bool:
                        case ES_TypeTag.Int:
                        case ES_TypeTag.Float:
                        case ES_TypeTag.Reference:
                            continue;
                    }

                    var mangledName = RoslynCompilerBackend.MangleTypeName (type);
                    var roslynType = assembly.GetType (namespacePrefix + mangledName, false);

                    if (roslynType is null)
                        continue;

                    type->RuntimeSize = Marshal.SizeOf (roslynType);
                }
            }
        }

        private MethodInfo GetFunctionMethodInfo ([DisallowNull] ES_FunctionData* func) {
            if (false) {
                // TODO: Add support for member functions.
            } else {
                if (functionMethodMappings.TryGetValue (func, out var ret)) {
                    Debug.Assert (ret is not null);
                    return ret;
                }

                ret = GetGlobalFunction (RoslynCompilerBackend.MangleGlobalFunctionName (func));

                functionMethodMappings.Add (func, ret);
                return ret;
            }
        }

        private MethodInfo GetGlobalFunction (ReadOnlySpan<char> funcName) {
            Debug.Assert (assembly is not null);

            const string globalStorageTypeName
                = RoslynCompilerBackend.NamespaceName
                + "."
                + RoslynCompilerBackend.GlobalStorageTypeName;
            var globalStorageType = assembly.GetType (globalStorageTypeName);
            Debug.Assert (globalStorageType is not null);

            var methodInfo = globalStorageType.GetMethod (funcName.GetPooledString ());
            Debug.Assert (methodInfo is not null);

            return methodInfo;
        }

        public T? GetFunctionDelegate<T> ([DisallowNull] ES_FunctionData* func) where T : Delegate {
            Debug.Assert (assembly is not null);

            var delegateType = typeof (T);
            var methodInfo = GetFunctionMethodInfo (func);

            if (methodDelegateMappings.TryGetValue ((methodInfo, delegateType), out var delRet)) {
                Debug.Assert (delRet is not null);
                Debug.Assert (delRet is T);

                return delRet as T;
            }

            var ret = Delegate.CreateDelegate (typeof (T), methodInfo, false) as T;
            if (ret is null)
                return null;

            methodDelegateMappings.Add ((methodInfo, delegateType), ret);
            return ret;
        }

        #endregion

        #region ================== IDisposable support

        ~RoslynBackendData () {
            if (!IsDisposed)
                DoDispose ();
        }

        private void DoDispose () {
            if (IsDisposed)
                return;

            dllStream.Dispose ();
            symbolsStream?.Dispose ();
            asmLoadContext?.Unload ();

            asmLoadContext = null;
            assembly = null;

            IsDisposed = true;
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }

    public unsafe sealed partial class RoslynCompilerBackend : ICompilerBackend, IDisposable {
        #region ================== Constants

        internal const string NamespaceName = "EchelonScriptCode";

        #endregion

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
            public string RoslynName;

            public VariableData (ES_TypeInfo* type, VariableFlags flags, string roslynName) {
                Flags = flags;
                Type = type;
                RoslynName = roslynName;
            }
        }

        #endregion

        #region ================== Instance fields

        private List<EchelonScriptErrorMessage> errorList;
        private List<EchelonScriptErrorMessage> warningList;
        private List<EchelonScriptErrorMessage> infoList;

        private EchelonScriptEnvironment? env;
        private EchelonScriptEnvironment.Builder? envBuilder;

        #endregion

        #region ================== Instance properties

        public bool IsDisposed { get; private set; }

        #endregion

        #region ================== Constructors

        public RoslynCompilerBackend () {
            errorList = null!;
            warningList = null!;
            infoList = null!;

            env = null;
            envBuilder = null;
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

            env = null;
        }

        public bool CompileEnvironment (EchelonScriptEnvironment environment, EchelonScriptEnvironment.Builder builder, Span<TranslationUnitData> transUnits) {
            CheckDisposed ();

            if (errorList.Count > 0)
                return false;

            try {
                BeginEnvironment (environment, builder);

                CompileCode (transUnits);

                EndEnvironment ();
            } catch when (!Debugger.IsAttached) {
                env = null;
                envBuilder = null;

                throw;
            }

            return true;
        }

        #endregion

        #region Compilation functions

        private void BeginEnvironment (EchelonScriptEnvironment environment, EchelonScriptEnvironment.Builder builder) {
            CheckDisposed ();

            if (env is not null)
                throw new CompilationException (ES_BackendErrors.EnvStarted);

            env = environment;
            envBuilder = builder;
        }

        private void EndEnvironment () {
            CheckDisposed ();
            CheckEnvironment ();

            Debug.Assert (envBuilder is not null);

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
            CheckEnvironment ();
            Debug.Assert (env is not null);
            Debug.Assert (envBuilder is not null);

            using var symbols = new SymbolStack<Symbol> (new Symbol ());
            var idPool = env.IdPool;

            using var memberDefinitions = new StructPooledList<MemberDeclarationSyntax> (CL_ClearMode.Auto);
            using var globalFunctions = new StructPooledList<MemberDeclarationSyntax> (CL_ClearMode.Auto);
            using var globalStaticConsBody = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);

            // Emit types and functions with no user code.
            foreach (var namespaceKVP in env.Namespaces) {
                var namespaceData = namespaceKVP.Value;

                foreach (var typeAddr in namespaceData.Types) {
                    var typePtr = typeAddr.Address;

                    switch (typePtr->TypeTag) {
                        case ES_TypeTag.Struct:
                            //GetOrGenerateStruct ((ES_StructData*) typePtr, out _);
                            break;

                        case ES_TypeTag.Void:
                        case ES_TypeTag.Bool:
                        case ES_TypeTag.Int:
                        case ES_TypeTag.Float:
                        case ES_TypeTag.Function:
                        case ES_TypeTag.Reference:
                        case ES_TypeTag.Const:
                        case ES_TypeTag.Immutable:
                            break;

                        case ES_TypeTag.Array: {
                            var arrayType = (ES_ArrayTypeData*) typePtr;
                            var allocFuncDefs = GenerateCode_AllocArrayFunc (arrayType);

                            globalFunctions.Add (allocFuncDefs.Item1);
                            globalFunctions.Add (allocFuncDefs.Item2);

                            break;
                        }

                        default:
                            throw new NotImplementedException ("Type not implemented yet.");
                    }
                }
            }

            // Pre-emit the function prototypes/headers so we don't get errors when trying to get
            // them for calls later.
            // TODO: Handle functions in types!
            foreach (var namespaceKVP in env.Namespaces) {
                var namespaceData = namespaceKVP.Value;

                foreach (var funcKVP in namespaceData.Functions)
                    ;//GetOrGenerateFunction (namespaceData, null, funcKVP.Key, out _, out _);
            }

            // Emit the type bodies. (Needs to be done before function bodies)
            foreach (ref var transUnit in transUnits) {
                BeginTranslationUnit (ref transUnit);

                foreach (ref var astUnit in transUnit.AstUnits.Span) {
                    if (!astUnit.Ast.Valid)
                        throw new CompilationException (ES_BackendErrors.FrontendError);

                    var src = astUnit.Ast.Source.Span;
                    PopulateSymbolsFromFrontend (symbols, ref astUnit, out var astStackCount);

                    foreach (var nmDef in astUnit.Ast.Namespaces) {
                        symbols.Push ();

                        var namespaceBuilder = GetNamespace (nmDef.NamespaceName);
                        var namespaceData = namespaceBuilder.NamespaceData;
                        ImportNamespaceSymbols (symbols, namespaceBuilder);

                        foreach (var def in nmDef.Contents) {
                            switch (def) {
                                case ES_AstClassDefinition classDef:
                                    throw new NotImplementedException ("[TODO] Classes not implemented yet.");

                                case ES_AstStructDefinition structDef: {
                                    var structId = idPool.GetIdentifier (structDef.Name.Text.Span);
                                    var structBuilder = namespaceBuilder.GetStruct (structId);
                                    Debug.Assert (structBuilder is not null);
                                    var structPtr = structBuilder.StructData;

                                    var structDecl = GenerateCode_Struct (ref transUnit, ref astUnit, symbols, src, structDef, structPtr);
                                    memberDefinitions.Add (structDecl);

                                    var staticConsCall = ExpressionStatement (InvocationExpression (MemberAccessExpression (
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        IdentifierName (structDecl.Identifier),
                                        IdentifierName (DefaultStaticConsName)
                                    )));

                                    break;
                                }

                                case ES_AstEnumDefinition enumDef:
                                    break;

                                case ES_AstFunctionDefinition funcDef: {
                                    var funcDecl = GenerateCode_Function (ref transUnit, namespaceData, symbols, src, null, funcDef);
                                    globalFunctions.Add (funcDecl);
                                    break;
                                }
                            }
                        }

                        symbols.Pop ();
                    }

                    for (; astStackCount > 0; astStackCount--)
                        symbols.Pop ();
                }

                EndTranslationUnit ();
            }

            // Add the global constructor to the global functions list.
            globalFunctions.Add (
                MethodDeclaration (
                    PredefinedType (Token (SyntaxKind.VoidKeyword)),
                    GlobalStaticConsName
                ).WithModifiers (TokenList (
                    Token (SyntaxKind.PublicKeyword),
                    Token (SyntaxKind.StaticKeyword)
                )).WithBody (BlockSpan (globalStaticConsBody.Span))
            );

            // Create the global storage type and add it to the definitions list.
            memberDefinitions.Add (
                StructDeclaration (GlobalStorageTypeName)
                .WithMembers (ListSpan (globalFunctions.Span))
                .WithModifiers (TokenList (
                    Token (SyntaxKind.PublicKeyword),
                    Token (SyntaxKind.UnsafeKeyword)
                ))
            );

            // Create the namespace and compilation unit.
            var namespaceName = IdentifierName (NamespaceName);
            var namespaceDecl = NamespaceDeclaration (namespaceName).WithMembers (ListSpan (memberDefinitions.Span));

            var compUnit = CompilationUnit ().WithMembers (
                SingletonList<MemberDeclarationSyntax> (namespaceDecl)
            ).AddUsings (
                UsingDirective (NamespaceNameSyntax ("System")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon", "Data")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon", "Data", "Types")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon", "GarbageCollection")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon", "Utilities")),

                UsingDirective (NameEquals (
                    IdentifierName (nameof (MethodImplAttribute))),
                    NamespaceNameSyntax ("System", "Runtime", "CompilerServices", nameof (MethodImplAttribute))
                ),
                UsingDirective (NameEquals (
                    IdentifierName (nameof (MethodImplOptions))),
                    NamespaceNameSyntax ("System", "Runtime", "CompilerServices", nameof (MethodImplOptions))
                )
            ).NormalizeWhitespace ();

            // Create the syntax tree and parse options.
            const string CsSourceFilePath = "EchelonScriptCodeC#.cs";
            var csParseOptions = new CSharpParseOptions (LanguageVersion.Latest, DocumentationMode.None, SourceCodeKind.Regular);
            var csSyntaxTree = SyntaxTree (compUnit, csParseOptions, CsSourceFilePath, Encoding.UTF8);

            // Get the reference DLLs.
            static void GetReferenceAPIs<T> (T set, Assembly assembly) where T : ISet<Assembly> {
                if (assembly.IsDynamic)
                    return;

                Debug.Assert (!string.IsNullOrWhiteSpace (assembly.Location));
                if (set.Contains (assembly))
                    return;

                set.Add (assembly);

                foreach (var refAssemblyName in assembly.GetReferencedAssemblies ())
                    GetReferenceAPIs (set, Assembly.Load (refAssemblyName));
            }

            using var refApisSet = new PooledSet<Assembly> ();
            GetReferenceAPIs (refApisSet, typeof (ES_FunctionData).Assembly);

            var refApis = new MetadataReference [refApisSet.Count];
            var refApisCount = 0;
            foreach (var assembly in refApisSet)
                refApis [refApisCount++] = MetadataReference.CreateFromFile (assembly.Location);

            // Compile the C# code and report errors.
            var csCompOptions = new CSharpCompilationOptions (OutputKind.DynamicallyLinkedLibrary,
                optimizationLevel: OptimizationLevel.Release,
                checkOverflow: false, allowUnsafe: true,
                concurrentBuild: true, deterministic: false
            ).WithAllowUnsafe (true);
            const string assemblyName = "EchelonScriptCodeAssembly"; // TODO: Generate a proper name for the assembly.
            var csComp = CSharpCompilation.Create (assemblyName, new [] { csSyntaxTree }, refApis, csCompOptions);

            const string error_CSError = "The C# compiler reported an error when compiling: {errMsg}";
            foreach (var diagMsg in csComp.GetDiagnostics ()) {
                if (diagMsg.Severity != DiagnosticSeverity.Error)
                    continue;

                errorList.Add (new (error_CSError.Replace ("{errMsg}", diagMsg.GetMessage ()), 0, 0, 0, 0));
            }

            if (errorList.Count > 0)
                return false;

            // Emit the DLL and report errors.
            using var dllStream = new MemoryStream ();
            using var pdbStream = new MemoryStream ();
            var embeddedTexts = new [] {
                EmbeddedText.FromSource (CsSourceFilePath, SourceText.From (csSyntaxTree.ToString (), Encoding.UTF8))
            };

            var emitOptions = new EmitOptions (
                metadataOnly: false, debugInformationFormat: DebugInformationFormat.PortablePdb
            );
            var emitResult = csComp.Emit (dllStream, pdbStream, embeddedTexts: embeddedTexts, options: emitOptions);

            dllStream.Seek (0, SeekOrigin.Begin);
            pdbStream.Seek (0, SeekOrigin.Begin);

            Debug.Assert (emitResult.Success || emitResult.Diagnostics.Length > 0);
            const string error_EmitError = "Roslyn reported an error when emitting the DLL: {errMsg}";
            foreach (var diagMsg in emitResult.Diagnostics) {
                if (diagMsg.Severity != DiagnosticSeverity.Error)
                    continue;

                errorList.Add (new (error_EmitError.Replace ("{errMsg}", diagMsg.GetMessage ()), 0, 0, 0, 0));
            }

            if (!emitResult.Success && errorList.Count < 1)
                errorList.Add (new ("Unknown Roslyn Emit error. Success was false but no errors reported.", 0, 0, 0, 0));

            if (errorList.Count > 0)
                return false;

            // Create the backend data and load the assembly.

            envBuilder.BackendData = new RoslynBackendData (env, dllStream, pdbStream, assemblyName);

            return true;
        }

        #endregion

        #region Roslyn utils

        private ExpressionSyntax PointerLiteral (void* value, TypeSyntax? type = null) {
            if (type is null)
                type = PointerType (PredefinedType (Token (SyntaxKind.VoidKeyword)));

            SyntaxToken litToken;

            switch (sizeof (void*)) {
                case sizeof (uint):
                    litToken = Literal ((uint) value);
                    break;

                case sizeof (ulong):
                    litToken = Literal ((ulong) value);
                    break;

                default:
                    throw new NotImplementedException ("Pointer size not implemented.");
            }

            return CastExpression (
                type,
                LiteralExpression (SyntaxKind.NumericLiteralExpression, litToken)
            );
        }

        private NameSyntax NamespaceNameSyntax (params string [] name) {
            Debug.Assert (name.Length > 0);

            NameSyntax ret = IdentifierName (name [0]);
            for (int i = 1; i < name.Length; i++)
                ret = QualifiedName (ret, IdentifierName (name [i]));

            return ret;
        }

        private SeparatedSyntaxList<TNode> SeparatedListSpan<TNode> (ReadOnlySpan<SyntaxNodeOrToken> nodes) where TNode : SyntaxNode {
            var nodesList = new SyntaxNodeOrTokenList ();
            foreach (var node in nodes)
                nodesList = nodesList.Add (node);

            return SeparatedList<TNode> (nodesList);
        }

        private SyntaxList<TNode> ListSpan<TNode> (Span<TNode> statements) where TNode : SyntaxNode
            => ListSpan ((ReadOnlySpan<TNode>) statements);

        private SyntaxList<TNode> ListSpan<TNode> (ReadOnlySpan<TNode> nodes) where TNode : SyntaxNode {
            var nodesList = new SyntaxList<TNode> ();
            foreach (var node in nodes)
                nodesList = nodesList.Add (node);

            return List (nodesList);
        }

        private BlockSyntax BlockSpan (ReadOnlySpan<StatementSyntax> statements) {
            var stmtsList = new SyntaxList<StatementSyntax> ();
            foreach (var stmt in statements)
                stmtsList = stmtsList.Add (stmt);

            return Block (stmtsList);
        }

        #endregion

        #region AST/Language utils

        private ES_NamespaceData.Builder GetNamespace (ES_AstDottableIdentifier namespaceName) {
            using var namespaceNameChars = namespaceName.ToPooledChars ();
            return GetNamespace (namespaceNameChars);
        }

        private ES_NamespaceData.Builder GetNamespace (ReadOnlySpan<char> namespaceStr) {
            var namespaceName = env!.IdPool.GetIdentifier (namespaceStr);

            if (!envBuilder!.NamespaceBuilders.TryGetValue (namespaceName, out var namespaceBuilder))
                throw new CompilationException (ES_BackendErrors.FrontendError);

            return namespaceBuilder;
        }

        private void ImportNamespaceSymbols (SymbolStack<Symbol> symbols, ES_NamespaceData.Builder namespaceBuilder) {
            var namespaceData = namespaceBuilder.NamespaceData;

            foreach (var type in namespaceData.Types)
                symbols.AddSymbol (type.Address->Name.TypeName, new Symbol (type));
            foreach (var funcKVP in namespaceData.Functions)
                symbols.AddSymbol (funcKVP.Key, new Symbol (funcKVP.Value));
        }

        private void AST_HandleImport (SymbolStack<Symbol> symbols, ES_AstImportStatement import) {
            var namespaceBuilder = GetNamespace (import.NamespaceName);
            var namespaceData = namespaceBuilder.NamespaceData;

            if (namespaceData is null)
                return;

            if (import.ImportedNames is null || import.ImportedNames.Length == 0) {
                foreach (var type in namespaceData.Types) {
                    if (!symbols.AddSymbol (type.Address->Name.TypeName, new Symbol (type)))
                        throw new CompilationException (ES_BackendErrors.FrontendError);
                }
                foreach (var funcKVP in namespaceData.Functions) {
                    if (!symbols.AddSymbol (funcKVP.Key, new Symbol (funcKVP.Value)))
                        throw new CompilationException (ES_BackendErrors.FrontendError);
                }
            } else {
                foreach (var importTk in import.ImportedNames) {
                    var name = env!.IdPool.GetIdentifier (importTk.Text.Span);

                    bool symbolFound = false;
                    bool isDuplicate = false;

                    if (!symbolFound) {
                        foreach (var typeData in namespaceData.Types) {
                            if (typeData.Address->Name.TypeName.Equals (name)) {
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
                        throw new CompilationException (ES_BackendErrors.FrontendError);
                }
            }
        }

        private void AST_HandleAlias (SymbolStack<Symbol> symbols, ES_AstTypeAlias alias) {
            var aliasName = alias.AliasName.Text.Span;
            var aliasId = env!.IdPool.GetIdentifier (aliasName);

            var origType = GetTypeRef (alias.OriginalName);

            if (!symbols.AddSymbol (aliasId, new Symbol (origType)))
                throw new CompilationException (ES_BackendErrors.FrontendError);
        }

        private ES_TypeInfo* GetTypeRef (ES_AstTypeDeclaration? typeDecl) {
            Debug.Assert (typeDecl is not null);

            var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;
            Debug.Assert (typeRef is not null);

            if (typeRef is null)
                throw new CompilationException (ES_BackendErrors.FrontendError);

            return typeRef.Reference;
        }

        #endregion

        #region Misc utils

        private void PopulateSymbolsFromFrontend (SymbolStack<Symbol> symbols, ref AstUnitData astUnit, out int astStackCount) {
            astStackCount = 0;

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
                            throw new NotImplementedException ("?");
                    }
                }
            }
        }

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
            if (false)
                throw new CompilationException (ES_BackendErrors.TransUnitIsNull);
        }

        #endregion

        #endregion

        #region ================== IDisposable support

        ~RoslynCompilerBackend () {
            if (!IsDisposed)
                DoDispose ();
        }

        private void DoDispose () {
            if (IsDisposed)
                return;

            env = null;
            IsDisposed = true;
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }
}
