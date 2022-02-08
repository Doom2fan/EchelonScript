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
using CommunityToolkit.HighPerformance;
using EchelonScriptCommon;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.GarbageCollection;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon.IR;
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
        private const string TypesNamespacePrefix = RoslynCompilerBackend.NamespaceName + ".";

        #region ================== Instance fields

        private EchelonScriptEnvironment environment;

        private Stream dllStream;
        private Stream symbolsStream;
        private AssemblyLoadContext? asmLoadContext;
        private Assembly? assembly;

        private Dictionary<Pointer<ES_FunctionData>, MethodInfo> functionMethodMappings;
        private Dictionary<(MethodInfo, Type), Delegate> methodDelegateMappings;

        private ArrayPointer<nint> rootsArray;

        #endregion

        #region ================== Instance properties

        public bool IsDisposed { get; private set; }

        #endregion

        #region ================== Constructors

        internal RoslynBackendData (
            ref RoslynCompilerBackend.PassData passData,
            ArrayPointer<nint> rootsArr,
            Stream peStream, Stream pdbStream, string name
        ) {
            environment = passData.Env;

            rootsArray = rootsArr;

            dllStream = peStream;
            symbolsStream = pdbStream;

            asmLoadContext = new AssemblyLoadContext ($"{name} load context", true);
            assembly = asmLoadContext.LoadFromStream (dllStream, symbolsStream);

            functionMethodMappings = new Dictionary<Pointer<ES_FunctionData>, MethodInfo> ();
            methodDelegateMappings = new Dictionary<(MethodInfo, Type), Delegate> ();

            foreach (var map in passData.Mappings) {
                switch (map.Type) {
                    case RoslynCompilerBackend.MappingType.Function: {
                        if (!map.TryGetFunction (out var funcData, out var roslynName))
                            Debug.Fail ("???");

                        Debug.Assert (roslynName is not null);
                        functionMethodMappings [funcData] = GetGlobalFunction (roslynName);

                        break;
                    }

                    default:
                        throw new NotImplementedException ("Mapping type not implemented.");
                }
            }

            var globalStaticConsMethodInfo = GetGlobalFunction (ES_Constants.GlobalStaticConstructorName);
            globalStaticConsMethodInfo.Invoke (null, null);
        }

        #endregion

        #region ================== Instance methods

        private MethodInfo? GetFunctionMethodInfo ([DisallowNull] ES_FunctionData* func) {
            if (false) {
                // TODO: Add support for member functions.
            } else {
                if (!functionMethodMappings.TryGetValue (func, out var ret))
                    return null;

                Debug.Assert (ret is not null);
                return ret;
            }
        }

        private MethodInfo GetGlobalFunction (ReadOnlySpan<char> funcName) => GetGlobalFunction (funcName.GetPooledString ());

        private MethodInfo GetGlobalFunction (string funcName) {
            Debug.Assert (assembly is not null);

            const string globalStorageTypeName
                = RoslynCompilerBackend.NamespaceName
                + "."
                + RoslynCompilerBackend.GlobalStorageTypeName;
            var globalStorageType = assembly.GetType (globalStorageTypeName);
            Debug.Assert (globalStorageType is not null);

            var methodInfo = globalStorageType.GetMethod (funcName);
            Debug.Assert (methodInfo is not null);

            return methodInfo;
        }

        public T? GetFunctionDelegate<T> ([DisallowNull] ES_FunctionData* func) where T : Delegate {
            Debug.Assert (assembly is not null);

            var delegateType = typeof (T);
            var methodInfo = GetFunctionMethodInfo (func);

            if (methodInfo is null)
                return null;

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

            if (rootsArray.Elements != null)
                ES_GarbageCollector.RemoveRoots ((ES_ObjectAddress**) rootsArray.Elements);

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

        internal enum MappingType {
            Function,
        }

        #endregion

        #region ================== Structs

        internal struct BackendMapping {
            public MappingType Type { get; private init; }
            private void* valuePointer { get; init; }
            private string? valueString { get; init; }

            public static BackendMapping Function (ES_FunctionData* funcData, string roslynName) {
                return new BackendMapping {
                    Type = MappingType.Function,
                    valuePointer = funcData,
                    valueString = roslynName,
                };
            }

            public bool TryGetFunction (out ES_FunctionData* funcData, out string? roslynName) {
                if (Type != MappingType.Function) {
                    funcData = null;
                    roslynName = null;
                    return false;
                }

                funcData = (ES_FunctionData*) valuePointer;
                roslynName = valueString;
                return true;
            }
        }

        internal ref struct PassData {
            public EchelonScriptEnvironment Env { get; init; }
            public EchelonScriptEnvironment.Builder EnvBuilder { get; init; }

            public List<EchelonScriptErrorMessage> ErrorList { get; init; }
            public List<EchelonScriptErrorMessage> WarnList { get; init; }
            public List<EchelonScriptErrorMessage> InfoList { get; init; }

            public ChronosLib.Unmanaged.IMemoryManager MemoryManager => EnvBuilder.MemoryManager;
            public UnmanagedIdentifierPool IdPool => Env.IdPool;

            public Ref<StructPooledList<BackendMapping>> MappingsInit { private get; init; }
            public ref StructPooledList<BackendMapping> Mappings => ref MappingsInit.Value;

            public Ref<StructPooledList<MemberDeclarationSyntax>> GlobalMembersInit { private get; init; }
            public ref StructPooledList<MemberDeclarationSyntax> GlobalMembers => ref GlobalMembersInit.Value;

            public Ref<StructPooledList<MemberDeclarationSyntax>> TypesInit { private get; init; }
            public ref StructPooledList<MemberDeclarationSyntax> Types => ref TypesInit.Value;

            public Dictionary<ArrayPointer<byte>, ESIR_StaticVariable> StaticVariables { get; init; }
            public Dictionary<ArrayPointer<byte>, ESIR_Function> Functions { get; init; }
            public Dictionary<Pointer<ES_TypeInfo>, ESIR_Struct> Structs { get; init; }

            public SymbolStack<LabelSymbol> LabelStack { get; init; }
        }

        #endregion

        #region ================== Instance fields

        private List<EchelonScriptErrorMessage> errorList;
        private List<EchelonScriptErrorMessage> warningList;
        private List<EchelonScriptErrorMessage> infoList;

        #endregion

        #region ================== Instance properties

        public bool IsDisposed { get; private set; }

        #endregion

        #region ================== Constructors

        public RoslynCompilerBackend () {
            errorList = null!;
            warningList = null!;
            infoList = null!;
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
        }

        public bool CompileEnvironment (EchelonScriptEnvironment environment, EchelonScriptEnvironment.Builder builder, ESIR_Tree irTree) {
            CheckDisposed ();

            if (errorList.Count > 0)
                return false;

            var mappings = new StructPooledList<BackendMapping> (CL_ClearMode.Auto);
            var members = new StructPooledList<MemberDeclarationSyntax> (CL_ClearMode.Auto);
            var types = new StructPooledList<MemberDeclarationSyntax> (CL_ClearMode.Auto);
            using var labelStack = new SymbolStack<LabelSymbol> (new LabelSymbol ());

            try {
                var passData = new PassData {
                    Env = environment,
                    EnvBuilder = builder,

                    ErrorList = errorList,
                    WarnList = warningList,
                    InfoList = infoList,

                    MappingsInit = new (ref mappings),
                    GlobalMembersInit = new (ref members),
                    TypesInit = new (ref types),

                    StaticVariables = new (),
                    Functions = new (),
                    Structs = new (),

                    LabelStack = labelStack,
                };

                return CompileCode (ref passData, irTree);
            } catch when (!Debugger.IsAttached) {
                mappings.Dispose ();
                members.Dispose ();
                types.Dispose ();

                throw;
            }
        }

        #endregion

        #region Compilation functions

        private static bool CompileCode (ref PassData passData, ESIR_Tree irTree) {
            foreach (var staticVar in irTree.StaticVariables.Elements) {
                if (!passData.StaticVariables.TryAdd (staticVar.Name, staticVar))
                    throw new CompilationException ("Cannot have multiple static variables with the same name.");
            }

            foreach (var func in irTree.Functions.Elements) {
                if (!passData.Functions.TryAdd (func.Name, func))
                    throw new CompilationException ("Cannot have multiple functions with the same name.");
            }

            foreach (var structDef in irTree.Structs.Elements) {
                if (!passData.Structs.TryAdd (structDef.Type, structDef))
                    throw new CompilationException ("Cannot have multiple structs with the same name.");
            }

            // Emit types and functions with no user code.
            foreach (var namespaceKVP in passData.Env.Namespaces) {
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
                            CompileCode_Array (ref passData, arrayType);
                            break;
                        }

                        default:
                            throw new NotImplementedException ("Type not implemented yet.");
                    }
                }
            }

            if (!CompileStaticVariables (ref passData, irTree.StaticVariables, out var gcRoots))
                return false;

            foreach (var structDef in irTree.Structs.Elements)
                CompileStruct (ref passData, structDef);

            foreach (var func in irTree.Functions.Elements)
                CompileFunction (ref passData, func);

            if (!CompileCSharp (ref passData, gcRoots))
                return false;

            return passData.ErrorList.Count > 0;
        }

        private static bool CompileStaticVariables (ref PassData passData, ESIR_List<ESIR_StaticVariable> staticVars, out ArrayPointer<nint> roots) {
            var alignmentVal = sizeof (nint);

            var totalMem = 0;
            using var rootsList = new StructPooledList<nint> (CL_ClearMode.Auto);

            var staticVarsMemRef = CompileCode_StaticVarsMem ();
            foreach (var staticVar in staticVars.Elements) {
                var varType = staticVar.Type.Pointer;
                var varRoslynType = GetRoslynType (varType);

                var baseOffs = (totalMem + alignmentVal - 1) / alignmentVal * alignmentVal;
                var refsSpan = rootsList.AddSpan (varType->RefsList.Length);
                totalMem = baseOffs + varType->RuntimeSize;

                var refNum = 0;
                foreach (var refOffs in varType->RefsList.Span)
                    refsSpan [refNum++] = baseOffs + refOffs;

                var accessExpr = RefExpression (PrefixUnaryExpression (
                    SyntaxKind.PointerIndirectionExpression,
                    CastExpression (
                        PointerType (varRoslynType),
                        BinaryExpression (
                            SyntaxKind.AddExpression,
                            staticVarsMemRef,
                            NumericLiteral (baseOffs)
                        )
                    )
                ));

                var mangledName = MangleStaticVariable (staticVar);
                passData.GlobalMembers.Add (
                    PropertyDeclaration (
                        RefType (varRoslynType),
                        mangledName
                    ).WithModifiers (TokenList (
                        Token (SyntaxKind.PublicKeyword),
                        Token (SyntaxKind.StaticKeyword)
                    )).WithAccessorList (AccessorList (ListParams (
                        GetterDeclaration ()
                        .WithAttributeLists (SingletonList (SimpleAttributeList (
                            Attribute_MethodImpl_AggressiveInlining (),
                            Attribute_ExcludeFromStackTrace ()
                        ))).WithExpressionBody (ArrowExpressionClause (accessExpr))
                    )))
                );
            }

            var staticVarsMem = ArrayPointer<byte>.Null;
            if (totalMem > 0) {
                staticVarsMem = passData.MemoryManager.GetArrayAligned<byte> (totalMem, sizeof (nint));
                staticVarsMem.Span.Clear ();
            }

            foreach (ref var root in rootsList.Span)
                root += (nint) staticVarsMem.Elements;

            var rootsArray = ArrayPointer<nint>.Null;
            if (rootsList.Count > 0) {
                rootsArray = passData.MemoryManager.GetArrayAligned<nint> (rootsList.Count, sizeof (nint));
                rootsList.Span.CopyTo (rootsArray.Span);
                ES_GarbageCollector.AddRoots ((ES_ObjectAddress**) rootsArray.Elements, rootsArray.Length);
            }
            roots = rootsArray;

            var roslynBytePtr = PointerType (PredefinedType (Token (SyntaxKind.ByteKeyword)));
            passData.GlobalMembers.Add (
                PropertyDeclaration (
                    roslynBytePtr,
                    StaticVarsMemName
                ).WithModifiers (TokenList (
                    Token (SyntaxKind.PublicKeyword),
                    Token (SyntaxKind.StaticKeyword)
                )).WithAccessorList (AccessorList (ListParams (
                    GetterDeclaration ()
                    .WithAttributeLists (SingletonList (SimpleAttributeList (
                        Attribute_MethodImpl_AggressiveInlining (),
                        Attribute_ExcludeFromStackTrace ()
                    ))).WithExpressionBody (ArrowExpressionClause (
                        PointerLiteral (staticVarsMem.Elements, roslynBytePtr)
                    ))
                )))
            );

            return true;
        }

        private static bool CompileCSharp (ref PassData passData, ArrayPointer<nint> gcRoots) {
            // Create the global storage type and add it to the definitions list.
            passData.Types.Add (
                StructDeclaration (GlobalStorageTypeName)
                .WithMembers (ListSpan (passData.GlobalMembers.Span))
                .WithModifiers (TokenList (
                    Token (SyntaxKind.PublicKeyword),
                    Token (SyntaxKind.UnsafeKeyword)
                ))
            );

            // Create the namespace and compilation unit.
            var namespaceName = IdentifierName (NamespaceName);
            var namespaceDecl = NamespaceDeclaration (namespaceName).WithMembers (ListSpan (passData.Types.Span));

            var compUnit = CompilationUnit ().WithMembers (
                SingletonList<MemberDeclarationSyntax> (namespaceDecl)
            ).AddUsings (
                UsingDirective (NamespaceNameSyntax ("System")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon", "Data")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon", "Data", "Types")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon", "GarbageCollection")),
                UsingDirective (NamespaceNameSyntax ("EchelonScriptCommon", "Utilities")),

                SingleImportDirective (NamespaceNameSyntax ("System", "Runtime", "CompilerServices"), nameof (MethodImplAttribute)),
                SingleImportDirective (NamespaceNameSyntax ("System", "Runtime", "CompilerServices"), nameof (MethodImplOptions)),

                SingleImportDirective (NamespaceNameSyntax ("System", "Runtime", "InteropServices"), nameof (StructLayoutAttribute)),
                SingleImportDirective (NamespaceNameSyntax ("System", "Runtime", "InteropServices"), nameof (LayoutKind)),
                SingleImportDirective (NamespaceNameSyntax ("System", "Runtime", "InteropServices"), nameof (FieldOffsetAttribute))
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
#if DEBUG
                optimizationLevel: OptimizationLevel.Debug,
#else
                optimizationLevel: OptimizationLevel.Release,
#endif
                checkOverflow: false, allowUnsafe: true,
                concurrentBuild: true, deterministic: false
            );
            const string assemblyName = "EchelonScriptCodeAssembly"; // TODO: Generate a proper name for the assembly.
            var csComp = CSharpCompilation.Create (assemblyName, new [] { csSyntaxTree }, refApis, csCompOptions);

            foreach (var diagMsg in csComp.GetDiagnostics ()) {
                if (diagMsg.Severity != DiagnosticSeverity.Error)
                    continue;

                var errPos = diagMsg.Location.GetLineSpan ().StartLinePosition;
                var errMsg = diagMsg.GetMessage ();

                passData.ErrorList.Add (new (
                    $"The C# compiler reported an error at line {errPos.Line}, column {errPos.Character} when compiling: {errMsg}",
                    0, 0, string.Empty.AsMemory (), 0, 0
                ));
            }

            if (passData.ErrorList.Count > 0)
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

                passData.ErrorList.Add (new (error_EmitError.Replace ("{errMsg}", diagMsg.GetMessage ()), 0, 0, string.Empty.AsMemory (), 0, 0));
            }

            if (!emitResult.Success && passData.ErrorList.Count < 1)
                passData.ErrorList.Add (new ("Unknown Roslyn Emit error. Success was false but no errors reported.", 0, 0, string.Empty.AsMemory (), 0, 0));

            if (passData.ErrorList.Count > 0)
                return false;

            // Create the backend data and load the assembly.
            passData.EnvBuilder.BackendData = new RoslynBackendData (ref passData, gcRoots, dllStream, pdbStream, assemblyName);

            return true;
        }

        #endregion

        #region Roslyn utils

        private static LiteralExpressionSyntax StringLiteral (string value)
            => LiteralExpression (SyntaxKind.StringLiteralExpression, Literal (value));

        private static LiteralExpressionSyntax NumericLiteral (int value)
            => LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (value));

        private static LiteralExpressionSyntax NumericLiteral (uint value)
            => LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (value));

        private static LiteralExpressionSyntax NumericLiteral (long value)
            => LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (value));

        private static LiteralExpressionSyntax NumericLiteral (ulong value)
            => LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (value));

        private static LiteralExpressionSyntax NumericLiteral (float value)
            => LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (value));

        private static LiteralExpressionSyntax NumericLiteral (double value)
            => LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (value));

        private static LiteralExpressionSyntax BoolLiteral (bool value)
            => LiteralExpression (value ? SyntaxKind.TrueLiteralExpression : SyntaxKind.FalseLiteralExpression);

        private static ExpressionSyntax PointerLiteral (void* value, TypeSyntax? type = null) {
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

        private static NameSyntax NamespaceNameSyntax (params string [] name) {
            Debug.Assert (name.Length > 0);

            NameSyntax ret = IdentifierName (name [0]);
            for (int i = 1; i < name.Length; i++)
                ret = QualifiedName (ret, IdentifierName (name [i]));

            return ret;
        }

        private static UsingDirectiveSyntax SingleImportDirective (NameSyntax namespaceName, string typeNameStr) {
            var typeName = IdentifierName (typeNameStr);
            var originalName = QualifiedName (namespaceName, typeName);

            return UsingDirective (NameEquals (typeName), originalName);
        }

        private static SeparatedSyntaxList<TNode> SeparatedListSpan<TNode> (ReadOnlySpan<SyntaxNodeOrToken> nodes) where TNode : SyntaxNode {
            var nodesList = new SyntaxNodeOrTokenList ();
            foreach (var node in nodes)
                nodesList = nodesList.Add (node);

            return SeparatedList<TNode> (nodesList);
        }

        private static SyntaxList<TNode> ListSpan<TNode> (Span<TNode> statements) where TNode : SyntaxNode
            => ListSpan ((ReadOnlySpan<TNode>) statements);

        private static SyntaxList<TNode> ListSpan<TNode> (ReadOnlySpan<TNode> nodes) where TNode : SyntaxNode {
            var nodesList = new SyntaxList<TNode> ();
            foreach (var node in nodes)
                nodesList = nodesList.Add (node);

            return List (nodesList);
        }

        private static SyntaxList<TNode> ListParams<TNode> (params TNode [] nodes) where TNode : SyntaxNode => List (nodes);

        private static SeparatedSyntaxList<TNode> SimpleSeparatedList<TNode> (Span<TNode> nodes, SyntaxToken separator)
            where TNode : SyntaxNode => SimpleSeparatedList ((ReadOnlySpan<TNode>) nodes, separator);

        private static SeparatedSyntaxList<TNode> SimpleSeparatedList<TNode> (ReadOnlySpan<TNode> nodes, SyntaxToken separator)
            where TNode : SyntaxNode {
            using var list = new StructPooledList<SyntaxNodeOrToken> (CL_ClearMode.Auto);
            list.EnsureCapacity (nodes.Length * 2 - 1);

            var isFirst = true;
            foreach (var node in nodes) {
                if (!isFirst)
                    list.Add (separator);

                list.Add (node);
                isFirst = false;
            }

            return SeparatedListSpan<TNode> (list.Span);
        }

        private static SeparatedSyntaxList<TNode> SimpleSeparatedList<TNode> (SyntaxToken separator, params TNode [] nodes)
            where TNode : SyntaxNode => SimpleSeparatedList<TNode> (nodes, separator);

        private static BlockSyntax BlockSpan (ReadOnlySpan<StatementSyntax> statements) {
            var stmtsList = new SyntaxList<StatementSyntax> ();
            foreach (var stmt in statements)
                stmtsList = stmtsList.Add (stmt);

            return Block (stmtsList);
        }

        private static FieldDeclarationSyntax SimpleFieldDeclaration (TypeSyntax type, SyntaxToken name, ExpressionSyntax? assignValue = null) {
            var varDeclarator = VariableDeclarator (name);
            if (assignValue is not null)
                varDeclarator = varDeclarator.WithInitializer (EqualsValueClause (assignValue));

            var varList = SingletonSeparatedList (varDeclarator);
            var varDecl = VariableDeclaration (type).WithVariables (varList);
            var fieldDecl = FieldDeclaration (varDecl);

            return fieldDecl;
        }

        private static AttributeListSyntax SingletonAttributeList (AttributeSyntax attr) => AttributeList (SingletonSeparatedList (attr));

        private static AttributeListSyntax SimpleAttributeList (ReadOnlySpan<AttributeSyntax> attrArgs)
            => AttributeList (SimpleSeparatedList (attrArgs, Token (SyntaxKind.CommaToken)));

        private static AttributeListSyntax SimpleAttributeList (params AttributeSyntax [] attrArgs)
            => SimpleAttributeList (attrArgs.AsSpan ());

        private static AttributeArgumentListSyntax SingletonAttributeArgumentList (AttributeArgumentSyntax attrArg)
            => AttributeArgumentList (SingletonSeparatedList (attrArg));

        private static AttributeArgumentListSyntax SimpleAttributeArgumentList (ReadOnlySpan<AttributeArgumentSyntax> attrArgs)
            => AttributeArgumentList (SimpleSeparatedList (attrArgs, Token (SyntaxKind.CommaToken)));

        private static AttributeArgumentListSyntax SimpleAttributeArgumentList (params AttributeArgumentSyntax [] attrArgs)
            => AttributeArgumentList (SimpleSeparatedList<AttributeArgumentSyntax> (attrArgs, Token (SyntaxKind.CommaToken)));

        private static ExpressionSyntax SimpleMemberAccess (ExpressionSyntax type, SimpleNameSyntax memberName)
            => MemberAccessExpression (SyntaxKind.SimpleMemberAccessExpression, type, memberName);

        private static ExpressionSyntax SimpleMemberAccess (string type, string memberName)
            => MemberAccessExpression (SyntaxKind.SimpleMemberAccessExpression, IdentifierName (type), IdentifierName (memberName));

        private static ExpressionSyntax PointerMemberAccess (ExpressionSyntax type, SimpleNameSyntax memberName)
            => MemberAccessExpression (SyntaxKind.PointerMemberAccessExpression, type, memberName);

        private static AttributeSyntax Attribute_MethodImpl_AggressiveInlining () {
            return Attribute (
                IdentifierName (nameof (MethodImplAttribute)),
                SingletonAttributeArgumentList (AttributeArgument (
                    SimpleMemberAccess (nameof (MethodImplOptions), nameof (MethodImplOptions.AggressiveInlining)
                )))
            );
        }

        private static AttributeSyntax Attribute_ExcludeFromStackTrace ()
            => Attribute (IdentifierName (nameof (ES_ExcludeFromStackTraceAttribute)));

        private static AttributeSyntax Attribute_StructLayout (string layoutKind, ExpressionSyntax? pack, ExpressionSyntax? size) {
            using var args = new StructPooledList<AttributeArgumentSyntax> (CL_ClearMode.Auto);
            args.Add (AttributeArgument (SimpleMemberAccess (nameof (LayoutKind), layoutKind)));

            if (pack is not null)
                args.Add (AttributeArgument (NameEquals ("Pack"), null, pack));

            if (size is not null)
                args.Add (AttributeArgument (NameEquals ("Size"), null, size));

            return Attribute (
                IdentifierName (nameof (StructLayoutAttribute)),
                SimpleAttributeArgumentList (args.Span)
            );
        }

        private static AttributeSyntax Attribute_FieldOffset (int value)
            => Attribute_FieldOffset (LiteralExpression (SyntaxKind.NumericLiteralExpression, Literal (value)));

        private static AttributeSyntax Attribute_FieldOffset (ExpressionSyntax value) {
            return Attribute (
                IdentifierName (nameof (FieldOffsetAttribute)),
                SingletonAttributeArgumentList (AttributeArgument (value))
            );
        }

        private static AccessorDeclarationSyntax GetterDeclaration () => AccessorDeclaration (SyntaxKind.GetAccessorDeclaration);

        private static AccessorDeclarationSyntax SetterDeclaration () => AccessorDeclaration (SyntaxKind.SetAccessorDeclaration);

        private static ExpressionSyntax Roslyn_CreateSpan (
            TypeSyntax spanType, ExpressionSyntax spanSrc, ExpressionSyntax spanLength,
            bool readOnly = false
        ) {
            return ObjectCreationExpression (
                GenericName (
                    Identifier ("Span")
                ).WithTypeArgumentList (TypeArgumentList (
                    SingletonSeparatedList (spanType)
                ))
            ).WithArgumentList (ArgumentList (
                SimpleSeparatedList (
                    Token (SyntaxKind.CommaToken),
                    Argument (spanSrc),
                    Argument (spanLength)
                )
            ));
        }

        #endregion

        #region Misc utils

        private void CheckDisposed () {
            if (IsDisposed)
                throw new ObjectDisposedException (GetType ().Name);
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

            IsDisposed = true;
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }
}
