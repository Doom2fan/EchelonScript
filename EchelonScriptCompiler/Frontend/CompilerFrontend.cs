/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend.Data;

namespace EchelonScriptCompiler.Frontend;

internal struct TranslationUnitData : IDisposable {
    #region ================== Instance fields

    public ES_Identifier Name;
    public PooledArray<AstUnitData> AstUnits;

    #endregion

    #region ================== IDisposable support

    private bool disposedValue;

    public void Dispose () {
        if (disposedValue)
            return;

        foreach (ref var astUnit in AstUnits.Span)
            astUnit.Dispose ();

        AstUnits.Dispose ();

        disposedValue = true;
    }

    #endregion
}

internal struct AstUnitData : IDisposable {
    #region ================== Instance fields and properties

    public ES_AbstractSyntaxTree Ast;
    public SymbolStack<FrontendSymbol> Symbols;
    public SourceData SourceData => new () { Code = Ast.Source.Span, FileName = Ast.FileName };

    #endregion

    #region ================== Instance methods

    public unsafe void Initialize (ES_AbstractSyntaxTree ast) {
        Ast = ast;

        Symbols = new SymbolStack<FrontendSymbol> (FrontendSymbol.SymbolNone);
    }

    #endregion

    #region ================== IDisposable support

    private bool disposedValue;

    public void Dispose () {
        if (disposedValue)
            return;

        Symbols.Dispose ();

        disposedValue = true;
    }

    #endregion
}

public enum FrontendSymbolType {
    None,
    Type,
    Variable,
    Function,
}

public enum FrontendSymbolFlags {
    UsingVar            = 1,
    RefVar              = 1 << 1,
    OutVar              = 1 << 2,
    CompileTimeConstant = 1 << 3,
    Writable            = 1 << 4,
}

internal struct FrontendVariable {
    public readonly ESC_TypeRef Type;
    public readonly ESIR_Expression IRExpression;

    internal FrontendVariable (ESC_TypeRef type, ESIR_Expression irExpr) {
        Type = type;
        IRExpression = irExpr;
    }
}

internal readonly struct FrontendSymbol {
    public static FrontendSymbol SymbolNone => new (FrontendSymbolType.None);

    #region ================== Instance fields

    private readonly ESC_TypeRef valType;
    private readonly FrontendVariable valVar;
    private readonly ESC_Function? valFunc;

    public readonly FrontendSymbolType Tag;
    public readonly FrontendSymbolFlags Flags;

    #endregion

    #region ================== Constructors

    private FrontendSymbol (FrontendSymbolType tag) {
        Tag = tag;

        valType = ESC_TypeRef.Null ();
        valVar = default;
        valFunc = null;

        Flags = 0;
    }

    private FrontendSymbol (ESC_TypeRef value, FrontendSymbolFlags flags) {
        Tag = FrontendSymbolType.Type;
        valType = value;
        Flags = flags;

        valFunc = null;
        valVar = default;
    }

    private FrontendSymbol (FrontendVariable value, FrontendSymbolFlags flags) {
        Tag = FrontendSymbolType.Variable;
        valVar = value;
        Flags = flags;

        valType = ESC_TypeRef.Null ();
        valFunc = null;
    }

    private FrontendSymbol (ESC_Function value, FrontendSymbolFlags flags) {
        Tag = FrontendSymbolType.Function;
        valFunc = value;
        Flags = flags;

        valType = ESC_TypeRef.Null ();
        valVar = default;
    }

    #endregion

    #region ================== Instance methods

    public ESC_TypeRef MatchType () {
        if (Tag != FrontendSymbolType.Type)
            throw new CompilationException ();

        return valType;
    }

    public FrontendVariable MatchVar () {
        if (Tag != FrontendSymbolType.Variable)
            throw new CompilationException ();

        return valVar;
    }

    public ESC_Function MatchFunction () {
        if (Tag != FrontendSymbolType.Function)
            throw new CompilationException ();

        return valFunc!;
    }

    #endregion

    public static FrontendSymbol NewVariable (FrontendVariable var, FrontendSymbolFlags flags = 0) => new (var, flags);

    public static FrontendSymbol NewType (ESC_TypeRef type, FrontendSymbolFlags flags = 0) => new (type, flags);

    public static FrontendSymbol NewFunction (ESC_Function data, FrontendSymbolFlags flags = 0) => new (data, flags);
}

public unsafe class SymbolStack<TSymbolType> : IDisposable {
    private unsafe struct Scope {
        public Dictionary<ES_Identifier, TSymbolType> Symbols;

        public Scope (Dictionary<ES_Identifier, TSymbolType> symbols) => Symbols = symbols;
    }

    #region ================== Instance fields

    private CL_PooledList<Scope> scopes;
    private CL_PooledList<Dictionary<ES_Identifier, TSymbolType>> pooledDicts;
    private int version;

    private TSymbolType notFoundValue;

    #endregion

    #region ================== Instance properties

    public int ScopesCount {
        get {
            CheckDisposed ();
            return scopes.Count;
        }
    }

    #endregion

    public SymbolStack (TSymbolType notFoundVal) {
        scopes = new CL_PooledList<Scope> ();
        pooledDicts = new CL_PooledList<Dictionary<ES_Identifier, TSymbolType>> ();
        version = 0;

        notFoundValue = notFoundVal;

        IsDisposed = false;
    }

    #region ================== Instance methods

    private Dictionary<ES_Identifier, TSymbolType> GetDict () {
        if (pooledDicts.Count < 1)
            return new Dictionary<ES_Identifier, TSymbolType> ();

        var idx = pooledDicts.Count - 1;

        var ret = pooledDicts [idx];
        pooledDicts.RemoveAt (idx);

        return ret;
    }

    public void Push () {
        CheckDisposed ();

        version++;

        scopes.Add (new Scope (GetDict ()));
    }

    public void Pop () {
        CheckDisposed ();

        version++;

        Debug.Assert (scopes.Count > 0);

        var idx = scopes.Count - 1;
        var dict = scopes [idx].Symbols;

        scopes.RemoveAt (idx);

        dict.Clear ();
        pooledDicts.Add (dict);
    }

    public TSymbolType? GetSymbol (ES_Identifier name) {
        CheckDisposed ();

        for (var i = scopes.Count - 1; i >= 0; i--) {
            var scope = scopes [i];

            if (scope.Symbols.TryGetValue (name, out var symbol))
                return symbol;
        }

        return notFoundValue;
    }

    public bool AddSymbol (ES_Identifier name, TSymbolType symbol) {
        CheckDisposed ();

        version++;

        var scope = scopes [^1];

        return scope.Symbols.TryAdd (name, symbol);
    }

    #endregion

    #region ================== IDisposable support

    public bool IsDisposed { get; private set; }

    private void CheckDisposed () {
        if (IsDisposed)
            throw new ObjectDisposedException (GetType ().Name);
    }

    ~SymbolStack () {
        if (!IsDisposed)
            DoDispose ();
    }

    protected virtual void DoDispose () {
        if (IsDisposed)
            return;

        scopes.Dispose ();
        pooledDicts.Dispose ();

        IsDisposed = true;
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion

    #region ================== Enumerator support

    public Enumerator GetEnumerator () => new (this);

    public struct Enumerator : IEnumerator<IReadOnlyDictionary<ES_Identifier, TSymbolType>> {
        #region ================== Instance fields

        private int version;

        private SymbolStack<TSymbolType> stack;
        private CL_PooledList<Scope>.Enumerator enumerator;

        #endregion

        #region ================== Instance properties

        public IReadOnlyDictionary<ES_Identifier, TSymbolType> Current {
            get {
                CheckValid ();
                return enumerator.Current.Symbols;
            }
        }

        object IEnumerator.Current => Current;

        #endregion

        public Enumerator (SymbolStack<TSymbolType> newStack) {
            stack = newStack;

            version = stack.version;
            enumerator = stack.scopes.GetEnumerator ();
        }

        #region ================== Instance methods

        private void CheckValid () {
            if (version != stack.version)
                throw new InvalidOperationException ("The stack was modified after the enumerator was created.");
        }

        public bool MoveNext () {
            CheckValid ();

            return enumerator.MoveNext ();
        }

        public void Reset () {
            CheckValid ();

            enumerator = stack.scopes.GetEnumerator ();
        }

        public void Dispose () { }

        #endregion
    }

    #endregion
}

internal unsafe sealed partial class CompilerFrontend : IDisposable {
    #region ================== Instance fields

    private List<EchelonScriptErrorMessage> errorList;
    private List<EchelonScriptErrorMessage> warningList;
    private List<EchelonScriptErrorMessage> infoList;
    private StructPooledList<TranslationUnitData> translationUnits;

    #endregion

    #region ================== Instance properties

    private EchelonScriptEnvironment.Builder? EnvironmentBuilder { get; set; }
    private EchelonScriptEnvironment? Environment { get; set; }

    #endregion

    public CompilerFrontend (
        List<EchelonScriptErrorMessage> errList,
        List<EchelonScriptErrorMessage> warnList,
        List<EchelonScriptErrorMessage> informList
    ) {
        errorList = errList;
        warningList = warnList;
        infoList = informList;

        translationUnits = new StructPooledList<TranslationUnitData> (CL_ClearMode.Auto);

        Environment = null;
        EnvironmentBuilder = null;
    }

    #region ================== Instance methods

    private static ESC_TypeRef GetTypeRef (ES_AstTypeDeclaration? typeDecl) {
        Debug.Assert (typeDecl is not null);

        var typeRef = typeDecl as ES_AstTypeDeclaration_TypeReference;
        Debug.Assert (typeRef is not null);

        return typeRef.Reference;
    }

    public void Setup (EchelonScriptEnvironment env, EchelonScriptEnvironment.Builder envBuilder) {
        CheckDisposed ();

        Environment = env;
        EnvironmentBuilder = envBuilder;
    }

    public void Reset () {
        foreach (var codeUnit in translationUnits)
            codeUnit.Dispose ();

        translationUnits.Clear ();

        Environment = null;
        EnvironmentBuilder = null;
    }

    /// <summary>Adds a code unit to the compiler.</summary>
    /// <param name="unitName">The name of the code unit.</param>
    /// <param name="astUnits">The ASTs in the code unit.</param>
    public void AddUnit (string unitName, ReadOnlySpan<ES_AbstractSyntaxTree> astUnits) {
        CheckDisposed ();
        CheckSetUp ();

        var unitNameId = Environment!.IdPool.GetIdentifier (unitName);
        var astUnitsArr = PooledArray<AstUnitData>.GetArray (astUnits.Length);

        var i = 0;
        foreach (var astUnit in astUnits) {
            if (astUnit == null || !astUnit.Valid)
                throw new CompilationException ("One of the ast units is invalid.");

            var astUnitData = new AstUnitData ();
            astUnitData.Initialize (astUnits [i]);

            astUnitsArr.Array [i] = astUnitData;

            i++;
        }

        var translationUnit = new TranslationUnitData {
            Name = unitNameId,
            AstUnits = astUnitsArr
        };

        translationUnits.Add (translationUnit);
    }

    public ESIR_Tree? CompileCode () {
        Debug.Assert (Environment is not null);
        Debug.Assert (EnvironmentBuilder is not null);

        var nullArr = (ESIR_Tree?) null;
        var idPool = Environment.IdPool;

        var symbols = new SymbolStack<FrontendSymbol> (FrontendSymbol.SymbolNone);
        var compileData = new CompileData {
            Env = Environment,
            EnvBuilder = EnvironmentBuilder,

            ErrorList = errorList,
            WarnList = warningList,
            InfoList = infoList,

            TranslationUnits = translationUnits.Span,
            Symbols = symbols,

            Namespaces = new (),

            TypeUnknown = new ESC_TypeUnknown (new (ES_Identifier.Empty, idPool.GetIdentifier ("#UNKNOWN_TYPE"))),
            TypeNull = new ESC_TypeNull (new (ES_Identifier.Empty, idPool.GetIdentifier ("#NULL"))),
        };

        CheckDisposed ();
        CheckSetUp ();

        if (errorList.Count > 0)
            return nullArr;

        Compiler_TypeCreation.CreateTypes (ref compileData);

        if (errorList.Count > 0)
            return nullArr;

        Compiler_TypeGathering.GatherTypes_Functions (ref compileData);

        if (errorList.Count > 0)
            return nullArr;

        Compiler_FunctionCreation.CreateFunctions (ref compileData);

        if (errorList.Count > 0)
            return nullArr;

        Compiler_TypeGathering.GatherTypes_Types (ref compileData);

        if (errorList.Count > 0)
            return nullArr;

        Compiler_TypeSizing.SizeTypes (ref compileData);

        if (errorList.Count > 0)
            return nullArr;

        Compiler_ConstantFolding.FoldConstants (ref compileData);

        if (errorList.Count > 0)
            return nullArr;

        var irTree = Compiler_TypeChecking.CheckTypes (ref compileData);

        if (errorList.Count > 0)
            return nullArr;

        Compiler_TypeSizing.SizeTypes (ref compileData);

        if (errorList.Count > 0)
            return nullArr;

        Compiler_TypeInfoGen.GenerateTypes (ref compileData);

        return irTree;
    }

    private void CheckDisposed () {
        if (disposedValue)
            throw new ObjectDisposedException (nameof (CompilerFrontend));
    }

    private void CheckSetUp () {
        if (Environment == null | EnvironmentBuilder == null)
            throw new CompilationException ("The frontend is not set up.");
    }

    #endregion

    #region ================== IDisposable support

    private bool disposedValue = false;

    ~CompilerFrontend () {
        if (!disposedValue)
            DoDispose ();
    }

    private void DoDispose () {
        if (disposedValue)
            return;
        Reset ();
        translationUnits.Dispose ();

        disposedValue = true;
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion
}
