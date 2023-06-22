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
using EchelonScript.Common;
using EchelonScript.Common.Data;
using EchelonScript.Compiler.CompilerCommon.MIR;

namespace EchelonScript.Compiler.Frontend;

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
    /*public readonly ESC_TypeRef Type;
    public readonly ESIR_Expression IRExpression;

    internal FrontendVariable (ESC_TypeRef type, ESIR_Expression irExpr) {
        Type = type;
        IRExpression = irExpr;
    }*/
}

internal readonly struct FrontendSymbol {
    /*public static FrontendSymbol SymbolNone => new (FrontendSymbolType.None);

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

    public static FrontendSymbol NewFunction (ESC_Function data, FrontendSymbolFlags flags = 0) => new (data, flags);*/
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

internal unsafe static partial class CompilerFrontend {
    public static ES_ObjectConst<MIRTree> CompileCode (ES_CompilerContext context) {
        var nullTree = ES_ObjectConst<MIRTree>.Null;

        //var symbols = new SymbolStack<FrontendSymbol> (FrontendSymbol.SymbolNone);
        var compileData = new CompileData {
            Context = context,

            //Symbols = symbols,
        };

        throw new NotImplementedException ();
    }
}
