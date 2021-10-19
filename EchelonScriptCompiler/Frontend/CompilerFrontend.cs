/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
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
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Frontend {
    public struct TranslationUnitData : IDisposable {
        #region ================== Instance fields

        public ArrayPointer<byte> Name;
        public PooledArray<AstUnitData> AstUnits;

        #endregion

        #region ================== IDisposable support

        private bool disposedValue;

        public void Dispose () {
            if (!disposedValue) {
                foreach (ref var astUnit in AstUnits.Span)
                    astUnit.Dispose ();

                AstUnits.Dispose ();

                disposedValue = true;
            }
        }

        #endregion
    }

    public struct AstUnitData : IDisposable {
        #region ================== Instance fields

        public ES_AbstractSyntaxTree Ast;
        public SymbolStack<FrontendSymbol> Symbols;

        #endregion

        #region ================== Instance methods

        public unsafe void Initialize (ES_AbstractSyntaxTree ast) {
            Ast = ast;

            Symbols = new SymbolStack<FrontendSymbol> (new FrontendSymbol (FrontendSymbolType.None, null, 0));
        }

        #endregion

        #region ================== IDisposable support

        private bool disposedValue;

        public void Dispose () {
            if (!disposedValue) {
                Symbols.Dispose ();

                disposedValue = true;
            }
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
        UsingVar,
    }

    public unsafe struct FrontendSymbol {
        #region ================== Instance fields

        private readonly void* value;

        public readonly FrontendSymbolType Tag;
        public readonly FrontendSymbolFlags Flags;

        #endregion

        #region ================== Constructors

        internal FrontendSymbol (FrontendSymbolType tag, void* value, FrontendSymbolFlags flags) {
            Tag = tag;
            this.value = value;
            Flags = flags;
        }

        #endregion

        #region ================== Instance methods

        public ES_TypeInfo* MatchType () {
            if (Tag != FrontendSymbolType.Type)
                throw new CompilationException ();

            return (ES_TypeInfo*) value;
        }

        public ES_TypeInfo* MatchVar () {
            if (Tag != FrontendSymbolType.Variable)
                throw new CompilationException ();

            return (ES_TypeInfo*) value;
        }

        public ES_FunctionData* MatchFunction () {
            if (Tag != FrontendSymbolType.Function)
                throw new CompilationException ();

            return (ES_FunctionData*) value;
        }

        #endregion
    }

    public unsafe struct Scope<TSymbolType> {
        public Dictionary<ArrayPointer<byte>, TSymbolType> Symbols;

        public Scope (Dictionary<ArrayPointer<byte>, TSymbolType> symbols) {
            Symbols = symbols;
        }
    }

    public unsafe class SymbolStack<TSymbolType> : IDisposable {
        #region ================== Instance fields

        private CL_PooledList<Scope<TSymbolType>> scopes;
        private CL_PooledList<Dictionary<ArrayPointer<byte>, TSymbolType>> pooledDicts;
        private int version;

        private TSymbolType notFoundValue;

        #endregion

        #region ================== Constructors

        public SymbolStack (TSymbolType notFoundVal) {
            scopes = new CL_PooledList<Scope<TSymbolType>> ();
            pooledDicts = new CL_PooledList<Dictionary<ArrayPointer<byte>, TSymbolType>> ();
            version = 0;

            notFoundValue = notFoundVal;

            IsDisposed = false;
        }

        #endregion

        #region ================== Instance methods

        private Dictionary<ArrayPointer<byte>, TSymbolType> GetDict () {
            if (pooledDicts.Count > 0) {
                int idx = pooledDicts.Count - 1;

                var ret = pooledDicts [idx];
                pooledDicts.RemoveAt (idx);

                return ret;
            }

            return new Dictionary<ArrayPointer<byte>, TSymbolType> ();
        }

        public void Push () {
            version++;

            scopes.Add (new Scope<TSymbolType> (GetDict ()));
        }

        public void Pop () {
            version++;

            Debug.Assert (scopes.Count > 0);

            var idx = scopes.Count - 1;
            var dict = scopes [idx].Symbols;

            scopes.RemoveAt (idx);

            dict.Clear ();
            pooledDicts.Add (dict);
        }

        public TSymbolType? GetSymbol (ArrayPointer<byte> name) {
            for (int i = scopes.Count - 1; i >= 0; i--) {
                var scope = scopes [i];

                if (scope.Symbols.TryGetValue (name, out var symbol))
                    return symbol;
            }

            return notFoundValue;
        }

        public bool AddSymbol (ArrayPointer<byte> name, TSymbolType symbol) {
            version++;

            var scope = scopes [^1];

            return scope.Symbols.TryAdd (name, symbol);
        }

        #endregion

        #region ================== IDisposable support

        public bool IsDisposed;

        private void CheckDisposed () {
            if (IsDisposed)
                throw new ObjectDisposedException (GetType ().Name);
        }

        ~SymbolStack () {
            if (!IsDisposed)
                DoDispose ();
        }

        protected virtual void DoDispose () {
            if (!IsDisposed) {
                scopes.Dispose ();
                pooledDicts.Dispose ();

                IsDisposed = true;
            }
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion

        #region ================== Enumerator support

        public Enumerator GetEnumerator () {
            return new Enumerator (this);
        }

        public struct Enumerator : IEnumerator<IReadOnlyDictionary<ArrayPointer<byte>, TSymbolType>> {
            #region ================== Instance fields

            private int version;

            private SymbolStack<TSymbolType> stack;
            private CL_PooledList<Scope<TSymbolType>>.Enumerator enumerator;

            #endregion

            #region ================== Instance properties

            public IReadOnlyDictionary<ArrayPointer<byte>, TSymbolType> Current {
                get {
                    CheckValid ();
                    return enumerator.Current.Symbols;
                }
            }

            object IEnumerator.Current => Current;

            #endregion

            #region ================== Constructors

            public Enumerator (SymbolStack<TSymbolType> newStack) {
                stack = newStack;

                version = stack.version;
                enumerator = stack.scopes.GetEnumerator ();
            }

            #endregion

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

    public unsafe partial class CompilerFrontend : IDisposable {
        #region ================== Instance fields

        protected List<EchelonScriptErrorMessage> errorList;
        protected List<EchelonScriptErrorMessage> warningList;
        protected List<EchelonScriptErrorMessage> infoList;
        protected StructPooledList<TranslationUnitData> translationUnits;
        protected int unitIdx;

        #endregion

        #region ================== Instance properties

        public EchelonScriptEnvironment.Builder? EnvironmentBuilder { get; protected set; }
        public EchelonScriptEnvironment? Environment { get; protected set; }

        #endregion

        #region ================== Constructors

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

            unitIdx = 0;
        }

        #endregion

        #region ================== Instance methods

        protected FrontendSymbol NewSymbolVariable (ES_TypeInfo* type, FrontendSymbolFlags flags = 0) {
            return new FrontendSymbol (FrontendSymbolType.Variable, type, flags);
        }

        protected FrontendSymbol NewSymbolType (ES_TypeInfo* type) {
            return new FrontendSymbol (FrontendSymbolType.Type, type, 0);
        }

        protected FrontendSymbol NewSymbolFunction (ES_FunctionData* data) {
            return new FrontendSymbol (FrontendSymbolType.Function, data, 0);
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

            int i = 0;
            foreach (var astUnit in astUnits) {
                if (astUnit == null || !astUnit.Valid)
                    throw new CompilationException ("One of the ast units is invalid.");

                var astUnitData = new AstUnitData ();
                astUnitData.Initialize (astUnits [i]);

                astUnitsArr.Array [i] = astUnitData;

                i++;
            }

            var translationUnit = new TranslationUnitData ();
            translationUnit.Name = unitNameId;
            translationUnit.AstUnits = astUnitsArr;

            translationUnits.Add (translationUnit);
        }

        public PooledArray<TranslationUnitData> CompileCode () {
            var nullArr = PooledArray<TranslationUnitData>.Empty ();

            CheckDisposed ();
            CheckSetUp ();

            if (errorList.Count > 0)
                return nullArr;

            GenerateBuiltinTypes ();

            if (errorList.Count > 0)
                return nullArr;

            foreach (ref var transUnit in translationUnits.Span)
                CreateTypes (ref transUnit);

            if (errorList.Count > 0)
                return nullArr;

            GenerateTypesList ();

            if (errorList.Count > 0)
                return nullArr;

            foreach (ref var transUnit in translationUnits.Span)
                GatherGlobalImports (ref transUnit);

            if (errorList.Count > 0)
                return nullArr;

            foreach (ref var transUnit in translationUnits.Span)
                CreateFunctions (ref transUnit);

            if (errorList.Count > 0)
                return nullArr;

            foreach (ref var transUnit in translationUnits.Span)
                GatherTypes (ref transUnit);

            if (errorList.Count > 0)
                return nullArr;

            foreach (ref var transUnit in translationUnits.Span)
                FoldConstants (ref transUnit);

            if (errorList.Count > 0)
                return nullArr;

            foreach (ref var transUnit in translationUnits.Span)
                CheckTypes (ref transUnit);

            if (errorList.Count > 0)
                return nullArr;

            return translationUnits.ToPooledArray ();
        }

        protected void CheckDisposed () {
            if (disposedValue)
                throw new ObjectDisposedException (nameof (CompilerFrontend));
        }

        protected void CheckSetUp () {
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

        protected virtual void DoDispose () {
            if (!disposedValue) {
                Reset ();
                translationUnits.Dispose ();

                disposedValue = true;
            }
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }
}
