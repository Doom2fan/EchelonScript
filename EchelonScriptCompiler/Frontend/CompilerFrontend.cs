/*
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
using ChronosLib.Pooled;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Utilities;

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
        public SymbolsStack Symbols;

        #endregion

        #region ================== Instance methods

        public void Initialize (ES_AbstractSyntaxTree ast) {
            Ast = ast;

            Symbols = new SymbolsStack ();
            Symbols.Initialize ();
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

    public unsafe struct SymbolsStack : IDisposable {
        #region ================== Instance fields

        private StructPooledList<Pointer<ES_TypeInfo>> importedTypes;
        private StructPooledList<int> stacks;

        #endregion

        #region ================== Instance methods

        public void Initialize () {
            importedTypes = new StructPooledList<Pointer<ES_TypeInfo>> (CL_ClearMode.Auto);
            stacks = new StructPooledList<int> (CL_ClearMode.Auto);
        }

        public void Push () {
            stacks.Add (0);
        }

        public void Pop () {
            Debug.Assert (stacks.Count > 0);

            int lastStack = stacks.Count - 1;

            importedTypes.RemoveEnd (stacks [lastStack]);
            stacks.RemoveAt (lastStack);
        }

        public void AddType (ES_TypeInfo* type) {
            Debug.Assert (stacks.Count > 0);

            importedTypes.Add (type);
            stacks [stacks.Count - 1]++;
        }

        public void AddTypes (ReadOnlySpan<Pointer<ES_TypeInfo>> types) {
            Debug.Assert (stacks.Count > 0);

            foreach (var type in types)
                importedTypes.Add (type);

            stacks [stacks.Count - 1] += importedTypes.Count;
        }

        public ES_TypeInfo* GetType (ArrayPointer<byte> name) {
            for (int i = importedTypes.Count - 1; i >= 0; i--) {
                var type = importedTypes [i];
                if (type.Address->TypeName.Equals (name))
                    return type;
            }

            return null;
        }

        #endregion

        #region ================== IDisposable support

        private bool disposedValue;

        public void Dispose () {
            if (!disposedValue) {
                importedTypes.Dispose ();

                disposedValue = true;
            }
        }

        #endregion
    }

    public unsafe partial class CompilerFrontend : IDisposable {
        #region ================== Instance fields

        protected List<EchelonScriptErrorMessage> errorList;
        protected StructPooledList<TranslationUnitData> translationUnits;
        protected int unitIdx;

        #endregion

        #region ================== Instance properties

        public EchelonScriptEnvironment.Builder? EnvironmentBuilder { get; protected set; }
        public EchelonScriptEnvironment? Environment { get; protected set; }

        #endregion

        #region ================== Constructors

        public CompilerFrontend (List<EchelonScriptErrorMessage> errList) {
            errorList = errList;

            translationUnits = new StructPooledList<TranslationUnitData> (CL_ClearMode.Auto);

            Environment = null;
            EnvironmentBuilder = null;

            unitIdx = 0;
        }

        #endregion

        #region ================== Instance methods

        protected ArrayPointer<byte> GetFullyQualifiedName (ArrayPointer<byte> namespaceName, ArrayPointer<byte> typeName) {
            using var fqnArr = PooledArray<byte>.GetArray (namespaceName.Length + typeName.Length + 2);
            var fqnSpan = fqnArr.Span;

            int len = 0;

            namespaceName.Span.CopyTo (fqnSpan.Slice (len, namespaceName.Length));
            len += namespaceName.Length;

            fqnSpan [len++] = (byte) ':';
            fqnSpan [len++] = (byte) ':';

            typeName.Span.CopyTo (fqnSpan.Slice (len, typeName.Length));

            return Environment!.IdPool.GetIdentifier (fqnSpan);
        }

        protected ArrayPointer<byte> GetFullyQualifiedName (ArrayPointer<byte> namespaceName, ReadOnlySpan<ArrayPointer<byte>> typeName) {
            Debug.Assert (typeName.Length > 0);

            int len = namespaceName.Length + 2 + (typeName.Length - 1); // Namespace name + "::" + "."s (if any)
            for (int i = 0; i < typeName.Length; i++)
                len += typeName [i].Length;

            using var fqnArr = PooledArray<byte>.GetArray (len);
            var fqnSpan = fqnArr.Span;

            len = 0;

            // Namespace
            namespaceName.Span.CopyTo (fqnSpan.Slice (len, namespaceName.Length));
            len += namespaceName.Length;

            // Namespace separator
            fqnSpan [len++] = (byte) ':';
            fqnSpan [len++] = (byte) ':';

            // First type name
            typeName [0].Span.CopyTo (fqnSpan.Slice (len, typeName [0].Length));
            len += typeName [0].Length;

            // Nested type names
            for (int i = 1; i < typeName.Length; i++) {
                fqnSpan [len++] = (byte) '.';

                typeName [i].Span.CopyTo (fqnSpan.Slice (len, typeName [i].Length));
                len += typeName [i].Length;
            }

            return Environment!.IdPool.GetIdentifier (fqnSpan);
        }

        public void Setup (EchelonScriptEnvironment env, EchelonScriptEnvironment.Builder envBuilder) {
            CheckDisposed ();

            Environment = env;
            EnvironmentBuilder = envBuilder;
        }

        public void Reset () {
            foreach (var codeUnit in translationUnits)
                codeUnit.AstUnits.Dispose ();

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

        public void CompileCode () {
            CheckDisposed ();
            CheckSetUp ();

            foreach (ref var transUnit in translationUnits.Span)
                GatherTypes (ref transUnit);

            if (errorList.Count > 0)
                return;

            GenerateTypesList ();

            if (errorList.Count > 0)
                return;

            foreach (ref var transUnit in translationUnits.Span)
                GatherGlobalImports (ref transUnit);

            if (errorList.Count > 0)
                return;

            foreach (ref var transUnit in translationUnits.Span)
                CheckTypes (ref transUnit);

            if (errorList.Count > 0)
                return;
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
