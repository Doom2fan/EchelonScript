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
using System.Runtime.InteropServices;
using System.Text;
using Collections.Pooled;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend.Parser;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Frontend {
    public class CompilerFrontend : IDisposable {
        #region ================== Instance fields

        protected List<EchelonScriptErrorMessage> errorList;

        #endregion

        #region ================== Instance properties

        public EchelonScriptEnvironment.Builder? EnvironmentBuilder { get; protected set; }
        public EchelonScriptEnvironment? Environment { get; protected set; }

        #endregion

        #region ================== Constructors

        public CompilerFrontend (List<EchelonScriptErrorMessage> errList) {
            errorList = errList;

            Environment = EchelonScriptEnvironment.CreateEnvironment (out var builder);
            EnvironmentBuilder = builder;
        }

        #endregion

        #region ================== Instance methods

        public ES_NamespaceData.Builder CreateOrGetNamespace (ReadOnlySpan<char> name) {
            CheckDisposed ();
            CheckSetUp ();

            var namePtr = Environment!.IdPool.GetIdentifier (name);
            if (EnvironmentBuilder!.NamespaceBuilders.TryGetValue (namePtr, out var ret))
                return ret;

            return EnvironmentBuilder.CreateNamespace (name);
        }

        public void Setup (EchelonScriptEnvironment env, EchelonScriptEnvironment.Builder envBuilder) {
            CheckDisposed ();

            Environment = env;
            EnvironmentBuilder = envBuilder;
        }

        public void Clear () {
            Environment = null;
            EnvironmentBuilder = null;
        }

        public void CompileCode (string unitName, ReadOnlySpan<ES_AbstractSyntaxTree> astUnits) {
            CheckDisposed ();
            CheckSetUp ();

            foreach (var astUnit in astUnits) {
                if (!astUnit.Valid)
                    throw new CompilationException ("One of the ast units is invalid.");
            }
        }

        protected void CheckDisposed () {
            if (disposedValue)
                throw new ObjectDisposedException (nameof (CompilerFrontend));
        }

        protected void CheckSetUp () {
            if (Environment == null || EnvironmentBuilder == null)
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
                Clear ();

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
