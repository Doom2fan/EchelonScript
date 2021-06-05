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
using ChronosLib.Pooled;
using EchelonScriptCompiler.Backends;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Frontend.Parser;

namespace EchelonScriptCompiler {
    public class EchelonScript_Compiler : IDisposable {
        #region ================== Instance fields

        protected List<EchelonScriptErrorMessage> errorsList;
        protected List<EchelonScriptErrorMessage> warningsList;
        protected List<EchelonScriptErrorMessage> infoList;
        protected EchelonScriptParser parser;
        protected CompilerFrontend frontend;
        protected ICompilerBackend? backend;

        protected bool disposeBackend;

        protected EchelonScriptEnvironment? environment;
        protected EchelonScriptEnvironment.Builder? environmentBuilder;

        #endregion

        #region ================== Instance properties

        public IReadOnlyList<EchelonScriptErrorMessage> Errors => errorsList;
        public IReadOnlyList<EchelonScriptErrorMessage> Warnings => warningsList;
        public IReadOnlyList<EchelonScriptErrorMessage> InfoMessages => infoList;

        #endregion

        #region ================== Constructors

        protected EchelonScript_Compiler () {
            errorsList = new List<EchelonScriptErrorMessage> ();
            warningsList = new List<EchelonScriptErrorMessage> ();
            infoList = new List<EchelonScriptErrorMessage> ();

            parser = new EchelonScriptParser (errorsList);
            frontend = new CompilerFrontend (errorsList, warningsList, infoList);
            backend = null;

            disposeBackend = false;
        }

        public static EchelonScript_Compiler Create () {
            var comp = new EchelonScript_Compiler ();

            comp.backend = null;

            return comp;
        }

        public static EchelonScript_Compiler Create<TBackend> ()
            where TBackend : ICompilerBackend, new () {
            var comp = new EchelonScript_Compiler ();

            comp.backend = new TBackend ();
            comp.backend.Initialize (comp.errorsList, comp.warningsList, comp.infoList);

            return comp;
        }

        #endregion

        #region ================== Instance methods

        public void Setup (out EchelonScriptEnvironment env) {
            if (environment != null)
                throw new CompilationException ("The compiler has already been set up!");

            environment = EchelonScriptEnvironment.CreateEnvironment (out environmentBuilder);
            frontend.Setup (environment, environmentBuilder);

            env = environment;
        }

        public void AddTranslationUnit (string unitName, ReadOnlySpan<ReadOnlyMemory<char>> codeTransUnit) {
            CheckDisposed ();

            bool foundErrors = false;

            var astUnitsList = new StructPooledList<ES_AbstractSyntaxTree> (CL_ClearMode.Auto);
            {
                foreach (var codeUnit in codeTransUnit) {
                    parser.Reset ();

                    var astUnit = parser.ParseCode (codeUnit);
                    astUnitsList.Add (astUnit);

                    foundErrors |= !astUnit.Valid;
                }
            }

            foundErrors |= errorsList.Count > 0;

            if (foundErrors)
                return;

            frontend.AddUnit (unitName, astUnitsList.Span);
        }

        public void Compile () {
            var code = frontend.CompileCode ();

            if (errorsList.Count > 0)
                return;

            if (backend is not null) {
                backend.CompileEnvironment (environment!, environmentBuilder!, code);

                if (errorsList.Count > 0)
                    return;
            }
        }

        public void Reset () {
            CheckDisposed ();

            errorsList.Clear ();
            warningsList.Clear ();
            infoList.Clear ();
            frontend.Reset ();

            environmentBuilder?.Dispose ();
            environment?.Dispose ();

            environmentBuilder = null;
            environment = null;
        }

        protected void CheckDisposed () {
            if (disposedValue)
                throw new ObjectDisposedException (nameof (EchelonScript_Compiler));
        }

        protected void CheckSetUp () {
            if (environment == null | environmentBuilder == null)
                throw new CompilationException ("The compiler is not set up.");
        }

        #endregion

        #region ================== IDisposable support

        private bool disposedValue = false;

        ~EchelonScript_Compiler () {
            if (!disposedValue)
                DoDispose ();
        }

        protected virtual void DoDispose () {
            if (!disposedValue) {
                parser?.Dispose ();
                frontend?.Dispose ();
                backend?.Dispose ();

                environmentBuilder?.Dispose ();

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
