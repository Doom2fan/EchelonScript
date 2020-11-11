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
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Frontend.Parser;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler {
    public class EchelonScriptCompiler : IDisposable {
        #region ================== Instance fields

        protected List<EchelonScriptErrorMessage> errorsList;
        protected EchelonScriptParser parser;
        protected CompilerFrontend frontend;

        #endregion

        #region ================== Instance properties

        public IReadOnlyList<EchelonScriptErrorMessage> Errors => errorsList;

        #endregion

        #region ================== Constructors

        public EchelonScriptCompiler () {
            errorsList = new List<EchelonScriptErrorMessage> ();
            parser = new EchelonScriptParser (errorsList);
            frontend = new CompilerFrontend (errorsList);
        }

        #endregion

        #region ================== Instance methods

        public void CompileTranslationUnit (string unitName, ReadOnlySpan<ReadOnlyMemory<char>> codeTransUnit) {
            CheckDisposed ();

            var astUnitsList = new StructPooledList<ES_AbstractSyntaxTree> (ClearMode.Auto);
            {
                foreach (var codeUnit in codeTransUnit) {
                    parser.Reset ();
                    astUnitsList.Add (parser.ParseCode (codeUnit));
                }
            }

            if (errorsList.Count > 0)
                return;

            frontend.CompileCode (unitName, astUnitsList.Span);

            return;
        }

        public void Reset () {
            CheckDisposed ();

            errorsList.Clear ();
            frontend.Clear ();
        }

        protected void CheckDisposed () {
            if (disposedValue)
                throw new ObjectDisposedException (nameof (EchelonScriptCompiler));
        }

        #endregion

        #region ================== IDisposable support

        private bool disposedValue = false;

        ~EchelonScriptCompiler () {
            if (!disposedValue)
                DoDispose ();
        }

        protected virtual void DoDispose () {
            if (!disposedValue) {
                parser?.Dispose ();
                frontend?.Dispose ();

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
