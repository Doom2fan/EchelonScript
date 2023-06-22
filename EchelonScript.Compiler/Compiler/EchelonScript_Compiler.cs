/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using EchelonScript.Common;
using EchelonScript.Common.Data;
using EchelonScript.Compiler.Backends;
using EchelonScript.Compiler.CompilerCommon.MIR;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.Parser;

namespace EchelonScript.Compiler;

public sealed class EchelonScript_Compiler : IDisposable {
    #region ================== Instance fields

    private List<ES_Diagnostic> diagnosticsList;
    private EchelonScriptParser parser;
    private ICompilerBackend? backend;

    private bool disposeBackend;

    #endregion

    #region ================== Instance properties

    public IReadOnlyList<ES_Diagnostic> Diagnostics => diagnosticsList;

    public bool HasBackend => backend is not null;

    #endregion

    #region ================== Constructors

    private EchelonScript_Compiler () {
        diagnosticsList = new List<ES_Diagnostic> ();

        parser = new EchelonScriptParser (diagnosticsList);
        backend = null;

        disposeBackend = false;
    }

    public static EchelonScript_Compiler Create () => new () { backend = null, };

    public static EchelonScript_Compiler Create<TBackend> ()
        where TBackend : ICompilerBackend, new () {
        var comp = new EchelonScript_Compiler { backend = new TBackend (), };
        //comp.backend.Initialize (comp.diagnosticsList);

        return comp;
    }

    #endregion

    #region ================== Instance methods

    public void Setup (ES_IdentifierPool idPool, out EchelonScriptEnvironment env) {
        throw new NotImplementedException ();
        /*if (environment != null)
            throw new CompilationException ("The compiler has already been set up!");

        environment = EchelonScriptEnvironment.CreateEnvironment (idPool, out environmentBuilder);
        frontend.Setup (environment, environmentBuilder);

        env = environment;*/
    }

    public void AddTranslationUnit (string unitName, ReadOnlySpan<(ReadOnlyMemory<char>, ReadOnlyMemory<char>)> codeTransUnit) {
        throw new NotImplementedException ();
        /*CheckDisposed ();

        var foundErrors = false;

        using var astUnitsList = new StructPooledList<ES_AbstractSyntaxTree> (CL_ClearMode.Auto);
        {
            foreach (var codeUnit in codeTransUnit) {
                parser.Reset ();

                var astUnit = parser.ParseCode (codeUnit.Item1, codeUnit.Item2);
                astUnitsList.Add (astUnit);

                foundErrors |= !astUnit.Valid;
            }
        }

        foundErrors |= diagnosticsList.Count > 0;

        if (foundErrors)
            return;

        frontend.AddUnit (unitName, astUnitsList.Span);*/
    }

    public bool Compile () {throw new NotImplementedException ();
        /*var code = frontend.CompileCode ();

        if (diagnosticsList.Count > 0 || code.IsNull ())
            return false;

        if (backend is null)
            return true;

        if (!backend.CompileEnvironment (environment!, environmentBuilder!, code))
            return false;

        if (diagnosticsList.Count > 0)
            return false;

        return true;*/
    }

    public ES_ObjectConst<MIRTree> CompileIR () {
        throw new NotImplementedException ();
    }

    public void Reset () {
        CheckDisposed ();

        diagnosticsList.Clear ();
    }

    private void CheckDisposed () {
        if (IsDisposed)
            throw new ObjectDisposedException (nameof (EchelonScript_Compiler));
    }

    private void CheckSetUp () {
        throw new NotImplementedException ();
    }

    #endregion

    #region ================== IDisposable support

    public bool IsDisposed { get; private set; }

    ~EchelonScript_Compiler () {
        if (!IsDisposed)
            DoDispose ();
    }

    private void DoDispose () {
        if (IsDisposed)
            return;

        parser?.Dispose ();
        if (disposeBackend)
            backend?.Dispose ();

        IsDisposed = true;
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion
}
