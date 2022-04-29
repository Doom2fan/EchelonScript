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
using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCompiler.Backends;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Frontend.Parser;

namespace EchelonScriptCompiler;

public sealed class EchelonScript_Compiler : IDisposable {
    #region ================== Instance fields

    private List<EchelonScriptErrorMessage> errorsList;
    private List<EchelonScriptErrorMessage> warningsList;
    private List<EchelonScriptErrorMessage> infoList;
    private EchelonScriptParser parser;
    private CompilerFrontend frontend;
    private ICompilerBackend? backend;

    private bool disposeBackend;

    private EchelonScriptEnvironment? environment;
    private EchelonScriptEnvironment.Builder? environmentBuilder;

    #endregion

    #region ================== Instance properties

    public IReadOnlyList<EchelonScriptErrorMessage> Errors => errorsList;
    public IReadOnlyList<EchelonScriptErrorMessage> Warnings => warningsList;
    public IReadOnlyList<EchelonScriptErrorMessage> InfoMessages => infoList;

    public bool HasBackend => backend is not null;

    #endregion

    #region ================== Constructors

    private EchelonScript_Compiler () {
        errorsList = new List<EchelonScriptErrorMessage> ();
        warningsList = new List<EchelonScriptErrorMessage> ();
        infoList = new List<EchelonScriptErrorMessage> ();

        parser = new EchelonScriptParser (errorsList);
        frontend = new CompilerFrontend (errorsList, warningsList, infoList);
        backend = null;

        disposeBackend = false;
    }

    public static EchelonScript_Compiler Create () => new () { backend = null, };

    public static EchelonScript_Compiler Create<TBackend> ()
        where TBackend : ICompilerBackend, new () {
        var comp = new EchelonScript_Compiler { backend = new TBackend (), };
        comp.backend.Initialize (comp.errorsList, comp.warningsList, comp.infoList);

        return comp;
    }

    #endregion

    #region ================== Instance methods

    public void Setup (ES_IdentifierPool idPool, out EchelonScriptEnvironment env) {
        if (environment != null)
            throw new CompilationException ("The compiler has already been set up!");

        environment = EchelonScriptEnvironment.CreateEnvironment (idPool, out environmentBuilder);
        frontend.Setup (environment, environmentBuilder);

        env = environment;
    }

    public void AddTranslationUnit (string unitName, ReadOnlySpan<(ReadOnlyMemory<char>, ReadOnlyMemory<char>)> codeTransUnit) {
        CheckDisposed ();

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

        foundErrors |= errorsList.Count > 0;

        if (foundErrors)
            return;

        frontend.AddUnit (unitName, astUnitsList.Span);
    }

    public bool Compile () {
        var code = frontend.CompileCode ();

        if (errorsList.Count > 0 || code is null)
            return false;

        if (backend is null)
            return true;

        if (!backend.CompileEnvironment (environment!, environmentBuilder!, code))
            return false;

        if (errorsList.Count > 0)
            return false;

        return true;
    }

    public ESIR_Tree? CompileIR () {
        var code = frontend.CompileCode ();

        if (errorsList.Count > 0)
            return null;

        return code;
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

    private void CheckDisposed () {
        if (IsDisposed)
            throw new ObjectDisposedException (nameof (EchelonScript_Compiler));
    }

    private void CheckSetUp () {
        if (environment == null | environmentBuilder == null)
            throw new CompilationException ("The compiler is not set up.");
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
        frontend?.Dispose ();
        if (disposeBackend)
            backend?.Dispose ();

        environmentBuilder?.Dispose ();

        IsDisposed = true;
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion
}
