/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Immutable;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;

namespace EchelonScript.Compiler;

public class ES_CompilerContext {
    public SourceMap SourceMap { get; private init; } = new ();
    public ImmutableArray<TranslationUnitData> TranslationUnits { get; init; }

    public ES_CompilerContext () { }

    public Action<ES_Diagnostic> ReportDiagnostic { get; init; } = (diag) => throw new CompilationException ("Missing ReportDiagnostic delegate");
}
