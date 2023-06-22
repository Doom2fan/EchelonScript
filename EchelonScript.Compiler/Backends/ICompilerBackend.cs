/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics.CodeAnalysis;
using EchelonScript.Common;
using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.CompilerCommon.MIR;

namespace EchelonScript.Compiler.Backends;

public unsafe interface IBackendData : IDisposable {
    #region ================== Instance properties

    public bool IsDisposed { get; }

    #endregion

    #region ================== Instance methods

    T? GetFunctionDelegate<T> ([DisallowNull] ES_FunctionData* func) where T : Delegate;

    #endregion
}

public unsafe interface ICompilerBackend : IDisposable {
    #region ================== Instance properties

    public bool IsDisposed { get; }

    #endregion

    #region ================== Instance methods

    void Initialize ();

    bool CompileEnvironment (ES_CompilerContext context, ES_ObjectConst<MIRTree> mirTree);

    #endregion
}
