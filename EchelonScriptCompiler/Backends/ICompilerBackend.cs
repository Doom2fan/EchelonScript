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
using System.Diagnostics.CodeAnalysis;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend;

namespace EchelonScriptCompiler.Backends {
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

        void Initialize (
            List<EchelonScriptErrorMessage> errList,
            List<EchelonScriptErrorMessage> warnList,
            List<EchelonScriptErrorMessage> infoMsgList
        );

        bool CompileEnvironment (EchelonScriptEnvironment environment, EchelonScriptEnvironment.Builder builder, Span<TranslationUnitData> transUnits);

        #endregion
    }
}
