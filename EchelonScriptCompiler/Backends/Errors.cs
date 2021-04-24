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
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Backends {
    public static class ES_BackendErrors {
        #region Compilation errors

        #region Generation functions

        #endregion

        #endregion

        #region Exceptions

        public const string EnvIsNull = "No environment started. BeginEnvironment must be called before calling this function.";
        public const string TransUnitIsNull = "No translation unit started. BeginTranslationUnit must be called before calling this function.";

        public const string EnvStarted = "An environment was already started. EndEnvironment or Reset must be called before calling this function.";
        public const string TransUnitStarted = "A translation unit was already started. EndTranslationUnit or Reset must be called before calling this function.";

        public const string NonExistentSymbol = "Non-existent symbol reached backend.";
        public const string FrontendError = "Type-checking or semantic analysis error reached backend.";

        #endregion
    }

    public static class ES_BackendWarnings {
        #region Compilation warnings

        #endregion
    }

    public static class ES_BackendInfoMsg {
        #region Compilation info

        #endregion
    }
}