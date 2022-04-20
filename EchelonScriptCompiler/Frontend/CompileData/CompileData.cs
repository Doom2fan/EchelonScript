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
using ChronosLib.Unmanaged;
using EchelonScriptCommon.Data;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend.Data;

namespace EchelonScriptCompiler.Frontend;

internal ref partial struct CompileData {
    public EchelonScriptEnvironment Env { get; init; }
    public EchelonScriptEnvironment.Builder EnvBuilder { get; init; }

    public List<EchelonScriptErrorMessage> ErrorList { get; init; }
    public List<EchelonScriptErrorMessage> WarnList { get; init; }
    public List<EchelonScriptErrorMessage> InfoList { get; init; }

    public Span<TranslationUnitData> TranslationUnits { get; init; }
    public SymbolStack<FrontendSymbol> Symbols { get; init; }

    public Dictionary<ES_Identifier, ESC_Namespace> Namespaces { get; init; }

    public ES_IdentifierPool IdPool => Env.IdPool;
    public IMemoryManager MemoryManager => EnvBuilder.MemoryManager;

    public ESC_TypeData TypeUnknown { get; init; }
    public ESC_TypeData TypeNull { get; init; }
}
