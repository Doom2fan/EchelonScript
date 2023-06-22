/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Collections.Immutable;
using EchelonScript.Common.Utilities;
using EchelonScript.Compiler.CompilerCommon;

namespace EchelonScript.Compiler.Frontend;

[NonCopyable]
internal ref partial struct CompileData {
    public ES_CompilerContext Context { get; init; }

    public readonly SourceMap SourceMap => Context.SourceMap;
    public readonly ImmutableArray<TranslationUnitData> TranslationUnits => Context.TranslationUnits;

    public SymbolStack<FrontendSymbol> Symbols { get; init; }
}
