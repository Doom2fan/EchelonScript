/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend.Data;

namespace EchelonScriptCompiler.Frontend;

internal ref partial struct CompileData {
    public ESC_Namespace? GetNamespace (ReadOnlySpan<char> name) => GetNamespace (IdPool.GetIdentifier (name));
    public ESC_Namespace? GetNamespace (ES_Identifier name) => Namespaces.TryGetValue (name, out var ns) ? ns : null;

    public ESC_Namespace? GetNamespace (SourceData src, ES_AstNodeBounds nodeBounds, ReadOnlySpan<char> nsName)
        => GetNamespace (src, nodeBounds, IdPool.GetIdentifier (nsName));
    public ESC_Namespace? GetNamespace (SourceData src, ES_AstNodeBounds nodeBounds, ES_Identifier nsName) {
        if (!Namespaces.TryGetValue (nsName, out var ns)) {
            var err = ES_FrontendErrors.GenNamespaceDoesntExist (
                nsName.GetCharsSpan ().GetPooledString (),
                src,
                nodeBounds
            );
            ErrorList.Add (err);
            return null;
        }

        return ns;
    }

    public ESC_Namespace GetOrCreateNamespace (ReadOnlySpan<char> name)
        => GetOrCreateNamespace (IdPool.GetIdentifier (name));
    public ESC_Namespace GetOrCreateNamespace (ES_Identifier name) {
        if (Namespaces.TryGetValue (name, out var ns))
            return ns;

        ns = new ESC_Namespace (name);
        Namespaces [name] = ns;

        return ns;
    }
}
