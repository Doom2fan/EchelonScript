/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using ChronosLib.Pooled;
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.CompilerCommon;

namespace EchelonScript.Compiler.Frontend.AST;

public sealed class ES_AbstractSyntaxTree : ES_AstNode {
    public readonly ReadOnlyMemory<char> FileName;
    public readonly ReadOnlyMemory<char> Source;

    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public bool Valid;
    public ES_AstImportStatement [] ImportStatements;
    public ES_AstTypeAlias [] TypeAliases;
    public ES_AstNamespace [] Namespaces;

    public ES_AbstractSyntaxTree (
        ReadOnlyMemory<char> source, ReadOnlyMemory<char> fileName,
        ES_AstImportStatement [] imports, ES_AstTypeAlias [] aliases, ES_AstNamespace [] namespaces,
        ES_AstNodeBounds bounds
    ) : base (1) {
        Source = source;
        FileName = fileName;

        ImportStatements = imports;
        TypeAliases = aliases;
        Namespaces = namespaces;

        this.bounds = bounds;
    }
}

public sealed class ES_AstNamespace : ES_AstNode {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstNamespaceIdentifier NamespaceName;
    public ES_AstNode? [] Contents;

    public ES_AstNamespace (
        ES_AstNamespaceIdentifier name, ES_AstNode? [] contents,
        EchelonScriptToken namespaceTk, EchelonScriptToken closeBraceTk
    ) : base (1) {
        NamespaceName = name;
        Contents = contents;

        bounds = new ES_AstNodeBounds {
            StartPos = namespaceTk.TextStartPos,
            EndPos = closeBraceTk.TextEndPos,
        };
    }
}
