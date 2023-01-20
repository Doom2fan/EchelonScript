/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace EchelonScript.Analyzers.Utils;

internal abstract class ParserBase {
    protected readonly CancellationToken cancellationToken;
    protected readonly Compilation compilation;
    private readonly Action<Diagnostic> reportDiagnostic;

    public ParserBase (Compilation compilation, Action<Diagnostic> reportDiagnostic, CancellationToken cancellationToken) {
        this.compilation = compilation;
        this.reportDiagnostic = reportDiagnostic;
        this.cancellationToken = cancellationToken;
    }

    protected void Diag (DiagnosticDescriptor desc, Location? location, params object? []? messageArgs)
        => reportDiagnostic (Diagnostic.Create (desc, location, messageArgs));

    protected string GetNamespaceString (TypeDeclarationSyntax typeDecl) {
        // determine the namespace the class is declared in, if any
        var potentialNamespaceParent = typeDecl.Parent;
        while (potentialNamespaceParent != null &&
               potentialNamespaceParent is not NamespaceDeclarationSyntax &&
               potentialNamespaceParent is not FileScopedNamespaceDeclarationSyntax)
            potentialNamespaceParent = potentialNamespaceParent.Parent;

        var nspace = string.Empty;
        if (potentialNamespaceParent is BaseNamespaceDeclarationSyntax namespaceParent) {
            nspace = namespaceParent.Name.ToString ();
            while (true) {
                namespaceParent = (namespaceParent.Parent as NamespaceDeclarationSyntax)!;
                if (namespaceParent == null)
                    break;

                nspace = $"{namespaceParent.Name}.{nspace}";
            }
        }

        return nspace;
    }
}
