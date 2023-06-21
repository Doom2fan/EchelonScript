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
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace EchelonScript.Analyzers.Utils;

internal abstract class ParserBase {
    protected readonly CancellationToken cancellationToken;
    protected readonly Compilation compilation;
    protected Action<Diagnostic> reportDiagnostic;

    public ParserBase (Compilation compilation, Action<Diagnostic> reportDiagnostic, CancellationToken cancellationToken) {
        this.compilation = compilation;
        this.reportDiagnostic = reportDiagnostic;
        this.cancellationToken = cancellationToken;
    }

    protected void Diag (DiagnosticDescriptor desc, params object? []? messageArgs)
        => reportDiagnostic (Diagnostic.Create (desc, null, messageArgs));

    protected void Diag (DiagnosticDescriptor desc, Location? location, params object? []? messageArgs)
        => reportDiagnostic (Diagnostic.Create (desc, location, messageArgs));

    protected void Diag (DiagnosticDescriptor desc, Location? location, IEnumerable<Location>? additionalLocations, params object? []? messageArgs)
        => reportDiagnostic (Diagnostic.Create (desc, location, additionalLocations, messageArgs));

    protected void DiagAuto<T> (DiagnosticDescriptor desc, T? locations, params object? []? messageArgs) where T : IReadOnlyList<Location> {
        if (locations is null || locations.Count < 0)
            Diag (desc, messageArgs);
        else if (locations.Count == 1)
            Diag (desc, locations [0], messageArgs);
        else
            Diag (desc, locations [0], locations.Skip (1), messageArgs);
    }

    protected static string GetNamespaceString (TypeDeclarationSyntax typeDecl) {
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

    protected static string? GetAccessMod (Accessibility accessMod) {
        return accessMod switch {
            Accessibility.NotApplicable => string.Empty,

            Accessibility.Private => "private",
            Accessibility.ProtectedAndInternal => throw new NotSupportedException (),
            Accessibility.Protected => "protected",
            Accessibility.Internal => "internal",
            Accessibility.ProtectedOrInternal => "protected internal",
            Accessibility.Public => "public",

            _ => null,
        };
    }

    protected static bool CompareTypeSymbols (INamedTypeSymbol? symbolA, INamedTypeSymbol? symbolB) {
        if (symbolA is null || symbolB is null)
            return false;

        return symbolA.Equals (symbolB, SymbolEqualityComparer.Default);
    }

    protected static bool CheckAttribute (AttributeData attr, INamedTypeSymbol? attrToCheck) {
        if (attr.AttributeClass is null || attrToCheck is null)
            return false;

        return CompareTypeSymbols (attr.AttributeClass, attrToCheck);
    }

    protected static object GetItem (TypedConstant arg) => arg.Kind == TypedConstantKind.Array ? arg.Values! : arg.Value!;
}
