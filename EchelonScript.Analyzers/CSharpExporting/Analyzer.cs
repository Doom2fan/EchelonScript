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
using System.Text;
using System.Threading;
using EchelonScript.Analyzers.CSharpExporting.Internal;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace EchelonScript.Analyzers.CSharpExporting;

[Generator]
public sealed partial class ES_ExportGenerator : IIncrementalGenerator {
    private static readonly SymbolDisplayFormat attrSymbolDisplayFormat = new (
        globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Omitted,
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes
    );

    public const string TypeCommonNamespace = "EchelonScript.Common";
    public const string ExportAttributeNamespace = $"{TypeCommonNamespace}.Exporting";
    public const string ExportInterfaceNamespace = $"{TypeCommonNamespace}.Exporting";

    public void Initialize (IncrementalGeneratorInitializationContext context) {
        // Filter for the structs with the attributes we want.
        var structDeclarations = context.SyntaxProvider.CreateSyntaxProvider (
            predicate: static (node, _) => node is StructDeclarationSyntax s && s.AttributeLists.Count > 0,
            transform: SelectTargetDeclarations
        ).Where (static m => m.Item1 is not null && m.Item2 is not null);

        // Combine the selected structs with the Compilation.
        var compilationAndStructs = context.CompilationProvider.Combine (structDeclarations.Collect ());

        // Generate the source using the compilation and structs.
        context.RegisterSourceOutput (compilationAndStructs, static (spc, source) => ExecuteAggregates (source.Left, source.Right!, spc));
    }

    private static (StructDeclarationSyntax?, INamedTypeSymbol?) SelectTargetDeclarations (GeneratorSyntaxContext ctx, CancellationToken cancellationToken) {
        var decl = (StructDeclarationSyntax) ctx.Node;

        if (ctx.SemanticModel.GetDeclaredSymbol (decl, cancellationToken) is not INamedTypeSymbol symbol)
            return (null, null);

        foreach (var attr in symbol.GetAttributes ()) {
            var attrName = attr.AttributeClass?.ToDisplayString (attrSymbolDisplayFormat);
            if (AggregateExporter_Parser.StructAttributeFullName.Equals (attrName, StringComparison.InvariantCulture) ||
                AggregateExporter_Parser.ClassAttributeFullName.Equals (attrName, StringComparison.InvariantCulture))
                return (decl, symbol);
        }

        return (null, null);
    }

    private static void ExecuteAggregates (Compilation compilation, ImmutableArray<(StructDeclarationSyntax, INamedTypeSymbol)> structs, SourceProductionContext context) {
        if (structs.IsDefaultOrEmpty)
            return;

        var structGen = new AggregateExporter_Parser (compilation, context.ReportDiagnostic, context.CancellationToken);
        var structsToExport = structGen.GetTypesToGenerate (structs);

        if (structsToExport.Count < 0)
            return;

        // Generate the source code and add it to the output.
        var structEmit = new AggregateExporter_Emitter (structsToExport, context.CancellationToken);
        var result = structEmit.Emit ();
        context.AddSource ("ExportedAggregates.g.cs", SourceText.From (result, Encoding.UTF8));
    }
}
