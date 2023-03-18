/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Collections.Immutable;
using System.Text;
using EchelonScript.Analyzers.CSharpExporting.Internal;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace EchelonScript.Analyzers.CSharpExporting;

[Generator]
public sealed partial class ES_ExportGenerator : IIncrementalGenerator {
    public const string TypeCommonNamespace = "EchelonScript.Common";
    public const string ExportAttributeNamespace = $"{TypeCommonNamespace}.Exporting";
    public const string ExportInterfaceNamespace = $"{TypeCommonNamespace}.Exporting";

    public void Initialize (IncrementalGeneratorInitializationContext context) {
        // Do a simple filter for structs.
        var structDeclarations = context.SyntaxProvider.ForAttributeWithMetadataName (
            AggregateExporter_Parser.StructAttributeFullName,
            static (node, _) => node is StructDeclarationSyntax,
            static (context, _) => context.TargetNode as StructDeclarationSyntax
        ).Where (static m => m is not null);

        // Combine the selected structs with the Compilation.
        var compilationAndStructs = context.CompilationProvider.Combine (structDeclarations.Collect ());

        // Generate the source using the compilation and structs.
        context.RegisterSourceOutput (compilationAndStructs, static (spc, source) => ExecuteAggregates (source.Left, source.Right!, spc));
    }

    private static void ExecuteAggregates (Compilation compilation, ImmutableArray<StructDeclarationSyntax> structs, SourceProductionContext context) {
        if (structs.IsDefaultOrEmpty)
            return;

        var structGen = new AggregateExporter_Parser (compilation, context.ReportDiagnostic, context.CancellationToken);
        var structsToExport = structGen.GetTypesToGenerate (structs);

        if (structsToExport.Count < 0)
            return;

        // Generate the source code and add it to the output.
        var structEmit = new AggregateExporter_Emitter ();
        var result = structEmit.Emit (structsToExport, context.CancellationToken);
        context.AddSource ("ExportedAggregates.g.cs", SourceText.From (result, Encoding.UTF8));
    }
}
