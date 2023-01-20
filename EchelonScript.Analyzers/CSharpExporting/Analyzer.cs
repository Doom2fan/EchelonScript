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
using EchelonScript.Analyzers.CSharpExporting.Internal;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace EchelonScript.Analyzers.CSharpExporting;

[Generator]
public sealed partial class ES_ExportGenerator : IIncrementalGenerator {
    public const string ExportAttributeNamespace = "EchelonScript.Common.Exporting";
    public const string ExportInterfaceNamespace = "EchelonScript.Common.Exporting";

    public void Initialize (IncrementalGeneratorInitializationContext context) {
        // Do a simple filter for structs.
        /*var structDeclarations = context.SyntaxProvider.CreateSyntaxProvider (
            predicate: static (s, _) => StructGenerator.IsSyntaxTargetForGeneration (s),
            transform: static (ctx, _) => StructGenerator.GetSemanticTargetForGeneration (ctx)
        ).Where (static m => m is not null)!;*/
        var structDeclarations = context.SyntaxProvider.ForAttributeWithMetadataName (
            AggregateExporter_Parser.StructAttributeFullName,
            static (node, _) => node is StructDeclarationSyntax,
            static (context, _) => context.TargetNode as StructDeclarationSyntax
        ).Where (static m => m is not null);

        // Combine the selected structs with the Compilation.
        var compilationAndStructs = context.CompilationProvider.Combine (structDeclarations.Collect ());

        // Generate the source using the compilation and structs.
        context.RegisterSourceOutput (compilationAndStructs, static (spc, source) => ExecuteAggregates (source.Left, source.Right!, spc));

        /*Aggregates:
        if (nm != null && !nm.All (IsValidIdentifier))
            throw new ArgumentException ("Invalid namespace.", nameof (nm));
        else if (!IsValidIdentifier (name))
            throw new ArgumentException ("Invalid name.", nameof (name));*/
        /*Fields:
        if (!IsValidIdentifier (name))
            throw new ArgumentException ("Invalid name.", nameof (name));*/
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
        var result = structEmit.ExportStruct (structsToExport);
        context.AddSource ("ExportedAggregates.g.cs", SourceText.From (result, Encoding.UTF8));
    }

    public static bool IsLatinLetter (char c) => (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');

    public static bool IsIntegerDigit (char c) => c >= '0' && c <= '9';

    public static bool IsValidIdentifier (ReadOnlySpan<char> id) {
        if (id.Length < 1)
            return false;

        if (!IsLatinLetter (id [0]) && id [0] != '_')
            return false;

        foreach (var c in id.Slice (1)) {
            if (!IsLatinLetter (c) && !IsIntegerDigit (c) && c != '_')
                return false;
        }

        return true;
    }
}
