/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Collections.Immutable;
using System.Linq;
using EchelonScript.Analyzers.CSharpExporting.Internal;
using EchelonScript.Analyzers.RoslynExtensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;

namespace EchelonScript.Analyzers.CSharpExporting;

[DiagnosticAnalyzer (LanguageNames.CSharp)]
public class ES_ExportDiagnosticSuppressor : DiagnosticSuppressor {
    private static readonly SuppressionDescriptor suppressUnusedFieldsRule = new SuppressionDescriptor (
        id: "SPR0001",
        suppressedDiagnosticId: "CS0169",
        justification: "Field symbols inside an exported struct's definition struct are used only by the source generator"
    );
    private static readonly SuppressionDescriptor suppressUnassignedFieldsRule = new SuppressionDescriptor (
        id: "SPR0002",
        suppressedDiagnosticId: "CS0649",
        justification: suppressUnusedFieldsRule.Justification
    );

    public override ImmutableArray<SuppressionDescriptor> SupportedSuppressions => ImmutableArray.Create (
        suppressUnusedFieldsRule,
        suppressUnassignedFieldsRule
    );

    public override void ReportSuppressions (SuppressionAnalysisContext context) {
        // Get the semantic representation of our marker attributes.
        var structExpAttr = context.Compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.StructAttributeFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);
        var classExpAttr = context.Compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.ClassAttributeFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);
        var supportedSuppressions = SupportedSuppressions;
        static bool CheckAttribute (AttributeData attr, INamedTypeSymbol? attrToCheck) {
            if (attr.AttributeClass is null || attrToCheck is null)
                return false;

            return attr.AttributeClass.Equals (attrToCheck, SymbolEqualityComparer.Default);
        }

        foreach (var diag in context.ReportedDiagnostics) {
            context.CancellationToken.ThrowIfCancellationRequested ();

            if (diag is null || diag.Location.SourceTree is null)
                continue;

            var node = diag.Location.SourceTree.GetRoot (context.CancellationToken).FindNode (diag.Location.SourceSpan);
            if (node == null)
                continue;

            var model = context.GetSemanticModel (node.SyntaxTree);
            var declaredSymbol = model.GetDeclaredSymbol (node, context.CancellationToken);
            if (declaredSymbol is null)
                continue;

            // Check if this is in struct with the expected name.
            if (!declaredSymbol.ContainingType.Name.Equals (AggregateExporter_Parser.DefinitionStructName))
                continue;

            // Skip if this struct isn't contained inside another type.
            var structSymbol = declaredSymbol.ContainingType;
            if (structSymbol.ContainingType is null)
                continue;

            // Check if this really is an exported struct.
            if (!structSymbol.ContainingType.GetAttributes ().Any (a => CheckAttribute (a, structExpAttr) || CheckAttribute (a, classExpAttr)))
                continue;

            foreach (var suppression in supportedSuppressions) {
                if (suppression.SuppressedDiagnosticId != diag.Id)
                    continue;

                context.ReportSuppression (Suppression.Create (suppression, diag));
                break;
            }
        }
    }
}
