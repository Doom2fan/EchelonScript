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
using EchelonScript.Analyzers.RoslynExtensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace EchelonScript.Analyzers.CSharpExporting.Internal;


internal sealed class AggregateExporter_Parser : Utils.ParserBase {
    public const string StructAttributeName = "ES_ExportStructAttribute";
    public const string ClassAttributeName = "ES_ExportClassAttribute";
    public const string StructAttributeFullName = $"{ES_ExportGenerator.ExportAttributeNamespace}.{StructAttributeName}";
    public const string ClassAttributeFullName = $"{ES_ExportGenerator.ExportAttributeNamespace}.{ClassAttributeName}";

    public AggregateExporter_Parser (Compilation compilation, Action<Diagnostic> reportDiagnostic, CancellationToken cancellationToken)
        : base (compilation, reportDiagnostic, cancellationToken) { }

    public IReadOnlyList<ExportedStruct> GetTypesToGenerate (IEnumerable<StructDeclarationSyntax> structs) {
        var hadError = false;
        var structsToGenerate = new List<ExportedStruct> ();

        // Get the semantic representation of our marker attributes.
        var structExportAttribute = compilation.GetAnalyzerTypeByMetadataName (StructAttributeFullName);
        if (structExportAttribute == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, null, StructAttributeFullName);
            return Array.Empty<ExportedStruct> ();
        }

        var classExportAttribute = compilation.GetAnalyzerTypeByMetadataName (ClassAttributeFullName);
        if (classExportAttribute == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, null, ClassAttributeFullName);
            return Array.Empty<ExportedStruct> ();
        }

        var fields = new List<ExportedField> ();
        foreach (var structDecl in structs) {
            // Stop if we're asked to.
            cancellationToken.ThrowIfCancellationRequested ();

            var structError = hadError;
            // Get the semantic representation of the struct syntax.
            var structSM = compilation.GetSemanticModel (structDecl.SyntaxTree);
            if (structSM.GetDeclaredSymbol (structDecl) is not INamedTypeSymbol structSymbol)
                continue;

            if (!structDecl.Modifiers.Any (SyntaxKind.PartialKeyword)) {
                Diag (
                    DiagnosticDescriptors.StructNotPartial,
                    structDecl.Identifier.GetLocation (),
                    structDecl.Identifier.ToString ()
                );
                structError = true;
            }

            // Get the full type name of the struct.
            var structName = structSymbol.ToString ();

            var structMembers = structSymbol.GetMembers ();
            foreach (var member in structDecl.Members) {
                var isStatic = false;
                var isPartial = false;
                var fieldError = structError;

                foreach (var mod in member.Modifiers) {
                    switch (mod.Kind ()) {
                        case SyntaxKind.StaticKeyword: isStatic = true; break;
                        case SyntaxKind.PartialKeyword: isPartial = true; break;
                    }
                }

                switch (member) {
                    case FieldDeclarationSyntax fieldDecl:
                        if (isStatic)
                            continue;

                        foreach (var varDecl in fieldDecl.Declaration.Variables) {
                            Diag (
                                DiagnosticDescriptors.InstanceFieldInAggregate,
                                varDecl.Identifier.GetLocation (),
                                varDecl.Identifier.ToString ()
                            );
                        }
                        structError = true;
                        continue;

                    case MethodDeclarationSyntax methodDecl:
                        throw new NotImplementedException ();

                    case PropertyDeclarationSyntax propDecl: {
                        if (isStatic)
                            continue;

                        if (!isPartial) {
                            Diag (
                                DiagnosticDescriptors.ExportedPropertiesMustBePartial,
                                propDecl.Identifier.GetLocation (),
                                propDecl.Identifier.ToString ()
                            );
                            fieldError = true;
                        }

                        // Skip adding this if there were any errors.
                        if (fieldError) {
                            structError = true;
                            continue;
                        }

                        break;
                    }
                }
            }

            if (structError) {
                hadError = true;
                continue;
            }

            static bool IsAllowedKind (SyntaxKind kind) =>
                kind == SyntaxKind.ClassDeclaration ||
                kind == SyntaxKind.StructDeclaration ||
                kind == SyntaxKind.RecordDeclaration;

            var parentClasses = Array.Empty<string> ();
            if (structDecl.Parent is TypeDeclarationSyntax parentClass) {
                var parentClassesList = new List<string> ();
                while (parentClass != null && IsAllowedKind (parentClass.Kind ())) {
                    if (parentClass.TypeParameterList?.Parameters.Count > 0) {
                        Diag (
                            DiagnosticDescriptors.ExportedTypeNestedInGeneric,
                            structDecl.Identifier.GetLocation (),
                            structDecl.Identifier.ToString ()
                        );

                        parentClassesList.Clear ();
                        break;
                    }
                    parentClassesList.Add ($"{parentClass.Keyword.ValueText} {parentClass.Identifier}");
                    parentClass = (parentClass.Parent as TypeDeclarationSyntax)!;
                }

                parentClasses = parentClassesList.ToArray ();
            }

            if (structError) {
                hadError = true;
                continue;
            }

            var fieldsArr = fields.ToArray ();
            fields.Capacity = 0;
            structsToGenerate.Add (new ExportedStruct {
                NativeName = structName,
                NativeNamespace = GetNamespaceString (structDecl),
                NativeParents = parentClasses,

                //ExportNamespace = ,
                //ExportName = ,

                Fields = fieldsArr,
            });
        }

        if (hadError)
            return Array.Empty<ExportedStruct> ();

        return structsToGenerate;
    }
}
