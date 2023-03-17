/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using EchelonScript.Analyzers.CSharpExporting.Internal;
using EchelonScript.Analyzers.RoslynExtensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace EchelonScript.Analyzers.CSharpExporting;

[DiagnosticAnalyzer (LanguageNames.CSharp)]
public class ES_ErrorAnalyzer : DiagnosticAnalyzer {
    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create (
        DiagnosticDescriptors.NonStructExported,
        DiagnosticDescriptors.ReferenceUsedOutsideExport,
        DiagnosticDescriptors.ExportUsedAsValueTypeOutsideExport
    );

    public override void Initialize (AnalysisContext context) {
        context.ConfigureGeneratedCodeAnalysis (GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution ();

        context.RegisterCompilationStartAction (
            compilationContext => {
                var analyzer = new Analyzer (compilationContext.Compilation, compilationContext.CancellationToken);
                analyzer.Initialize ();

                compilationContext.RegisterSyntaxNodeAction (analyzer.AnalyzeTypeDeclarationNode, ImmutableArray.Create (
                    SyntaxKind.StructDeclaration,
                    SyntaxKind.ClassDeclaration,
                    SyntaxKind.RecordDeclaration,
                    SyntaxKind.RecordStructDeclaration
                ));
            }
        );
    }

    private class Analyzer : Utils.ParserBase {
        private struct TypeInfo {
            public bool IsStruct;
            public bool IsExported;
            public bool IsGCRef;

            public bool IsValidExport => IsStruct && IsExported;
        }

        private static Action<Diagnostic> EmptyAction = _ => throw new Exception ("reportDiagnostic not set.");

        private INamedTypeSymbol? structExportAttribute;
        private INamedTypeSymbol? classExportAttribute;

        private INamedTypeSymbol? objectRefInterface;
        private INamedTypeSymbol? arrayRefInterface;

        private ConcurrentDictionary<ITypeSymbol, TypeInfo> checkedTypes;

        public Analyzer (Compilation compilation, CancellationToken cancellationToken)
            : base (compilation, EmptyAction, cancellationToken) {
            checkedTypes = new (SymbolEqualityComparer.Default);
        }

        protected static void Diag (SyntaxNodeAnalysisContext nodeContext, DiagnosticDescriptor desc, params object? []? messageArgs)
            => nodeContext.ReportDiagnostic (Diagnostic.Create (desc, null, messageArgs));

        protected static void Diag (SyntaxNodeAnalysisContext nodeContext, DiagnosticDescriptor desc, Location? location, params object? []? messageArgs)
            => nodeContext.ReportDiagnostic (Diagnostic.Create (desc, location, messageArgs));

        protected static void Diag (SyntaxNodeAnalysisContext nodeContext, DiagnosticDescriptor desc, Location? location, IEnumerable<Location>? additionalLocations, params object? []? messageArgs)
            => nodeContext.ReportDiagnostic (Diagnostic.Create (desc, location, additionalLocations, messageArgs));

        protected static void Diag (SyntaxNodeAnalysisContext nodeContext, DiagnosticDescriptor desc, IReadOnlyList<Location>? locations, params object? []? messageArgs) {
            if (locations is null || locations.Count < 0)
                Diag (nodeContext, desc, messageArgs);
            else if (locations.Count == 1)
                Diag (nodeContext, desc, locations [0], messageArgs);
            else
                Diag (nodeContext, desc, locations [0], locations.Skip (1), messageArgs);
        }

        public void Initialize () {
            structExportAttribute = compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.StructAttributeFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);
            classExportAttribute = compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.ClassAttributeFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);

            objectRefInterface = compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.ObjectRefInterfaceFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);
            arrayRefInterface = compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.ArrayRefInterfaceFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);
        }

        private TypeInfo CheckType (ITypeSymbol typeSymbol) {
            if (checkedTypes.TryGetValue (typeSymbol, out var typeInfo))
                return typeInfo;

            typeInfo = new TypeInfo {
                IsStruct = typeSymbol.TypeKind == TypeKind.Struct,
                IsExported = typeSymbol.GetAttributes ().Any (attr => CheckAttribute (attr, structExportAttribute) || CheckAttribute (attr, classExportAttribute)),
                IsGCRef = typeSymbol.AllInterfaces.Any (i => CompareTypeSymbols (i, objectRefInterface) || CompareTypeSymbols (i, arrayRefInterface)),
            };

            checkedTypes.TryAdd (typeSymbol, typeInfo);

            return typeInfo;
        }

        public void AnalyzeTypeDeclarationNode (SyntaxNodeAnalysisContext nodeContext) {
            if (nodeContext.Node is not TypeDeclarationSyntax typeNode)
                throw new NotSupportedException ("Invalid node.");

            if (typeNode.Members.Count < 1 && typeNode.AttributeLists.Count < 1)
                return;

            var typeNodeSM = compilation.GetSemanticModel (typeNode.SyntaxTree);
            if (typeNodeSM.GetDeclaredSymbol (typeNode) is not INamedTypeSymbol typeSymbol)
                return;

            if (typeNode.Parent is TypeDeclarationSyntax parentTypeNode && typeNodeSM.GetDeclaredSymbol (parentTypeNode) is INamedTypeSymbol parentTypeSymbol) {
                var parentTypeInfo = CheckType (parentTypeSymbol);
                if (typeSymbol.Name.Equals (AggregateExporter_Parser.DefinitionStructName, StringComparison.InvariantCulture) && parentTypeInfo.IsValidExport)
                    return;
            }

            var typeInfo = CheckType (typeSymbol);
            if (!typeInfo.IsStruct && typeInfo.IsExported) {
                Diag (
                    nodeContext,

                    DiagnosticDescriptors.NonStructExported,
                    typeSymbol.Locations,
                    typeSymbol.Name
                );
            }

            foreach (var memberNode in typeNode.Members) {
                cancellationToken.ThrowIfCancellationRequested ();

                if (memberNode is FieldDeclarationSyntax fieldDecl) {
                    var varDeclaration = fieldDecl.Declaration;

                    if (typeNodeSM.GetTypeInfo (varDeclaration.Type).ConvertedType is not ITypeSymbol fieldType)
                        continue;

                    var fieldTypeInfo = CheckType (fieldType);
                    if (fieldTypeInfo.IsGCRef && !typeInfo.IsValidExport) {
                        foreach (var variable in varDeclaration.Variables) {
                            Diag (
                                nodeContext,

                                DiagnosticDescriptors.ReferenceUsedOutsideExport,
                                variable.Identifier.GetLocation (),
                                variable.Identifier.ToString (),
                                typeSymbol.Name
                            );
                        }
                    } else if (fieldTypeInfo.IsValidExport && !typeInfo.IsValidExport) {
                        foreach (var variable in varDeclaration.Variables) {
                            Diag (
                                nodeContext,

                                DiagnosticDescriptors.ExportUsedAsValueTypeOutsideExport,
                                variable.Identifier.GetLocation (),
                                variable.Identifier.ToString (),
                                typeSymbol.Name,
                                fieldType.Name
                            );
                        }
                    }
                } else if (memberNode is PropertyDeclarationSyntax propDecl) {
                    if (typeNodeSM.GetTypeInfo (propDecl.Type).ConvertedType is not ITypeSymbol propType)
                        continue;

                    var propTypeInfo = CheckType (propType);
                    if (propTypeInfo.IsGCRef && !typeInfo.IsValidExport) {
                        Diag (
                            nodeContext,

                            DiagnosticDescriptors.ReferenceUsedOutsideExport,
                            propDecl.Identifier.GetLocation (),
                            propDecl.Identifier.ToString (),
                            typeSymbol.Name
                        );
                    } else if (propTypeInfo.IsValidExport && !typeInfo.IsValidExport) {
                        Diag (
                            nodeContext,

                            DiagnosticDescriptors.ExportUsedAsValueTypeOutsideExport,
                            propDecl.Identifier.GetLocation (),
                            propDecl.Identifier.ToString (),
                            typeSymbol.Name,
                            propType.Name
                        );
                    }
                }
            }
        }
    }
}
