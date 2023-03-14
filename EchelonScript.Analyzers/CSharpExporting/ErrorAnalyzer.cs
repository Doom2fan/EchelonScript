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
        DiagnosticDescriptors.NoReferencesOutsideExports
    );

    public override void Initialize (AnalysisContext context) {
        context.ConfigureGeneratedCodeAnalysis (GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution ();

        context.RegisterCompilationStartAction (
            compilationContext => {
                var analyzer = new Analyzer (compilationContext.Compilation, compilationContext.CancellationToken);

                compilationContext.RegisterSyntaxNodeAction (analyzer.AnalyzeSyntaxNode, ImmutableArray.Create (
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

        public void AnalyzeSyntaxNode (SyntaxNodeAnalysisContext nodeContext) {
            if (nodeContext.Node is not TypeDeclarationSyntax typeNode)
                throw new NotSupportedException ("Invalid node.");

            if (typeNode.Members.Count < 1)
                return;

            reportDiagnostic = nodeContext.ReportDiagnostic;

            try {
                var typeNodeSM = compilation.GetSemanticModel (typeNode.SyntaxTree);
                if (typeNodeSM.GetDeclaredSymbol (typeNode) is not INamedTypeSymbol typeSymbol)
                    return;

                var typeInfo = CheckType (typeSymbol);
                if (!typeInfo.IsStruct && typeInfo.IsExported) {
                    Diag (
                        DiagnosticDescriptors.NonStructExported,
                        typeSymbol.Locations,
                        typeSymbol.Name
                    );
                }

                foreach (var memberNode in typeNode.Members) {
                    cancellationToken.ThrowIfCancellationRequested ();

                    if (memberNode is FieldDeclarationSyntax fieldDecl) {
                        var varDeclaration = fieldDecl.Declaration;

                        if (typeNodeSM.GetDeclaredSymbol (varDeclaration.Type) is not ITypeSymbol fieldType)
                            continue;

                        var fieldTypeInfo = CheckType (fieldType);
                        if (fieldTypeInfo.IsGCRef && (!typeInfo.IsStruct || !typeInfo.IsExported)) {
                            Diag (
                                DiagnosticDescriptors.NoReferencesOutsideExports,
                                varDeclaration.Type.GetLocation (),
                                typeSymbol.Name
                            );
                        }
                    } else if (memberNode is PropertyDeclarationSyntax propDecl) {
                        if (typeNodeSM.GetDeclaredSymbol (propDecl.Type) is not ITypeSymbol propType)
                            continue;

                        var propTypeInfo = CheckType (propType);
                        if (propTypeInfo.IsGCRef && (!typeInfo.IsStruct || !typeInfo.IsExported)) {
                            Diag (
                                DiagnosticDescriptors.NoReferencesOutsideExports,
                                propDecl.Identifier.GetLocation (),
                                typeSymbol.Name
                            );
                        }
                    }
                }
            } finally {
                reportDiagnostic = EmptyAction;
            }
        }
    }
}
