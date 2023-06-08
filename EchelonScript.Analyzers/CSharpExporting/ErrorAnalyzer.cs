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
using System.Reflection;
using System.Threading;
using EchelonScript.Analyzers.CSharpExporting.Internal;
using EchelonScript.Analyzers.RoslynExtensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace EchelonScript.Analyzers.CSharpExporting;

[DiagnosticAnalyzer (LanguageNames.CSharp)]
public class ES_ErrorAnalyzer : DiagnosticAnalyzer {
    // TODO: Disallow GC refs from being used arbitrarily in methods.
    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create (
        DiagnosticDescriptors.NonStructExported,
        DiagnosticDescriptors.ReferenceUsedOutsideExport,
        DiagnosticDescriptors.ExportUsedAsValueTypeOutsideExport,
        DiagnosticDescriptors.DefinitionStructReferenced,
        DiagnosticDescriptors.ClassUsedAsAValueType
    );

    public override void Initialize (AnalysisContext context) {
        context.ConfigureGeneratedCodeAnalysis (GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution ();

        context.RegisterCompilationStartAction (
            compilationContext => {
                var analyzer = new Analyzer (compilationContext.Compilation, compilationContext.CancellationToken);
                analyzer.Initialize (compilationContext);
            }
        );
    }

    private class Analyzer : Utils.ParserBase {
        private struct TypeInfo {
            public ITypeSymbol Symbol;
            public bool IsStruct;

            public bool IsExportedStruct;
            public bool IsExportedClass;

            public bool IsObjectRef;
            public bool IsArrayRef;

            public bool IsExportDefinition;

            public bool IsExported => IsExportedClass | IsExportedStruct;
            public bool IsGCRef => IsObjectRef | IsArrayRef;
            public bool IsValidExport => IsStruct && IsExported;
        }

        private static Action<Diagnostic> EmptyAction = _ => throw new Exception ("reportDiagnostic not set.");

        private INamedTypeSymbol? structExportAttribute;
        private INamedTypeSymbol? classExportAttribute;
        private INamedTypeSymbol? allowExportedClassAttribute;

        private INamedTypeSymbol? objectRefInterface;
        private INamedTypeSymbol? arrayRefInterface;

        private ConcurrentDictionary<ITypeSymbol, TypeInfo> checkedTypes;

        public Analyzer (Compilation compilation, CancellationToken cancellationToken)
            : base (compilation, EmptyAction, cancellationToken) {
            checkedTypes = new (SymbolEqualityComparer.Default);
        }

        #region Diag

        protected static void Diag<T> (T context, Diagnostic diag) {
            if (context is SyntaxNodeAnalysisContext syntaxContext)
                syntaxContext.ReportDiagnostic (diag);
            else if (context is SymbolAnalysisContext symbolContext)
                symbolContext.ReportDiagnostic (diag);
            else if (context is OperationAnalysisContext opContext)
                opContext.ReportDiagnostic (diag);
        }

        protected static void Diag<T> (T context, DiagnosticDescriptor desc, params object? []? messageArgs)
            => Diag (context, Diagnostic.Create (desc, null, messageArgs));

        protected static void Diag<T> (T context, DiagnosticDescriptor desc, Location? location, params object? []? messageArgs)
            => Diag (context, Diagnostic.Create (desc, location, messageArgs));

        protected static void Diag<T> (T context, DiagnosticDescriptor desc, Location? location, IEnumerable<Location>? additionalLocations, params object? []? messageArgs)
            => Diag (context, Diagnostic.Create (desc, location, additionalLocations, messageArgs));

        protected static void Diag<T, U> (T context, DiagnosticDescriptor desc, U? locations, params object? []? messageArgs) where U : IReadOnlyList<Location> {
            if (locations is null || locations.Count < 0)
                Diag (context, Diagnostic.Create (desc, null, messageArgs));
            else if (locations.Count == 1)
                Diag (context, Diagnostic.Create (desc, locations [0], messageArgs));
            else
                Diag (context, Diagnostic.Create (desc, locations [0], locations.Skip (1), messageArgs));
        }

        protected static void DiagAuto<T, U> (T context, DiagnosticDescriptor desc, U? location, params object? []? messageArgs) {
            if (location is null)
                return;

            if (location is IReadOnlyList<Location> locationsReadOnlyList)
                Diag (context, desc, locationsReadOnlyList, messageArgs);
            else if (location is Location locationSingle)
                Diag (context, desc, locationSingle, messageArgs);
            else
                throw new ArgumentException ($"Invalid argument type. ({nameof (U)})", nameof (location));
        }

        #endregion

        public void Initialize (CompilationStartAnalysisContext compilationContext) {
            RegisterActions (compilationContext);

            structExportAttribute = compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.StructAttributeFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);
            classExportAttribute = compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.ClassAttributeFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);
            allowExportedClassAttribute = compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.AllowExportedClassAttributeFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);

            objectRefInterface = compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.ObjectRefInterfaceFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);
            arrayRefInterface = compilation.GetTypeByMetadataNameAndAssembly (AggregateExporter_Parser.ArrayRefInterfaceFullName, AggregateExporter_Parser.AssemblyNameEchelonCommon);
        }

        private void RegisterActions (CompilationStartAnalysisContext compilationContext) {
            compilationContext.RegisterSyntaxNodeAction (AnalyzeTypeDeclarationNode, ImmutableArray.Create (
                SyntaxKind.StructDeclaration,
                SyntaxKind.ClassDeclaration,
                SyntaxKind.RecordDeclaration,
                SyntaxKind.RecordStructDeclaration
            ));

            compilationContext.RegisterSyntaxNodeAction (AnalyzeIdentifierNameNode, ImmutableArray.Create (
                SyntaxKind.IdentifierName
            ));

            compilationContext.RegisterSymbolAction (symbolContext => {
                var paramSymbol = (IParameterSymbol) symbolContext.Symbol;

                if (paramSymbol.Type is null)
                    return;

                var allowClass = paramSymbol.RefKind == RefKind.Out || paramSymbol.RefKind == RefKind.Ref;
                var typeInfo = CheckType (paramSymbol.Type);
                CheckClassAllowed (symbolContext, paramSymbol.Locations, ref typeInfo, allowClass);
            }, SymbolKind.Parameter);

            compilationContext.RegisterSymbolAction (symbolContext => {
                var fieldSymbol = (IFieldSymbol) symbolContext.Symbol;

                if (fieldSymbol.Type is null)
                    return;

                var typeInfo = CheckType (fieldSymbol.Type);
                var allowClass = fieldSymbol.RefKind == RefKind.Ref;
                CheckClassAllowed (symbolContext, fieldSymbol.Locations, ref typeInfo, allowClass);
            }, SymbolKind.Field);

            compilationContext.RegisterSymbolAction (symbolContext => {
                var methodSymbol = (IMethodSymbol) symbolContext.Symbol;

                if (!methodSymbol.ReturnsVoid && methodSymbol.ReturnType is not null) {
                    var returnTypeInfo = CheckType (methodSymbol.ReturnType);
                    var allowClassRet = methodSymbol.ReturnsByRef || methodSymbol.ReturnsByRefReadonly;
                    CheckClassAllowed (symbolContext, methodSymbol.Locations, ref returnTypeInfo, allowClassRet);
                }
            }, SymbolKind.Method);

            compilationContext.RegisterOperationAction (opContext => {
                var varDecl = (IVariableDeclaratorOperation) opContext.Operation;

                if (varDecl.Symbol is null)
                    return;

                if (varDecl.Symbol.Type is null)
                    return;

                var varSymbol = varDecl.Symbol;
                var allowClass = varSymbol.IsRef;
                var typeInfo = CheckType (varSymbol.Type);
                CheckClassAllowed (opContext, varSymbol.Locations, ref typeInfo, allowClass);
            }, OperationKind.VariableDeclarator);

            compilationContext.RegisterOperationAction (opContext => {
                var invocation = (IInvocationOperation) opContext.Operation;
                var targetMethod = invocation.TargetMethod;
                var methodLocation = invocation.Syntax?.GetLocation ();

                var genericArgs = targetMethod.TypeArguments;
                foreach (var genericParam in targetMethod.TypeParameters) {
                    cancellationToken.ThrowIfCancellationRequested ();

                    var genericArg = genericArgs [genericParam.Ordinal];
                    if (genericArg.Equals (genericParam, SymbolEqualityComparer.Default))
                        continue;

                    var genericArgInfo = CheckType (genericArg);
                    var hasClassAllowArg = genericParam.GetAttributes ().Any (attr => CheckAttribute (attr, allowExportedClassAttribute));
                    CheckClassAllowed (opContext, methodLocation, ref genericArgInfo, hasClassAllowArg);
                }

                foreach (var arg in invocation.Arguments) {
                    cancellationToken.ThrowIfCancellationRequested ();

                    if (arg.Type is null)
                        continue;

                    var argTypeInfo = CheckType (arg.Type);
                    var allowClass = arg.IsImplicit || arg.ArgumentKind == ArgumentKind.DefaultValue;
                    if (arg.Parameter is not null) {
                        var param = arg.Parameter;
                        allowClass |= param.RefKind == RefKind.Out || param.RefKind == RefKind.Ref;
                    }
                    CheckClassAllowed (opContext, arg.Syntax?.GetLocation (), ref argTypeInfo, allowClass);
                }
            }, OperationKind.Invocation);

            compilationContext.RegisterOperationAction (opContext => {
                var invocation = (IDynamicInvocationOperation) opContext.Operation;

                foreach (var arg in invocation.Arguments) {
                    cancellationToken.ThrowIfCancellationRequested ();

                    if (arg.Type is null || arg.IsImplicit)
                        continue;

                    var argTypeInfo = CheckType (arg.Type);
                    CheckClassAllowed (opContext, arg.Syntax?.GetLocation (), ref argTypeInfo, false);
                }
            }, OperationKind.DynamicInvocation);

            compilationContext.RegisterOperationAction (opContext => {
                var arrayNew = (IArrayCreationOperation) opContext.Operation;

                if (arrayNew.Type is null)
                    return;

                var elemTypeInfo = CheckType (arrayNew.Type);
                CheckClassAllowed (opContext, arrayNew.Syntax?.GetLocation (), ref elemTypeInfo, false);
            }, OperationKind.ArrayCreation);

            compilationContext.RegisterOperationAction (opContext => {
                var arrayInit = (IArrayInitializerOperation) opContext.Operation;

                foreach (var elemVal in arrayInit.ElementValues) {
                    cancellationToken.ThrowIfCancellationRequested ();

                    if (elemVal.Type is null)
                        continue;

                    var elemValTypeInfo = CheckType (elemVal.Type);
                    CheckClassAllowed (opContext, elemVal.Syntax?.GetLocation (), ref elemValTypeInfo, false);
                }
            }, OperationKind.ArrayInitializer);
        }

        private bool CheckClassAllowed<T, U> (T context, U? locations, ref TypeInfo typeInfo, bool allowThisLevel) {
            cancellationToken.ThrowIfCancellationRequested ();

            if (typeInfo.IsExportedClass && !allowThisLevel) {
                DiagAuto (
                    context,

                    DiagnosticDescriptors.ClassUsedAsAValueType,
                    locations,
                    typeInfo.Symbol.Name
                );
                return false;
            }

            if (typeInfo.Symbol is not INamedTypeSymbol symbol) {
                if (typeInfo.Symbol is IArrayTypeSymbol arraySymbol) {
                    var elemTypeInfo = CheckType (arraySymbol.ElementType);
                    return CheckClassAllowed (context, locations, ref elemTypeInfo, false);
                }

                return true;
            }

            if (symbol.IsGenericType) {
                var typeArgs = symbol.TypeArguments;
                foreach (var typeParam in symbol.TypeParameters) {
                    cancellationToken.ThrowIfCancellationRequested ();

                    var typeArg = typeArgs [typeParam.Ordinal];
                    if (typeArg.Equals (typeParam, SymbolEqualityComparer.Default))
                        continue;

                    var typeArgInfo = CheckType (typeArg);
                    var hasClassAllowArg = typeParam.GetAttributes ().Any (attr => CheckAttribute (attr, allowExportedClassAttribute));
                    if (!CheckClassAllowed (context, locations, ref typeArgInfo, hasClassAllowArg))
                        return false;
                }
            }

            return true;
        }

        private bool StringIsExportDef (string text)
            => text != null && text.Equals (AggregateExporter_Parser.DefinitionStructName, StringComparison.InvariantCulture);

        private TypeInfo CheckType (ITypeSymbol typeSymbol) {
            if (checkedTypes.TryGetValue (typeSymbol, out var typeInfo))
                return typeInfo;

            typeInfo = new TypeInfo {
                Symbol = typeSymbol,
                IsStruct = typeSymbol.TypeKind == TypeKind.Struct,

                IsExportedStruct = typeSymbol.GetAttributes ().Any (attr => CheckAttribute (attr, structExportAttribute)),
                IsExportedClass = typeSymbol.GetAttributes ().Any (attr => CheckAttribute (attr, classExportAttribute)),

                IsObjectRef = typeSymbol.AllInterfaces.Any (i => CompareTypeSymbols (i, objectRefInterface)),
                IsArrayRef = typeSymbol.AllInterfaces.Any (i => CompareTypeSymbols (i, arrayRefInterface)),

                IsExportDefinition = false,
            };
            if (typeSymbol.ContainingType != null && StringIsExportDef (typeSymbol.Name)) {
                var parentTypeInfo = CheckType (typeSymbol.ContainingType);
                typeInfo.IsExportDefinition = parentTypeInfo.IsValidExport;
            }

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

            var typeInfo = CheckType (typeSymbol);
            if (typeInfo.IsExportDefinition)
                return;
            else if (!typeInfo.IsStruct && typeInfo.IsExported) {
                Diag (
                    nodeContext,

                    DiagnosticDescriptors.NonStructExported,
                    typeSymbol.Locations,
                    typeSymbol.Name
                );
            } else if (typeInfo.IsGCRef) // Let the definitions for GC refs do whatever they want.
                return;

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

        public void AnalyzeIdentifierNameNode (SyntaxNodeAnalysisContext nodeContext) {
            if (nodeContext.Node is not IdentifierNameSyntax identName)
                return;

            if (identName.Parent is null)
                return;

            if (!StringIsExportDef (identName.Identifier.Text))
                return;

            SyntaxNode symbolName = identName;
            if (identName.Parent is QualifiedNameSyntax qualNameSyntax)
                symbolName = qualNameSyntax;

            if (nodeContext.SemanticModel.GetSymbolInfo (symbolName).Symbol is not ITypeSymbol typeSymbol)
                return;

            var typeInfo = CheckType (typeSymbol);
            if (typeInfo.IsExportDefinition) {
                Diag (
                    nodeContext,

                    DiagnosticDescriptors.DefinitionStructReferenced,
                    identName.GetLocation ()
                );
            }
        }
    }
}
