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
using System.Diagnostics;
using System.Linq;
using System.Threading;
using EchelonScript.Analyzers.RoslynExtensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace EchelonScript.Analyzers.CSharpExporting.Internal;

internal sealed class AggregateExporter_Parser : Utils.ParserBase {
    public const string AssemblyNameEchelonCommon = "EchelonScript.Common";

    public const string StructAttributeName = "ES_ExportStructAttribute";
    public const string ClassAttributeName = "ES_ExportClassAttribute";
    public const string FieldAttributeName = "ES_ExportFieldAttribute";

    public const string StructAttributeFullName = $"{ES_ExportGenerator.ExportAttributeNamespace}.{StructAttributeName}";
    public const string ClassAttributeFullName = $"{ES_ExportGenerator.ExportAttributeNamespace}.{ClassAttributeName}";
    public const string FieldAttributeFullName = $"{ES_ExportGenerator.ExportAttributeNamespace}.{FieldAttributeName}";

    public const string ObjectRefInterfaceFullName = $"{ES_ExportGenerator.ExportInterfaceNamespace}.IES_ReferenceType";
    public const string ArrayRefInterfaceFullName = $"{ES_ExportGenerator.ExportInterfaceNamespace}.IES_ArrayType";

    public const string AccessModifierFullName = $"{ES_ExportGenerator.TypeCommonNamespace}.ES_AccessModifier";
    public const string ConstnessFullName = $"{ES_ExportGenerator.TypeCommonNamespace}.ES_Constness";

    public const string DefinitionStructName = "ExportDefinition";

    private INamedTypeSymbol? structExportAttribute;
    private INamedTypeSymbol? classExportAttribute;
    private INamedTypeSymbol? fieldExportAttribute;

    private INamedTypeSymbol? objectRefInterface;
    private INamedTypeSymbol? arrayRefInterface;

    private INamedTypeSymbol? accessModEnum;
    private INamedTypeSymbol? constnessEnum;

    public AggregateExporter_Parser (Compilation compilation, Action<Diagnostic> reportDiagnostic, CancellationToken cancellationToken)
        : base (compilation, reportDiagnostic, cancellationToken) { }

    public IReadOnlyList<ExportedStruct> GetTypesToGenerate (IEnumerable<StructDeclarationSyntax> structs) {
        var hadError = false;
        var structsToGenerate = new List<ExportedStruct> ();

        // Get the semantic representation of our marker attributes and other types.
        structExportAttribute = compilation.GetTypeByMetadataNameAndAssembly (StructAttributeFullName, AssemblyNameEchelonCommon);
        if (structExportAttribute == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, StructAttributeFullName);
            return Array.Empty<ExportedStruct> ();
        }
        classExportAttribute = compilation.GetTypeByMetadataNameAndAssembly (ClassAttributeFullName, AssemblyNameEchelonCommon);
        if (classExportAttribute == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, ClassAttributeFullName);
            return Array.Empty<ExportedStruct> ();
        }
        fieldExportAttribute = compilation.GetTypeByMetadataNameAndAssembly (FieldAttributeFullName, AssemblyNameEchelonCommon);
        if (fieldExportAttribute == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, FieldAttributeFullName);
            return Array.Empty<ExportedStruct> ();
        }

        objectRefInterface = compilation.GetTypeByMetadataNameAndAssembly (ObjectRefInterfaceFullName, AssemblyNameEchelonCommon);
        if (objectRefInterface == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, ObjectRefInterfaceFullName);
            return Array.Empty<ExportedStruct> ();
        }
        arrayRefInterface = compilation.GetTypeByMetadataNameAndAssembly (ArrayRefInterfaceFullName, AssemblyNameEchelonCommon);
        if (arrayRefInterface == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, ArrayRefInterfaceFullName);
            return Array.Empty<ExportedStruct> ();
        }

        accessModEnum = compilation.GetTypeByMetadataNameAndAssembly (AccessModifierFullName, AssemblyNameEchelonCommon);
        if (accessModEnum == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, AccessModifierFullName);
            return Array.Empty<ExportedStruct> ();
        }
        constnessEnum = compilation.GetTypeByMetadataNameAndAssembly (ConstnessFullName, AssemblyNameEchelonCommon);
        if (constnessEnum == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, ConstnessFullName);
            return Array.Empty<ExportedStruct> ();
        }

        var fields = new List<ExportedField> ();
        foreach (var structDecl in structs) {
            // Stop if we're asked to.
            cancellationToken.ThrowIfCancellationRequested ();

            fields.Clear ();

            var structError = hadError;
            // Get the semantic representation of the struct syntax.
            var structSM = compilation.GetSemanticModel (structDecl.SyntaxTree);
            if (structSM.GetDeclaredSymbol (structDecl) is not INamedTypeSymbol structSymbol)
                continue;

            if (!structDecl.Modifiers.Any (SyntaxKind.PartialKeyword)) {
                Diag (
                    DiagnosticDescriptors.StructNotPartial,
                    structSymbol.Locations,
                    structSymbol.Name
                );
                structError = true;
            }

            var exportAttribute = (AttributeData?) null;
            var isClass = false;
            foreach (var attr in structSymbol.GetAttributes ()) {
                var isClassAttr = CheckAttribute (attr, classExportAttribute);

                if (!CheckAttribute (attr, structExportAttribute) && !isClassAttr)
                    continue;

                exportAttribute = attr;
                isClass = isClassAttr;
                break;
            }
            if (exportAttribute is null) {
                Debug.Fail ($"{nameof (AggregateExporter_Parser)} encountered a struct with no attributes.");
                throw new NotSupportedException ();
            }

            // Get the full type name of the struct.
            var structName = structSymbol.Name;

            foreach (var member in structSymbol.GetMembers ()) {
                var isStatic = member.IsStatic;

                switch (member) {
                    case IFieldSymbol fieldDecl:
                        if (isStatic)
                            continue;

                        Diag (
                            DiagnosticDescriptors.InstanceMemberInAggregate,
                            fieldDecl.Locations,
                            fieldDecl.Name
                        );
                        break;
                    case IPropertySymbol propDecl:
                        if (isStatic)
                            continue;

                        Diag (
                            DiagnosticDescriptors.InstanceMemberInAggregate,
                            propDecl.Locations,
                            propDecl.Name
                        );
                        break;
                    case IMethodSymbol methodDecl:
                        if (isStatic)
                            continue;

                        switch (methodDecl.MethodKind) {
                            case MethodKind.AnonymousFunction:
                            case MethodKind.Constructor:
                            case MethodKind.Destructor: // We ignore these because C# doesn't allow them anyway.
                            case MethodKind.PropertyGet:
                            case MethodKind.PropertySet:
                            case MethodKind.ReducedExtension:
                            case MethodKind.StaticConstructor:
                                continue;

                            case MethodKind.Ordinary:
                            case MethodKind.DeclareMethod:
                            case MethodKind.LocalFunction:
                                break;

                            // No idea what these mean. Probably implicitly-created methods?
                            // Or they're used for method calls.
                            case MethodKind.DelegateInvoke:
                            case MethodKind.EventAdd:
                            case MethodKind.EventRaise:
                            case MethodKind.EventRemove:
                            case MethodKind.BuiltinOperator:
                            case MethodKind.FunctionPointerSignature:
                                break;

                            case MethodKind.ExplicitInterfaceImplementation:
                            case MethodKind.Conversion:
                            case MethodKind.UserDefinedOperator:
                                // TODO: Make these error out if necessary.
                                break;
                        }

                        Diag (
                            DiagnosticDescriptors.InstanceMemberInAggregate,
                            methodDecl.Locations,
                            methodDecl.Name
                        );
                        break;

                    case TypeDeclarationSyntax:
                        break;
                }
            }

            var structDefStructs = structSymbol.GetTypeMembers (DefinitionStructName);
            if (structDefStructs.Length > 1)
                continue;
            if (structDefStructs.Length < 1) {
                Diag (
                    DiagnosticDescriptors.DefinitionStructMissing,
                    structSymbol.Locations,
                    structSymbol.Name
                );
                continue;
            }
            var defStructSymbol = structDefStructs.First ();

            if (defStructSymbol.TypeKind != TypeKind.Struct) {
                Diag (
                    DiagnosticDescriptors.DefinitionStructMustBePartial,
                    structSymbol.Locations,
                    structSymbol.Name
                );
            }

            if (defStructSymbol.TypeParameters.Length > 0) {
                Diag (
                    DiagnosticDescriptors.DefinitionStructCannotBeGeneric,
                    structSymbol.Locations,
                    structSymbol.Name
                );
                structError = true;
            }

            if (!defStructSymbol.IsUnmanagedType) {
                Diag (
                    DiagnosticDescriptors.DefinitionStructMustBeUnmanaged,
                    structSymbol.Locations,
                    structSymbol.Name
                );
                structError = true;
            }

            Debug.Assert (!defStructSymbol.IsImplicitlyDeclared);
            if (defStructSymbol.DeclaringSyntaxReferences.Length > 1) {
                Diag (
                    DiagnosticDescriptors.DefinitionStructMustBeDeclaredOnce,
                    defStructSymbol.Locations,
                    defStructSymbol.Name
                );
            }

            foreach (var member in defStructSymbol.GetMembers ()) {
                var isStatic = member.IsStatic;
                var fieldError = structError;

                switch (member) {
                    case IFieldSymbol fieldSymbol: {
                        if (isStatic) {
                            Diag (
                                DiagnosticDescriptors.StaticMemberInDefStruct,
                                fieldSymbol.Locations,
                                fieldSymbol.Name
                            );
                            continue;
                        }

                        if (fieldSymbol.IsReadOnly) {
                            Diag (
                                DiagnosticDescriptors.ExportedMembersCannotBeReadonly,
                                fieldSymbol.Locations,
                                fieldSymbol.Name
                            );
                            fieldError = true;
                        }

                        var fieldTypeSymbol = fieldSymbol.Type;
                        if (fieldTypeSymbol.IsRefLikeType) {
                            Diag (
                                DiagnosticDescriptors.RefMembersNotAllowed,
                                fieldSymbol.Locations,
                                fieldSymbol.Name
                            );
                            fieldError = true;
                        }

                        if (fieldTypeSymbol.IsReferenceType) {
                            Diag (
                                DiagnosticDescriptors.ReferenceTypesNotAllowed,
                                fieldSymbol.Locations,
                                fieldSymbol.Name
                            );
                            fieldError = true;
                        }

                        if (!CheckTypeAllowed (fieldTypeSymbol, out var fieldSpecialType)) {
                            Diag (
                                DiagnosticDescriptors.DisallowedTypeInField,
                                fieldSymbol.Locations,
                                fieldSymbol.Name
                            );
                            fieldError = true;
                        }

                        var fieldNativeAccessMod = GetAccessMod (fieldSymbol.DeclaredAccessibility);
                        if (fieldNativeAccessMod is null) {
                            fieldError = true;
                            fieldNativeAccessMod = string.Empty;
                        }

                        // Skip adding this if there were any errors.
                        if (fieldError) {
                            structError = true;
                            continue;
                        }

                        var fieldNativeName = fieldSymbol.Name;
                        var fieldAccessMod = $"{AccessModifierFullName}.Private";
                        var fieldConstness = $"{ConstnessFullName}.Const";
                        var fieldExportName = fieldNativeName;
                        var fieldType = fieldTypeSymbol.ToString ();

                        var fieldAttr = fieldSymbol.GetAttributes ().FirstOrDefault (attr => CheckAttribute (attr, fieldExportAttribute));
                        if (fieldAttr is not null && fieldAttr.NamedArguments.Any ()) {
                            // Attribute syntax: [ES_ExportField (Name = "Foo", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
                            foreach (var namedArg in fieldAttr.NamedArguments) {
                                var argValue = namedArg.Value;
                                if (argValue.Kind == TypedConstantKind.Error) {
                                    structError = true;
                                    continue;
                                }

                                switch (namedArg.Key) {
                                    case "Name": fieldExportName = (string) GetItem (argValue); break;
                                    case "AccessModifier": fieldAccessMod = argValue.ToCSharpString (); break;
                                    case "Constness": fieldConstness = argValue.ToCSharpString (); break;
                                }
                            }
                        }

                        fields.Add (new () {
                            SpecialType = fieldSpecialType,

                            ExportName = fieldExportName,
                            AccessModifier = fieldAccessMod,
                            Constness = fieldConstness,

                            FieldType = fieldType,
                            PropertyName = fieldNativeName,
                            PropertyAccessibility = fieldNativeAccessMod,
                        });

                        break;
                    }

                    case IPropertySymbol propSymbol:
                        throw new NotImplementedException ();
                    case IMethodSymbol methodSymbol:
                        if (methodSymbol.IsImplicitlyDeclared)
                            continue;

                        throw new NotImplementedException ();
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
                            structSymbol.Locations,
                            structSymbol.Name
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

            var exportNamespace = (string?) null;
            var exportName = (string?) null;

            // Attribute syntax:
            //   [ES_ExportStruct (Namespace = "Foo.Bar", Name = "Baz")]
            //   [ES_ExportClass (Namespace = "Foo.Bar", Name = "Baz", ParentClass = typeof (Foo))]
            foreach (var namedArg in exportAttribute.NamedArguments) {
                var argValue = namedArg.Value;
                if (argValue.Kind == TypedConstantKind.Error) {
                    structError = true;
                    continue;
                }

                switch (namedArg.Key) {
                    case "Namespace": exportNamespace = (string) GetItem (argValue); break;
                    case "Name": exportName = (string) GetItem (argValue); break;
                    case "ParentClass":
                        if (!isClass)
                            continue;
                        Debug.Fail (""); // TODO: Handle parent classes.
                        break;
                }
            }

            if (exportName is null)
                exportName = structDecl.Identifier.ToString ();

            var fieldsArr = fields.ToArray ();
            fields.Clear ();
            structsToGenerate.Add (new ExportedStruct {
                NativeName = structName,
                NativeNamespace = GetNamespaceString (structDecl),
                NativeParents = parentClasses,

                ExportNamespace = exportNamespace,
                ExportName = exportName,

                Fields = fieldsArr,
            });
        }

        if (hadError)
            return Array.Empty<ExportedStruct> ();

        return structsToGenerate;
    }

    public bool CheckTypeAllowed (ITypeSymbol type, out ExportedFieldSpecialType specialType) {
        specialType = ExportedFieldSpecialType.None;
        if (!type.IsUnmanagedType)
            return false;
        if (!type.IsValueType)
            return false;
        if (type.IsReferenceType)
            return false;

        switch (type.SpecialType) {
            case SpecialType.System_Boolean:
            case SpecialType.System_SByte:
            case SpecialType.System_Int16:
            case SpecialType.System_Int32:
            case SpecialType.System_Int64:
            case SpecialType.System_Byte:
            case SpecialType.System_UInt16:
            case SpecialType.System_UInt32:
            case SpecialType.System_UInt64:
            case SpecialType.System_Single:
            case SpecialType.System_Double:
                return true;
        }

        var typeAttribs = type.GetAttributes ();
        var hasExpAttribs = typeAttribs.Any (attr => CheckAttribute (attr, structExportAttribute) || CheckAttribute (attr, classExportAttribute));
        var isGCRef = type.AllInterfaces.Any (i => CompareTypeSymbols (i, objectRefInterface));
        var isGCArray = type.AllInterfaces.Any (i => CompareTypeSymbols (i, arrayRefInterface));
        if (!hasExpAttribs && !isGCRef && !isGCArray)
            return false;

        if (isGCRef)
            specialType = ExportedFieldSpecialType.Reference;
        if (isGCArray)
            specialType = ExportedFieldSpecialType.Array;

        return true;
    }
}
