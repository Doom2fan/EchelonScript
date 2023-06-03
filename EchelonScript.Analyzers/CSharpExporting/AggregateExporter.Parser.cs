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
    public const string ProtectedFieldAttributeName = "ES_ProtectedFieldAttribute";

    public const string StructAttributeFullName = $"{ES_ExportGenerator.ExportAttributeNamespace}.{StructAttributeName}";
    public const string ClassAttributeFullName = $"{ES_ExportGenerator.ExportAttributeNamespace}.{ClassAttributeName}";
    public const string FieldAttributeFullName = $"{ES_ExportGenerator.ExportAttributeNamespace}.{FieldAttributeName}";
    public const string ProtectedFieldAttributeFullName = $"{ES_ExportGenerator.ExportAttributeNamespace}.{ProtectedFieldAttributeName}";

    public const string ObjectRefInterfaceFullName = $"{ES_ExportGenerator.ExportInterfaceNamespace}.IES_ReferenceType";
    public const string ArrayRefInterfaceFullName = $"{ES_ExportGenerator.ExportInterfaceNamespace}.IES_ArrayType";

    public const string AccessModifierFullName = $"{ES_ExportGenerator.TypeCommonNamespace}.ES_AccessModifier";
    public const string ConstnessFullName = $"{ES_ExportGenerator.TypeCommonNamespace}.ES_Constness";

    public const string DefinitionStructName = "ExportDefinition";

    private INamedTypeSymbol? structExportAttribute;
    private INamedTypeSymbol? classExportAttribute;
    private INamedTypeSymbol? fieldExportAttribute;
    private INamedTypeSymbol? protectedFieldAttribute;

    private INamedTypeSymbol? objectRefInterface;
    private INamedTypeSymbol? arrayRefInterface;

    private INamedTypeSymbol? accessModEnum;
    private INamedTypeSymbol? constnessEnum;

    private Dictionary<INamedTypeSymbol, StructData> structsParsed;

    public AggregateExporter_Parser (Compilation compilation, Action<Diagnostic> reportDiagnostic, CancellationToken cancellationToken)
        : base (compilation, reportDiagnostic, cancellationToken) {
        structsParsed = new (SymbolEqualityComparer.Default);
    }

    private struct StructWork {
        public bool Error;

        public INamedTypeSymbol Symbol;
        public INamedTypeSymbol DefSymbol;

        public bool IsClass;
        public bool IsSealed;
        public INamedTypeSymbol? BaseClass;

        public List<ExportedField> Fields;
        public string ExportNamespace;
        public string ExportName;
    }

    private class StructData {
        public bool Error;

        public bool Loading;
        public bool Loaded;

        public ExportedStruct ExportedStruct;

        public void SetError () {
            Error = true;
        }

        public void SetLoading () {
            if (Error)
                throw new Exception ("Struct already errored-out.");
            else if (Loaded)
                throw new Exception ("Struct already loaded.");
            else if (Loading)
                throw new Exception ("Struct already loading.");

            Loading = true;
        }

        public void SetLoaded () {
            if (Error)
                throw new Exception ("Struct already errored-out.");
            else if (Loaded)
                throw new Exception ("Struct already loaded.");
            else if (!Loading)
                throw new Exception ("Struct not set as loading.");

            Loaded = true;
            Loading = false;
        }
    }

    // TODO: Forbid using classes directly instead of through refs.
    public IReadOnlyList<ExportedStruct> GetTypesToGenerate (IEnumerable<(StructDeclarationSyntax, INamedTypeSymbol)> structs) {
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
        protectedFieldAttribute = compilation.GetTypeByMetadataNameAndAssembly (ProtectedFieldAttributeFullName, AssemblyNameEchelonCommon);
        if (protectedFieldAttribute == null) {
            Diag (DiagnosticDescriptors.MissingRequiredType, ProtectedFieldAttributeFullName);
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

        foreach (var (_, symbol) in structs)
            GetStruct (symbol);

        if (structsParsed.Count < 1)
            return Array.Empty<ExportedStruct> ();

        var structsToGenerate = new List<ExportedStruct> (structsParsed.Count);
        foreach (var structData in structsParsed.Values) {
            if (structData.Error)
                continue;

            structsToGenerate.Add (structData.ExportedStruct);
        }

        return structsToGenerate;
    }

    private StructData GetStruct (INamedTypeSymbol structSymbol) {
        if (structsParsed.TryGetValue (structSymbol, out var structData))
            return structData;

        structData = new ();
        structsParsed.Add (structSymbol, structData);
        ParseStruct (structSymbol, structData);
        return structData;
    }

    private void ParseStruct (INamedTypeSymbol structSymbol, StructData structData) {
        // Stop if we're asked to.
        cancellationToken.ThrowIfCancellationRequested ();

        structData.SetLoading ();

        var structWork = new StructWork {
            Error = false,
            Symbol = structSymbol,
            Fields = new List<ExportedField> ()
        };

        if (!GetStructData (ref structWork)) {
            structData.SetError ();
            return;
        }

        CheckRealStructMembers (ref structWork);

        if (!GetDefStructData (ref structWork)) {
            structData.SetError ();
            return;
        }

        CheckDefStructMembers (ref structWork);

        CheckParentClasses (ref structWork, out var parentClasses);

        if (structWork.Error) {
            structData.SetError ();
            return;
        }

        var fieldsArr = structWork.Fields.ToArray ();
        structData.ExportedStruct = new () {
            NativeName = structWork.Symbol.Name,
            NativeNamespace = structWork.Symbol.GetFullNamespace (),
            NativeParents = parentClasses,

            IsClass = structWork.IsClass,
            IsSealed = structWork.IsSealed,
            BaseClass = structWork.BaseClass,

            ExportNamespace = structWork.ExportNamespace,
            ExportName = structWork.ExportName,

            Fields = fieldsArr,
        };

        structData.SetLoaded ();
    }

    private bool GetStructData (ref StructWork structWork) {
        var exportAttribute = (AttributeData?) null;
        foreach (var attr in structWork.Symbol.GetAttributes ()) {
            var isClassAttr = CheckAttribute (attr, classExportAttribute);

            if (!CheckAttribute (attr, structExportAttribute) && !isClassAttr)
                continue;

            exportAttribute = attr;
            structWork.IsClass = isClassAttr;
            break;
        }

        if (exportAttribute is null) {
            structWork.Error = true;
            return false;
        }

        // Check for errors.
        var isPartial = structWork.Symbol.DeclaringSyntaxReferences.Any (syntaxRef => ((TypeDeclarationSyntax) syntaxRef.GetSyntax (cancellationToken)).Modifiers.Any (SyntaxKind.PartialKeyword));
        if (!isPartial) {
            Diag (
                DiagnosticDescriptors.StructNotPartial,
                structWork.Symbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
        }

        if (structWork.Symbol.IsRefLikeType) {
            Diag (
                DiagnosticDescriptors.StructIsRef,
                structWork.Symbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
        }

        if (structWork.Symbol.TypeParameters.Length > 0) {
            Diag (
                DiagnosticDescriptors.StructIsGeneric,
                structWork.Symbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
        }

        if (!structWork.Symbol.IsUnmanagedType) {
            Diag (
                DiagnosticDescriptors.StructNotUnmanaged,
                structWork.Symbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
        }

        // Attribute syntax:
        //   [ES_ExportStruct (Namespace = "Foo.Bar", Name = "Baz")]
        //   [ES_ExportClass (Namespace = "Foo.Bar", Name = "Baz", Sealed = true, ParentClass = typeof (Foo))]
        foreach (var namedArg in exportAttribute.NamedArguments) {
            var argValue = namedArg.Value;
            if (argValue.Kind == TypedConstantKind.Error) {
                structWork.Error = true;
                continue;
            }

            switch (namedArg.Key) {
                case "Namespace": {
                    structWork.ExportNamespace = (string) GetItem (argValue);
                    if (!IsValidNamespace (structWork.ExportNamespace.AsSpan ())) {
                        Diag (
                            DiagnosticDescriptors.InvalidNamespace,
                            structWork.Symbol.Locations,
                            structWork.Symbol.Name
                        );
                        structWork.Error = true;
                    }
                    break;
                }
                case "Name": {
                    structWork.ExportName = (string) GetItem (argValue);
                    if (!IsValidIdentifier (structWork.ExportName.AsSpan ())) {
                        Diag (
                            DiagnosticDescriptors.InvalidTypeName,
                            structWork.Symbol.Locations,
                            structWork.Symbol.Name
                        );
                        structWork.Error = true;
                    }
                    break;
                }
                case "Sealed":
                    if (!structWork.IsClass)
                        continue;

                    structWork.IsSealed = (bool) GetItem (argValue);
                    break;

                case "ParentClass":
                    if (!structWork.IsClass)
                        continue;

                    structWork.BaseClass = (INamedTypeSymbol?) GetItem (argValue);
                    break;
            }
        }

        if (structWork.ExportName is null) {
            structWork.ExportName = structWork.Symbol.Name;
            if (!IsValidIdentifier (structWork.ExportName.AsSpan ())) {
                Diag (
                    DiagnosticDescriptors.InvalidAutoTypeName,
                    structWork.Symbol.Locations,
                    structWork.Symbol.Name
                );
                structWork.Error = true;
            }
        }

        if (structWork.BaseClass is not null) {
            var baseClassData = GetStruct (structWork.BaseClass);
            GetParentClassData (ref structWork, baseClassData);
        }

        return true;
    }

    private void GetParentClassData (ref StructWork structWork, StructData baseClassData) {
        if (structWork.BaseClass is null)
            return;
        if (baseClassData.Error)
            return;

        if (baseClassData.Loading) {
            Diag (
                DiagnosticDescriptors.InheritanceCycle,
                structWork.Symbol.Locations,
                structWork.Symbol.Name,
                structWork.BaseClass.Name
            );
            structWork.Error = true;
            return;
        }

        if (!baseClassData.ExportedStruct.IsClass) {
            Diag (
                DiagnosticDescriptors.ClassInheritsNonClass,
                structWork.Symbol.Locations,
                structWork.Symbol.Name,
                structWork.BaseClass.Name
            );
            structWork.Error = true;
        }

        if (baseClassData.ExportedStruct.IsSealed) {
            Diag (
                DiagnosticDescriptors.InheritsFromSealedType,
                structWork.Symbol.Locations,
                structWork.Symbol.Name,
                structWork.BaseClass.Name
            );
            structWork.Error = true;
        }

        structWork.Fields.AddRange (baseClassData.ExportedStruct.Fields);
    }

    private void CheckRealStructMembers (ref StructWork structWork) {
        foreach (var member in structWork.Symbol.GetMembers ()) {
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
                    structWork.Error = true;
                    break;
                case IPropertySymbol propDecl:
                    if (isStatic)
                        continue;

                    Diag (
                        DiagnosticDescriptors.InstanceMemberInAggregate,
                        propDecl.Locations,
                        propDecl.Name
                    );
                    structWork.Error = true;
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
                    structWork.Error = true;
                    break;

                case TypeDeclarationSyntax:
                    break;
            }
        }
    }

    private bool GetDefStructData (ref StructWork structWork) {
        var structDefStructs = structWork.Symbol.GetTypeMembers (DefinitionStructName);
        if (structDefStructs.Length > 1)
            return false;
        if (structDefStructs.Length < 1) {
            Diag (
                DiagnosticDescriptors.DefinitionStructMissing,
                structWork.Symbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
            return false;
        }
        structWork.DefSymbol = structDefStructs.First ();

        if (structWork.DefSymbol.TypeKind != TypeKind.Struct) {
            Diag (
                DiagnosticDescriptors.DefinitionTypeNotAStruct,
                structWork.DefSymbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
        }

        if (structWork.DefSymbol.IsRefLikeType) {
            Diag (
                DiagnosticDescriptors.DefinitionStructIsRef,
                structWork.DefSymbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
        }

        if (structWork.DefSymbol.TypeParameters.Length > 0) {
            Diag (
                DiagnosticDescriptors.DefinitionStructIsGeneric,
                structWork.DefSymbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
        }

        if (!structWork.DefSymbol.IsUnmanagedType) {
            Diag (
                DiagnosticDescriptors.DefinitionStructNotUnmanaged,
                structWork.DefSymbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
        }

        if (structWork.DefSymbol.DeclaredAccessibility != Accessibility.Private &&
            structWork.DefSymbol.DeclaredAccessibility != Accessibility.NotApplicable) {
            Diag (
                DiagnosticDescriptors.DefinitionStructNotPrivate,
                structWork.DefSymbol.Locations,
                structWork.Symbol.Name
            );
            structWork.Error = true;
        }

        Debug.Assert (!structWork.DefSymbol.IsImplicitlyDeclared);
        Debug.Assert (structWork.DefSymbol.DeclaringSyntaxReferences.Length > 0);
        if (structWork.DefSymbol.DeclaringSyntaxReferences.Length > 1) {
            Diag (
                DiagnosticDescriptors.DefinitionStructDeclaredInMultiplePlaces,
                structWork.DefSymbol.Locations,
                structWork.DefSymbol.Name
            );
            structWork.Error = true;
        }
        if (structWork.DefSymbol.DeclaringSyntaxReferences [0].GetSyntax () is not TypeDeclarationSyntax defStructDecl) {
            Debug.Fail ("Definition struct's declaring syntax is not a TypeDeclarationSyntax. (???)");
            throw new NotSupportedException ();
        }
        if (!defStructDecl.Modifiers.Any (SyntaxKind.PartialKeyword)) {
            Diag (
                DiagnosticDescriptors.DefinitionStructNotPartial,
                structWork.DefSymbol.Locations,
                structWork.DefSymbol.Name
            );
            structWork.Error = true;
        }

        return true;
    }

    private void CheckDefStructMembers (ref StructWork structWork) {
        foreach (var member in structWork.DefSymbol.GetMembers ()) {
            var isStatic = member.IsStatic;
            var fieldError = structWork.Error;

            switch (member) {
                case IFieldSymbol fieldSymbol: {
                    if (isStatic) {
                        Diag (
                            DiagnosticDescriptors.StaticMemberInDefStruct,
                            fieldSymbol.Locations,
                            fieldSymbol.Name
                        );
                        fieldError = true;
                        continue;
                    }

                    if (fieldSymbol.IsReadOnly) {
                        Diag (
                            DiagnosticDescriptors.ExportedMemberIsReadonly,
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
                    } else if (!fieldTypeSymbol.IsUnmanagedType) {
                        Diag (
                            DiagnosticDescriptors.ManagedTypesNotAllowed,
                            fieldSymbol.Locations,
                            fieldSymbol.Name
                        );
                        fieldError = true;
                    }

                    if (!CheckFieldTypeAllowed (fieldSymbol, out var fieldSpecialType)) {
                        Diag (
                            DiagnosticDescriptors.DisallowedTypeInField_NotExportOrPrimitive,
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

                    var fieldSourceType = structWork.Symbol.Name;
                    var fieldNativeName = fieldSymbol.Name;
                    var fieldAccessMod = $"{AccessModifierFullName}.Private";
                    var fieldConstness = $"{ConstnessFullName}.Const";
                    var fieldExportName = (string?) null;
                    var fieldType = fieldTypeSymbol.ToString ();

                    var fieldProtected = fieldSymbol.GetAttributes ().Any (attr => CheckAttribute (attr, protectedFieldAttribute));
                    var fieldAttr = fieldSymbol.GetAttributes ().FirstOrDefault (attr => CheckAttribute (attr, fieldExportAttribute));
                    if (fieldAttr is not null && fieldAttr.NamedArguments.Any ()) {
                        // Attribute syntax: [ES_ExportField (Name = "Foo", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
                        foreach (var namedArg in fieldAttr.NamedArguments) {
                            var argValue = namedArg.Value;
                            if (argValue.Kind == TypedConstantKind.Error) {
                                fieldError = true;
                                continue;
                            }

                            switch (namedArg.Key) {
                                case "Name": {
                                    fieldExportName = (string) GetItem (argValue);
                                    if (!IsValidIdentifier (fieldExportName.AsSpan ())) {
                                        Diag (
                                            DiagnosticDescriptors.InvalidFieldName,
                                            fieldSymbol.Locations,
                                            fieldNativeName
                                        );
                                        fieldError = true;
                                    }
                                    break;
                                }
                                case "AccessModifier": fieldAccessMod = argValue.ToCSharpString (); break;
                                case "Constness": fieldConstness = argValue.ToCSharpString (); break;
                            }
                        }
                    }

                    if (fieldProtected && !structWork.IsClass) {
                        Diag (
                            DiagnosticDescriptors.ProtectedMemberInStruct,
                            fieldSymbol.Locations,
                            fieldNativeName,
                            structWork.Symbol.Name
                        );
                        fieldError = true;
                    } else if (fieldProtected) {
                        if (structWork.IsSealed) {
                            Diag (
                                DiagnosticDescriptors.ProtectedMemberInSealedClass,
                                fieldSymbol.Locations,
                                fieldNativeName,
                                structWork.Symbol.Name
                            );
                            fieldError = true;
                        }

                        fieldNativeAccessMod = "protected";
                    }

                    if (fieldExportName is null) {
                        fieldExportName = fieldNativeName;
                        if (!IsValidIdentifier (fieldExportName.AsSpan ())) {
                            Diag (
                                DiagnosticDescriptors.InvalidAutoFieldName,
                                fieldSymbol.Locations,
                                fieldNativeName
                            );
                            fieldError = true;
                        }
                    }

                    foreach (var otherField in structWork.Fields) {
                        var sameSource = otherField.SourceType.Equals (fieldSourceType, StringComparison.InvariantCulture);

                        if (!sameSource && fieldNativeName.Equals (otherField.PropertyName, StringComparison.InvariantCulture)) {
                            Diag (
                                DiagnosticDescriptors.FieldNameUsedInBase,
                                fieldSymbol.Locations,
                                structWork.Symbol.Name,
                                fieldNativeName
                            );
                            fieldError = true;
                        }

                        if (fieldExportName.Equals (otherField.ExportName, StringComparison.InvariantCulture)) {
                            Diag (
                                sameSource ? DiagnosticDescriptors.DuplicateFieldExportName : DiagnosticDescriptors.FieldExportNameUsedInBase,
                                fieldSymbol.Locations,
                                structWork.Symbol.Name,
                                fieldExportName
                            );
                            fieldError = true;
                        }
                    }

                    // Skip adding this if there were any errors.
                    if (fieldError) {
                        structWork.Error = true;
                        continue;
                    }

                    structWork.Fields.Add (new () {
                        SpecialType = fieldSpecialType,

                        NoExport = fieldAttr is null,
                        ExportName = fieldExportName,
                        AccessModifier = fieldAccessMod,
                        Constness = fieldConstness,

                        SourceType = fieldSourceType,
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
    }

    private void CheckParentClasses (ref StructWork structWork, out string [] parentClasses) {
        static bool IsAllowedKind (SyntaxKind kind) =>
            kind == SyntaxKind.ClassDeclaration ||
            kind == SyntaxKind.StructDeclaration ||
            kind == SyntaxKind.RecordDeclaration;

        parentClasses = Array.Empty<string> ();
        if (structWork.Symbol.DeclaringSyntaxReferences [0].GetSyntax ().Parent is TypeDeclarationSyntax parentClass) {
            var parentClassesList = new List<string> ();
            while (parentClass != null && IsAllowedKind (parentClass.Kind ())) {
                var parentError = false;
                if (parentClass.TypeParameterList?.Parameters.Count > 0) {
                    Diag (
                        DiagnosticDescriptors.ExportedTypeNestedInGeneric,
                        structWork.Symbol.Locations,
                        structWork.Symbol.Name
                    );
                    parentError = true;
                }

                if (!parentClass.Modifiers.Any (SyntaxKind.PartialKeyword)) {
                    Diag (
                        DiagnosticDescriptors.ExportedTypeNestedInNonPartial,
                        structWork.Symbol.Locations,
                        structWork.Symbol.Name
                    );
                    parentError = true;
                }

                if (parentError) {
                    structWork.Error = true;
                    parentClassesList.Clear ();
                    break;
                }
                parentClassesList.Add ($"{parentClass.Keyword.ValueText} {parentClass.Identifier}");
                parentClass = (parentClass.Parent as TypeDeclarationSyntax)!;
            }

            parentClasses = parentClassesList.ToArray ();
        }
    }

    private bool CheckFieldTypeAllowed (IFieldSymbol fieldSymbol, out ExportedFieldSpecialType specialType) {
        var type = fieldSymbol.Type;
        specialType = ExportedFieldSpecialType.None;

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

    private static bool IsLatinLetter (char c) => (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');

    private static bool IsIntegerDigit (char c) => c >= '0' && c <= '9';

    private static bool IsValidIdentifier (ReadOnlySpan<char> id) {
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

    private static bool IsValidNamespace (ReadOnlySpan<char> id) {
        if (id.Length < 1)
            return false;

        while (id.Length > 0) {
            var sepIndex = id.IndexOf ('.');

            if (sepIndex == 0)
                return false;

            if (sepIndex == -1)
                sepIndex = id.Length;

            if (!IsValidIdentifier (id.Slice (0, sepIndex)))
                return false;

            if (sepIndex >= id.Length)
                break;

            id = id.Slice (sepIndex + 1);
        }

        return true;
    }
}
