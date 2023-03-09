﻿/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace EchelonScript.Analyzers.CSharpExporting.Internal;

internal sealed class AggregateExporter_Emitter {
    private const int DefaultStringBuildCapacity = 1024;
    private const string IndentString = "    ";
    private static readonly string GeneratedCodeAttribute =
        $"global::System.CodeDom.Compiler.GeneratedCodeAttribute (" +
        $"\"{typeof (AggregateExporter_Emitter).Assembly.GetName ().Name}\", " +
        $"\"{typeof (AggregateExporter_Emitter).Assembly.GetName ().Version}\")";

    public const string TypeInterfaceName = "IES_ExportedType";
    public const string TypeInterfaceFullName = $"{ES_ExportGenerator.ExportInterfaceNamespace}.{TypeInterfaceName}";

    public const string TypeTableNamespace = $"{ES_ExportGenerator.TypeCommonNamespace}.Data";
    public const string TypeTableName = "ES_TypeTable";
    public const string TypeTableFullName = $"{TypeTableNamespace}.{TypeTableName}";

    public const string TypeTypeLoaderFullName = $"{TypeTableFullName}.TypeLoader";
    public const string TypeTypeLoadTokenFullName = $"{TypeTableFullName}.TypeLoadToken";
    public const string TypeBasicTypeInfoFullName = $"{TypeTableFullName}.BasicTypeInfo";

    public const string DataTypeNamespace = $"{ES_ExportGenerator.TypeCommonNamespace}.Data.Types";
    public const string TypeFieldInfoFullName = $"{DataTypeNamespace}.ES_FieldInfo";
    public const string TypeFunctionInfoFullName = $"{DataTypeNamespace}.ES_FunctionInfo";
    public const string TypeMethodInfoFullName = $"{DataTypeNamespace}.ES_MethodInfo";
    public const string TypeInterfaceDataFullName = $"{DataTypeNamespace}.ES_TypeInterfaceData";

    public const string TypeObjectAddress = $"{ES_ExportGenerator.TypeCommonNamespace}.ES_ObjectAddress";
    public const string TypeArrayAddress = $"{ES_ExportGenerator.TypeCommonNamespace}.ES_ArrayAddress";
    public const string TypeUtf8StringFullName = $"{ES_ExportGenerator.TypeCommonNamespace}.Utilities.ES_Utf8String";

    public const string MarshalFullName = "System.Runtime.InteropServices.Marshal";
    public const string UnsafeFullName = "System.Runtime.CompilerServices.Unsafe";

    private readonly StringBuilder builder = new (DefaultStringBuildCapacity);

    public string Emit (IReadOnlyList<ExportedStruct> exportedStructs, CancellationToken cancellationToken) {
        builder.Clear ();
        builder.AppendLine ($@"
// <auto-generated/>
#nullable enable
#pragma warning disable CS9084");

        foreach (var expStruct in exportedStructs) {
            cancellationToken.ThrowIfCancellationRequested ();
            EmitStruct (expStruct);
        }

        builder.AppendLine ($@"
#pragma warning restore CS9084");

        return builder.ToString ();
    }

    private void EmitStruct (ExportedStruct expStruct) {
        var indentation = string.Empty;

        // Begin the namespace, if any.
        if (!string.IsNullOrWhiteSpace (expStruct.NativeNamespace)) {
            builder.Append ($@"
{indentation}namespace {expStruct.NativeNamespace}
{indentation}{{");
            IndentationAdd (ref indentation);
        }

        // Begin the parents.
        foreach (var parent in expStruct.NativeParents) {
            builder.Append ($@"
{indentation}partial {parent}
{indentation}{{");
            IndentationAdd (ref indentation);
        }

        // Begin the struct.
        builder.Append ($@"
{indentation}unsafe partial struct {expStruct.NativeName} : {TypeInterfaceFullName}
{indentation}{{");
        IndentationAdd (ref indentation);

        // Output the fields.
        var fieldAutoNames = new string [expStruct.Fields.Length];
        var fieldCount = 0;
        foreach (var expField in expStruct.Fields) {
            var privFieldName = $@"__AUTOGEN_{expField.PropertyName}_{expField.ExportName}";
            string privFieldType;
            string propGetter;

            fieldAutoNames [fieldCount] = privFieldName;

            switch (expField.SpecialType) {
                case ExportedFieldSpecialType.None:
                    privFieldType = expField.FieldType;
                    propGetter = $"=> ref this.{privFieldName};";
                    break;
                case ExportedFieldSpecialType.Reference:
                    privFieldType = TypeObjectAddress;
                    propGetter = $"=> ref {UnsafeFullName}.As<{TypeObjectAddress}, {expField.FieldType}> (ref this.{privFieldName});";
                    break;
                case ExportedFieldSpecialType.Array:
                    privFieldType = TypeArrayAddress;
                    propGetter = $"=> ref {UnsafeFullName}.As<{TypeArrayAddress}, {expField.FieldType}> (ref this.{privFieldName});";
                    break;
                default: throw new NotImplementedException ("Unknown special type.");
            }

            builder.Append ($@"
{indentation}private {privFieldType} {privFieldName};
{indentation}{expField.PropertyAccessibility} ref {expField.FieldType} {expField.PropertyName} {propGetter}
");
            fieldCount++;
        }
        builder.Append ($@"
{indentation}{TypeBasicTypeInfoFullName} {TypeInterfaceFullName}.GetBasicData ({TypeTypeLoaderFullName} typeLoader) => new
{indentation}(
{indentation}{IndentString}typeLoader.AllocateFQN (""{expStruct.ExportNamespace ?? string.Empty}"", ""{expStruct.ExportName}""),
{indentation}{IndentString}{TypeUtf8StringFullName}.Empty,
{indentation}{IndentString}sizeof ({expStruct.NativeName})
{indentation});");

        // Begin the type init function.
        builder.Append ($@"
{indentation}void {TypeInterfaceFullName}.InitializeType ({TypeTypeLoaderFullName} typeLoader, ref {TypeTypeLoadTokenFullName} typeToken)
{indentation}{{");
        IndentationAdd (ref indentation);

        // Emit the fields list.
        builder.Append ($@"
{indentation}var fieldsArray = ");
        if (expStruct.Fields.Length > 0) {
            builder.Append ($@"new {TypeFieldInfoFullName} []
{indentation}{{");
            IndentationAdd (ref indentation);

            fieldCount = 0;
            foreach (var field in expStruct.Fields) {
                var fieldInternalName = fieldAutoNames [fieldCount];

                builder.Append ($@"
{indentation}new {TypeFieldInfoFullName} ()
{indentation}{{");

                IndentationAdd (ref indentation);
                builder.Append ($@"
{indentation}NoExport = {field.NoExport},
{indentation}AccessModifier = {field.AccessModifier},
{indentation}Name = typeLoader.AllocateString (""{field.ExportName}""),
{indentation}Constness = {field.Constness},
{indentation}Type = typeLoader.GetType<{field.FieldType}> (true),
{indentation}Offset = {MarshalFullName}.OffsetOf<{expStruct.NativeName}> (nameof (this.{fieldInternalName})),");
                IndentationRemove (ref indentation);

                builder.Append ($@"
{indentation}}},");

                fieldCount++;
            }

            IndentationRemove (ref indentation);
            builder.Append ($@"
{indentation}}};");
        } else
            builder.Append ($@"System.ReadOnlySpan<{TypeFieldInfoFullName}>.Empty;");

        // Emit the methods list.
        builder.Append ($@"
{indentation}var methodsArray = ");
        if (false) {
            builder.Append ($@"new {TypeMethodInfoFullName} [] {{");
            IndentationAdd (ref indentation);

            IndentationRemove (ref indentation);
            builder.Append ($@"
{indentation}}};");
        } else
            builder.Append ($@"System.ReadOnlySpan<{TypeMethodInfoFullName}>.Empty;");

        // Emit the functions list.
        builder.Append ($@"
{indentation}var functionsArray = ");
        if (false) {
            builder.Append ($@"new {TypeFunctionInfoFullName} [] {{");
            IndentationAdd (ref indentation);

            IndentationRemove (ref indentation);
            builder.Append ($@"
{indentation}}};");
        } else
            builder.Append ($@"System.ReadOnlySpan<{TypeFunctionInfoFullName}>.Empty;");

        // Emit the type creation call.
        builder.Append ($@"

{indentation}typeLoader.CreateStruct (
{indentation}{IndentString}ref typeToken,

{indentation}{IndentString}System.ReadOnlySpan<{TypeInterfaceDataFullName}>.Empty,

{indentation}{IndentString}fieldsArray,
{indentation}{IndentString}methodsArray,
{indentation}{IndentString}functionsArray,

{indentation}{IndentString}false
{indentation});");

        // End the type init function.
        IndentationRemove (ref indentation);
        builder.Append ($@"
{indentation}}}");

        // End the struct.
        IndentationRemove (ref indentation);
        builder.Append ($@"
{indentation}}}");

        // End the parent types.
        for (int i = 0; i < expStruct.NativeParents.Length; i++) {
            IndentationRemove (ref indentation);
            builder.Append ($@"
{indentation}}}");
        }

        // End the namespace, if any.
        if (!string.IsNullOrWhiteSpace (expStruct.NativeNamespace)) {
            IndentationRemove (ref indentation);
            builder.AppendLine ($@"
{indentation}}}");
        }
}

    private void IndentationAdd (ref string indent) => indent += IndentString;

    private void IndentationRemove (ref string indent) => indent = indent.Substring (0, indent.Length - IndentString.Length);
}