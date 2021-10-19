/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics.CodeAnalysis;
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.Backends.RoslynBackend {
    public unsafe sealed partial class RoslynCompilerBackend {
        internal const string DefaultStaticConsName = "_defaultStaticCons";
        internal const string GlobalStaticConsName = "_globalStaticCons";
        internal const string GlobalStorageTypeName = "_globalFunctionsStorageType";

        private static string MangleDefaultConstructorName (ES_TypeInfo* typeName, bool isStatic) {
            // Sample name: "struct.System.Numerics__Vector2"
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // The prefix.
            if (!isStatic)
                mangleChars.AddRange ("defaultConstructor!");
            else
                mangleChars.AddRange ("defaultStaticConstructor!");

            // The namespace.
            var namespaceName = typeName->Name.NamespaceName.Span;
            var namespaceSpan = mangleChars.AddSpan (namespaceName.Length);
            Encoding.ASCII.GetChars (namespaceName, namespaceSpan);

            // The mangled namespace separator.
            mangleChars.Add ('_', 2);

            // The function name.
            var structName = typeName->Name.TypeName.Span;
            var structSpan = mangleChars.AddSpan (structName.Length);
            Encoding.ASCII.GetChars (structName, structSpan);

            return mangleChars.Span.GetPooledString ();
        }

        internal static string MangleGlobalFunctionName ([DisallowNull] ES_FunctionData* func) {
            // Sample name: "System.Math__FMath.Sin"
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // The namespace.
            mangleChars.AddRange (func->Name.NamespaceNameString);

            // The mangled namespace separator.
            mangleChars.AddRange ("__");

            // The function name.
            mangleChars.AddRange (func->Name.TypeNameString);

            /*// Add the type's mangled name.
            mangleChars.AddRange ("_");
            using var typeMangle = MangleFunctionType (func->FunctionType);
            mangleChars.AddRange (typeMangle);*/

            return mangleChars.Span.GetPooledString ();
        }

        internal static string MangleMemberFunctionName ([DisallowNull] ES_TypeInfo* owner, [DisallowNull] ES_FunctionData* func) {
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // The type name.
            mangleChars.AddRange (owner->Name.TypeNameString);

            // The mangled namespace separator.
            mangleChars.AddRange ("__");

            // The function name.
            mangleChars.AddRange (func->Name.TypeNameString);

            return mangleChars.Span.GetPooledString ();
        }

        #region Type names

        private static string MangleStructName ([DisallowNull] ES_StructData* structData) {
            // Sample name: "struct.System.Numerics__Vector2"
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // The prefix.
            mangleChars.AddRange ("struct!");

            // The namespace.
            var namespaceName = structData->TypeInfo.Name.NamespaceName.Span;
            var namespaceSpan = mangleChars.AddSpan (namespaceName.Length);
            Encoding.ASCII.GetChars (namespaceName, namespaceSpan);

            // The mangled namespace separator.
            mangleChars.Add (':', 2);

            // The function name.
            var structName = structData->TypeInfo.Name.TypeName.Span;
            var structSpan = mangleChars.AddSpan (structName.Length);
            Encoding.ASCII.GetChars (structName, structSpan);

            return mangleChars.Span.GetPooledString ();
        }

        private static string MangleFunctionType ([DisallowNull] ES_FunctionPrototypeData* type) {
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // Add the return type.
            mangleChars.AddRange ("Ret");
            mangleChars.AddRange (type->ReturnType->Name.NamespaceNameString);
            mangleChars.AddRange ("__");
            mangleChars.AddRange (type->ReturnType->Name.TypeNameString);

            // Add the arg types.
            foreach (var arg in type->ArgumentsList.Span) {
                mangleChars.AddRange ("_");

                switch (arg.ArgType) {
                    case ES_ArgumentType.Normal: break;
                    case ES_ArgumentType.In: break;
                    case ES_ArgumentType.Out: mangleChars.AddRange ("out"); break;
                    case ES_ArgumentType.Ref: mangleChars.AddRange ("ref"); break;
                }

                mangleChars.AddRange (arg.ValueType->Name.NamespaceNameString);
                mangleChars.AddRange ("__");
                mangleChars.AddRange (arg.ValueType->Name.TypeNameString);
            }

            return mangleChars.Span.GetPooledString ();
        }

        internal static string MangleTypeName ([DisallowNull] ES_TypeInfo* type) {
            switch (type->TypeTag) {
                case ES_TypeTag.Struct:
                    return MangleStructName ((ES_StructData*) type);

                case ES_TypeTag.Function:
                    return MangleFunctionType ((ES_FunctionPrototypeData*) type);

                case ES_TypeTag.Void:
                case ES_TypeTag.Bool:
                case ES_TypeTag.Int:
                case ES_TypeTag.Float:
                    throw new NotSupportedException ("Type cannot be name-mangled.");

                case ES_TypeTag.Class:
                case ES_TypeTag.Enum:
                case ES_TypeTag.Interface:
                case ES_TypeTag.Reference:
                case ES_TypeTag.Const:
                case ES_TypeTag.Immutable:
                case ES_TypeTag.Array:
                    throw new NotImplementedException ("[TODO] Not implemented yet.");

                default:
                    throw new NotImplementedException ("Type tag not implemented.");
            }
        }

        #endregion
    }
}
