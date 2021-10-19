/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using System.Text;
using ChronosLib.Pooled;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;

namespace EchelonScriptCompiler.Backends.LLVMBackend {
    public unsafe sealed partial class LLVMCompilerBackend {
        private static PooledArray<char> MangleStaticVarName (ES_TypeInfo* parentType, ArrayPointer<byte> varName) {
            // Sample name: "struct.System.Numerics__Vector2"
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // The prefix.
            mangleChars.AddRange ("staticVar.");

            // The namespace.
            var namespaceName = parentType->Name.NamespaceName.Span;
            var namespaceSpan = mangleChars.AddSpan (namespaceName.Length);
            Encoding.ASCII.GetChars (namespaceName, namespaceSpan);

            // The mangled namespace separator.
            mangleChars.Add ('_', 2);

            // The function name.
            var structName = parentType->Name.TypeName.Span;
            var structSpan = mangleChars.AddSpan (structName.Length);
            Encoding.ASCII.GetChars (structName, structSpan);

            mangleChars.Add ('.');

            var varSpan = mangleChars.AddSpan (varName.Length);
            Encoding.ASCII.GetChars (varName.Span, varSpan);

            return mangleChars.ToPooledArray ();
        }

        private static PooledArray<char> MangleDefaultConstructorName (ES_TypeInfo* typeName, bool isStatic) {
            // Sample name: "struct.System.Numerics__Vector2"
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // The prefix.
            if (!isStatic)
                mangleChars.AddRange ("defaultConstructor.");
            else
                mangleChars.AddRange ("defaultStaticConstructor.");

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

            return mangleChars.ToPooledArray ();
        }

        private static PooledArray<char> MangleStructName ([DisallowNull] ES_StructData* structData) {
            // Sample name: "struct.System.Numerics__Vector2"
            using var mangleChars = new StructPooledList<char> (CL_ClearMode.Auto);

            // The prefix.
            mangleChars.AddRange ("struct.");

            // The namespace.
            var namespaceName = structData->TypeInfo.Name.NamespaceName.Span;
            var namespaceSpan = mangleChars.AddSpan (namespaceName.Length);
            Encoding.ASCII.GetChars (namespaceName, namespaceSpan);

            // The mangled namespace separator.
            mangleChars.Add ('_', 2);

            // The function name.
            var structName = structData->TypeInfo.Name.TypeName.Span;
            var structSpan = mangleChars.AddSpan (structName.Length);
            Encoding.ASCII.GetChars (structName, structSpan);

            return mangleChars.ToPooledArray ();
        }

        private static PooledArray<char> MangleFunctionType (ES_FunctionPrototypeData* type) {
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

            return mangleChars.ToPooledArray ();
        }

        internal static PooledArray<char> MangleFunctionName ([DisallowNull] ES_FunctionData* func) {
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

            return mangleChars.ToPooledArray ();
        }
    }
}
