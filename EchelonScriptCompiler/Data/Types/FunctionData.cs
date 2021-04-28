/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Data.Types {
    [Flags]
    public enum ES_FunctionFlags : int {
        /// <summary>The function is a member function of a type, and has an implicit self pointer.</summary>
        Method = 1,
        /// <summary>Function is virtual and may be overridden in derived classes. (May only be used in classes)</summary>
        Virtual = 1 << 1,
        /// <summary>Function has no function body and must be overridden in derived classes. (May only be used in classes)</summary>
        Abstract = 1 << 2,
    }

    public enum ES_ArgumentType {
        /// <summary>Passed normally.</summary>
        Normal,
        /// <summary>Passed by reference.</summary>
        Ref,
        /// <summary>Passed as const.</summary>
        In,
        /// <summary>Passed by reference, and requires setting before returning.</summary>
        Out,
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionData", ES_ExportAttributeBase.AggregateType.Struct)]
    public readonly unsafe struct ES_FunctionData {
        #region ================== Instance fields

        public readonly ArrayPointer<byte> Name;
        public readonly ArrayPointer<byte> FullyQualifiedName;

        public readonly ES_AccessModifier AccessModifier;
        public readonly ArrayPointer<byte> SourceUnit;

        public readonly ES_FunctionPrototypeData* FunctionType;
        public readonly ArrayPointer<ES_FunctionArgData> Arguments;
        public readonly int OptionalArgsCount;

        public readonly void* FunctionPointer;

        #endregion

        #region ================== Instance properties

        public readonly string NameString {
            get => StringPool.Shared.GetOrAdd (Name.Span, Encoding.ASCII);
        }
        public readonly string FullyQualifiedNameString {
            get => StringPool.Shared.GetOrAdd (FullyQualifiedName.Span, Encoding.ASCII);
        }

        public readonly string SourceUnitString {
            get => StringPool.Shared.GetOrAdd (SourceUnit.Span, Encoding.ASCII);
        }

        #endregion

        #region ================== Constructors

        public ES_FunctionData (
            ArrayPointer<byte> name, ArrayPointer<byte> fqn,
            ES_AccessModifier accessMod, ArrayPointer<byte> sourceUnit,
            ES_FunctionPrototypeData* functionType, ArrayPointer<ES_FunctionArgData> args, int optArgCount
        ) {
            Debug.Assert (functionType is not null);
            Debug.Assert (args.Length == functionType->ArgumentsList.Length);

            Name = name;
            FullyQualifiedName = fqn;

            AccessModifier = accessMod;
            SourceUnit = sourceUnit;

            FunctionType = functionType;
            Arguments = args;
            OptionalArgsCount = optArgCount;

            FunctionPointer = null;
        }

        public ES_FunctionData (ES_FunctionData funcData, void* funcPtr) {
            this = funcData;

            FunctionPointer = funcPtr;
        }

        #endregion
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionArgData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_FunctionArgData {
        #region ================== Instance fields

        public readonly ArrayPointer<byte> Name;
        public void* DefaultValue;

        #endregion

        #region ================== Constructors

        public ES_FunctionArgData (ArrayPointer<byte> name, void* defaultVal) {
            Name = name;
            DefaultValue = defaultVal;
        }

        #endregion
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionTypeArgData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_FunctionPrototypeArgData {
        #region ================== Instance fields

        public readonly ES_ArgumentType ArgType;
        public readonly ES_TypeInfo* ValueType;

        #endregion

        #region ================== Constructors

        public ES_FunctionPrototypeArgData (ES_ArgumentType argType, ES_TypeInfo* valueType) {
            ArgType = argType;
            ValueType = valueType;
        }

        #endregion
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionTypeData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_FunctionPrototypeData {
        public unsafe sealed class Builder {
            #region ================== Instance fields

            private ES_FunctionPrototypeData* functionProtoData;

            #endregion

            #region ================== Instance properties

            /// <summary>The pointer to the function prototype this builder is for.</summary>
            public ES_FunctionPrototypeData* FunctionProtoData => functionProtoData;

            public ES_TypeInfo* ReturnType {
                get => functionProtoData->returnType;
                set => functionProtoData->returnType = value;
            }

            public ArrayPointer<ES_FunctionPrototypeArgData> ArgumentsList {
                get => functionProtoData->argumentsList;
                set => functionProtoData->argumentsList = value;
            }

            #endregion

            #region ================== Constructors

            internal Builder ([DisallowNull] ES_FunctionPrototypeData* data, ES_AccessModifier accessMod,
                ArrayPointer<byte> typeName, ArrayPointer<byte> fullyQualifiedName,
                ArrayPointer<byte> sourceUnit
            ) {
                functionProtoData = data;
                data->TypeInfo = new ES_TypeInfo (ES_TypeTag.Function, accessMod, sourceUnit, typeName, fullyQualifiedName);
                data->TypeInfo.RuntimeSize = IntPtr.Size;
            }

            #endregion
        }

        #region ================== Instance fields

        public ES_TypeInfo TypeInfo;
        private ES_TypeInfo* returnType;
        private ArrayPointer<ES_FunctionPrototypeArgData> argumentsList;

        #endregion

        #region ================== Constructors

        public ES_FunctionPrototypeData (ES_AccessModifier accessMod,
            ES_TypeInfo* retType, ArrayPointer<ES_FunctionPrototypeArgData> argsList,
            ArrayPointer<byte> typeName, ArrayPointer<byte> fullyQualifiedName,
            ArrayPointer<byte> sourceUnit
        ) {
            TypeInfo = new ES_TypeInfo (ES_TypeTag.Function, accessMod, sourceUnit, typeName, fullyQualifiedName);
            TypeInfo.RuntimeSize = IntPtr.Size;

            returnType = retType;
            argumentsList = argsList;
        }

        #endregion

        #region ================== Instance properties

        public ES_TypeInfo* ReturnType => returnType;
        public ArrayPointer<ES_FunctionPrototypeArgData> ArgumentsList => argumentsList;

        #endregion
    }
}
