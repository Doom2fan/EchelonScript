/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;

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

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionArgData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_FunctionArgData {
        #region ================== Instance fields

        private ES_TypeInfo* argType;
        private ArrayPointer<byte> argName;

        #endregion

        #region ================== Instance properties

        public ES_TypeInfo* ArgType => argType;
        public ArrayPointer<byte> ArgName => argName;

        #endregion

        #region ================== Constructors

        public ES_FunctionArgData (ArrayPointer<byte> name, ES_TypeInfo* type) {
            argName = name;
            argType = type;
        }

        #endregion
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FunctionData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_FunctionData {
        public unsafe sealed class Builder {
            #region ================== Instance fields

            private ES_FunctionData* functionData;

            #endregion

            #region ================== Instance properties

            /// <summary>The pointer to the function this builder is for.</summary>
            public ES_FunctionData* FunctionData => functionData;

            public ES_FunctionFlags Flags {
                get => functionData->flags;
                set => functionData->flags = value;
            }

            public ES_TypeInfo* ParentType {
                get => functionData->parentType;
                set => functionData->parentType = value;
            }

            public ES_TypeInfo* ReturnType {
                get => functionData->returnType;
                set => functionData->returnType = value;
            }

            public ArrayPointer<ES_FunctionArgData> ArgumentsList {
                get => functionData->argumentsList;
                set => functionData->argumentsList = value;
            }

            #endregion

            #region ================== Constructors

            internal Builder ([DisallowNull] ES_FunctionData* data, ES_AccessModifier accessMod,
                ArrayPointer<byte> typeName, ArrayPointer<byte> fullyQualifiedName,
                ArrayPointer<byte> sourceUnit
            ) {
                functionData = data;
                data->TypeInfo = new ES_TypeInfo (ES_TypeTag.Function, accessMod, sourceUnit, typeName, fullyQualifiedName);
            }

            #endregion
        }

        #region ================== Instance fields

        public ES_TypeInfo TypeInfo;
        private ES_FunctionFlags flags;
        private ES_TypeInfo* parentType;
        private ES_TypeInfo* returnType;
        private ArrayPointer<ES_FunctionArgData> argumentsList;

        #endregion

        #region ================== Instance properties

        public ES_FunctionFlags Flags => flags;
        public ES_TypeInfo* ParentType => parentType;
        public ES_TypeInfo* ReturnType => returnType;
        public ArrayPointer<ES_FunctionArgData> ArgumentsList => argumentsList;

        #endregion
    }
}
