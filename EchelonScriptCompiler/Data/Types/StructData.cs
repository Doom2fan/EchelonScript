/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Data.Types {
    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "StructData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_StructData {
        public unsafe sealed class Builder {
            #region ================== Instance fields

            private ES_StructData* structData;
            private ES_TypeMembers.Builder membersListBuilder;

            #endregion

            #region ================== Instance properties

            /// <summary>The pointer to the struct this builder is for.</summary>
            public ES_StructData* StructData => structData;

            /// <summary>The interfaces list of this struct.</summary>
            public ArrayPointer<Pointer<ES_InterfaceData>> InterfacesList {
                get => structData->interfacesList;
                set => structData->interfacesList = value;
            }

            /// <summary>The members list of this struct.</summary>
            public ES_TypeMembers* MembersList => &structData->TypeInfo.MembersList;

            /// <summary>The builder for the members list of this struct.</summary>
            public ES_TypeMembers.Builder MembersListBuilder => membersListBuilder;

            #endregion

            #region ================== Constructors

            internal Builder ([DisallowNull] ES_StructData* data, ES_AccessModifier accessMod,
                ES_FullyQualifiedName fullyQualifiedName, ArrayPointer<byte> sourceUnit
            ) {
                structData = data;
                data->TypeInfo = new ES_TypeInfo (ES_TypeTag.Struct, accessMod, sourceUnit, fullyQualifiedName);
                membersListBuilder = new ES_TypeMembers.Builder (&structData->TypeInfo.MembersList, &structData->TypeInfo);
            }

            #endregion
        }

        #region ================== Instance fields

        public ES_TypeInfo TypeInfo;
        private ArrayPointer<Pointer<ES_InterfaceData>> interfacesList;

        #endregion

        #region ================== Instance properties

        /// <summary>The interfaces list of this struct.</summary>
        public ArrayPointer<Pointer<ES_InterfaceData>> InterfacesList => interfacesList;

        #endregion
    }
}
