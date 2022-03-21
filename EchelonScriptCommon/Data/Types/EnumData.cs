/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;

namespace EchelonScriptCommon.Data.Types;

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "EnumData", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_EnumData {
    public unsafe sealed class Builder {
        #region ================== Instance properties

        /// <summary>The pointer to the enum this builder is for.</summary>
        public ES_EnumData* EnumData { get; }

        public ES_TypeInfo* BaseType {
            get => EnumData->baseType;
            set => EnumData->baseType = value;
        }

        #endregion

        #region ================== Constructors

        internal Builder ([DisallowNull] ES_EnumData* data, ES_AccessModifier accessMod,
            ES_FullyQualifiedName fullyQualifiedName, ES_Identifier sourceUnit
        ) {
            EnumData = data;
            data->TypeInfo = new ES_TypeInfo (ES_TypeTag.Enum, accessMod, ES_TypeFlag.NoRefs, sourceUnit, fullyQualifiedName);
        }

        #endregion
    }

    #region ================== Instance fields

    public ES_TypeInfo TypeInfo;
    private ES_TypeInfo* baseType;

    #endregion

    #region ================== Instance properties

    public ES_TypeInfo* BaseType => baseType;

    #endregion
}
