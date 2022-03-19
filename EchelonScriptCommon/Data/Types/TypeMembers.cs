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
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCommon.Data.Types;

public enum ES_MemberType : int {
    Field = 0,
    Function = 1,
}

public enum ES_MemberFlags : int {
    Static = 1 << 0,
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ES_MemberData {
    public readonly ES_AccessModifier AccessModifier;
    public readonly ES_MemberType MemberType;
    public readonly ES_MemberFlags Flags;
    public readonly ES_Identifier Name;
    public readonly ES_Identifier SourceUnit;

    public ES_MemberData (
        ES_AccessModifier accessMod,
        ES_MemberType type, ES_MemberFlags flags,
        ES_Identifier name, ES_Identifier srcUnit
    ) {
        AccessModifier = accessMod;
        MemberType = type;
        Flags = flags;
        Name = name;
        SourceUnit = srcUnit;
    }
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ES_MemberData_Variable {
    public readonly ES_MemberData Info;
    public int Offset;
    public readonly ES_TypeInfo* Type;

    public ES_MemberData_Variable (
        ES_Identifier name, ES_Identifier srcUnit, ES_AccessModifier accessMod,
        ES_MemberFlags flags, int offset, ES_TypeInfo* type
    ) {
        Info = new ES_MemberData (accessMod, ES_MemberType.Field, flags, name, srcUnit);
        Offset = offset;
        Type = type;
    }
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
public unsafe struct ES_MemberData_Function {
    public readonly ES_MemberData Info;
    public ES_FunctionData FunctionData;

    public ES_MemberData_Function (
        ES_MemberFlags flags, ES_FullyQualifiedName fqn,
        ES_AccessModifier accessMod, ES_Identifier srcUnit,
        ES_FunctionPrototypeData* funcType, ArrayPointer<ES_FunctionArgData> args, int optArgCount
    ) {
        Info = new ES_MemberData (accessMod, ES_MemberType.Field, flags, fqn.TypeName, srcUnit);
        FunctionData = new ES_FunctionData (
            fqn, accessMod, srcUnit,
            funcType, args, optArgCount
        );
    }
}

[StructLayout (LayoutKind.Sequential, Pack = 1)]
[ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "TypeMembers", ES_ExportAttributeBase.AggregateType.Struct)]
public unsafe struct ES_TypeMembers {
    public unsafe sealed class Builder {
        #region ================== Instance properties

        /// <summary>The pointer to the struct this builder is for.</summary>
        public ES_TypeInfo* OwnerType { get; }
        /// <summary>The pointer to the symbols list this builder is for.</summary>
        public ES_TypeMembers* MembersData { get; }

        /// <summary>The members list of this type.</summary>
        public ArrayPointer<Pointer<ES_MemberData>> MembersList {
            get => MembersData->membersList;
            set => MembersData->membersList = value;
        }

        #endregion

        #region ================== Constructors

        internal Builder ([DisallowNull] ES_TypeMembers* data, [DisallowNull] ES_TypeInfo* owner) {
            OwnerType = owner;
            MembersData = data;
        }

        #endregion
    }

    #region ================== Instance fields

    private ArrayPointer<Pointer<ES_MemberData>> membersList;

    #endregion

    #region ================== Instance properties

    /// <summary>The members list of this type.</summary>
    public ArrayPointer<Pointer<ES_MemberData>> MembersList => membersList;

    #endregion
}
