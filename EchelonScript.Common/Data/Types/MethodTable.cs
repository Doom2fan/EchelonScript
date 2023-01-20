/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScript.Common.Utilities;

namespace EchelonScript.Common.Data.Types;

public unsafe readonly struct ES_MethodTable {
    public readonly int RuntimeSize;
    public readonly ES_TypeFlag Flags;

    public readonly ES_FunctionData** VTable;
    public readonly ushort VTableLength;

    public readonly nint* RefsList;
    public readonly ushort RefsCount;

    public readonly ES_MethodTable* ParentType;
    public readonly ES_TypeInfo* TypeInfo;

    public readonly ES_TypeInterfaceData* Interfaces;
    public readonly ushort InterfacesCount;

    #region Init properties

    internal int RuntimeSizeInit { init => RuntimeSize = value; }
    internal ES_TypeFlag FlagsInit { init => Flags = value; }

    internal ArrayPointer<Pointer<ES_FunctionData>> VTableInit {
        init {
            VTable = (ES_FunctionData**) value.Elements;
            VTableLength = (ushort) value.Length;
        }
    }

    internal ArrayPointer<nint> RefsListInit {
        init {
            RefsList = value.Elements;
            RefsCount = (ushort) value.Length;
        }
    }

    internal ES_MethodTable* ParentTypeInit { init => ParentType = value; }
    internal ES_TypeInfo* TypeInfoInit { init => TypeInfo = value; }

    internal ArrayPointer<ES_TypeInterfaceData> InterfacesInit {
        init {
            Interfaces = value.Elements;
            InterfacesCount = (ushort) value.Length;
        }
    }

    #endregion

    internal ES_MethodTable (
        int runtimeSize, ES_TypeFlag flags,
        ES_FunctionData** vtable, ushort vtableLen,
        nint* refsList, ushort refsCount,
        ES_MethodTable* parentType, ES_TypeInfo* typeInfo,
        ES_TypeInterfaceData* interfaces, ushort interfacesCount
    ) {
        flags = flags & ~ES_TypeFlag.NoRefs;
        if (refsCount < 1)
            flags |= ES_TypeFlag.NoRefs;

        RuntimeSize = runtimeSize;
        Flags = flags;

        VTable = vtable;
        VTableLength = vtableLen;

        RefsList = refsList;
        RefsCount = refsCount;

        ParentType = parentType;
        TypeInfo = typeInfo;

        Interfaces = interfaces;
        InterfacesCount = interfacesCount;
    }

    internal ES_MethodTable (
        ES_MethodTable methodTable,
        int? runtimeSize = null,
        ES_TypeFlag? flags = null,
        (Pointer<Pointer<ES_FunctionData>>, ushort)? vtable = null,
        (Pointer<nint>, ushort)? refsList = null,
        Pointer<ES_MethodTable>? parentType = null,
        (Pointer<ES_TypeInterfaceData>, ushort)? interfaces = null
    ) {
        RuntimeSize = runtimeSize ?? methodTable.RuntimeSize;
        TypeInfo = methodTable.TypeInfo;

        if (vtable.HasValue) {
            VTable = (ES_FunctionData**) (vtable.Value.Item1.Address);
            VTableLength = vtable.Value.Item2;
        } else {
            VTable = methodTable.VTable;
            VTableLength = methodTable.VTableLength;
        }
        (RefsList, RefsCount) = refsList ?? ((Pointer<nint>) methodTable.RefsList, methodTable.RefsCount);
        (Interfaces, InterfacesCount) = interfaces ?? ((Pointer<ES_TypeInterfaceData>) methodTable.Interfaces, methodTable.InterfacesCount);
        ParentType = parentType ?? methodTable.ParentType;

        flags = (flags ?? methodTable.Flags) & ~ES_TypeFlag.NoRefs;
        if (RefsCount < 1)
            flags |= ES_TypeFlag.NoRefs;
        Flags = flags.Value;
    }

    public Span<nint> GetRefsList () => new (RefsList, RefsCount);
    public Span<Pointer<ES_FunctionData>> GetVTable () => new (VTable, VTableLength);
}

public unsafe readonly struct ES_TypeInterfaceData {
    public readonly ES_MethodTable* InterfaceType;
    public readonly ArrayPointer<Pointer<ES_FunctionData>> Functions;

    public ES_TypeInterfaceData (ES_MethodTable* interfaceType, ArrayPointer<Pointer<ES_FunctionData>> funcs) {
        InterfaceType = interfaceType;
        Functions = funcs;
    }
}
