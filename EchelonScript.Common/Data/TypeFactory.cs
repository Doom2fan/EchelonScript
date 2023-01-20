/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Text;
using ChronosLib.Pooled;
using EchelonScript.Common.Utilities;
using static TerraFX.Interop.Mimalloc;

namespace EchelonScript.Common.Data.Types;

[NonCopyable]
internal unsafe struct ES_TypeFactory {
    private static nuint TypeAlignment => (nuint) sizeof (void*);

    private IntPtr mimallocHeap;
    private nint* pointerRefList;

    public void Initialize (IntPtr heap) {
        mimallocHeap = heap;
        pointerRefList = null;
    }

    public T* AllocateType<T> () where T : unmanaged => mi_heap_malloc_aligned_tp<T> (mimallocHeap, TypeAlignment);

    public T* AllocateType<T> (T value) where T : unmanaged {
        var ret = mi_heap_malloc_aligned_tp<T> (mimallocHeap, TypeAlignment);
        *ret = value;
        return ret;
    }

    public ArrayPointer<T> AllocateN<T> (int count) where T : unmanaged
        => new (mi_heap_mallocn_tp<T> (mimallocHeap, (nuint) count), count);

    public ArrayPointer<T> AllocateArray<T> (ReadOnlySpan<T> data) where T : unmanaged {
        if (data.Length < 1)
            return ArrayPointer<T>.Null;

        var ret = AllocateN<T> (data.Length);
        data.CopyTo (ret.Span);
        return ret;
    }

    public ES_Utf8String AllocateString (ReadOnlySpan<char> chars) {
        var enc = Encoding.UTF8;
        var byteCount = enc.GetByteCount (chars);

        if (byteCount <= ES_Utf8String.MaxLocalTextSize) {
            Span<byte> buf = stackalloc byte [byteCount];
            enc.GetBytes (chars, buf);
            return new (buf);
        }

        var ret = AllocateN<byte> (byteCount);
        enc.GetBytes (chars, ret.Span);

        return new (ret);
    }

    public ES_Utf8String AllocateString (ReadOnlySpan<char> chars, ReadOnlySpan<char> prefix, ReadOnlySpan<char> suffix) {
        var enc = Encoding.UTF8;
        var byteCount = enc.GetByteCount (chars) + enc.GetByteCount (prefix) + enc.GetByteCount (suffix);

        if (byteCount <= ES_Utf8String.MaxLocalTextSize) {
            Span<byte> buf = stackalloc byte [byteCount];

            var curIdx = enc.GetBytes (prefix, buf);
            curIdx += enc.GetBytes (chars, buf [curIdx..]);
            enc.GetBytes (suffix, buf [curIdx..]);

            return new (buf);
        } else {
            var ret = AllocateN<byte> (byteCount);

            var curIdx = enc.GetBytes (prefix, ret.Span);
            curIdx += enc.GetBytes (chars, ret.Span [curIdx..]);
            enc.GetBytes (suffix, ret.Span [curIdx..]);

            return new (ret);
        }
    }

    private void AddFQNToChars (ref StructPooledList<char> chars, ES_FullyQualifiedName fqn, ES_Constness constness) {
        switch (constness) {
            case ES_Constness.Mutable: break;
            case ES_Constness.Const: chars.AddRange ("const("); break;
            case ES_Constness.Immutable: chars.AddRange ("immutable("); break;

            default: throw new NotImplementedException ("Constness kind not implemented.");
        }

        fqn.GetNameAsTypeChars (ref chars);

        if (constness != ES_Constness.Mutable)
            chars.Add (')');
    }

    private nint* GetPointerRefList () {
        if (pointerRefList == null)
            pointerRefList = AllocateType<nint> (0);

        return pointerRefList;
    }

    public ES_MethodTable* GetVoid () {
        var name = AllocateString (ES_PrimitiveTypeConsts.Void);

        var ret = AllocateType<ES_MethodTable> ();
        *ret = new (
            1, ES_TypeFlag.ValueType | ES_TypeFlag.NoRefs,
            null, 0,
            null, 0,
            null, AllocateType<ES_TypeInfo> (new (
                ES_TypeTag.Void,
                ret,
                ES_Utf8String.Empty, new (ES_Utf8String.Empty, name)
            )),
            null, 0
        );

        return ret;
    }

    public ES_MethodTable* GetBool () {
        var name = AllocateString (ES_PrimitiveTypeConsts.Bool);

        var ret = AllocateType<ES_MethodTable> ();
        *ret = new (
            1, ES_TypeFlag.ValueType | ES_TypeFlag.NoRefs,
            null, 0,
            null, 0,
            null, AllocateType<ES_TypeInfo> (new (
                ES_TypeTag.Void,
                ret,
                ES_Utf8String.Empty, new (ES_Utf8String.Empty, name)
            )),
            null, 0
        );

        return ret;
    }

    public ES_MethodTable* GetInt (ES_IntSize size, bool unsigned) => GetInt (new ES_IntInfo (size, unsigned));
    public ES_MethodTable* GetInt (ES_IntInfo intInfo) {
        var name = AllocateString (ES_PrimitiveTypeConsts.GetIntName (intInfo.Size, intInfo.Unsigned));

        var ret = AllocateType<ES_MethodTable> ();
        *ret = new (
            ES_PrimitiveTypeConsts.GetIntMemorySize (intInfo.Size), ES_TypeFlag.ValueType | ES_TypeFlag.NoRefs,
            null, 0,
            null, 0,
            null, AllocateType<ES_TypeInfo> (new (
                ret,
                ES_Utf8String.Empty, new (ES_Utf8String.Empty, name),
                intInfo
            )),
            null, 0
        );

        return ret;
    }

    public ES_MethodTable* GetFloat (ES_FloatSize size) => GetFloat (new ES_FloatInfo (size));
    public ES_MethodTable* GetFloat (ES_FloatInfo floatInfo) {
        var name = AllocateString (ES_PrimitiveTypeConsts.GetFloatName (floatInfo.Size));

        var ret = AllocateType<ES_MethodTable> ();
        *ret = new (
            ES_PrimitiveTypeConsts.GetFloatMemorySize (floatInfo.Size), ES_TypeFlag.ValueType | ES_TypeFlag.NoRefs,
            null, 0,
            null, 0,
            null, AllocateType<ES_TypeInfo> (new (
                ret,
                ES_Utf8String.Empty, new (ES_Utf8String.Empty, name),
                floatInfo
            )),
            null, 0
        );

        return ret;
    }

    public ES_MethodTable* GetReference (ES_Constness pointedConst, ES_MethodTable* pointedType) {
        ES_Utf8String name;
        {
            var chars = new StructPooledList<char> (CL_ClearMode.Auto);
            try {
                chars.Add ('&');
                AddFQNToChars (ref chars, pointedType->TypeInfo->Name, pointedConst);

                name = AllocateString (chars.Span);
            } finally {
                chars.Dispose ();
            }
        }

        var ret = AllocateType<ES_MethodTable> ();
        *ret = new (
            sizeof (void*), ES_TypeFlag.ValueType,
            null, 0,
            GetPointerRefList (), 1,
            null, AllocateType<ES_TypeInfo> (new (
                ret,
                ES_Utf8String.Empty, new (ES_Utf8String.Empty, name),
                AllocateType<ES_ReferenceInfo> (new (pointedConst, pointedType))
            )),
            null, 0
        );

        return ret;
    }

    public ES_MethodTable* GetArray (int rank, ES_Constness elemConst, ES_MethodTable* elemType) {
        ES_Utf8String name;
        {
            var chars = new StructPooledList<char> (CL_ClearMode.Auto);
            try {
                AddFQNToChars (ref chars, elemType->TypeInfo->Name, elemConst);
                chars.Add ('[');
                chars.Add (',', rank - 1);
                chars.Add (']');

                name = AllocateString (chars.Span);
            } finally {
                chars.Dispose ();
            }
        }

        var ret = AllocateType<ES_MethodTable> ();
        *ret = new (
            sizeof (void*), ES_TypeFlag.ValueType,
            null, 0,
            GetPointerRefList (), 1,
            null, AllocateType<ES_TypeInfo> (new (
                ret,
                ES_Utf8String.Empty, new (ES_Utf8String.Empty, name),
                AllocateType<ES_ArrayInfo> (new (rank, elemConst, elemType))
            )),
            null, 0
        );

        return ret;
    }

    public ES_MethodTable* GetStruct (
        bool nativeType, int size,
        ES_FullyQualifiedName fqn,
        ES_StructInfo* structInfo,
        ReadOnlySpan<Pointer<ES_FunctionData>> vtable,
        ReadOnlySpan<nint> refsList,
        ReadOnlySpan<ES_TypeInterfaceData> interfaces
    ) {
        var interfacesPtr = AllocateArray (interfaces);
        var vtablePtr = AllocateArray (vtable);
        var refsPtr = AllocateArray (refsList);

        var typeFlags = nativeType ? ES_TypeFlag.NativeType : ES_TypeFlag.None;

        var ret = AllocateType<ES_MethodTable> ();
        *ret = new (
            size, typeFlags,
            (ES_FunctionData**) vtablePtr.Elements, (ushort) vtablePtr.Length,
            refsPtr.Elements, (ushort) refsPtr.Length,
            null, AllocateType<ES_TypeInfo> (new (
                ret,
                ES_Utf8String.Empty, fqn,
                structInfo
            )),
            interfacesPtr.Elements, (ushort) interfacesPtr.Length
        );

        return ret;
    }

    public ES_MethodTable* GetClass (
        bool nativeType, int size,
        ES_FullyQualifiedName fqn,
        ES_ClassInfo* classInfo, ES_MethodTable* parentType,
        ReadOnlySpan<Pointer<ES_FunctionData>> vtable,
        ReadOnlySpan<nint> refsList,
        ReadOnlySpan<ES_TypeInterfaceData> interfaces
    ) {
        var interfacesPtr = AllocateArray (interfaces);
        var vtablePtr = AllocateArray (vtable);
        var refsPtr = AllocateArray (refsList);

        var typeFlags = nativeType ? ES_TypeFlag.NativeType : ES_TypeFlag.None;

        var ret = AllocateType<ES_MethodTable> ();
        *ret = new (
            size, typeFlags,
            (ES_FunctionData**) vtablePtr.Elements, (ushort) vtablePtr.Length,
            refsPtr.Elements, (ushort) refsPtr.Length,
            parentType, AllocateType<ES_TypeInfo> (new (
                ret,
                ES_Utf8String.Empty, fqn,
                classInfo
            )),
            interfacesPtr.Elements, (ushort) interfacesPtr.Length
        );

        return ret;
    }

    public void SetRefsList (ES_MethodTable* methodTable, ReadOnlySpan<nint> refsList) {
        var refsPtr = AllocateArray (refsList);

        var typeFlags = methodTable->Flags & ~ES_TypeFlag.NoRefs;
        *methodTable = new (*methodTable, refsList: ((Pointer<nint>) refsPtr.Elements, (ushort) refsPtr.Length));
    }

    public void SetVTable (ES_MethodTable* methodTable, ReadOnlySpan<Pointer<ES_FunctionData>> vtable) {
        var vtablePtr = AllocateArray (vtable);

        var typeFlags = methodTable->Flags & ~ES_TypeFlag.NoRefs;
        *methodTable = new (*methodTable, vtable: ((Pointer<Pointer<ES_FunctionData>>)vtablePtr.Elements, (ushort) vtablePtr.Length));
    }

    public void SetInterfacesList (ES_MethodTable* methodTable, ReadOnlySpan<ES_TypeInterfaceData> interfacesList) {
        var interfacesPtr = AllocateArray (interfacesList);

        var typeFlags = methodTable->Flags & ~ES_TypeFlag.NoRefs;
        *methodTable = new (*methodTable, interfaces: ((Pointer<ES_TypeInterfaceData>) interfacesPtr.Elements, (ushort) interfacesPtr.Length));
    }
}
