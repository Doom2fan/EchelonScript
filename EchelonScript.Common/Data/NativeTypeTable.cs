/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using ChronosLib.Pooled;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.Exporting;
using EchelonScript.Common.Utilities;
using ExhaustiveMatching;
using static TerraFX.Interop.Mimalloc;

namespace EchelonScript.Common.Data;

public unsafe readonly struct ES_NativeTypeStoredData {
    internal readonly bool Loaded { get; init; }
    internal readonly bool Loading { get; init; }

    public readonly Type Type { get; init; }
    public readonly ES_MethodTable* MethodTable { get; init; }
}

public unsafe class ES_NativeTypeTable : IDisposable {
    private static ES_NativeTypeTable instance;

    private struct TypeLoadData {
        public Type Type;
        public ES_MethodTable* MethodTable;
    }

    [NonCopyable]
    public struct TypeLoadToken {
        internal bool Initialized { get; init; }
        internal bool Created { get; set; }
        public ES_MethodTable* MethodTable { get; internal init; }

        internal TypeLoadToken (ES_MethodTable* methodTable) {
            Initialized = true;
            Created = false;
            MethodTable = methodTable;
        }
    }

    public class NativeTypeLoader {
        private ES_NativeTypeTable typeTable;

        internal NativeTypeLoader (ES_NativeTypeTable tTable) => typeTable = tTable;

        public ES_MethodTable* GetType<T> () where T : unmanaged, IES_ExportedType => throw new NotImplementedException ();

        public ES_MethodTable* GetVoid () => Primitive_Void.MethodTable;
        public ES_MethodTable* GetBool () => GetNativeType<bool> ().MethodTable;
        public ES_MethodTable* GetInt (ES_IntSize size, bool unsigned) => (size switch {
            ES_IntSize.Int8  => unsigned ? Primitive_UInt8  : Primitive_SInt8,
            ES_IntSize.Int16 => unsigned ? Primitive_UInt16 : Primitive_SInt16,
            ES_IntSize.Int32 => unsigned ? Primitive_UInt32 : Primitive_SInt32,
            ES_IntSize.Int64 => unsigned ? Primitive_UInt64 : Primitive_SInt64,

            _ => throw ExhaustiveMatch.Failed (size),
        }).MethodTable;
        public ES_MethodTable* GetFloat (ES_FloatSize size) => (size switch {
            ES_FloatSize.Single => Primitive_Float32,
            ES_FloatSize.Double => Primitive_Float64,

            _ => throw ExhaustiveMatch.Failed (size),
        }).MethodTable;

        public ES_MethodTable* GetReference<T> (ES_Constness constness) where T : unmanaged, IES_ExportedType => throw new NotImplementedException ();
        public ES_MethodTable* GetArray<T> (ES_Constness constness, nint rank) where T : unmanaged, IES_ExportedType => throw new NotImplementedException ();

        public T* AllocateType<T> (T data) where T : unmanaged => typeTable.typeFactory.AllocateType (data);
        public ArrayPointer<T> AllocateArray<T> (Span<T> data) where T : unmanaged => typeTable.typeFactory.AllocateArray<T> (data);
        public ArrayPointer<T> AllocateArray<T> (ReadOnlySpan<T> data) where T : unmanaged => typeTable.typeFactory.AllocateArray (data);

        public void CreateStruct (
            ref TypeLoadToken typeToken,

            ES_FullyQualifiedName name,
            ES_Utf8String sourceUnit,

            int runtimeSize,
            ReadOnlySpan<ES_TypeInterfaceData> interfaces,

            ReadOnlySpan<ES_FieldInfo> fields,
            ReadOnlySpan<ES_MethodInfo> methods,
            ReadOnlySpan<ES_FunctionInfo> functions,

            bool nativeType
        ) {
            Debug.Assert (typeToken.Initialized);
            Debug.Assert (!typeToken.Created);
            if (typeToken.Created)
                throw new ArgumentException ("Invalid type token.", nameof (typeToken));
            if (typeToken.Created)
                throw new ArgumentException ("Type was already initialized.", nameof (typeToken));

            typeToken.Created = true;

            var methodTablePtr = typeToken.MethodTable;
            var typeInfoPtr = typeToken.MethodTable->TypeInfo;

            var typeFlags = ES_TypeFlag.ValueType;
            if (nativeType)
                typeFlags |= ES_TypeFlag.NativeType;

            using var refsList = new StructPooledList<nint> (CL_ClearMode.Auto);
            foreach (var field in fields) {
                Debug.Assert (field.Type != null);
                foreach (var refPtr in field.Type->GetRefsList ())
                    refsList.Add (field.Offset + refPtr);
            }
            var refListPtr = AllocateArray (refsList.Span);
            refsList.Dispose ();

            var structInfo = AllocateType (new ES_StructInfo {
                Fields = AllocateArray (fields),
                Methods = AllocateArray (methods),
                Functions = AllocateArray (functions),
            });

            *methodTablePtr = *methodTablePtr with {
                RuntimeSizeInit = runtimeSize,
                FlagsInit = typeFlags,

                VTableInit = ArrayPointer<Pointer<ES_FunctionData>>.Null,

                RefsListInit = refListPtr,

                ParentTypeInit = null,
                TypeInfoInit = typeInfoPtr,

                InterfacesInit = AllocateArray (interfaces),
            };
            *typeInfoPtr = *typeInfoPtr with {
                MethodTableInit = methodTablePtr,
                TypeTagInit = ES_TypeTag.Struct,

                ExtraDataInit = structInfo,

                NameInit = name,
                SourceUnitInit = sourceUnit,
            };
        }
    }

    #region ================== Instance fields

    private bool isDisposed;

    private IntPtr mimallocHeap;
    private ES_TypeFactory typeFactory;
    private NativeTypeLoader nativeTypeLoader;
    private Dictionary<Type, ES_NativeTypeStoredData> typeMap;
    private Stack<TypeLoadData> typesToLoad;

    #endregion

    #region ================== Static properties

    public static ES_NativeTypeStoredData Primitive_Void { get; }
    public static ES_NativeTypeStoredData Primitive_Bool => GetNativeType<bool> ();

    public static ES_NativeTypeStoredData Primitive_Float32 => GetNativeType<float> ();
    public static ES_NativeTypeStoredData Primitive_Float64 => GetNativeType<double> ();

    public static ES_NativeTypeStoredData Primitive_SInt8  => GetNativeType<sbyte> ();
    public static ES_NativeTypeStoredData Primitive_SInt16 => GetNativeType<short> ();
    public static ES_NativeTypeStoredData Primitive_SInt32 => GetNativeType<int> ();
    public static ES_NativeTypeStoredData Primitive_SInt64 => GetNativeType<long> ();

    public static ES_NativeTypeStoredData Primitive_UInt8  => GetNativeType<byte> ();
    public static ES_NativeTypeStoredData Primitive_UInt16 => GetNativeType<ushort> ();
    public static ES_NativeTypeStoredData Primitive_UInt32 => GetNativeType<uint> ();
    public static ES_NativeTypeStoredData Primitive_UInt64 => GetNativeType<ulong> ();

    #endregion

    #region ================== Constructors

    private ES_NativeTypeTable () {
        isDisposed = false;

        mimallocHeap = mi_heap_new ();
        typeFactory.Initialize (mimallocHeap);
        typeMap = new ();
        typesToLoad = new ();
        nativeTypeLoader = new (this);
    }

    static ES_NativeTypeTable () {
        instance = new () {
            typeFactory = new ES_TypeFactory ()
        };
        Primitive_Void = FillFromType (typeof (void), instance.typeFactory.GetVoid (), true);
    }

    #endregion

    ~ES_NativeTypeTable () {
        Dispose ();
    }

    #region ================== Static methods

    public static ES_NativeTypeStoredData GetNativeType<T> () where T : unmanaged {
        if (!instance.TryGetNativeType_Internal (typeof (T), out var ret, true))
            throw new ArgumentException ("Type is not a valid native type.", "type");

        instance.LoadNativeTypes ();

        return ret;
    }

    public static bool TryGetNativeType<T> (out ES_NativeTypeStoredData result) where T : unmanaged
        => instance.TryGetNativeType_Internal (typeof (T), out result, true);

    private static ES_NativeTypeStoredData FillFromType (Type nativeType, ES_MethodTable* methodTable, bool loaded) => new () {
        Loaded = loaded,
        Loading = false,

        Type = nativeType,
        MethodTable = methodTable,
    };

    #endregion

    #region ================== Instance methods

    private void AddType (ES_NativeTypeStoredData storedData) => typeMap.Add (storedData.Type, storedData);

    private void MarkLoading (Type type) {
        var typeData = typeMap [type];

        Debug.Assert (!typeData.Loaded);
        Debug.Assert (!typeData.Loading);

        typeMap [type] = typeData with { Loading = true, };
    }

    private void MarkLoaded (Type type) {
        var typeData = typeMap [type];

        Debug.Assert (!typeData.Loaded);
        Debug.Assert (typeData.Loading);

        typeMap [type] = typeData with { Loading = false, Loaded = true, };
    }

    private (bool Loading, bool Loaded) CheckLoaded (Type type) {
        var typeData = typeMap [type];

        return (typeData.Loading, typeData.Loaded);
    }

    private ES_FullyQualifiedName GetFQN (ReadOnlySpan<string> nsStrings, ReadOnlySpan<char> name) {
        static int WriteBytes (ReadOnlySpan<string> strings, Span<byte> byteBuffer) {
            var enc = Encoding.UTF8;

            var bufOffset = 0;
            foreach (var str in strings) {
                if (bufOffset > 0)
                    byteBuffer [bufOffset++] = (byte) '.';

                bufOffset += enc.GetBytes (str, byteBuffer [bufOffset..]);
            }

            return bufOffset;
        }

        var nameStr = typeFactory.AllocateString (name);
        if (nsStrings.Length < 1)
            return new (ES_Utf8String.Empty, nameStr);

        var enc = Encoding.UTF8;
        var byteCount = nsStrings.Length - 1;
        foreach (var str in nsStrings)
            byteCount += enc.GetByteCount (str);

        if (byteCount <= ES_Utf8String.MaxLocalTextSize) {
            Span<byte> localBuf = stackalloc byte [byteCount];

            var bufOffset = WriteBytes (nsStrings, localBuf);
            Debug.Assert (bufOffset == byteCount);

            return new (new (localBuf), nameStr);
        } else {
            var memData = typeFactory.AllocateN<byte> (byteCount);

            var bufOffset = WriteBytes (nsStrings, memData.Span);
            Debug.Assert (bufOffset == byteCount);

            return new (new (memData), nameStr);
        }
    }

    #region Type creation

    private bool TryGetNativeType_Internal (Type type, out ES_NativeTypeStoredData result, bool doLoad)
        => TryGetNativeType_Internal (type, out result, out var loadData, doLoad);

    private bool TryGetNativeType_Internal (Type type, out ES_NativeTypeStoredData result, out TypeLoadData loadData, bool doLoad) {
        if (typeMap.TryGetValue (type, out result)) {
            Debug.Assert (!doLoad || result.Loaded);
            loadData = default;
            return true;
        }

        if (
            TryAddSpecialType (type, out result, out loadData) ||
            TryHandleAggregate (type, out result, out loadData, doLoad)
        ) {
            if (doLoad) {
                result = typeMap [type];
                Debug.Assert (!doLoad || result.Loaded);
            }

            return true;
        }

        return false;
    }

    private bool TryAddSpecialType (Type type, out ES_NativeTypeStoredData result, out TypeLoadData loadData) {
        if (type == typeof (bool))
            result = FillFromType (type, typeFactory.GetBool (), true);
        // Signed ints
        else if (type == typeof (sbyte))
            result = FillFromType (type, typeFactory.GetInt (ES_IntSize.Int8, false), true);
        else if (type == typeof (short))
            result = FillFromType (type, typeFactory.GetInt (ES_IntSize.Int16, false), true);
        else if (type == typeof (int))
            result = FillFromType (type, typeFactory.GetInt (ES_IntSize.Int32, false), true);
        else if (type == typeof (long))
            result = FillFromType (type, typeFactory.GetInt (ES_IntSize.Int64, false), true);
        // Unsigned ints
        else if (type == typeof (byte))
            result = FillFromType (type, typeFactory.GetInt (ES_IntSize.Int8, true), true);
        else if (type == typeof (ushort))
            result = FillFromType (type, typeFactory.GetInt (ES_IntSize.Int16, true), true);
        else if (type == typeof (uint))
            result = FillFromType (type, typeFactory.GetInt (ES_IntSize.Int32, true), true);
        else if (type == typeof (ulong))
            result = FillFromType (type, typeFactory.GetInt (ES_IntSize.Int64, true), true);
        // Floats
        else if (type == typeof (float))
            result = FillFromType (type, typeFactory.GetFloat (ES_FloatSize.Single), true);
        else if (type == typeof (double))
            result = FillFromType (type, typeFactory.GetFloat (ES_FloatSize.Double), true);

        // References
        else if (type == typeof (ES_ObjectAddress))
            result = FillFromType (type, typeFactory.GetReference (ES_Constness.Mutable, Primitive_Void.MethodTable), true);
        else if (ES_Utils.TryGetNativeObjectRefInfo (type, out var objRefPointedType, out var objRefConstness)) {
            if (!TryGetNativeType_Internal (objRefPointedType, out var pointedType, false)) {
                result = default;
                loadData = default;
                return false;
            }

            result = FillFromType (type, typeFactory.GetReference (objRefConstness, pointedType.MethodTable), true);
        } else if (ES_Utils.TryGetNativeArrayInfo (type, out var arrayElemType, out var arrayRank, out var arrayConstness)) {
            if (!TryGetNativeType_Internal (arrayElemType, out var pointedType, false)) {
                result = default;
                loadData = default;
                return false;
            }

            result = FillFromType (type, typeFactory.GetArray (arrayRank, arrayConstness, pointedType.MethodTable), true);
        }

        // Non-matching
        else {
            result = default;
            loadData = default;
            return false;
        }

        AddType (result);

        loadData = default;
        return true;
    }

    private bool TryHandleAggregate (Type type, out ES_NativeTypeStoredData result, out TypeLoadData loadData, bool doLoad) {
        var aggregateAttribute = type.GetCustomAttribute<ES_ExportStructAttribute> (false);

        if (aggregateAttribute is null) {
            result = default;
            loadData = default;
            return false;
        }

        var typeSize = Marshal.SizeOf (type);
        var nsString = aggregateAttribute.ExportNamespace ?? Array.Empty<string> ();

        var methodTable = typeFactory.GetStruct (
            true, typeSize,
            GetFQN (nsString, aggregateAttribute.ExportName),
            typeFactory.AllocateType<ES_StructInfo> (new ()),
            ReadOnlySpan<Pointer<ES_FunctionData>>.Empty,
            ReadOnlySpan<nint>.Empty,
            ReadOnlySpan<ES_TypeInterfaceData>.Empty
        );

        result = FillFromType (type, methodTable, false);
        AddType (result);

        loadData = new TypeLoadData {
            Type = type,
            MethodTable = methodTable,
        };
        if (doLoad)
            LoadNativeType (loadData);
        else
            typesToLoad.Push (loadData);

        return true;
    }

    #endregion

    #region Type loading

    private void LoadNativeTypes () {
        while (typesToLoad.Count > 0)
            LoadNativeType (typesToLoad.Pop ());
    }

    private void LoadNativeType (TypeLoadData typeData) {
        var loadFlags = CheckLoaded (typeData.Type);
        if (loadFlags.Loaded)
            return;
        else if (loadFlags.Loading)
            throw new Exception ("Native type loading exception: Recursive loop.");

        LoadNativeType_Aggregate (typeData.Type, null!, typeData.MethodTable);
    }

    private void LoadNativeType_Aggregate (Type type, ES_ExportStructAttribute aggregateAttrib, ES_MethodTable* methodTable) {
        MarkLoading (type);

        using var refsList = new StructPooledList<nint> (CL_ClearMode.Auto);
        using var fieldsList = new StructPooledList<ES_FieldInfo> (CL_ClearMode.Auto);
        foreach (var field in type.GetFields (BindingFlags.Public | BindingFlags.NonPublic)) {
            var fieldAttributes = field.GetCustomAttribute<ES_ExportFieldAttribute> (false);
            var fieldOffset = Marshal.OffsetOf (type, field.Name);

            if (!TryGetNativeType_Internal (field.FieldType, out var fieldType, out var fieldLoadData, true))
                continue;

            if (!fieldType.Loaded) {
                LoadNativeType (fieldLoadData);
                fieldType = typeMap [field.FieldType];
            }

            refsList.AddRange (fieldType.MethodTable->GetRefsList ());

            if (fieldAttributes is null)
                continue;

            fieldsList.Add (new () {
                Name = typeFactory.AllocateString (fieldAttributes.ExportName),
                Constness = fieldAttributes.Constness,
                Type = fieldType.MethodTable,
                Offset = fieldOffset,
            });
        }

        MarkLoaded (type);
    }

    #endregion

    public void Dispose () {
        if (isDisposed)
            return;

        typeMap.Clear ();
        mi_heap_destroy (mimallocHeap);
        mimallocHeap = IntPtr.Zero;

        isDisposed = true;
    }

    #endregion
}
