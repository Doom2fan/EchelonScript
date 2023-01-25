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
using ChronosLib.Pooled;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.Exporting;
using EchelonScript.Common.Utilities;
using static TerraFX.Interop.Mimalloc;

namespace EchelonScript.Common.Data;

internal delegate bool ES_InitializeNativeTypeDelegate (ES_TypeTable.TypeLoader typeLoader, ref ES_TypeTable.TypeLoadToken typeToken, bool doLoad);
public unsafe delegate void ES_InitializeScriptTypeDelegate (ES_TypeTable.TypeLoader typeLoader, void* data, ref ES_TypeTable.TypeLoadToken typeToken);

internal unsafe interface IES_NativeTypeStoredData {
    internal bool Loaded { get; }
    internal bool Loading { get; }

    public Type ClrType { get; }
    public ES_MethodTable* MethodTable { get; }
}

public unsafe sealed class ES_NativeTypeStoredData<T> : IES_NativeTypeStoredData where T : unmanaged {
    public readonly static ES_NativeTypeStoredData<T> Instance = new ();

    internal bool Loaded { get; set; }
    internal bool Loading { get; set; }

    public Type ClrType { get; private set; }
    public ES_MethodTable* MethodTable { get; private set; }

    bool IES_NativeTypeStoredData.Loaded => Loaded;
    bool IES_NativeTypeStoredData.Loading => Loading;

    private ES_NativeTypeStoredData () {
        Loaded = false;
        Loading = false;

        ClrType = typeof (T);
        MethodTable = null;
    }

    internal void StartInit (ES_TypeTable.TypeLoader typeLoader, ref ES_TypeTable.TypeLoadToken typeToken) {
        Loading = true;
        MethodTable = typeToken.MethodTable;

        if (default (T) is IES_ExportedType exportedType) {
            var basicData = exportedType.GetBasicData (typeLoader);

            *MethodTable = *MethodTable with {
                RuntimeSizeInit = basicData.RuntimeSize,
            };
            *MethodTable->TypeInfo = *MethodTable->TypeInfo with {
                NameInit = basicData.Name,
                SourceUnitInit = basicData.SourceUnit,
            };
        }
    }

    internal void EndInit (bool loaded) {
        Debug.Assert (Loading);
        Debug.Assert (MethodTable != null);

        (Loading, Loaded) = (false, loaded);
        if (!loaded)
            MethodTable = null;
    }

    internal bool InitializeType (ES_TypeTable.TypeLoader typeLoader, ref ES_TypeTable.TypeLoadToken typeToken, bool doLoad) {
        Debug.Assert (Loading);
        Debug.Assert (!Loaded);

        if (Loaded)
            return true;
        if (!Loading)
            return false;

        if (
            TryHandlePrimitive (typeLoader, ref typeToken) ||
            TryHandleReference (typeLoader, ref typeToken)
        )
            goto Success;

        if (!doLoad)
            return false;

        if (TryHandleExport (typeLoader, ref typeToken))
            goto Success;

        EndInit (false);
        throw new EchelonScriptTypeLoadException (nameof (T));

    Success:
        EndInit (true);
        return true;
    }

    private bool TryHandlePrimitive (ES_TypeTable.TypeLoader typeLoader, ref ES_TypeTable.TypeLoadToken typeToken) {
        if (ClrType == typeof (ES_Void))
            typeLoader.CreateVoid (ref typeToken);
        else if (ClrType == typeof (bool))
            typeLoader.CreateBool (ref typeToken);
        // Signed ints
        else if (ClrType == typeof (sbyte))
            typeLoader.CreateInt (ref typeToken, ES_IntSize.Int8, false);
        else if (ClrType == typeof (short))
            typeLoader.CreateInt (ref typeToken, ES_IntSize.Int16, false);
        else if (ClrType == typeof (int))
            typeLoader.CreateInt (ref typeToken, ES_IntSize.Int32, false);
        else if (ClrType == typeof (long))
            typeLoader.CreateInt (ref typeToken, ES_IntSize.Int64, false);
        // Unsigned ints
        else if (ClrType == typeof (byte))
            typeLoader.CreateInt (ref typeToken, ES_IntSize.Int8, true);
        else if (ClrType == typeof (ushort))
            typeLoader.CreateInt (ref typeToken, ES_IntSize.Int16, true);
        else if (ClrType == typeof (uint))
            typeLoader.CreateInt (ref typeToken, ES_IntSize.Int32, true);
        else if (ClrType == typeof (ulong))
            typeLoader.CreateInt (ref typeToken, ES_IntSize.Int64, true);
        // Floats
        else if (ClrType == typeof (float))
            typeLoader.CreateFloat (ref typeToken, ES_FloatSize.Single);
        else if (ClrType == typeof (double))
            typeLoader.CreateFloat (ref typeToken, ES_FloatSize.Double);

        // Non-matching
        else
            return false;

        return typeToken.Created;
    }

    private bool TryHandleReference (ES_TypeTable.TypeLoader typeLoader, ref ES_TypeTable.TypeLoadToken typeToken) {
        if (ClrType == typeof (ES_ObjectAddress) || ClrType == typeof (ES_ArrayAddress))
            typeLoader.GetType<ES_Object<ES_Void>> (true);
        else if (default (T) is IES_ReferenceType refType) {
            refType.InitializeType (typeLoader, ref typeToken);
            return typeToken.Created;
        }

        return false;
    }

    private static bool TryHandleExport (ES_TypeTable.TypeLoader typeLoader, ref ES_TypeTable.TypeLoadToken typeToken) {
        if (default (T) is not IES_ExportedType exportedType)
            return false;

        exportedType.InitializeType (typeLoader, ref typeToken);
        return typeToken.Created;
    }
}

public unsafe sealed class ES_TypeTable : IDisposable {
    private static ES_TypeTable nativeInstance;

    private struct TypeLoadData {
        public ES_MethodTable* MethodTable;
        public ES_InitializeNativeTypeDelegate? NativeInitDelegate;
        public ES_InitializeScriptTypeDelegate? ScriptInitDelegate;
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

        internal void Validate (string paramName) {
            if (!Initialized)
                throw new ArgumentException ("Invalid type token.", paramName);
            if (Created)
                throw new ArgumentException ("Type was already initialized.", paramName);
        }
    }

    public readonly struct BasicTypeInfo {
        public ES_FullyQualifiedName Name { get; private init; }
        public ES_Utf8String SourceUnit { get; private init; }
        public int RuntimeSize { get; private init; }

        public BasicTypeInfo (
            ES_FullyQualifiedName name,
            ES_Utf8String sourceUnit,
            int size
        ) {
            Name = name;
            SourceUnit = sourceUnit;
            RuntimeSize = size;
        }
    }

    // TODO: wtf do we do about script types? Turning this into a generic type loader might be more trouble than it's worth.
    // Or maybe the generic version should work off of data structures? That way we could have it properly walk through the tree.
    // Maybe we should make it so types do "basic" initialization first (e.g., names, sizes, etc.) then actually load fully.
    public sealed class TypeLoader {
        private ES_TypeTable typeTable;
        private nint* pointerRefList;

        internal TypeLoader (ES_TypeTable tTable) => typeTable = tTable;

        private ArrayPointer<nint> GetPointerRefList () {
            if (pointerRefList == null)
                pointerRefList = AllocateType<nint> (0);

            return new (pointerRefList, 1);
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

        public ES_FullyQualifiedName AllocateFQN (ReadOnlySpan<char> nsName, ReadOnlySpan<char> name)
            => new (typeTable.typeFactory.AllocateString (nsName), typeTable.typeFactory.AllocateString (name));
        public ES_Utf8String AllocateString (ReadOnlySpan<char> text)
            => typeTable.typeFactory.AllocateString (text);

        private ES_MethodTable* GetType_Internal<T> (bool load) where T : unmanaged {
            if (!nativeInstance.TryGetNativeType_Internal<T> (out var ret, load))
                throw new ArgumentException ("Type is not a valid native type.", "T");

            return ret;
        }
        public ES_MethodTable* GetType<T> (bool load) where T : unmanaged => GetType_Internal<T> (load);
        //public ES_MethodTable* GetScriptType () => typeTable.TryGetScriptType ();

        public T* AllocateType<T> () where T : unmanaged => typeTable.typeFactory.AllocateType<T> ();
        public T* AllocateType<T> (T data) where T : unmanaged => typeTable.typeFactory.AllocateType (data);
        public ArrayPointer<T> AllocateN<T> (int count) where T : unmanaged => typeTable.typeFactory.AllocateN<T> (count);
        public ArrayPointer<T> AllocateArray<T> (Span<T> data) where T : unmanaged => typeTable.typeFactory.AllocateArray<T> (data);
        public ArrayPointer<T> AllocateArray<T> (ReadOnlySpan<T> data) where T : unmanaged => typeTable.typeFactory.AllocateArray (data);

        internal ES_MethodTable* AllocateMethodTable () {
            var ret = AllocateType<ES_MethodTable> ();
            *ret = new (
                -1, 0,
                null, 0,
                null, 0,
                null, AllocateType<ES_TypeInfo> (new () { MethodTableInit = ret }),
                null, 0
            );

            return ret;
        }

        public void CreateReference<TPointed> (ref TypeLoadToken typeToken, ES_Constness pointedConst) where TPointed : unmanaged {
            Debug.Assert (typeToken.Initialized);
            Debug.Assert (!typeToken.Created);
            typeToken.Validate (nameof (typeToken));

            typeToken.Created = true;

            var pointedType = GetType_Internal<TPointed> (false);
            var methodTablePtr = typeToken.MethodTable;
            var typeInfoPtr = typeToken.MethodTable->TypeInfo;

            var typeFlags = ES_TypeFlag.ValueType | ES_TypeFlag.NativeType;

            ES_Utf8String name;
            {
                var chars = new StructPooledList<char> (CL_ClearMode.Auto);
                try {
                    chars.Add ('&');
                    AddFQNToChars (ref chars, pointedType->TypeInfo->Name, pointedConst);

                    name = typeTable.typeFactory.AllocateString (chars.Span);
                } finally {
                    chars.Dispose ();
                }
            }

            *methodTablePtr = *methodTablePtr with {
                RuntimeSizeInit = sizeof (void*),
                FlagsInit = typeFlags,

                VTableInit = ArrayPointer<Pointer<ES_FunctionData>>.Null,

                RefsListInit = GetPointerRefList (),

                ParentTypeInit = null,
                TypeInfoInit = typeInfoPtr,

                InterfacesInit = ArrayPointer<ES_TypeInterfaceData>.Null,
            };
            *typeInfoPtr = *typeInfoPtr with {
                MethodTableInit = methodTablePtr,
                TypeTagInit = ES_TypeTag.Reference,

                ExtraDataInit = AllocateType<ES_ReferenceInfo> (new (pointedConst, pointedType)),

                NameInit = AllocateFQN ("", ES_PrimitiveTypeConsts.Void),
                SourceUnitInit = ES_Utf8String.Empty,
            };
        }
        public void CreateArray<TPointed> (ref TypeLoadToken typeToken, ES_Constness constness, int rank) where TPointed : unmanaged => throw new NotImplementedException ();

        public void CreateStruct (
            ref TypeLoadToken typeToken,

            ReadOnlySpan<ES_TypeInterfaceData> interfaces,

            ReadOnlySpan<ES_FieldInfo> fields,
            ReadOnlySpan<ES_MethodInfo> methods,
            ReadOnlySpan<ES_FunctionInfo> functions,

            bool nativeType
        ) {
            Debug.Assert (typeToken.Initialized);
            Debug.Assert (!typeToken.Created);
            typeToken.Validate (nameof (typeToken));

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
            };
        }

        public void CreateVoid (ref TypeLoadToken typeToken) {
            Debug.Assert (typeToken.Initialized);
            Debug.Assert (!typeToken.Created);
            typeToken.Validate (nameof (typeToken));

            typeToken.Created = true;

            var methodTablePtr = typeToken.MethodTable;
            var typeInfoPtr = typeToken.MethodTable->TypeInfo;

            var typeFlags = ES_TypeFlag.ValueType | ES_TypeFlag.NativeType | ES_TypeFlag.NoRefs;

            *methodTablePtr = *methodTablePtr with {
                RuntimeSizeInit = 1,
                FlagsInit = typeFlags,

                VTableInit = ArrayPointer<Pointer<ES_FunctionData>>.Null,

                RefsListInit = ArrayPointer<nint>.Null,

                ParentTypeInit = null,
                TypeInfoInit = typeInfoPtr,

                InterfacesInit = ArrayPointer<ES_TypeInterfaceData>.Null,
            };
            *typeInfoPtr = *typeInfoPtr with {
                MethodTableInit = methodTablePtr,
                TypeTagInit = ES_TypeTag.Void,

                ExtraDataInit = null,

                NameInit = AllocateFQN ("", ES_PrimitiveTypeConsts.Void),
                SourceUnitInit = ES_Utf8String.Empty,
            };
        }

        public void CreateBool (ref TypeLoadToken typeToken) {
            Debug.Assert (typeToken.Initialized);
            Debug.Assert (!typeToken.Created);
            typeToken.Validate (nameof (typeToken));

            typeToken.Created = true;

            var methodTablePtr = typeToken.MethodTable;
            var typeInfoPtr = typeToken.MethodTable->TypeInfo;

            var typeFlags = ES_TypeFlag.ValueType | ES_TypeFlag.NativeType | ES_TypeFlag.NoRefs;

            *methodTablePtr = *methodTablePtr with {
                RuntimeSizeInit = 1,
                FlagsInit = typeFlags,

                VTableInit = ArrayPointer<Pointer<ES_FunctionData>>.Null,

                RefsListInit = ArrayPointer<nint>.Null,

                ParentTypeInit = null,
                TypeInfoInit = typeInfoPtr,

                InterfacesInit = ArrayPointer<ES_TypeInterfaceData>.Null,
            };
            *typeInfoPtr = *typeInfoPtr with {
                MethodTableInit = methodTablePtr,
                TypeTagInit = ES_TypeTag.Void,

                ExtraDataInit = null,

                NameInit = AllocateFQN ("", ES_PrimitiveTypeConsts.Bool),
                SourceUnitInit = ES_Utf8String.Empty,
            };
        }

        public void CreateInt (ref TypeLoadToken typeToken, ES_IntSize intSize, bool unsigned) {
            Debug.Assert (typeToken.Initialized);
            Debug.Assert (!typeToken.Created);
            typeToken.Validate (nameof (typeToken));

            typeToken.Created = true;

            var methodTablePtr = typeToken.MethodTable;
            var typeInfoPtr = typeToken.MethodTable->TypeInfo;

            var typeFlags = ES_TypeFlag.ValueType | ES_TypeFlag.NativeType | ES_TypeFlag.NoRefs;

            *methodTablePtr = *methodTablePtr with {
                RuntimeSizeInit = ES_PrimitiveTypeConsts.GetIntMemorySize (intSize),
                FlagsInit = typeFlags,

                VTableInit = ArrayPointer<Pointer<ES_FunctionData>>.Null,

                RefsListInit = ArrayPointer<nint>.Null,

                ParentTypeInit = null,
                TypeInfoInit = typeInfoPtr,

                InterfacesInit = ArrayPointer<ES_TypeInterfaceData>.Null,
            };
            *typeInfoPtr = *typeInfoPtr with {
                MethodTableInit = methodTablePtr,
                TypeTagInit = ES_TypeTag.Int,

                ExtraDataInit = (void*) new ES_IntInfo (intSize, unsigned),

                NameInit = AllocateFQN ("", ES_PrimitiveTypeConsts.GetIntName (intSize, unsigned)),
                SourceUnitInit = ES_Utf8String.Empty,
            };
        }

        public void CreateFloat (ref TypeLoadToken typeToken, ES_FloatSize floatSize) {
            Debug.Assert (typeToken.Initialized);
            Debug.Assert (!typeToken.Created);
            typeToken.Validate (nameof (typeToken));

            typeToken.Created = true;

            var methodTablePtr = typeToken.MethodTable;
            var typeInfoPtr = typeToken.MethodTable->TypeInfo;

            var typeFlags = ES_TypeFlag.ValueType | ES_TypeFlag.NativeType | ES_TypeFlag.NoRefs;

            *methodTablePtr = *methodTablePtr with {
                RuntimeSizeInit = ES_PrimitiveTypeConsts.GetFloatMemorySize (floatSize),
                FlagsInit = typeFlags,

                VTableInit = ArrayPointer<Pointer<ES_FunctionData>>.Null,

                RefsListInit = ArrayPointer<nint>.Null,

                ParentTypeInit = null,
                TypeInfoInit = typeInfoPtr,

                InterfacesInit = ArrayPointer<ES_TypeInterfaceData>.Null,
            };
            *typeInfoPtr = *typeInfoPtr with {
                MethodTableInit = methodTablePtr,
                TypeTagInit = ES_TypeTag.Float,

                ExtraDataInit = (void*) new ES_FloatInfo (floatSize),

                NameInit = AllocateFQN ("", ES_PrimitiveTypeConsts.GetFloatName (floatSize)),
                SourceUnitInit = ES_Utf8String.Empty,
            };
        }
    }

    #region ================== Instance fields

    private bool isDisposed;

    private IntPtr mimallocHeap;
    private ES_TypeFactory typeFactory;
    private TypeLoader nativeTypeLoader;
    private Stack<TypeLoadData> typesToLoad;

    #endregion

    #region ================== Constructors

    private ES_TypeTable () {
        isDisposed = false;

        mimallocHeap = mi_heap_new ();
        typeFactory.Initialize (mimallocHeap);
        nativeTypeLoader = new (this);
        typesToLoad = new ();
    }

    static ES_TypeTable () {
        nativeInstance = new ();
    }

    #endregion

    ~ES_TypeTable () {
        Dispose ();
    }

    #region ================== Static methods

    public static ES_MethodTable* GetNativeType<T> () where T : unmanaged {
        var success = nativeInstance.TryGetNativeType_Internal<T> (out var ret, true);
        nativeInstance.InitializeTypes ();
        if (!success)
            throw new ArgumentException ("Type is not a valid native type.", "T");

        return ret;
    }

    public static bool TryGetNativeType<T> (out ES_MethodTable* result) where T : unmanaged {
        var success = nativeInstance.TryGetNativeType_Internal<T> (out result, true);
        nativeInstance.InitializeTypes ();
        return success;
    }

    #endregion

    #region ================== Instance methods

    /*public bool TryGetScriptType (ES_InitializeNativeTypeDelegate typeInitializer, out ES_MethodTable* result) {
        var typeToken = new TypeLoadToken (nativeTypeLoader.AllocateMethodTable ());

        dataInst.InitializeType (nativeTypeLoader, ref typeToken);

        if (!typeToken.Created) {
            result = null;
            return false;
        }

        result = typeToken.MethodTable;
        return true;
    }*/

    private bool TryGetNativeType_Internal<T> (out ES_MethodTable* result, bool load) where T : unmanaged {
        var dataInst = ES_NativeTypeStoredData<T>.Instance;
        if (dataInst.Loaded || (dataInst.Loading && !load)) {
            Debug.Assert (dataInst.MethodTable != null);
            result = dataInst.MethodTable;
            return true;
        }

        var typeToken = new TypeLoadToken (nativeTypeLoader.AllocateMethodTable ());

        dataInst.StartInit (nativeTypeLoader, ref typeToken);
        var initResult = dataInst.InitializeType (nativeTypeLoader, ref typeToken, load);
        if (!load && !initResult) {
            result = typeToken.MethodTable;
            typesToLoad.Push (new () { MethodTable = result, NativeInitDelegate = dataInst.InitializeType });

            return true;
        }

        if (!typeToken.Created) {
            result = null;
            return false;
        }

        result = typeToken.MethodTable;
        return true;
    }

    private void InitializeTypes () {
        while (typesToLoad.Count > 0) {
            var type = typesToLoad.Pop ();
            var typeToken = new TypeLoadToken (type.MethodTable);

            type.NativeInitDelegate! (nativeTypeLoader, ref typeToken, true);
        }
    }

    public void Dispose () {
        if (isDisposed)
            return;

        mi_heap_destroy (mimallocHeap);
        mimallocHeap = IntPtr.Zero;

        isDisposed = true;
    }

    #endregion
}
