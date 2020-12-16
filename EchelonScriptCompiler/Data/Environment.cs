/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;
using ChronosLib.Pooled;
using Collections.Pooled;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Utilities;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Data {
    public unsafe class ES_NamespaceData : IDisposable {
        public class Builder : IDisposable {
            #region ================== Instance fields

            private ES_NamespaceData namespaceData;

            #endregion

            #region ================== Instance properties

            public ES_NamespaceData NamespaceData => namespaceData;

            public PooledDictionary<ArrayPointer<byte>, ES_ClassData.Builder> ClassBuilders { get; protected set; }
            public PooledDictionary<ArrayPointer<byte>, ES_StructData.Builder> StructBuilders { get; protected set; }
            public PooledDictionary<ArrayPointer<byte>, ES_EnumData.Builder> EnumBuilders { get; protected set; }
            public PooledDictionary<ArrayPointer<byte>, ES_FunctionData.Builder> FunctionBuilders { get; protected set; }

            public int TypesStartIdx {
                get => namespaceData.typesStartIdx;
                set => namespaceData.typesStartIdx = value;
            }

            public int TypesLength {
                get => namespaceData.typesLength;
                set => namespaceData.typesLength = value;
            }

            #endregion

            #region ================== Constructors

            public Builder ([DisallowNull] ES_NamespaceData nm) {
                namespaceData = nm;

                ClassBuilders = new PooledDictionary<ArrayPointer<byte>, ES_ClassData.Builder> ();
                StructBuilders = new PooledDictionary<ArrayPointer<byte>, ES_StructData.Builder> ();
                EnumBuilders = new PooledDictionary<ArrayPointer<byte>, ES_EnumData.Builder> ();
                FunctionBuilders = new PooledDictionary<ArrayPointer<byte>, ES_FunctionData.Builder> ();
            }

            #endregion

            #region ================== Instance methods

            public ES_TypeTag? CheckTypeExists (ArrayPointer<byte> name, ES_TypeTag? ignoredType) {
                if (ignoredType != ES_TypeTag.Class && ClassBuilders.TryGetValue (name, out var asd))
                    return ES_TypeTag.Class;

                if (ignoredType != ES_TypeTag.Struct && StructBuilders.TryGetValue (name, out var fs))
                    return ES_TypeTag.Struct;

                return null;
            }

            public ES_ClassData.Builder GetOrCreateClass (ES_AccessModifier accessMod,
                ArrayPointer<byte> name, ArrayPointer<byte> fullyQualifiedName,
                ArrayPointer<byte> sourceUnit
            ) {
                CheckDisposed ();

                if (CheckTypeExists (name, ES_TypeTag.Class) != null)
                    throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

                if (ClassBuilders.TryGetValue (name, out var builder))
                    return builder;

                var classDataPtr = (ES_ClassData*) Marshal.AllocHGlobal (sizeof (ES_ClassData));

                builder = new ES_ClassData.Builder (classDataPtr, accessMod, name, fullyQualifiedName, sourceUnit);
                ClassBuilders.Add (name, builder);

                var unknType = namespaceData.environment.TypeUnknownValue;

                builder.BaseClass = (ES_ClassData*) unknType;
                builder.InterfacesList = ArrayPointer<Pointer<ES_InterfaceData>>.Null;

                return builder;
            }

            public ES_StructData.Builder GetOrCreateStruct (ES_AccessModifier accessMod,
                ArrayPointer<byte> name, ArrayPointer<byte> fullyQualifiedName,
                ArrayPointer<byte> sourceUnit
            ) {
                CheckDisposed ();

                if (CheckTypeExists (name, ES_TypeTag.Struct) != null)
                    throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

                if (StructBuilders.TryGetValue (name, out var builder))
                    return builder;

                var structDataPtr = (ES_StructData*) Marshal.AllocHGlobal (sizeof (ES_StructData));

                builder = new ES_StructData.Builder (structDataPtr, accessMod, name, fullyQualifiedName, sourceUnit);
                StructBuilders.Add (name, builder);

                return builder;
            }

            public ES_EnumData.Builder GetOrCreateEnum (ES_AccessModifier accessMod,
                ArrayPointer<byte> name, ArrayPointer<byte> fullyQualifiedName,
                ArrayPointer<byte> sourceUnit
            ) {
                CheckDisposed ();

                if (CheckTypeExists (name, ES_TypeTag.Enum) != null)
                    throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

                if (EnumBuilders.TryGetValue (name, out var builder))
                    return builder;

                var enumDataPtr = (ES_EnumData*) Marshal.AllocHGlobal (sizeof (ES_EnumData));

                builder = new ES_EnumData.Builder (enumDataPtr, accessMod, name, fullyQualifiedName, sourceUnit);
                EnumBuilders.Add (name, builder);

                return builder;
            }

            public ES_FunctionData.Builder GetOrCreateFunction (ES_AccessModifier accessMod,
                ArrayPointer<byte> name, ArrayPointer<byte> fullyQualifiedName,
                ArrayPointer<byte> sourceUnit
            ) {
                CheckDisposed ();

                if (CheckTypeExists (name, ES_TypeTag.Function) != null)
                    throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

                if (FunctionBuilders.TryGetValue (name, out var builder))
                    return builder;

                var classDataPtr = (ES_FunctionData*) Marshal.AllocHGlobal (sizeof (ES_FunctionData));

                builder = new ES_FunctionData.Builder (classDataPtr, accessMod, name, fullyQualifiedName, sourceUnit);
                FunctionBuilders.Add (name, builder);

                return builder;
            }

            public ES_ClassData.Builder? GetClass (ArrayPointer<byte> name) {
                CheckDisposed ();

                if (ClassBuilders.TryGetValue (name, out var builder))
                    return builder;

                return null;
            }

            public ES_StructData.Builder? GetStruct (ArrayPointer<byte> name) {
                CheckDisposed ();

                if (StructBuilders.TryGetValue (name, out var builder))
                    return builder;

                return null;
            }

            public ES_EnumData.Builder? GetEnum (ArrayPointer<byte> name) {
                CheckDisposed ();

                if (EnumBuilders.TryGetValue (name, out var builder))
                    return builder;

                return null;
            }

            public ES_FunctionData.Builder? GetFunction (ArrayPointer<byte> name) {
                CheckDisposed ();

                if (FunctionBuilders.TryGetValue (name, out var builder))
                    return builder;

                return null;
            }

            protected void CheckDisposed () {
                if (disposedValue)
                    throw new ObjectDisposedException (nameof (ES_NamespaceData.Builder));
            }

            #endregion

            #region ================== IDisposable support

            private bool disposedValue = false;

            ~Builder () {
                if (!disposedValue)
                    DoDispose ();
            }

            protected virtual void DoDispose () {
                if (!disposedValue) {
                    ClassBuilders?.Dispose ();

                    disposedValue = true;
                }
            }

            public void Dispose () {
                DoDispose ();
                GC.SuppressFinalize (this);
            }

            #endregion
        }

        #region ================== Instance fields

        protected EchelonScriptEnvironment environment;
        protected ArrayPointer<byte> namespaceName;

        protected int typesStartIdx;
        protected int typesLength;

        #endregion

        #region ================== Instance properties

        public ArrayPointer<byte> NamespaceName => namespaceName;

        public string NamespaceNameString {
            get {
                return StringPool.Shared.GetOrAdd (namespaceName.Span, Encoding.ASCII);
            }
        }

        public int TypesStartIdx => typesStartIdx;
        public int TypesLength => typesLength;

        public ReadOnlySpan<Pointer<ES_TypeInfo>> TypeSpan
            => environment.TypesList.Span.Slice (typesStartIdx, typesLength);

        #endregion

        #region ================== Constructors

        public ES_NamespaceData (EchelonScriptEnvironment env, ArrayPointer<byte> name) {
            environment = env;
            namespaceName = name;
        }

        #endregion

        #region ================== IDisposable support

        private bool disposedValue = false;

        ~ES_NamespaceData () {
            if (!disposedValue)
                DoDispose ();
        }

        protected virtual void DoDispose () {
            if (!disposedValue) {
                disposedValue = true;
            }
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }

    public unsafe class EchelonScriptEnvironment : IDisposable {
        public class Builder : IDisposable {
            #region ================== Instance fields

            private EchelonScriptEnvironment environment;

            #endregion

            #region ================== Instance properties

            public PooledDictionary<ArrayPointer<byte>, ES_NamespaceData.Builder> NamespaceBuilders { get; protected set; }

            public PooledList<Pointer<ES_TypeInfo>> TypesList => environment.typesList;

            #endregion

            #region ================== Constructors

            internal Builder (EchelonScriptEnvironment env) {
                environment = env;

                NamespaceBuilders = new PooledDictionary<ArrayPointer<byte>, ES_NamespaceData.Builder> ();
            }

            #endregion

            #region ================== Instance methods

            public ES_NamespaceData.Builder GetOrCreateNamespace (ArrayPointer<byte> namePtr) {
                CheckDisposed ();

                if (NamespaceBuilders.TryGetValue (namePtr, out var builder))
                    return builder;

                var namespaceData = new ES_NamespaceData (environment, namePtr);
                environment.namespacesDict.Add (namePtr, namespaceData);

                builder = new ES_NamespaceData.Builder (namespaceData);
                NamespaceBuilders.Add (namePtr, builder);

                return builder;
            }

            public IntPtr GetUnmanagedMemory (int byteCount) {
                var ptr = Marshal.AllocHGlobal (byteCount);

                environment.allocatedMemory.Add ((byte*) ptr);

                return ptr;
            }

            public T* GetUnmanagedMemory<T> (int count)
                where T : unmanaged {
                var ptr = (T*) Marshal.AllocHGlobal (sizeof (T) * count);

                environment.allocatedMemory.Add ((byte*) ptr);

                return ptr;
            }

            protected void CheckDisposed () {
                if (disposedValue)
                    throw new ObjectDisposedException (nameof (EchelonScriptEnvironment));
            }

            #endregion

            #region ================== IDisposable support

            private bool disposedValue = false;

            ~Builder () {
                if (!disposedValue)
                    DoDispose ();
            }

            protected virtual void DoDispose () {
                if (!disposedValue) {
                    NamespaceBuilders?.Dispose ();

                    disposedValue = true;
                }
            }

            public void Dispose () {
                DoDispose ();
                GC.SuppressFinalize (this);
            }

            #endregion
        }

        #region ================== Instance fields

        protected PooledList<Pointer<ES_TypeInfo>> typesList;
        protected PooledList<Pointer<byte>> allocatedMemory;
        protected Dictionary<ArrayPointer<byte>, ES_NamespaceData> namespacesDict;
        protected ES_TypeInfo* typeUnknownValue;

        #endregion

        #region ================== Instance properties

        public UnmanagedIdentifierPool IdPool { get; protected set; }

        public IReadOnlyPooledList<Pointer<ES_TypeInfo>> TypesList => typesList;

        public IReadOnlyDictionary<ArrayPointer<byte>, ES_NamespaceData> Namespaces => namespacesDict;

        public ES_TypeInfo* TypeUnknownValue => typeUnknownValue;

        #endregion

        #region ================== Constructors

        protected EchelonScriptEnvironment () {
            namespacesDict = new Dictionary<ArrayPointer<byte>, ES_NamespaceData> ();
            IdPool = new UnmanagedIdentifierPool ();

            typesList = new PooledList<Pointer<ES_TypeInfo>> ();
            allocatedMemory = new PooledList<Pointer<byte>> ();

            var unknTypeId = IdPool.GetIdentifier ("#UNKNOWN_TYPE");
            var unknType = (ES_TypeInfo*) Marshal.AllocHGlobal (sizeof (ES_TypeInfo));
            *unknType = new ES_TypeInfo (ES_TypeTag.UNKNOWN, ES_AccessModifier.Public, ArrayPointer<byte>.Null, unknTypeId, unknTypeId);
            typeUnknownValue = unknType;
        }

        #endregion

        #region ================== Static methods

        public static EchelonScriptEnvironment CreateEnvironment (out Builder builder) {
            var ret = new EchelonScriptEnvironment ();

            builder = new Builder (ret);

            return ret;
        }

        #endregion

        #region ================== Instance methods

        public ES_TypeInfo* GetFullyQualifiedType (ArrayPointer<byte> fullyQualifiedName) {
            ReadOnlySpan<byte> namespaceSep = stackalloc byte [2] { (byte) ':', (byte) ':' };
            int namespaceSepPos = fullyQualifiedName.Span.IndexOf (namespaceSep);

            Debug.Assert (namespaceSepPos > -1, "The specified name must be fully qualified.");

            var namespaceName = IdPool.GetIdentifier (fullyQualifiedName.Span.Slice (0, namespaceSepPos));
            var typeName = IdPool.GetIdentifier (fullyQualifiedName.Span.Slice (namespaceSepPos + namespaceSep.Length));

            if (!Namespaces.TryGetValue (namespaceName, out var namespaceData))
                return null;

            var namespaceTypes = namespaceData.TypeSpan;

            foreach (var typePtr in namespaceTypes) {
                var type = typePtr.Address;

                if (type->TypeName.Equals (typeName))
                    return type;
            }

            return null;
        }

        #endregion

        #region ================== IDisposable support

        private bool disposedValue = false;

        ~EchelonScriptEnvironment () {
            if (!disposedValue)
                DoDispose ();
        }

        protected virtual void DoDispose () {
            if (!disposedValue) {
                foreach (var kvp in namespacesDict)
                    kvp.Value.Dispose ();

                foreach (var type in typesList)
                    Marshal.FreeHGlobal (type);

                foreach (var mem in allocatedMemory)
                    Marshal.FreeHGlobal (mem);

                namespacesDict.Clear ();
                IdPool?.Dispose ();
                typesList?.Dispose ();
                allocatedMemory?.Dispose ();

                disposedValue = true;
            }
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }
}
