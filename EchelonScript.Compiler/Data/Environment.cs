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
using System.Diagnostics.CodeAnalysis;
using ChronosLib.Pooled;
using ChronosLib.Unmanaged;
using Collections.Pooled;
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.Utilities;
using EchelonScript.Compiler.Backends;
using EchelonScript.Compiler.Frontend;

namespace EchelonScript.Compiler.Data;

public unsafe sealed class ES_NamespaceData {
    internal sealed class Builder : IDisposable {
        #region ================== Instance properties

        public ES_NamespaceData NamespaceData { get; }
        public ES_Identifier NamespaceName => NamespaceData.namespaceName;

        public List<Pointer<ES_TypeInfo>> Types => NamespaceData.types;
        public Dictionary<ES_Identifier, Pointer<ES_FunctionData>> Functions => NamespaceData.functions;

        #endregion

        internal Builder ([DisallowNull] ES_NamespaceData nm) => NamespaceData = nm;

        #region ================== Instance methods

        private void CheckDisposed () {
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

        private void DoDispose () {
            if (disposedValue)
                return;

            disposedValue = true;
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }

    #region ================== Instance fields

    private ES_Identifier namespaceName;
    private Dictionary<ES_Identifier, Pointer<ES_FunctionData>> functions;

    private List<Pointer<ES_TypeInfo>> types;

    #endregion

    #region ================== Instance properties

    public ES_Identifier NamespaceName => namespaceName;

    public IReadOnlyDictionary<ES_Identifier, Pointer<ES_FunctionData>> Functions => functions;

    public IReadOnlyList<Pointer<ES_TypeInfo>> Types => types;

    #endregion

    public ES_NamespaceData (ES_Identifier name) {
        namespaceName = name;

        types = new List<Pointer<ES_TypeInfo>> ();
        functions = new Dictionary<ES_Identifier, Pointer<ES_FunctionData>> ();
    }
}

public unsafe sealed class EchelonScriptEnvironment : IDisposable {
    public sealed class Builder : IDisposable {
        #region ================== Instance fields

        private EchelonScriptEnvironment environment;

        #endregion

        #region ================== Instance properties

        internal PooledDictionary<ES_Identifier, ES_NamespaceData.Builder> NamespaceBuilders { get; private set; }

        public IMemoryManager MemoryManager => environment.memManager;

        internal ES_TypeInfo* TypeUnknown {
            get => environment.typeUnknown;
            set => environment.typeUnknown = value;
        }

        internal ES_TypeInfo* TypeNull {
            get => environment.typeNull;
            set => environment.typeNull = value;
        }

        internal ES_TypeInfo* TypeVoid {
            get => environment.typeVoid;
            set => environment.typeVoid = value;
        }

        internal ES_TypeInfo* TypeBool {
            get => environment.typeBool;
            set => environment.typeBool = value;
        }

        internal ES_TypeInfo* TypeFloat32 {
            get => environment.typeFloat32;
            set => environment.typeFloat32 = value;
        }

        internal ES_TypeInfo* TypeFloat64 {
            get => environment.typeFloat64;
            set => environment.typeFloat64 = value;
        }

        public IBackendData? BackendData {
            get => environment.backendData;
            set => environment.backendData = value;
        }

        public ArrayPointer<nint> ReferenceTypeRefList { get; private init; }

        #endregion

        internal Builder (EchelonScriptEnvironment env) {
            environment = env;

            NamespaceBuilders = new ();

            ReferenceTypeRefList = MemoryManager.GetArrayAligned<nint> (1, sizeof (nint));
            ReferenceTypeRefList.Span [0] = 0;
        }

        #region ================== Instance methods

        /*internal ES_NamespaceData.Builder GetOrCreateNamespace (ES_Identifier namePtr) {
            CheckDisposed ();

            if (NamespaceBuilders.TryGetValue (namePtr, out var builder))
                return builder;

            var namespaceData = new ES_NamespaceData (namePtr);
            environment.namespacesDict.Add (namePtr, namespaceData);

            builder = new ES_NamespaceData.Builder (namespaceData);
            NamespaceBuilders.Add (namePtr, builder);

            return builder;
        }*/

        private void CheckDisposed () {
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

        private void DoDispose () {
            if (disposedValue)
                return;

            foreach (var builderKVP in NamespaceBuilders)
                builderKVP.Value.Dispose ();

            NamespaceBuilders?.Dispose ();
            NamespaceBuilders = null!;

            disposedValue = true;
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }

    #region ================== Instance fields

    private IMemoryManager memManager;
    private Dictionary<ES_Utf8String, ES_NamespaceData> namespacesDict;

    private IBackendData? backendData;

    private ES_TypeInfo* typeUnknown;
    private ES_TypeInfo* typeVoid;
    private ES_TypeInfo* typeNull;
    private ES_TypeInfo* typeBool;
    private ES_TypeInfo* typeFloat32;
    private ES_TypeInfo* typeFloat64;

    #endregion

    #region ================== Instance properties

    public ES_IdentifierPool IdPool { get; private set; }

    public IReadOnlyDictionary<ES_Utf8String, ES_NamespaceData> Namespaces => namespacesDict;

    public ES_Utf8String GlobalsNamespace { get; private set; }
    public ES_Utf8String GeneratedNamespace { get; private set; }

    public ES_TypeInfo* TypeUnknown => typeUnknown;
    public ES_TypeInfo* TypeVoid => typeVoid;
    public ES_TypeInfo* TypeNull => typeNull;
    public ES_TypeInfo* TypeBool => typeBool;
    public ES_TypeInfo* TypeFloat32 => typeFloat32;
    public ES_TypeInfo* TypeFloat64 => typeFloat64;

    #endregion

    private EchelonScriptEnvironment (ES_IdentifierPool idPool) {
        memManager = new BasicMemoryManager ();

        namespacesDict = new ();
        IdPool = idPool;

        backendData = null;

        // FIXME:
        //GlobalsNamespace = IdPool.GetIdentifier (ES_Constants.GlobalsNamespace);
        //GeneratedNamespace = IdPool.GetIdentifier (ES_Constants.GeneratedNamespace);
    }

    #region ================== Static methods

    internal static EchelonScriptEnvironment CreateEnvironment (ES_IdentifierPool idPool, out Builder builder) {
        var ret = new EchelonScriptEnvironment (idPool);

        builder = new Builder (ret);

        return ret;
    }

    #endregion

    #region ================== Instance methods

#if false
    private ES_Identifier GetFunctionTypeName (ES_TypeInfo* returnType, ReadOnlySpan<ES_FunctionPrototypeArg> args) {
        Debug.Assert (returnType != null);

        var charList = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            charList.AddRange ("@FuncType<");

            // Add the return type.
            charList.AddRange ("ret@");
            GetNiceTypeName (ref charList, returnType, true);

            charList.AddRange (", ");

            // Add the arguments.
            var firstArg = true;
            foreach (var arg in args) {
                Debug.Assert (arg.ValueType != null);

                if (!firstArg)
                    charList.AddRange (", ");
                else
                    firstArg = false;

                charList.AddRange ("arg ");
                switch (arg.ArgType) {
                    case ES_ArgumentType.Normal: charList.AddRange ("normal@"); break;
                    case ES_ArgumentType.In: charList.AddRange ("in@"); break;
                    case ES_ArgumentType.Out: charList.AddRange ("out@"); break;
                    case ES_ArgumentType.Ref: charList.AddRange ("ref@"); break;
                }

                GetNiceTypeName (ref charList, arg.ValueType, true);
            }

            charList.AddRange (">");

            return IdPool.GetIdentifier (charList.Span);
        } finally {
            charList.Dispose ();
        }
    }

    public ES_TypeInfo* GetIntType (ES_IntSize size, bool unsigned) {
        var intName = IdPool.GetIdentifier (ES_PrimitiveTypeConsts.GetIntName (size, unsigned));
        var intType = GetFullyQualifiedType (GlobalsNamespace, intName);
        Debug.Assert (intType is not null && intType->TypeTag == ES_TypeTag.Int);

        return intType;
    }

    public ES_TypeInfo* GetArrayIndexType () => GetIntType (ES_IntSize.Int32, false);

    public ES_TypeInfo* GetFullyQualifiedType (ES_Utf8String namespaceName, ES_Utf8String typeName)
        => GetFullyQualifiedType (new ES_FullyQualifiedName (namespaceName, typeName));

    public ES_TypeInfo* GetFullyQualifiedType (ES_FullyQualifiedName fullyQualifiedName) {
        if (!Namespaces.TryGetValue (fullyQualifiedName.NamespaceName, out var namespaceData))
            return null;

        var typesList = namespaceData.Types;

        foreach (var typePtr in typesList) {
            var type = typePtr.Address;

            if (type->Name.TypeName.Equals (fullyQualifiedName.TypeName))
                return type;
        }

        return null;
    }

    public T? GetFunctionDelegate<T> (ES_Identifier namespaceName, ES_Identifier functionName)
        where T : Delegate {
        if (backendData is null)
            return null;

        foreach (var nmKVP in namespacesDict) {
            if (!nmKVP.Key.Equals (namespaceName))
                continue;

            foreach (var funcDataKVP in nmKVP.Value.Functions) {
                if (!funcDataKVP.Key.Equals (functionName))
                    continue;

                return backendData.GetFunctionDelegate<T> (funcDataKVP.Value);
            }
        }

        return null;
    }

    public T? GetFunctionDelegate<T> (ES_FunctionData* funcData) where T : Delegate
        => backendData?.GetFunctionDelegate<T> (funcData);

    #region Nice names

    public void GetNiceTypeName (ref StructPooledList<char> charList, ES_TypeInfo* type, bool fullyQualified)
        => ES_TypeInfo.GetNiceTypeName (ref charList, type, fullyQualified, GlobalsNamespace, GeneratedNamespace);

    public string GetNiceTypeNameString (ES_TypeInfo* type, bool fullyQualified) {
        var charList = new StructPooledList<char> (CL_ClearMode.Auto);
        try {
            GetNiceTypeName (ref charList, type, fullyQualified);
            return charList.Span.GetPooledString ();
        } finally {
            charList.Dispose ();
        }
    }

    #endregion
#endif

    #endregion

    #region ================== IDisposable support

    private bool disposedValue = false;

    ~EchelonScriptEnvironment () {
        if (!disposedValue)
            DoDispose ();
    }

    private void DoDispose () {
        if (disposedValue)
            return;

        backendData?.Dispose ();
        namespacesDict.Clear ();
        memManager?.Dispose ();

        disposedValue = true;
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion
}
