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
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScriptCommon;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.Backends;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Frontend;

namespace EchelonScriptCompiler.Data;

public unsafe class ES_NamespaceData {
    public class Builder : IDisposable {
        #region ================== Instance fields

        private EchelonScriptEnvironment env;
        private EchelonScriptEnvironment.Builder envBuilder;

        #endregion

        #region ================== Instance properties

        public ES_NamespaceData NamespaceData { get; }
        public ES_Identifier NamespaceName => NamespaceData.namespaceName;

        public PooledDictionary<ES_Identifier, ES_ClassData.Builder> ClassBuilders { get; protected set; }
        public PooledDictionary<ES_Identifier, ES_StructData.Builder> StructBuilders { get; protected set; }
        public PooledDictionary<ES_Identifier, ES_EnumData.Builder> EnumBuilders { get; protected set; }
        public Dictionary<ES_Identifier, Pointer<ES_FunctionData>> Functions => NamespaceData.functions;

        #endregion

        #region ================== Constructors

        public Builder (
            [DisallowNull] EchelonScriptEnvironment env, [DisallowNull] EchelonScriptEnvironment.Builder envBuilder,
            [DisallowNull] ES_NamespaceData nm
        ) {
            this.env = env;
            this.envBuilder = envBuilder;

            NamespaceData = nm;

            ClassBuilders = new ();
            StructBuilders = new ();
            EnumBuilders = new ();
        }

        #endregion

        #region ================== Instance methods

        public ES_TypeTag? CheckTypeExists (ES_Identifier name, ES_TypeTag? ignoredType) {
            if (ignoredType != ES_TypeTag.Class && ClassBuilders.TryGetValue (name, out var _))
                return ES_TypeTag.Class;

            if (ignoredType != ES_TypeTag.Struct && StructBuilders.TryGetValue (name, out var _))
                return ES_TypeTag.Struct;

            if (ignoredType != ES_TypeTag.Enum && EnumBuilders.TryGetValue (name, out var _))
                return ES_TypeTag.Enum;

            if (ignoredType != ES_TypeTag.FuncPrototype && NamespaceData.functions.TryGetValue (name, out var _))
                return ES_TypeTag.FuncPrototype;

            return null;
        }

        public ES_ClassData.Builder GetOrCreateClass (ES_AccessModifier accessMod,
            ES_Identifier name, ES_Identifier sourceUnit
        ) {
            CheckDisposed ();

            if (CheckTypeExists (name, ES_TypeTag.Class) != null)
                throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

            if (ClassBuilders.TryGetValue (name, out var builder))
                return builder;

            var classDataPtr = envBuilder.MemoryManager.GetMemory<ES_ClassData> ();

            builder = new ES_ClassData.Builder (classDataPtr, accessMod, new ES_FullyQualifiedName (NamespaceName, name), sourceUnit);
            ClassBuilders.Add (name, builder);
            NamespaceData.types.Add (&classDataPtr->TypeInfo);

            var unknType = NamespaceData.environment.TypeUnknownValue;

            builder.BaseClass = (ES_ClassData*) unknType;
            builder.InterfacesList = ArrayPointer<Pointer<ES_InterfaceData>>.Null;

            return builder;
        }

        public ES_StructData.Builder GetOrCreateStruct (ES_AccessModifier accessMod,
            ES_Identifier name, ES_Identifier sourceUnit
        ) {
            CheckDisposed ();

            if (CheckTypeExists (name, ES_TypeTag.Struct) != null)
                throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

            if (StructBuilders.TryGetValue (name, out var builder))
                return builder;

            var structDataPtr = envBuilder.MemoryManager.GetMemory<ES_StructData> ();

            builder = new ES_StructData.Builder (structDataPtr, accessMod, new ES_FullyQualifiedName (NamespaceName, name), sourceUnit);
            StructBuilders.Add (name, builder);
            NamespaceData.types.Add (&structDataPtr->TypeInfo);

            return builder;
        }

        public ES_EnumData.Builder GetOrCreateEnum (ES_AccessModifier accessMod,
            ES_Identifier name, ES_Identifier sourceUnit
        ) {
            CheckDisposed ();

            if (CheckTypeExists (name, ES_TypeTag.Enum) != null)
                throw new CompilationException (ES_FrontendErrors.ClashingTypeExists);

            if (EnumBuilders.TryGetValue (name, out var builder))
                return builder;

            var enumDataPtr = envBuilder.MemoryManager.GetMemory<ES_EnumData> ();

            builder = new ES_EnumData.Builder (enumDataPtr, accessMod, new ES_FullyQualifiedName (NamespaceName, name), sourceUnit);
            EnumBuilders.Add (name, builder);
            NamespaceData.types.Add (&enumDataPtr->TypeInfo);

            return builder;
        }

        public ES_ClassData.Builder? GetClass (ES_Identifier name) {
            CheckDisposed ();

            if (ClassBuilders.TryGetValue (name, out var builder))
                return builder;

            return null;
        }

        public ES_StructData.Builder? GetStruct (ES_Identifier name) {
            CheckDisposed ();

            if (StructBuilders.TryGetValue (name, out var builder))
                return builder;

            return null;
        }

        public ES_EnumData.Builder? GetEnum (ES_Identifier name) {
            CheckDisposed ();

            if (EnumBuilders.TryGetValue (name, out var builder))
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
            if (disposedValue)
                return;

            ClassBuilders?.Dispose ();
            StructBuilders?.Dispose ();
            EnumBuilders?.Dispose ();

            disposedValue = true;
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }

    #region ================== Instance fields

    protected EchelonScriptEnvironment environment;
    protected ES_Identifier namespaceName;
    protected Dictionary<ES_Identifier, Pointer<ES_FunctionData>> functions;

    protected List<Pointer<ES_TypeInfo>> types;

    #endregion

    #region ================== Instance properties

    public ES_Identifier NamespaceName => namespaceName;

    public IReadOnlyDictionary<ES_Identifier, Pointer<ES_FunctionData>> Functions => functions;

    public List<Pointer<ES_TypeInfo>> Types => types;

    #endregion

    #region ================== Constructors

    public ES_NamespaceData (EchelonScriptEnvironment env, ES_Identifier name) {
        environment = env;
        namespaceName = name;

        types = new List<Pointer<ES_TypeInfo>> ();
        functions = new Dictionary<ES_Identifier, Pointer<ES_FunctionData>> ();
    }

    #endregion
}

public unsafe class EchelonScriptEnvironment : IDisposable {
    public class Builder : IDisposable {
        #region ================== Instance fields

        private EchelonScriptEnvironment environment;

        #endregion

        #region ================== Instance properties

        public PooledDictionary<ES_Identifier, ES_NamespaceData.Builder> NamespaceBuilders { get; protected set; }

        public Dictionary<IntPtr, ES_AstNode> PointerAstMap { get; protected set; }

        public IMemoryManager MemoryManager => environment.memManager;

        public ES_TypeInfo* TypeVoid {
            get => environment.typeVoid;
            set => environment.typeVoid = value;
        }

        public ES_TypeInfo* TypeBool {
            get => environment.typeBool;
            set => environment.typeBool = value;
        }

        public ES_TypeInfo* TypeFloat32 {
            get => environment.typeFloat32;
            set => environment.typeFloat32 = value;
        }

        public ES_TypeInfo* TypeFloat64 {
            get => environment.typeFloat64;
            set => environment.typeFloat64 = value;
        }

        public IBackendData? BackendData {
            get => environment.backendData;
            set => environment.backendData = value;
        }

        public ArrayPointer<nint> ReferenceTypeRefList { get; private init; }

        #endregion

        #region ================== Constructors

        internal Builder (EchelonScriptEnvironment env) {
            environment = env;

            NamespaceBuilders = new ();
            PointerAstMap = new ();

            ReferenceTypeRefList = MemoryManager.GetArrayAligned<nint> (1, sizeof (nint));
            ReferenceTypeRefList.Span [0] = 0;
        }

        #endregion

        #region ================== Instance methods

        public ES_NamespaceData.Builder GetOrCreateNamespace (ES_Identifier namePtr) {
            CheckDisposed ();

            if (NamespaceBuilders.TryGetValue (namePtr, out var builder))
                return builder;

            var namespaceData = new ES_NamespaceData (environment, namePtr);
            environment.namespacesDict.Add (namePtr, namespaceData);

            builder = new ES_NamespaceData.Builder (environment, this, namespaceData);
            NamespaceBuilders.Add (namePtr, builder);

            return builder;
        }

        public ES_FunctionPrototypeData* GetOrAddFunctionType (ES_TypeInfo* returnType, ReadOnlySpan<ES_FunctionPrototypeArgData> args, bool doAdd) {
            var name = environment.GetFunctionTypeName (returnType, args);
            var fqn = new ES_FullyQualifiedName (environment.GeneratedNamespace, name);

            var funcType = (ES_FunctionPrototypeData*) environment.GetFullyQualifiedType (fqn);

            if (funcType == null && doAdd) {
                funcType = MemoryManager.GetMemory<ES_FunctionPrototypeData> (1);

                var argsList = ArrayPointer<ES_FunctionPrototypeArgData>.Null;
                if (args.Length > 0) {
                    argsList = MemoryManager.GetArray<ES_FunctionPrototypeArgData> (args.Length);
                    args.CopyTo (argsList.Span);
                }

                *funcType = new ES_FunctionPrototypeData (
                    ES_AccessModifier.Public,
                    returnType, argsList,
                    fqn, ES_Identifier.Empty
                );

                var namespaceData = GetOrCreateNamespace (environment.GeneratedNamespace).NamespaceData;
                namespaceData.Types.Add (&funcType->TypeInfo);
            }

            return funcType;
        }

        public void GenerateMembersList (
            ES_TypeMembers.Builder membersBuilder,
            ReadOnlySpan<ES_MemberData_Variable> varsList,
            ReadOnlySpan<ES_MemberData_Function> funcsList
        ) {
            if (varsList.Length + funcsList.Length == 0) {
                membersBuilder.MembersList = ArrayPointer<Pointer<ES_MemberData>>.Null;
                return;
            }

            var varsSize = varsList.Length * sizeof (ES_MemberData_Variable);
            var funcsSize = funcsList.Length * sizeof (ES_MemberData_Function);

            var memArea = IntPtr.Zero;
            var membersList = MemoryManager.GetArray<Pointer<ES_MemberData>> (varsList.Length + funcsList.Length);

            if (varsSize + funcsSize > 0)
                memArea = MemoryManager.GetMemory (varsSize + funcsSize);

            var varsSpan = new ArrayPointer<ES_MemberData_Variable> ((ES_MemberData_Variable*) memArea, varsList.Length);
            var funcsSpan = new ArrayPointer<ES_MemberData_Function> ((ES_MemberData_Function*) (memArea + varsSize), funcsList.Length);

            // Copy the member data structs to their final memory location and set the pointers to said memory location.
            var membersCount = 0;
            var idx = 0;
            foreach (var memberVar in varsList) {
                varsSpan.Span [idx] = memberVar;
                membersList.Span [membersCount] = (ES_MemberData*) (varsSpan.Elements + idx);

                membersCount++;
                idx++;
            }

            idx = 0;
            foreach (var memberFunc in funcsList) {
                funcsSpan.Span [idx] = memberFunc;
                membersList.Span [idx] = (ES_MemberData*) (funcsSpan.Elements + idx);

                membersCount++;
                idx++;
            }

            membersBuilder.MembersList = membersList;
        }

        #region Derived type creation

        internal ES_TypeInfo* RemoveConstness (ES_TypeInfo* type) => RemoveConstness (type, true);

        private ES_TypeInfo* RemoveConstness (ES_TypeInfo* type, bool immutable) {
            switch (type->TypeTag) {
                case ES_TypeTag.UNKNOWN:
                case ES_TypeTag.Null:
                case ES_TypeTag.Void:
                case ES_TypeTag.Bool:
                case ES_TypeTag.Int:
                case ES_TypeTag.Float:
                case ES_TypeTag.FuncPrototype:
                case ES_TypeTag.Struct:
                case ES_TypeTag.Class:
                case ES_TypeTag.Enum:
                case ES_TypeTag.Interface:
                    return type;

                case ES_TypeTag.Reference: {
                    var refData = (ES_ReferenceData*) type;

                    var cleanPointedType = RemoveConstness (refData->PointedType, immutable);

                    if (refData->PointedType == cleanPointedType)
                        return type;

                    return CreateReferenceType (cleanPointedType);
                }

                case ES_TypeTag.Array: {
                    var arrayType = (ES_ArrayTypeData*) type;

                    var cleanElemType = RemoveConstness (arrayType->ElementType, immutable);

                    if (arrayType->ElementType == cleanElemType)
                        return type;

                    return CreateArrayType (cleanElemType, arrayType->DimensionsCount);
                }

                case ES_TypeTag.Immutable: {
                    if (!immutable)
                        return type;

                    goto case ES_TypeTag.Const;
                }
                case ES_TypeTag.Const: {
                    var constData = (ES_ConstData*) type;

                    return RemoveConstness (constData->InnerType, immutable);
                }

                default:
                    throw new NotImplementedException ("Type not implemented.");
            }
        }

        private void GetGeneratedTypeName (ref StructPooledList<char> charsList, ES_TypeInfo* baseType) {
            // Format sample: "NamespaceName__TypeName"
            var baseFQN = baseType->Name;

            var noNamespace = (
                baseFQN.NamespaceName.Equals (environment.GeneratedNamespace) ||
                baseFQN.NamespaceName.Equals (environment.GlobalsNamespace)
            );

            if (!noNamespace) {
                charsList.AddRange (baseFQN.NamespaceName.GetCharsSpan ());
                charsList.AddRange ("__");
            }

            charsList.AddRange (baseFQN.TypeName.GetCharsSpan ());
        }

        private StructPooledList<char> GetGeneratedTypeName (ES_TypeInfo* baseType) => GetGeneratedTypeName (baseType, "", "");

        private StructPooledList<char> GetGeneratedTypeName (ES_TypeInfo* baseType, ReadOnlySpan<char> prefix, ReadOnlySpan<char> suffix) {
            var charsList = new StructPooledList<char> (CL_ClearMode.Auto);
            try {
                charsList.AddRange (prefix);
                GetGeneratedTypeName (ref charsList, baseType);
                charsList.AddRange (suffix);

                return charsList.Move ();
            } finally {
                charsList.Dispose ();
            }
        }

        public ES_TypeInfo* CreateReferenceType (ES_TypeInfo* baseType) {
            // Format sample: "@generated::NamespaceName__TypeName&"
            using var charsList = GetGeneratedTypeName (baseType, "&", "");

            var refId = environment.IdPool.GetIdentifier (charsList.Span);
            var refFQN = new ES_FullyQualifiedName (environment.GeneratedNamespace, refId);

            var refType = environment.GetFullyQualifiedType (refFQN);

            if (refType is not null) {
                Debug.Assert (refType->TypeTag == ES_TypeTag.Reference);
                return refType;
            }

            refType = (ES_TypeInfo*) environment.memManager.GetMemory<ES_ReferenceData> ();
            *((ES_ReferenceData*) refType) = new ES_ReferenceData (refFQN, baseType);

            GetOrCreateNamespace (environment.GeneratedNamespace).NamespaceData.Types.Add (refType);

            return refType;
        }

        public ES_TypeInfo* CreateNullableType (ES_TypeInfo* baseType) {
            throw new NotImplementedException ("[TODO] Nullables not implemented yet.");
        }

        private ES_TypeInfo* CreateConstType (ES_TypeInfo* baseType, bool immutable) {
            // Format sample: "@generated::const(NamespaceName__TypeName)" or "@generated::immutable(NamespaceName__TypeName)"
            var enc = ES_Encodings.Identifier;

            baseType = RemoveConstness (baseType, immutable);

            Debug.Assert (baseType->TypeTag != ES_TypeTag.Const && baseType->TypeTag != ES_TypeTag.Immutable);

            var prefixName = immutable ? "immutable(" : "const(";
            using var charsList = GetGeneratedTypeName (baseType, prefixName, ")");

            var constId = environment.IdPool.GetIdentifier (charsList.Span);
            var constFQN = new ES_FullyQualifiedName (environment.GeneratedNamespace, constId);

            var constType = environment.GetFullyQualifiedType (constFQN);

            if (constType is not null) {
                Debug.Assert (constType->TypeTag == (immutable ? ES_TypeTag.Immutable : ES_TypeTag.Const));
                return constType;
            }

            constType = (ES_TypeInfo*) environment.memManager.GetMemory<ES_ConstData> ();
            *((ES_ConstData*) constType) = new ES_ConstData (constFQN, baseType, immutable);

            GetOrCreateNamespace (environment.GeneratedNamespace).NamespaceData.Types.Add (constType);

            return constType;
        }

        public ES_TypeInfo* CreateConstType (ES_TypeInfo* baseType) => CreateConstType (baseType, false);

        public ES_TypeInfo* CreateImmutableType (ES_TypeInfo* baseType) => CreateConstType (baseType, true);

        public ES_TypeInfo* CreateArrayType (ES_TypeInfo* elementType, int dimensionCount) {
            // Format sample: "@generated::NamespaceName__TypeName[,,]"
            using var charsList = GetGeneratedTypeName (elementType);

            charsList.Add ('[');
            charsList.Add (',', dimensionCount - 1);
            charsList.Add (']');

            var arrId = environment.IdPool.GetIdentifier (charsList.Span);
            var arrFQN = new ES_FullyQualifiedName (environment.GeneratedNamespace, arrId);

            var arrType = environment.GetFullyQualifiedType (arrFQN);

            if (arrType is not null) {
                Debug.Assert (arrType->TypeTag == ES_TypeTag.Array);
                return arrType;
            }

            arrType = (ES_TypeInfo*) environment.memManager.GetMemory<ES_ArrayTypeData> ();
            *((ES_ArrayTypeData*) arrType) = new ES_ArrayTypeData (arrFQN, elementType, dimensionCount);

            /* Create the array's members. */
            var typeIndex = environment.GetArrayIndexType ();

            using var memberVars = new StructPooledList<ES_MemberData_Variable> (CL_ClearMode.Auto);
            memberVars.Add (new ES_MemberData_Variable (
                environment.IdPool.GetIdentifier (dimensionCount < 2 ? "Length" : "TotalLength"),
                ES_Identifier.Empty,
                ES_AccessModifier.Public,
                0,
                0,
                typeIndex
            ));

            memberVars.Add (new ES_MemberData_Variable (
                environment.IdPool.GetIdentifier ("Rank"),
                ES_Identifier.Empty,
                ES_AccessModifier.Public,
                0,
                0,
                environment.GetIntType (ES_IntSize.Int8, true)
            ));

            for (var i = 0; i < dimensionCount; i++) {
                memberVars.Add (new ES_MemberData_Variable (
                    environment.IdPool.GetIdentifier ($"LengthD{i}"),
                    ES_Identifier.Empty,
                    ES_AccessModifier.Public,
                    0,
                    0,
                    typeIndex
                ));
            }

            var membersBuilder = new ES_TypeMembers.Builder (&arrType->MembersList, arrType);

            GenerateMembersList (membersBuilder, memberVars.Span, null);

            GetOrCreateNamespace (environment.GeneratedNamespace).NamespaceData.Types.Add (arrType);

            return arrType;
        }

        #endregion

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
            if (disposedValue)
                return;

            foreach (var builderKVP in NamespaceBuilders)
                builderKVP.Value.Dispose ();

            NamespaceBuilders?.Dispose ();
            NamespaceBuilders = null!;

            PointerAstMap.Clear ();
            PointerAstMap = null!;

            disposedValue = true;
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }

    #region ================== Instance fields

    protected IMemoryManager memManager;
    protected Dictionary<ES_Identifier, ES_NamespaceData> namespacesDict;

    protected IBackendData? backendData;

    protected ES_TypeInfo* typeUnknownValue;
    protected ES_TypeInfo* typeVoid;
    protected ES_TypeInfo* typeNull;
    protected ES_TypeInfo* typeBool;
    protected ES_TypeInfo* typeFloat32;
    protected ES_TypeInfo* typeFloat64;

    #endregion

    #region ================== Instance properties

    public ES_IdentifierPool IdPool { get; protected set; }

    public IReadOnlyDictionary<ES_Identifier, ES_NamespaceData> Namespaces => namespacesDict;

    public ES_Identifier GlobalsNamespace { get; private set; }
    public ES_Identifier GeneratedNamespace { get; private set; }

    public ES_TypeInfo* TypeUnknownValue => typeUnknownValue;
    public ES_TypeInfo* TypeVoid => typeVoid;
    public ES_TypeInfo* TypeNull => typeNull;
    public ES_TypeInfo* TypeBool => typeBool;
    public ES_TypeInfo* TypeFloat32 => typeFloat32;
    public ES_TypeInfo* TypeFloat64 => typeFloat64;

    #endregion

    #region ================== Constructors

    protected EchelonScriptEnvironment (ES_IdentifierPool idPool) {
        memManager = new BasicMemoryManager ();

        namespacesDict = new ();
        IdPool = idPool;

        backendData = null;

        GlobalsNamespace = IdPool.GetIdentifier (ES_Constants.GlobalsNamespace);
        GeneratedNamespace = IdPool.GetIdentifier (ES_Constants.GeneratedNamespace);

        var unknTypeId = IdPool.GetIdentifier ("#UNKNOWN_TYPE");
        var unknType = memManager.GetMemory<ES_TypeInfo> ();
        *unknType = new ES_TypeInfo (
            ES_TypeTag.UNKNOWN, ES_AccessModifier.Public, ES_TypeFlag.None, ES_Identifier.Empty,
            new ES_FullyQualifiedName (ES_Identifier.Empty, unknTypeId)
        );
        typeUnknownValue = unknType;

        var nullTypeId = IdPool.GetIdentifier ("#NULL");
        var nullType = memManager.GetMemory<ES_TypeInfo> ();
        *nullType = new ES_TypeInfo (
            ES_TypeTag.Null, ES_AccessModifier.Public, ES_TypeFlag.None, ES_Identifier.Empty,
            new ES_FullyQualifiedName (ES_Identifier.Empty, nullTypeId)
        );
        typeNull = nullType;
    }

    #endregion

    #region ================== Static methods

    public static EchelonScriptEnvironment CreateEnvironment (ES_IdentifierPool idPool, out Builder builder) {
        var ret = new EchelonScriptEnvironment (idPool);

        builder = new Builder (ret);

        return ret;
    }

    #endregion

    #region ================== Instance methods

    protected ES_Identifier GetFunctionTypeName (ES_TypeInfo* returnType, ReadOnlySpan<ES_FunctionPrototypeArgData> args) {
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
        var intName = IdPool.GetIdentifier (ES_PrimitiveTypes.GetIntName (size, unsigned));
        var intType = GetFullyQualifiedType (GlobalsNamespace, intName);
        Debug.Assert (intType is not null && intType->TypeTag == ES_TypeTag.Int);

        return intType;
    }

    public ES_TypeInfo* GetArrayIndexType () => GetIntType (ES_IntSize.Int32, false);

    public ES_TypeInfo* GetFullyQualifiedType (ES_Identifier namespaceName, ES_Identifier typeName)
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

    public string GetFunctionSignatureString (ReadOnlySpan<ES_FunctionPrototypeArgData> argsList) {
        using var chars = new StructPooledList<char> (CL_ClearMode.Auto);

        chars.EnsureCapacity (2);

        chars.Add ('(');

        var firstArg = true;
        foreach (var arg in argsList) {
            if (!firstArg)
                chars.AddRange (", ");
            else
                firstArg = false;

            switch (arg.ArgType) {
                case ES_ArgumentType.Normal: break;
                case ES_ArgumentType.In: chars.AddRange ("in "); break;
                case ES_ArgumentType.Out: chars.AddRange ("out "); break;
                case ES_ArgumentType.Ref: chars.AddRange ("ref "); break;
            }

            chars.AddRange (GetNiceTypeNameString (arg.ValueType, true));
        }

        chars.Add (')');

        return StringPool.Shared.GetOrAdd (chars.Span);
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

    #endregion

    #region ================== IDisposable support

    private bool disposedValue = false;

    ~EchelonScriptEnvironment () {
        if (!disposedValue)
            DoDispose ();
    }

    protected virtual void DoDispose () {
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
