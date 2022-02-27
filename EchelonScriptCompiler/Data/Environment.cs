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
        public ArrayPointer<byte> NamespaceName => NamespaceData.namespaceName;

        public PooledDictionary<ArrayPointer<byte>, ES_ClassData.Builder> ClassBuilders { get; protected set; }
        public PooledDictionary<ArrayPointer<byte>, ES_StructData.Builder> StructBuilders { get; protected set; }
        public PooledDictionary<ArrayPointer<byte>, ES_EnumData.Builder> EnumBuilders { get; protected set; }
        public Dictionary<ArrayPointer<byte>, Pointer<ES_FunctionData>> Functions => NamespaceData.functions;

        #endregion

        #region ================== Constructors

        public Builder (
            [DisallowNull] EchelonScriptEnvironment env, [DisallowNull] EchelonScriptEnvironment.Builder envBuilder,
            [DisallowNull] ES_NamespaceData nm
        ) {
            this.env = env;
            this.envBuilder = envBuilder;

            NamespaceData = nm;

            ClassBuilders = new PooledDictionary<ArrayPointer<byte>, ES_ClassData.Builder> ();
            StructBuilders = new PooledDictionary<ArrayPointer<byte>, ES_StructData.Builder> ();
            EnumBuilders = new PooledDictionary<ArrayPointer<byte>, ES_EnumData.Builder> ();
        }

        #endregion

        #region ================== Instance methods

        public ES_TypeTag? CheckTypeExists (ArrayPointer<byte> name, ES_TypeTag? ignoredType) {
            if (ignoredType != ES_TypeTag.Class && ClassBuilders.TryGetValue (name, out var _))
                return ES_TypeTag.Class;

            if (ignoredType != ES_TypeTag.Struct && StructBuilders.TryGetValue (name, out var _))
                return ES_TypeTag.Struct;

            if (ignoredType != ES_TypeTag.Enum && EnumBuilders.TryGetValue (name, out var _))
                return ES_TypeTag.Enum;

            if (ignoredType != ES_TypeTag.Function && NamespaceData.functions.TryGetValue (name, out var _))
                return ES_TypeTag.Function;

            return null;
        }

        public ES_ClassData.Builder GetOrCreateClass (ES_AccessModifier accessMod,
            ArrayPointer<byte> name, ArrayPointer<byte> sourceUnit
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
            ArrayPointer<byte> name, ArrayPointer<byte> sourceUnit
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
            ArrayPointer<byte> name, ArrayPointer<byte> sourceUnit
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
    protected ArrayPointer<byte> namespaceName;
    protected Dictionary<ArrayPointer<byte>, Pointer<ES_FunctionData>> functions;

    protected List<Pointer<ES_TypeInfo>> types;

    #endregion

    #region ================== Instance properties

    public ArrayPointer<byte> NamespaceName => namespaceName;

    public string NamespaceNameString => StringPool.Shared.GetOrAdd (namespaceName.Span, ES_Encodings.Identifier);

    public IReadOnlyDictionary<ArrayPointer<byte>, Pointer<ES_FunctionData>> Functions => functions;

    public List<Pointer<ES_TypeInfo>> Types => types;

    #endregion

    #region ================== Constructors

    public ES_NamespaceData (EchelonScriptEnvironment env, ArrayPointer<byte> name) {
        environment = env;
        namespaceName = name;

        types = new List<Pointer<ES_TypeInfo>> ();
        functions = new Dictionary<ArrayPointer<byte>, Pointer<ES_FunctionData>> ();
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

            NamespaceBuilders = new PooledDictionary<ArrayPointer<byte>, ES_NamespaceData.Builder> ();
            PointerAstMap = new Dictionary<IntPtr, ES_AstNode> ();

            ReferenceTypeRefList = MemoryManager.GetArrayAligned<nint> (1, sizeof (nint));
            ReferenceTypeRefList.Span [0] = 0;
        }

        #endregion

        #region ================== Instance methods

        public ES_NamespaceData.Builder GetOrCreateNamespace (ArrayPointer<byte> namePtr) {
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
            var fqn = new ES_FullyQualifiedName (environment.GeneratedTypesNamespace, name);

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
                    fqn, ArrayPointer<byte>.Null
                );

                var namespaceData = GetOrCreateNamespace (environment.GeneratedTypesNamespace).NamespaceData;
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
                case ES_TypeTag.Function:
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

        private PooledArray<byte> GetGeneratedTypeName (ES_TypeInfo* baseType, int prefixLen = 0, int suffixLen = 0) {
            // Format sample: "NamespaceName__TypeName"
            var baseFQN = baseType->Name;

            var noNamespace = (
                baseFQN.NamespaceName.Equals (environment.GeneratedTypesNamespace) ||
                baseFQN.NamespaceName.Equals (environment.GlobalTypesNamespace)
            );

            var idLen = prefixLen + baseFQN.NamespaceName.Length + 2 + baseFQN.TypeName.Length + suffixLen;
            if (noNamespace)
                idLen = prefixLen + baseFQN.TypeName.Length + suffixLen;

            var idBase = PooledArray<byte>.GetArray (idLen);
            var idBaseSpan = idBase.Span [prefixLen..^suffixLen];

            var idx = 0;

            if (!noNamespace) {
                baseFQN.NamespaceName.Span.CopyTo (idBaseSpan);
                idx += baseFQN.NamespaceName.Length;

                idBaseSpan.Slice (idx, 2).Fill ((byte) '_');
                idx += 2;
            }

            baseFQN.TypeName.Span.CopyTo (idBaseSpan [idx..]);

            return idBase;
        }

        public ES_TypeInfo* CreateReferenceType (ES_TypeInfo* baseType) {
            // Format sample: "@generated::NamespaceName__TypeName&"
            using var idBase = GetGeneratedTypeName (baseType, suffixLen: 1);
            idBase.Span [^1] = (byte) '&';

            var refId = environment.IdPool.GetIdentifier (idBase);
            var refFQN = new ES_FullyQualifiedName (environment.GeneratedTypesNamespace, refId);

            var refType = environment.GetFullyQualifiedType (refFQN);

            if (refType is not null) {
                Debug.Assert (refType->TypeTag == ES_TypeTag.Reference);
                return refType;
            }

            refType = (ES_TypeInfo*) environment.memManager.GetMemory<ES_ReferenceData> ();
            *((ES_ReferenceData*) refType) = new ES_ReferenceData (refFQN, baseType);

            GetOrCreateNamespace (environment.GeneratedTypesNamespace).NamespaceData.Types.Add (refType);

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

            var prefixName = immutable ? "immutable" : "const";
            var prefixBytesLen = enc.GetByteCount (prefixName);

            using var idBase = GetGeneratedTypeName (baseType, prefixLen: prefixBytesLen + 1, suffixLen: 1);
            enc.GetBytes (prefixName, idBase.Span [..prefixBytesLen]);
            idBase.Span [prefixBytesLen] = (byte) '(';
            idBase.Span [^1] = (byte) ')';

            var constId = environment.IdPool.GetIdentifier (idBase);
            var constFQN = new ES_FullyQualifiedName (environment.GeneratedTypesNamespace, constId);

            var constType = environment.GetFullyQualifiedType (constFQN);

            if (constType is not null) {
                Debug.Assert (constType->TypeTag == (immutable ? ES_TypeTag.Immutable : ES_TypeTag.Const));
                return constType;
            }

            constType = (ES_TypeInfo*) environment.memManager.GetMemory<ES_ConstData> ();
            *((ES_ConstData*) constType) = new ES_ConstData (constFQN, baseType, immutable);

            GetOrCreateNamespace (environment.GeneratedTypesNamespace).NamespaceData.Types.Add (constType);

            return constType;
        }

        public ES_TypeInfo* CreateConstType (ES_TypeInfo* baseType) => CreateConstType (baseType, false);

        public ES_TypeInfo* CreateImmutableType (ES_TypeInfo* baseType) => CreateConstType (baseType, true);

        public ES_TypeInfo* CreateArrayType (ES_TypeInfo* elementType, int dimensionCount) {
            // Format sample: "@generated::NamespaceName__TypeName[,,]"
            var suffixLen = 2 + (dimensionCount - 1);
            using var idBase = GetGeneratedTypeName (elementType, suffixLen: suffixLen);
            var idBaseSpan = idBase.Span;

            var idx = idBase.RealLength - suffixLen;

            idBaseSpan [idx++] = (byte) '[';
            idBaseSpan.Slice (idx, dimensionCount - 1).Fill ((byte) ',');
            idx += dimensionCount - 1;
            idBaseSpan [idx++] = (byte) ']';

            var arrId = environment.IdPool.GetIdentifier (idBase);
            var arrFQN = new ES_FullyQualifiedName (environment.GeneratedTypesNamespace, arrId);

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
                ArrayPointer<byte>.Null,
                ES_AccessModifier.Public,
                0,
                0,
                typeIndex
            ));

            memberVars.Add (new ES_MemberData_Variable (
                environment.IdPool.GetIdentifier ("Rank"),
                ArrayPointer<byte>.Null,
                ES_AccessModifier.Public,
                0,
                0,
                environment.GetIntType (ES_IntSize.Int8, true)
            ));

            for (var i = 0; i < dimensionCount; i++) {
                memberVars.Add (new ES_MemberData_Variable (
                    environment.IdPool.GetIdentifier ($"LengthD{i}"),
                    ArrayPointer<byte>.Null,
                    ES_AccessModifier.Public,
                    0,
                    0,
                    typeIndex
                ));
            }

            var membersBuilder = new ES_TypeMembers.Builder (&arrType->MembersList, arrType);

            GenerateMembersList (membersBuilder, memberVars.Span, null);

            GetOrCreateNamespace (environment.GeneratedTypesNamespace).NamespaceData.Types.Add (arrType);

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
    protected Dictionary<ArrayPointer<byte>, ES_NamespaceData> namespacesDict;

    protected IBackendData? backendData;

    protected ES_TypeInfo* typeUnknownValue;
    protected ES_TypeInfo* typeVoid;
    protected ES_TypeInfo* typeNull;
    protected ES_TypeInfo* typeBool;
    protected ES_TypeInfo* typeFloat32;
    protected ES_TypeInfo* typeFloat64;

    #endregion

    #region ================== Instance properties

    public UnmanagedIdentifierPool IdPool { get; protected set; }

    public IReadOnlyDictionary<ArrayPointer<byte>, ES_NamespaceData> Namespaces => namespacesDict;

    public ArrayPointer<byte> GlobalTypesNamespace { get; private set; }
    public ArrayPointer<byte> GeneratedTypesNamespace { get; private set; }

    public ES_TypeInfo* TypeUnknownValue => typeUnknownValue;
    public ES_TypeInfo* TypeVoid => typeVoid;
    public ES_TypeInfo* TypeNull => typeNull;
    public ES_TypeInfo* TypeBool => typeBool;
    public ES_TypeInfo* TypeFloat32 => typeFloat32;
    public ES_TypeInfo* TypeFloat64 => typeFloat64;

    #endregion

    #region ================== Constructors

    protected EchelonScriptEnvironment () {
        namespacesDict = new Dictionary<ArrayPointer<byte>, ES_NamespaceData> ();
        IdPool = new UnmanagedIdentifierPool ();

        memManager = new BasicMemoryManager ();

        backendData = null;

        GlobalTypesNamespace = IdPool.GetIdentifier ("@globals");
        GeneratedTypesNamespace = IdPool.GetIdentifier ("@generated");

        var unknTypeId = IdPool.GetIdentifier ("#UNKNOWN_TYPE");
        var unknType = memManager.GetMemory<ES_TypeInfo> ();
        *unknType = new ES_TypeInfo (
            ES_TypeTag.UNKNOWN, ES_AccessModifier.Public, ES_TypeFlag.None, ArrayPointer<byte>.Null,
            new ES_FullyQualifiedName (ArrayPointer<byte>.Null, unknTypeId)
        );
        typeUnknownValue = unknType;

        var nullTypeId = IdPool.GetIdentifier ("#NULL");
        var nullType = memManager.GetMemory<ES_TypeInfo> ();
        *nullType = new ES_TypeInfo (
            ES_TypeTag.Null, ES_AccessModifier.Public, ES_TypeFlag.None, ArrayPointer<byte>.Null,
            new ES_FullyQualifiedName (ArrayPointer<byte>.Null, nullTypeId)
        );
        typeNull = nullType;
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

    protected ArrayPointer<byte> GetFunctionTypeName (ES_TypeInfo* returnType, ReadOnlySpan<ES_FunctionPrototypeArgData> args) {
        Debug.Assert (returnType != null);

        using var charList = new StructPooledList<char> (CL_ClearMode.Auto);

        charList.AddRange ("@FuncType<");

        // Add the return type.
        charList.AddRange ("ret@");
        foreach (var c in returnType->Name.NamespaceName.Span)
            charList.Add ((char) c);

        charList.AddRange ("::");
        foreach (var c in returnType->Name.TypeName.Span)
            charList.Add ((char) c);

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

            foreach (var c in arg.ValueType->Name.NamespaceName.Span)
                charList.Add ((char) c);
            charList.AddRange ("::");
            foreach (var c in arg.ValueType->Name.TypeName.Span)
                charList.Add ((char) c);
        }

        charList.AddRange (">");

        return IdPool.GetIdentifier (charList.Span);
    }

    public ES_TypeInfo* GetIntType (ES_IntSize size, bool unsigned) {
        var intName = IdPool.GetIdentifier (ES_PrimitiveTypes.GetIntName (size, unsigned));
        var intType = GetFullyQualifiedType (GlobalTypesNamespace, intName);
        Debug.Assert (intType is not null && intType->TypeTag == ES_TypeTag.Int);

        return intType;
    }

    public ES_TypeInfo* GetArrayIndexType () => GetIntType (ES_IntSize.Int32, false);

    public ES_TypeInfo* GetFullyQualifiedType (ArrayPointer<byte> namespaceName, ArrayPointer<byte> typeName)
        => GetFullyQualifiedType (new ES_FullyQualifiedName (namespaceName, typeName));

    public ES_TypeInfo* GetFullyQualifiedType (ES_FullyQualifiedName fullyQualifiedName) {
        var namespaceName = IdPool.GetIdentifier (fullyQualifiedName.NamespaceName.Span);
        if (!Namespaces.TryGetValue (namespaceName, out var namespaceData))
            return null;

        var typesList = namespaceData.Types;

        var typeName = IdPool.GetIdentifier (fullyQualifiedName.TypeName.Span);
        foreach (var typePtr in typesList) {
            var type = typePtr.Address;

            if (type->Name.TypeName.Equals (typeName))
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

            chars.AddRange (arg.ValueType->Name.GetNameAsTypeString ());
        }

        chars.Add (')');

        return StringPool.Shared.GetOrAdd (chars.Span);
    }

    public T? GetFunctionDelegate<T> (ArrayPointer<byte> namespaceName, ArrayPointer<byte> functionName)
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
        IdPool?.Dispose ();
        memManager?.Dispose ();

        disposedValue = true;
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion
}
