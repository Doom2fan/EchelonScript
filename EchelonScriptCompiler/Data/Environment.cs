/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Text;
using ChronosLib.Pooled;
using ChronosLib.Unmanaged;
using Collections.Pooled;
using EchelonScriptCompiler.Backends;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Frontend;
using EchelonScriptCompiler.Utilities;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Data {
    public unsafe class ES_NamespaceData {
        public class Builder : IDisposable {
            #region ================== Instance fields

            private EchelonScriptEnvironment env;
            private EchelonScriptEnvironment.Builder envBuilder;
            private ES_NamespaceData namespaceData;

            #endregion

            #region ================== Instance properties

            public ES_NamespaceData NamespaceData => namespaceData;

            public PooledDictionary<ArrayPointer<byte>, ES_ClassData.Builder> ClassBuilders { get; protected set; }
            public PooledDictionary<ArrayPointer<byte>, ES_StructData.Builder> StructBuilders { get; protected set; }
            public PooledDictionary<ArrayPointer<byte>, ES_EnumData.Builder> EnumBuilders { get; protected set; }
            public Dictionary<ArrayPointer<byte>, Pointer<ES_FunctionData>> Functions => namespaceData.functions;

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

            public Builder (
                [DisallowNull] EchelonScriptEnvironment env, [DisallowNull] EchelonScriptEnvironment.Builder envBuilder,
                [DisallowNull] ES_NamespaceData nm
            ) {
                this.env = env;
                this.envBuilder = envBuilder;

                namespaceData = nm;

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

                if (ignoredType != ES_TypeTag.Function && namespaceData.functions.TryGetValue (name, out var _))
                    return ES_TypeTag.Function;

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

                var classDataPtr = envBuilder.MemoryManager.GetMemory<ES_ClassData> ();

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

                var structDataPtr = envBuilder.MemoryManager.GetMemory<ES_StructData> ();

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

                var enumDataPtr = envBuilder.MemoryManager.GetMemory<ES_EnumData> ();

                builder = new ES_EnumData.Builder (enumDataPtr, accessMod, name, fullyQualifiedName, sourceUnit);
                EnumBuilders.Add (name, builder);

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
                if (!disposedValue) {
                    ClassBuilders?.Dispose ();
                    StructBuilders?.Dispose ();
                    EnumBuilders?.Dispose ();

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
        protected Dictionary<ArrayPointer<byte>, Pointer<ES_FunctionData>> functions;

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

        public IReadOnlyDictionary<ArrayPointer<byte>, Pointer<ES_FunctionData>> Functions => functions;

        public int TypesStartIdx => typesStartIdx;
        public int TypesLength => typesLength;

        public ReadOnlySpan<Pointer<ES_TypeInfo>> TypeSpan
            => environment.TypesList.Span.Slice (typesStartIdx, typesLength);

        #endregion

        #region ================== Constructors

        public ES_NamespaceData (EchelonScriptEnvironment env, ArrayPointer<byte> name) {
            environment = env;
            namespaceName = name;

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

            public PooledList<Pointer<ES_TypeInfo>> TypesList => environment.typesList;

            public ArrayPointer<Pointer<ES_TypeInfo>> BuiltinTypesList {
                get => environment.builtinTypesList;
                set => environment.builtinTypesList = value;
            }

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

            #endregion

            #region ================== Constructors

            internal Builder (EchelonScriptEnvironment env) {
                environment = env;

                NamespaceBuilders = new PooledDictionary<ArrayPointer<byte>, ES_NamespaceData.Builder> ();
                PointerAstMap = new Dictionary<IntPtr, ES_AstNode> ();
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

            #region BinaryOpCompat

            public bool BinaryOpCompat (ES_TypeInfo* lhsType, ES_TypeInfo* rhsType, SimpleBinaryExprType exprType, out ES_TypeInfo* finalType, out bool isConst) {
                finalType = environment.TypeUnknownValue;

                if (lhsType->TypeTag == ES_TypeTag.UNKNOWN || rhsType->TypeTag == ES_TypeTag.UNKNOWN) {
                    isConst = false;
                    return true;
                }

                if (lhsType->TypeTag == ES_TypeTag.Int && rhsType->TypeTag == ES_TypeTag.Int)
                    return BinaryOpCompat_IntInt (lhsType, rhsType, exprType, out finalType, out isConst);

                if (lhsType->TypeTag == ES_TypeTag.Bool && rhsType->TypeTag == ES_TypeTag.Bool)
                    return BinaryOpCompat_BoolBool (lhsType, rhsType, exprType, out finalType, out isConst);

                if (lhsType->TypeTag == ES_TypeTag.Float && rhsType->TypeTag == ES_TypeTag.Float)
                    return BinaryOpCompat_FloatFloat (lhsType, rhsType, exprType, out finalType, out isConst);

                if (lhsType->TypeTag == ES_TypeTag.Float && rhsType->TypeTag == ES_TypeTag.Int)
                    return BinaryOpCompat_FloatInt (lhsType, rhsType, exprType, out finalType, out isConst);

                isConst = false;
                return false;
            }

            private bool BinaryOpCompat_IntInt (ES_TypeInfo* lhsType, ES_TypeInfo* rhsType, SimpleBinaryExprType exprType, out ES_TypeInfo* finalType, out bool isConst) {
                Debug.Assert (lhsType->TypeTag == ES_TypeTag.Int);
                Debug.Assert (rhsType->TypeTag == ES_TypeTag.Int);

                var lhsIntType = (ES_IntTypeData*) lhsType;
                var rhsIntType = (ES_IntTypeData*) rhsType;

                finalType = environment.TypeUnknownValue;

                switch (exprType) {
                    case SimpleBinaryExprType.Power:
                    case SimpleBinaryExprType.Multiply:
                    case SimpleBinaryExprType.Divide:
                    case SimpleBinaryExprType.Modulo:
                    case SimpleBinaryExprType.Add:
                    case SimpleBinaryExprType.Subtract:
                    case SimpleBinaryExprType.BitAnd:
                    case SimpleBinaryExprType.BitXor:
                    case SimpleBinaryExprType.BitOr:
                        isConst = true;
                        break;

                    case SimpleBinaryExprType.ShiftLeft:
                    case SimpleBinaryExprType.ShiftRight:
                    case SimpleBinaryExprType.ShiftRightUnsigned:
                        isConst = true;
                        break;

                    case SimpleBinaryExprType.LesserThan:
                    case SimpleBinaryExprType.GreaterThan:
                    case SimpleBinaryExprType.LesserThanEqual:
                    case SimpleBinaryExprType.GreaterThanEqual:
                    case SimpleBinaryExprType.Equals:
                    case SimpleBinaryExprType.NotEquals:
                        isConst = true;
                        break;

                    case SimpleBinaryExprType.Assign:
                    case SimpleBinaryExprType.AssignAdd:
                    case SimpleBinaryExprType.AssignSubtract:
                    case SimpleBinaryExprType.AssignMultiply:
                    case SimpleBinaryExprType.AssignDivide:
                    case SimpleBinaryExprType.AssignModulo:
                    case SimpleBinaryExprType.AssignPower:
                    case SimpleBinaryExprType.AssignBitAnd:
                    case SimpleBinaryExprType.AssignBitOr:
                    case SimpleBinaryExprType.AssignXor:

                    case SimpleBinaryExprType.AssignShiftLeft:
                    case SimpleBinaryExprType.AssignShiftRight:
                    case SimpleBinaryExprType.AssignShiftRightUnsigned:
                        isConst = false;
                        break;

                    default:
                        isConst = false;
                        return false;
                }

                bool isCompatible;

                if (exprType.IsBitShift ()) {
                    if (!rhsIntType->Unsigned)
                        return false;

                    if (rhsIntType->IntSize > lhsIntType->IntSize)
                        return false;

                    return true;
                } else if (lhsIntType->Unsigned == rhsIntType->Unsigned)
                    isCompatible = true;
                else
                    isCompatible = false;

                if (isCompatible) {
                    if (exprType.IsAssignment ()) {
                        if (lhsIntType->IntSize < rhsIntType->IntSize)
                            return false;
                    } else if (!exprType.IsComparison ()) {
                        if (lhsIntType->IntSize >= rhsIntType->IntSize)
                            finalType = lhsType;
                        else
                            finalType = rhsType;
                    } else
                        finalType = environment.TypeBool;
                }

                return isCompatible;
            }

            private bool BinaryOpCompat_BoolBool (ES_TypeInfo* lhsType, ES_TypeInfo* rhsType, SimpleBinaryExprType exprType, out ES_TypeInfo* finalType, out bool isConst) {
                Debug.Assert (lhsType->TypeTag == ES_TypeTag.Bool);
                Debug.Assert (rhsType->TypeTag == ES_TypeTag.Bool);

                switch (exprType) {
                    case SimpleBinaryExprType.BitAnd:
                    case SimpleBinaryExprType.BitXor:
                    case SimpleBinaryExprType.BitOr:
                        isConst = true;
                        break;

                    case SimpleBinaryExprType.Equals:
                    case SimpleBinaryExprType.NotEquals:
                        isConst = true;
                        break;

                    case SimpleBinaryExprType.LogicalAnd:
                    case SimpleBinaryExprType.LogicalOr:
                        isConst = true;
                        break;

                    case SimpleBinaryExprType.Assign:
                    case SimpleBinaryExprType.AssignBitAnd:
                    case SimpleBinaryExprType.AssignBitOr:
                    case SimpleBinaryExprType.AssignXor:
                        isConst = false;
                        break;

                    default:
                        finalType = environment.TypeUnknownValue;
                        isConst = false;
                        return false;
                }

                finalType = environment.TypeBool;
                return true;
            }

            private bool BinaryOpCompat_FloatFloat (ES_TypeInfo* lhsType, ES_TypeInfo* rhsType, SimpleBinaryExprType exprType, out ES_TypeInfo* finalType, out bool isConst) {
                Debug.Assert (lhsType->TypeTag == ES_TypeTag.Float);
                Debug.Assert (rhsType->TypeTag == ES_TypeTag.Float);

                var lhsFloatType = (ES_FloatTypeData*) lhsType;
                var rhsFloatType = (ES_FloatTypeData*) rhsType;

                finalType = environment.TypeUnknownValue;

                if (lhsFloatType->FloatSize != rhsFloatType->FloatSize) {
                    isConst = false;
                    return false;
                }

                switch (exprType) {
                    case SimpleBinaryExprType.Power:
                    case SimpleBinaryExprType.Multiply:
                    case SimpleBinaryExprType.Divide:
                    case SimpleBinaryExprType.Modulo:
                    case SimpleBinaryExprType.Add:
                    case SimpleBinaryExprType.Subtract:
                        isConst = true;
                        break;

                    case SimpleBinaryExprType.LesserThan:
                    case SimpleBinaryExprType.GreaterThan:
                    case SimpleBinaryExprType.LesserThanEqual:
                    case SimpleBinaryExprType.GreaterThanEqual:
                    case SimpleBinaryExprType.Equals:
                    case SimpleBinaryExprType.NotEquals:
                        isConst = true;
                        break;

                    case SimpleBinaryExprType.Assign:
                    case SimpleBinaryExprType.AssignAdd:
                    case SimpleBinaryExprType.AssignSubtract:
                    case SimpleBinaryExprType.AssignMultiply:
                    case SimpleBinaryExprType.AssignDivide:
                    case SimpleBinaryExprType.AssignModulo:
                    case SimpleBinaryExprType.AssignPower:
                        isConst = false;
                        break;

                    default:
                        isConst = false;
                        return false;
                }

                finalType = !exprType.IsComparison () ? lhsType : environment.typeBool;

                return true;
            }

            private bool BinaryOpCompat_FloatInt (ES_TypeInfo* lhsType, ES_TypeInfo* rhsType, SimpleBinaryExprType exprType, out ES_TypeInfo* finalType, out bool isConst) {
                Debug.Assert (lhsType->TypeTag == ES_TypeTag.Float);
                Debug.Assert (rhsType->TypeTag == ES_TypeTag.Int);

                finalType = environment.TypeUnknownValue;

                switch (exprType) {
                    case SimpleBinaryExprType.Power:
                        isConst = true;
                        break;

                    case SimpleBinaryExprType.AssignPower:
                        isConst = false;
                        break;

                    default:
                        isConst = false;
                        return false;
                }

                finalType = lhsType;

                return true;
            }

            #endregion

            #region UnaryOpCompat

            public bool UnaryOpCompat (ES_TypeInfo* exprType, SimpleUnaryExprType op, out ES_TypeInfo* finalType, out bool isConst) {
                switch (exprType->TypeTag) {
                    case ES_TypeTag.Int:
                        return UnaryOpCompat_Int (exprType, op, out finalType, out isConst);

                    case ES_TypeTag.Bool:
                        return UnaryOpCompat_Bool (exprType, op, out finalType, out isConst);

                    case ES_TypeTag.Float:
                        return UnaryOpCompat_Float (exprType, op, out finalType, out isConst);

                    default:
                        finalType = environment.TypeUnknownValue;
                        isConst = false;
                        return false;
                }
            }

            private bool UnaryOpCompat_Int (ES_TypeInfo* exprType, SimpleUnaryExprType op, out ES_TypeInfo* finalType, out bool isConst) {
                Debug.Assert (exprType->TypeTag == ES_TypeTag.Int);

                switch (op) {
                    case SimpleUnaryExprType.Positive:
                    case SimpleUnaryExprType.BitNot:
                        finalType = exprType;
                        isConst = true;
                        return true;

                    case SimpleUnaryExprType.Negative: {
                        var intData = (ES_IntTypeData*) exprType;

                        if (!intData->Unsigned) {
                            finalType = exprType;
                            isConst = true;
                            return true;
                        } else {
                            finalType = environment.TypeUnknownValue;
                            isConst = false;
                            return false;
                        }
                    }

                    case SimpleUnaryExprType.LogicalNot:
                        finalType = environment.TypeUnknownValue;
                        isConst = false;
                        return false;

                    default:
                        throw new NotImplementedException ();
                }
            }

            private bool UnaryOpCompat_Bool (ES_TypeInfo* exprType, SimpleUnaryExprType op, out ES_TypeInfo* finalType, out bool isConst) {
                Debug.Assert (exprType->TypeTag == ES_TypeTag.Bool);

                switch (op) {
                    case SimpleUnaryExprType.LogicalNot:
                        finalType = exprType;
                        isConst = true;
                        return true;

                    case SimpleUnaryExprType.Positive:
                    case SimpleUnaryExprType.Negative:
                    case SimpleUnaryExprType.BitNot:
                        finalType = environment.TypeUnknownValue;
                        isConst = false;
                        return false;

                    default:
                        throw new NotImplementedException ();
                }
            }

            private bool UnaryOpCompat_Float (ES_TypeInfo* exprType, SimpleUnaryExprType op, out ES_TypeInfo* finalType, out bool isConst) {
                Debug.Assert (exprType->TypeTag == ES_TypeTag.Float);

                switch (op) {
                    case SimpleUnaryExprType.Positive:
                    case SimpleUnaryExprType.Negative:
                        finalType = exprType;
                        isConst = true;
                        return true;

                    case SimpleUnaryExprType.LogicalNot:
                    case SimpleUnaryExprType.BitNot:
                        finalType = environment.TypeUnknownValue;
                        isConst = false;
                        return false;

                    default:
                        throw new NotImplementedException ();
                }
            }

            #endregion

            public ES_FunctionPrototypeData* GetOrAddFunctionType (ES_TypeInfo* returnType, ReadOnlySpan<ES_FunctionPrototypeArgData> args, bool doAdd) {
                var name = environment.GetFunctionTypeName (returnType, args);
                var fqn = environment.GetFullyQualifiedName (ArrayPointer<byte>.Null, name);

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
                        name, fqn, ArrayPointer<byte>.Null
                    );
                }

                return funcType;
            }

            #region Derived type creation

            public ES_TypeInfo* CreatePointerType (ES_TypeInfo* baseType) {
                throw new NotImplementedException ();
            }

            public ES_TypeInfo* CreateNullableType (ES_TypeInfo* baseType) {
                throw new NotImplementedException ();
            }

            public ES_TypeInfo* CreateConstType (ES_TypeInfo* baseType) {
                throw new NotImplementedException ();
            }

            public ES_TypeInfo* CreateImmutableType (ES_TypeInfo* baseType) {
                throw new NotImplementedException ();
            }

            public ES_TypeInfo* CreateArrayType (ES_TypeInfo* elementType, int dimensionCount) {
                throw new NotImplementedException ();
            }

            public ES_TypeInfo* CreateArrayType (ES_TypeInfo* elementType, ReadOnlySpan<int> dimensionsSizes) {
                throw new NotImplementedException ();
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
                if (!disposedValue) {
                    NamespaceBuilders?.Dispose ();
                    NamespaceBuilders = null!;

                    PointerAstMap.Clear ();
                    PointerAstMap = null!;

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
        protected ArrayPointer<Pointer<ES_TypeInfo>> builtinTypesList;
        protected IMemoryManager memManager;
        protected Dictionary<ArrayPointer<byte>, ES_NamespaceData> namespacesDict;

        protected IBackendData? backendData;

        protected ES_TypeInfo* typeUnknownValue;
        protected ES_TypeInfo* typeVoid;
        protected ES_TypeInfo* typeBool;
        protected ES_TypeInfo* typeFloat32;
        protected ES_TypeInfo* typeFloat64;

        #endregion

        #region ================== Instance properties

        public UnmanagedIdentifierPool IdPool { get; protected set; }

        public IReadOnlyPooledList<Pointer<ES_TypeInfo>> TypesList => typesList;

        public ArrayPointer<Pointer<ES_TypeInfo>> BuiltinTypesList => builtinTypesList;

        public IReadOnlyDictionary<ArrayPointer<byte>, ES_NamespaceData> Namespaces => namespacesDict;

        public ES_TypeInfo* TypeUnknownValue => typeUnknownValue;
        public ES_TypeInfo* TypeVoid => typeVoid;
        public ES_TypeInfo* TypeBool => typeBool;
        public ES_TypeInfo* TypeFloat32 => typeFloat32;
        public ES_TypeInfo* TypeFloat64 => typeFloat64;

        #endregion

        #region ================== Constructors

        protected EchelonScriptEnvironment () {
            namespacesDict = new Dictionary<ArrayPointer<byte>, ES_NamespaceData> ();
            IdPool = new UnmanagedIdentifierPool ();

            typesList = new PooledList<Pointer<ES_TypeInfo>> ();
            memManager = new BasicMemoryManager ();

            backendData = null;

            var unknTypeId = IdPool.GetIdentifier ("#UNKNOWN_TYPE");
            var unknType = memManager.GetMemory<ES_TypeInfo> ();
            *unknType = new ES_TypeInfo (
                ES_TypeTag.UNKNOWN, ES_AccessModifier.Public, ArrayPointer<byte>.Null,
                unknTypeId, GetFullyQualifiedName (ArrayPointer<byte>.Null, unknTypeId)
            );
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

        protected ArrayPointer<byte> GetFunctionTypeName (ES_TypeInfo* returnType, ReadOnlySpan<ES_FunctionPrototypeArgData> args) {
            static PooledArray<char> SanitizeFQN (ReadOnlySpan<char> fqn) {
                var argName = PooledArray<char>.GetArray (fqn.Length);

                fqn.CopyTo (argName);

                var namespaceSepIdx = argName.Span.IndexOf ("::");
                Debug.Assert (namespaceSepIdx > -1);
                argName.Span.Slice (namespaceSepIdx, 2).Fill ('@');

                return argName;
            }

            Debug.Assert (returnType != null);

            using var charList = new StructPooledList<char> (CL_ClearMode.Auto);

            charList.AddRange ("@FuncType<");

            // Add the return type.
            charList.AddRange ("ret@");
            using var retName = SanitizeFQN (returnType->FullyQualifiedNameString);
            charList.AddRange (retName);
            retName.Dispose ();

            // Add the arguments.
            bool firstArg = true;
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

                using var argName = SanitizeFQN (arg.ValueType->FullyQualifiedNameString);
                charList.AddRange (argName);
            }

            charList.AddRange (">");

            return IdPool.GetIdentifier (charList.Span);
        }

        public ArrayPointer<byte> GetFullyQualifiedName (ArrayPointer<byte> namespaceName, ArrayPointer<byte> typeName) {
            using var fqnArr = PooledArray<byte>.GetArray (namespaceName.Length + typeName.Length + 2);
            var fqnSpan = fqnArr.Span;

            int len = 0;

            namespaceName.Span.CopyTo (fqnSpan.Slice (len, namespaceName.Length));
            len += namespaceName.Length;

            fqnSpan [len++] = (byte) ':';
            fqnSpan [len++] = (byte) ':';

            typeName.Span.CopyTo (fqnSpan.Slice (len, typeName.Length));

            return IdPool.GetIdentifier (fqnSpan);
        }

        public ArrayPointer<byte> GetFullyQualifiedName (ArrayPointer<byte> namespaceName, ReadOnlySpan<ArrayPointer<byte>> typeName) {
            Debug.Assert (typeName.Length > 0);

            int len = namespaceName.Length + 2 + (typeName.Length - 1); // Namespace name + "::" + "."s (if any)
            for (int i = 0; i < typeName.Length; i++)
                len += typeName [i].Length;

            using var fqnArr = PooledArray<byte>.GetArray (len);
            var fqnSpan = fqnArr.Span;

            len = 0;

            // Namespace
            namespaceName.Span.CopyTo (fqnSpan.Slice (len, namespaceName.Length));
            len += namespaceName.Length;

            // Namespace separator
            fqnSpan [len++] = (byte) ':';
            fqnSpan [len++] = (byte) ':';

            // First type name
            typeName [0].Span.CopyTo (fqnSpan.Slice (len, typeName [0].Length));
            len += typeName [0].Length;

            // Nested type names
            for (int i = 1; i < typeName.Length; i++) {
                fqnSpan [len++] = (byte) '.';

                typeName [i].Span.CopyTo (fqnSpan.Slice (len, typeName [i].Length));
                len += typeName [i].Length;
            }

            return IdPool.GetIdentifier (fqnSpan);
        }

        public ES_TypeInfo* GetFullyQualifiedType (ArrayPointer<byte> fullyQualifiedName) {
            ReadOnlySpan<byte> namespaceSep = stackalloc byte [2] { (byte) ':', (byte) ':' };
            int namespaceSepPos = fullyQualifiedName.Span.IndexOf (namespaceSep);

            Debug.Assert (namespaceSepPos > -1, "The specified name must be fully qualified.");

            ReadOnlySpan<Pointer<ES_TypeInfo>> typesSpan;
            if (namespaceSepPos > 0) {
                var namespaceName = IdPool.GetIdentifier (fullyQualifiedName.Span.Slice (0, namespaceSepPos));

                if (!Namespaces.TryGetValue (namespaceName, out var namespaceData))
                    return null;

                typesSpan = namespaceData.TypeSpan;
            } else
                typesSpan = builtinTypesList.Span;

            var typeName = IdPool.GetIdentifier (fullyQualifiedName.Span.Slice (namespaceSepPos + namespaceSep.Length));
            foreach (var typePtr in typesSpan) {
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
                backendData?.Dispose ();
                namespacesDict.Clear ();
                IdPool?.Dispose ();
                typesList?.Dispose ();
                memManager?.Dispose ();

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
