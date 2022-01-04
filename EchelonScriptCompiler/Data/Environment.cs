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
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.Backends;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Frontend;

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
            public ArrayPointer<byte> NamespaceName => namespaceData.namespaceName;

            public PooledDictionary<ArrayPointer<byte>, ES_ClassData.Builder> ClassBuilders { get; protected set; }
            public PooledDictionary<ArrayPointer<byte>, ES_StructData.Builder> StructBuilders { get; protected set; }
            public PooledDictionary<ArrayPointer<byte>, ES_EnumData.Builder> EnumBuilders { get; protected set; }
            public Dictionary<ArrayPointer<byte>, Pointer<ES_FunctionData>> Functions => namespaceData.functions;

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
                namespaceData.types.Add (&classDataPtr->TypeInfo);

                var unknType = namespaceData.environment.TypeUnknownValue;

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
                namespaceData.types.Add (&structDataPtr->TypeInfo);

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
                namespaceData.types.Add (&enumDataPtr->TypeInfo);

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

        protected List<Pointer<ES_TypeInfo>> types;

        #endregion

        #region ================== Instance properties

        public ArrayPointer<byte> NamespaceName => namespaceName;

        public string NamespaceNameString {
            get {
                return StringPool.Shared.GetOrAdd (namespaceName.Span, Encoding.ASCII);
            }
        }

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

                if (lhsType->TypeTag == ES_TypeTag.Reference && rhsType->TypeTag == ES_TypeTag.Reference)
                    return BinaryOpCompat_RefRef (lhsType, rhsType, exprType, out finalType, out isConst);

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

                        finalType = lhsType;
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

            private bool BinaryOpCompat_RefRef (ES_TypeInfo* lhsType, ES_TypeInfo* rhsType, SimpleBinaryExprType exprType, out ES_TypeInfo* finalType, out bool isConst) {
                Debug.Assert (lhsType->TypeTag == ES_TypeTag.Reference);
                Debug.Assert (rhsType->TypeTag == ES_TypeTag.Reference);

                finalType = environment.TypeUnknownValue;
                isConst = false;

                if (lhsType != rhsType)
                    return false;

                switch (exprType) {
                    case SimpleBinaryExprType.Assign:
                        finalType = lhsType;
                        break;

                    case SimpleBinaryExprType.Equals:
                    case SimpleBinaryExprType.NotEquals:
                        finalType = TypeBool;
                        break;

                    default:
                        return false;
                }

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

                    case ES_TypeTag.Reference:
                        return UnaryOpCompat_Ref (exprType, op, out finalType, out isConst);

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

                    case SimpleUnaryExprType.Dereference:
                    case SimpleUnaryExprType.LogicalNot:
                        finalType = environment.TypeUnknownValue;
                        isConst = false;
                        return false;

                    default:
                        throw new NotImplementedException ("Operation not implemented.");
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
                    case SimpleUnaryExprType.Dereference:
                        finalType = environment.TypeUnknownValue;
                        isConst = false;
                        return false;

                    default:
                        throw new NotImplementedException ("Operation not implemented.");
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
                    case SimpleUnaryExprType.Dereference:
                        finalType = environment.TypeUnknownValue;
                        isConst = false;
                        return false;

                    default:
                        throw new NotImplementedException ("Operation not implemented.");
                }
            }

            private bool UnaryOpCompat_Ref (ES_TypeInfo* exprType, SimpleUnaryExprType op, out ES_TypeInfo* finalType, out bool isConst) {
                Debug.Assert (exprType->TypeTag == ES_TypeTag.Reference);

                var refType = (ES_ReferenceData*) exprType;

                switch (op) {
                    case SimpleUnaryExprType.Dereference:
                        finalType = refType->PointedType;
                        isConst = false;
                        return true;

                    case SimpleUnaryExprType.Positive:
                    case SimpleUnaryExprType.Negative:
                    case SimpleUnaryExprType.LogicalNot:
                    case SimpleUnaryExprType.BitNot:
                        finalType = environment.TypeUnknownValue;
                        isConst = false;
                        return false;

                    default:
                        throw new NotImplementedException ("Operation not implemented.");
                }
            }

            #endregion

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
                var varsSize = varsList.Length * sizeof (ES_MemberData_Variable);
                var funcsSize = funcsList.Length * sizeof (ES_MemberData_Function);

                IntPtr memArea = IntPtr.Zero;
                var membersList = MemoryManager.GetArray<Pointer<ES_MemberData>> (varsList.Length + funcsList.Length);

                if (varsSize + funcsSize > 0)
                    memArea = MemoryManager.GetMemory (varsSize + funcsSize);

                var varsSpan = new ArrayPointer<ES_MemberData_Variable> ((ES_MemberData_Variable*) memArea, varsList.Length);
                var funcsSpan = new ArrayPointer<ES_MemberData_Function> ((ES_MemberData_Function*) (memArea + varsSize), funcsList.Length);

                // Copy the member data structs to their final memory location and set the pointers to said memory location.
                int membersCount = 0;
                int idx = 0;
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

            public ES_TypeInfo* CreateReferenceType (ES_TypeInfo* baseType) {
                // Format sample: "@generated::NamespaceName__TypeName&"
                var baseFQN = baseType->Name;

                using var idBase = UnmanagedArray<byte>.GetArray (baseFQN.NamespaceName.Length + baseFQN.TypeName.Length + 3);
                var idBaseSpan = idBase.Span;

                int idx = 0;

                baseFQN.NamespaceName.Span.CopyTo (idBaseSpan);
                idx += baseFQN.NamespaceName.Length;

                idBaseSpan.Slice (idx, 2).Fill ((byte) '_');
                idx += 2;

                baseFQN.TypeName.Span.CopyTo (idBaseSpan.Slice (idx));
                idx += baseFQN.TypeName.Length;

                idBaseSpan [idx++] = (byte) '&';

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

            public ES_TypeInfo* CreateConstType (ES_TypeInfo* baseType) {
                throw new NotImplementedException ("[TODO] Const not implemented yet.");
            }

            public ES_TypeInfo* CreateImmutableType (ES_TypeInfo* baseType) {
                throw new NotImplementedException ("[TODO] Immutable not implemented yet.");
            }

            public ES_TypeInfo* CreateArrayType (ES_TypeInfo* elementType, int dimensionCount) {
                // Format sample: "@generated::NamespaceName__TypeName[,,]"
                var elemFQN = elementType->Name;

                using var idBase = UnmanagedArray<byte>.GetArray (elemFQN.NamespaceName.Length + elemFQN.TypeName.Length + 4 + (dimensionCount - 1));
                var idBaseSpan = idBase.Span;

                int idx = 0;

                elemFQN.NamespaceName.Span.CopyTo (idBaseSpan);
                idx += elemFQN.NamespaceName.Length;

                idBaseSpan.Slice (idx, 2).Fill ((byte) '_');
                idx += 2;

                elemFQN.TypeName.Span.CopyTo (idBaseSpan.Slice (idx));
                idx += elemFQN.TypeName.Length;

                idBaseSpan [idx++] = (byte) '[';
                for (int i = 1; i < dimensionCount; i++)
                    idBaseSpan [idx++] = (byte) ',';
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

                for (int i = 0; i < dimensionCount; i++) {
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
                if (!disposedValue) {
                    foreach (var builderKVP in NamespaceBuilders)
                        builderKVP.Value.Dispose ();

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

        public IReadOnlyDictionary<ArrayPointer<byte>, ES_NamespaceData> Namespaces => namespacesDict;

        public ArrayPointer<byte> GlobalTypesNamespace { get; private set; }
        public ArrayPointer<byte> GeneratedTypesNamespace { get; private set; }

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

            memManager = new BasicMemoryManager ();

            backendData = null;

            GlobalTypesNamespace = IdPool.GetIdentifier ("@globals");
            GeneratedTypesNamespace = IdPool.GetIdentifier ("@generated");

            var unknTypeId = IdPool.GetIdentifier ("#UNKNOWN_TYPE");
            var unknType = memManager.GetMemory<ES_TypeInfo> ();
            *unknType = new ES_TypeInfo (
                ES_TypeTag.UNKNOWN, ES_AccessModifier.Public, ArrayPointer<byte>.Null,
                new ES_FullyQualifiedName (ArrayPointer<byte>.Null, unknTypeId)
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

        public ES_TypeInfo* GetFullyQualifiedType (ArrayPointer<byte> namespaceName, ArrayPointer<byte> typeName) {
            return GetFullyQualifiedType (new ES_FullyQualifiedName (namespaceName, typeName));
        }

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

            bool firstArg = true;
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
            if (!disposedValue) {
                backendData?.Dispose ();
                namespacesDict.Clear ();
                IdPool?.Dispose ();
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
