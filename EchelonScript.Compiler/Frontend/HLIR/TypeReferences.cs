/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using System.Runtime.CompilerServices;
using EchelonScript.Common.Data;

namespace EchelonScript.Compiler.Frontend.HLIR;

public enum HLIRTypeDeclKind {
    TypeName,
    Reference,
    Array,
    Nullable,
    Const,
    Immutable,
}

public readonly struct HLIRTypeDecl {
    public HLIRTypeDeclKind Kind { get; private init; }
    public HLIRNodeBounds Bounds { get; private init; }

    private readonly ES_Identifier typeNameId;
    private readonly int arrayRank;

    #region ================== Constructors

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private HLIRTypeDecl (HLIRTypeDeclKind kind, HLIRNodeBounds bounds, ES_Identifier nameId, int arrRank) {
        Kind = kind;
        Bounds = bounds;

        typeNameId = nameId;
        arrayRank = arrRank;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static HLIRTypeDecl TypeName (ES_Identifier typeName, HLIRNodeBounds bounds)
        => new (HLIRTypeDeclKind.TypeName, bounds, typeName, 0);

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static HLIRTypeDecl Reference (HLIRNodeBounds bounds)
        => new (HLIRTypeDeclKind.Reference, bounds, ES_Identifier.Empty, 0);

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static HLIRTypeDecl Array (HLIRNodeBounds bounds, int rank)
        => new (HLIRTypeDeclKind.Array, bounds, ES_Identifier.Empty, rank);

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static HLIRTypeDecl Nullable (HLIRNodeBounds bounds)
        => new (HLIRTypeDeclKind.Nullable, bounds, ES_Identifier.Empty, 0);

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static HLIRTypeDecl Const (HLIRNodeBounds bounds)
        => new (HLIRTypeDeclKind.Const, bounds, ES_Identifier.Empty, 0);

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static HLIRTypeDecl Immutable (HLIRNodeBounds bounds)
        => new (HLIRTypeDeclKind.Immutable, bounds, ES_Identifier.Empty, 0);

    #endregion

    #region ================== Matchers/Getters

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public ES_Identifier GetTypeName () {
        Debug.Assert (Kind == HLIRTypeDeclKind.TypeName);
        return typeNameId;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public int GetArrayRank () {
        Debug.Assert (Kind == HLIRTypeDeclKind.Array);
        return arrayRank;
    }

    #endregion
}
