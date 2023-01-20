/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;

namespace EchelonScript.Compiler.Frontend.HLIR;

public struct HLIRNodeBounds {
    public int StartPos;
    public int EndPos;

    public HLIRNodeBounds (int pos) => StartPos = EndPos = pos;

    public HLIRNodeBounds (int start, int end) {
        StartPos = start;
        EndPos = end;
    }
}

public readonly struct HLIRId : IEquatable<HLIRId>, IComparable<HLIRId> {
    private readonly uint idNumber;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    private HLIRId (uint idNum) => idNumber = idNum;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static HLIRId First () => new (0);

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public HLIRId Next () => new (idNumber + 1);

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public override int GetHashCode () {
        const int a = 6;
        const int b = 13;
        const int c = 25;
        uint uhash = idNumber * 982451653u;
        var redongled =
            ((uhash << a) | (uhash >> (32 - a))) ^
            ((uhash << b) | (uhash >> (32 - b))) ^
            ((uhash << c) | (uhash >> (32 - c)));
        return (int) redongled;
    }

    #region ================== Equality functions

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public bool Equals (HLIRId other) => idNumber == other.idNumber;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public override bool Equals ([NotNullWhen (true)] object? obj) {
        if (obj is HLIRId other)
            return this == other;

        return false;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static bool operator == (HLIRId left, HLIRId right) => left.idNumber == right.idNumber;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static bool operator != (HLIRId left, HLIRId right) => !(left == right);

    #endregion

    #region ================== Comparison functions

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public int CompareTo (HLIRId other) => idNumber.CompareTo (other.idNumber);

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static bool operator > (HLIRId operand1, HLIRId operand2) => operand1.idNumber > operand2.idNumber;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static bool operator < (HLIRId operand1, HLIRId operand2) => operand1.idNumber < operand2.idNumber;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static bool operator >= (HLIRId operand1, HLIRId operand2) => operand1.idNumber >= operand2.idNumber;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static bool operator <= (HLIRId operand1, HLIRId operand2) => operand1.idNumber <= operand2.idNumber;

    #endregion
}
