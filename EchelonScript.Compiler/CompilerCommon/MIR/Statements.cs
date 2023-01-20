/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace EchelonScript.Compiler.CompilerCommon.MIR;

public struct MIRConstant {
    private enum Kind {
        Bool,
        Int,
        Float,
        Double,
        String,
    }

    private Kind valueKind;
    private long valueInt;
    private MIRString valueString;

    private MIRConstant (Kind kind, long value) {
        Debug.Assert (kind != Kind.String);

        valueKind = kind;
        valueInt = value;

        valueString = default;
    }
    public MIRConstant (bool val) : this (Kind.Bool, val ? 1 : 0) { }
    public MIRConstant (long val) : this (Kind.Int, val) { }
    public MIRConstant (ulong val) : this (Kind.Int, (long) val) { }
    public MIRConstant (float val) : this (Kind.Int, Unsafe.As<float, int> (ref val)) { }
    public MIRConstant (double val) : this (Kind.Int, Unsafe.As<double, long> (ref val)) { }
    public MIRConstant (MIRString val) {
        valueKind = Kind.String;
        valueString = val;

        valueInt = default;
    }

    public bool IsBool () => valueKind == Kind.Bool;
    public bool IsInt () => valueKind == Kind.Int;
    public bool IsFloat () => valueKind == Kind.Float;
    public bool IsDouble () => valueKind == Kind.Double;
    public bool IsString () => valueKind == Kind.String;

    public bool GetBool () {
        if (!IsBool ())
            throw new MIRADTException ();

        return valueInt != 0;
    }
    public long GetInt () {
        if (!IsInt ())
            throw new MIRADTException ();

        return valueInt;
    }
    public float GetFloat () {
        if (!IsFloat ())
            throw new MIRADTException ();

        var valInt = (int) valueInt;
        return Unsafe.As<int, float> (ref valInt);
    }
    public double GetDouble () {
        if (!IsDouble ())
            throw new MIRADTException ();

        return Unsafe.As<long, double> (ref valueInt);
    }
    public MIRString GetString () {
        if (!IsString ())
            throw new MIRADTException ();

        return valueString;
    }
}

public enum MIRValueKind {

}

public struct MIRValue {
    private enum Kind {
        Register,
        Constant,
    }

    private Kind valueKind;
    private long valueRegister;
    private MIRConstant valueConstant;

    public MIRValue (long registerIndex) {
        valueKind = Kind.Register;
        valueRegister = registerIndex;

        valueConstant = default;
    }
    public MIRValue (MIRConstant constant) {
        valueKind = Kind.Constant;
        valueConstant = constant;

        valueRegister = default;
    }

    public bool IsRegister () => valueKind == Kind.Register;
    public bool IsConstant () => valueKind == Kind.Constant;

    public long GetRegister () {
        if (!IsRegister ())
            throw new MIRADTException ();

        return valueRegister;
    }
    public MIRConstant GetConstant () {
        if (!IsConstant ())
            throw new MIRADTException ();

        return valueConstant;
    }
}

public struct MIRStatement {

}
