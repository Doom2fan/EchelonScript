/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using ClrGCHandle = System.Runtime.InteropServices.GCHandle;

namespace EchelonScriptCommon;

public unsafe static class ES_DotNetIntrinsicsImpl {
    #region Integer division

    public static int IntegerDivision (int lhs, int rhs) {
        if (rhs == 0)
            throw new EchelonScriptIntegerDivisionByZeroException ();

        return lhs / rhs;
    }

    public static long IntegerDivision (long lhs, long rhs) {
        if (rhs == 0)
            throw new EchelonScriptIntegerDivisionByZeroException ();

        return lhs / rhs;
    }

    public static uint IntegerDivision (uint lhs, uint rhs) {
        if (rhs == 0)
            throw new EchelonScriptIntegerDivisionByZeroException ();

        return lhs / rhs;
    }

    public static ulong IntegerDivision (ulong lhs, ulong rhs) {
        if (rhs == 0)
            throw new EchelonScriptIntegerDivisionByZeroException ();

        return lhs / rhs;
    }

    public static int IntegerModulo (int lhs, int rhs) {
        if (rhs == 0)
            throw new EchelonScriptIntegerDivisionByZeroException ();

        return lhs % rhs;
    }

    public static long IntegerModulo (long lhs, long rhs) {
        if (rhs == 0)
            throw new EchelonScriptIntegerDivisionByZeroException ();

        return lhs % rhs;
    }

    public static uint IntegerModulo (uint lhs, uint rhs) {
        if (rhs == 0)
            throw new EchelonScriptIntegerDivisionByZeroException ();

        return lhs % rhs;
    }

    public static ulong IntegerModulo (ulong lhs, ulong rhs) {
        if (rhs == 0)
            throw new EchelonScriptIntegerDivisionByZeroException ();

        return lhs % rhs;
    }

    #endregion

    #region Nice names

    public static ES_Identifier GetNiceTypeName (
        IntPtr idPoolHandle,
        ES_TypeInfo* type,
        bool fullyQualified,
        ES_Identifier globalTypesNS,
        ES_Identifier generatedTypesNS
    ) {
        var charsList = new StructPooledList<char> (CL_ClearMode.Auto);
        try {
            var idPool = (ES_IdentifierPool?) ClrGCHandle.FromIntPtr (idPoolHandle).Target;

            ES_TypeInfo.GetNiceTypeName (ref charsList, type, fullyQualified, globalTypesNS, generatedTypesNS);

            return idPool!.GetIdentifier (charsList.Span);
        } finally {
            charsList.Dispose ();
        }
    }

    #endregion
}
