/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScriptCommon {
    public static class ES_DotNetIntrinsicsImpl {
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
    }
}
