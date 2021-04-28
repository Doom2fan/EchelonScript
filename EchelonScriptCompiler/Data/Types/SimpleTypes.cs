/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Runtime.InteropServices;

namespace EchelonScriptCompiler.Data.Types {
    public enum ES_IntSize : int {
        Int8  = 0,
        Int16 = 1,
        Int32 = 2,
        Int64 = 3,
    }

    public enum ES_FloatSize : int {
        Single = 0,
        Double = 1,
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "IntData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_IntTypeData {
        #region ================== Instance fields

        public ES_TypeInfo TypeInfo;
        public readonly ES_IntSize IntSize;
        public readonly bool Unsigned;

        #endregion

        #region ================== Constructors

        public ES_IntTypeData (
            ES_AccessModifier accessMod, ArrayPointer<byte> sourceUnit,
            ArrayPointer<byte> name, ArrayPointer<byte> fullyQualifiedName,
            ES_IntSize size, bool unsigned
        ) {
            TypeInfo = new ES_TypeInfo (ES_TypeTag.Int, accessMod, sourceUnit, name, fullyQualifiedName);

            IntSize = size;
            Unsigned = unsigned;

            TypeInfo.RuntimeSize = size switch {
                ES_IntSize.Int8  => 1,
                ES_IntSize.Int16 => 2,
                ES_IntSize.Int32 => 4,
                ES_IntSize.Int64 => 8,

                _ => throw new NotImplementedException (),
            };
        }

        #endregion
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "FloatData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_FloatTypeData {
        #region ================== Instance fields

        public ES_TypeInfo TypeInfo;
        public readonly ES_FloatSize FloatSize;

        #endregion

        #region ================== Constructors

        public ES_FloatTypeData (
            ES_AccessModifier accessMod, ArrayPointer<byte> sourceUnit,
            ArrayPointer<byte> name, ArrayPointer<byte> fullyQualifiedName,
            ES_FloatSize size
        ) {
            TypeInfo = new ES_TypeInfo (ES_TypeTag.Float, accessMod, sourceUnit, name, fullyQualifiedName);

            FloatSize = size;

            TypeInfo.RuntimeSize = size switch {
                ES_FloatSize.Single => 4,
                ES_FloatSize.Double => 8,

                _ => throw new NotImplementedException (),
            };
        }

        #endregion
    }

    [StructLayout (LayoutKind.Sequential, Pack = 1)]
    [ES_ExportAggregate (new [] { "EchelonScript", "Reflection" }, "PointerData", ES_ExportAttributeBase.AggregateType.Struct)]
    public unsafe struct ES_PointerData {
        #region ================== Instance fields

        public ES_TypeInfo TypeInfo;
        public ES_TypeInfo* PointedType;

        #endregion

        #region ================== Constructors

        public ES_PointerData (
            ES_AccessModifier accessMod, ArrayPointer<byte> sourceUnit,
            ArrayPointer<byte> name, ArrayPointer<byte> fullyQualifiedName,
            ES_TypeInfo* pointedType
        ) {
            TypeInfo = new ES_TypeInfo (ES_TypeTag.Pointer, accessMod, sourceUnit, name, fullyQualifiedName);
            TypeInfo.RuntimeSize = sizeof (IntPtr);

            PointedType = pointedType;
        }

        #endregion
    }
}
