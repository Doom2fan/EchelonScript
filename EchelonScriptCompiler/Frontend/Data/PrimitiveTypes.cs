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
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.Frontend.Data;

internal class ESC_TypeVoid : ESC_TypeOpaque {
    public ESC_TypeVoid (ES_FullyQualifiedName name) : base (name, null) => AccessModifier = ES_AccessModifier.Public;

    public override int GetRuntimeSize () => 0;
    public override bool IsReferenceType () => false;
}

internal class ESC_TypeBool : ESC_TypeData {
    public override ES_FullyQualifiedName Name { get; }

    public ESC_TypeBool (ES_FullyQualifiedName name) {
        Name = name;

        AccessModifier = ES_AccessModifier.Public;
        Flags = ESC_TypeFlag.Analyzed;
    }

    public override int GetRuntimeSize () => 1;
    public override bool IsReferenceType () => false;
    public override bool IsConstant () => false;
    public override bool IsWritable () => true;

    public override IEnumerable<ESC_TypeMember> GetMembers () => Array.Empty<ESC_TypeMember> ();
    public override IEnumerable<nint> GetGCRefs () => Array.Empty<nint> ();
}

internal class ESC_TypeInt : ESC_TypeData {
    public int RuntimeSize { get; private init; }
    public override ES_FullyQualifiedName Name { get; }

    public ES_IntSize Size { get; private init; }
    public bool Unsigned { get; private init; }

    public ESC_TypeInt (ES_FullyQualifiedName name, ES_IntSize size, bool unsigned) {
        Name = name;

        AccessModifier = ES_AccessModifier.Public;
        Flags = ESC_TypeFlag.Analyzed;

        Size = size;
        Unsigned = unsigned;
    }

    public override int GetRuntimeSize () {
        return Size switch {
            ES_IntSize.Int8 => 1,
            ES_IntSize.Int16 => 2,
            ES_IntSize.Int32 => 4,
            ES_IntSize.Int64 => 8,

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }
    public override bool IsReferenceType () => false;
    public override bool IsConstant () => false;
    public override bool IsWritable () => true;

    public override IEnumerable<ESC_TypeMember> GetMembers () => Array.Empty<ESC_TypeMember> ();
    public override IEnumerable<nint> GetGCRefs () => Array.Empty<nint> ();
}

internal class ESC_TypeFloat : ESC_TypeData {
    public int RuntimeSize { get; private init; }
    public override ES_FullyQualifiedName Name { get; }

    public ES_FloatSize Size { get; private init; }

    public ESC_TypeFloat (ES_FullyQualifiedName name, ES_FloatSize size) {
        Name = name;

        AccessModifier = ES_AccessModifier.Public;
        Flags = ESC_TypeFlag.Analyzed;

        Size = size;
    }

    public override int GetRuntimeSize () {
        return Size switch {
            ES_FloatSize.Single => 4,
            ES_FloatSize.Double => 8,

            _ => throw new NotImplementedException ("Size not implemented."),
        };
    }
    public override bool IsReferenceType () => false;
    public override bool IsConstant () => false;
    public override bool IsWritable () => true;

    public override IEnumerable<ESC_TypeMember> GetMembers () => Array.Empty<ESC_TypeMember> ();
    public override IEnumerable<nint> GetGCRefs () => Array.Empty<nint> ();
}
