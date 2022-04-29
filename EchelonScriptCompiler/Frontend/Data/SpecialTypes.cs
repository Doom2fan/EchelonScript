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
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.Frontend.Data;

internal class ESC_TypeUnknown : ESC_TypeOpaque {
    public ESC_TypeUnknown (ES_FullyQualifiedName name) : base (name, null) {
        AccessModifier = ES_AccessModifier.Public;
        SourceUnit = ES_Identifier.Empty;
    }

    public override int GetRuntimeSize () => 0;
    public override bool IsReferenceType () => false;
}

internal class ESC_TypeNull : ESC_TypeOpaque {
    public ESC_TypeNull (ES_FullyQualifiedName name) : base (name, null) {
        AccessModifier = ES_AccessModifier.Public;
        SourceUnit = ES_Identifier.Empty;
        Flags |= ESC_TypeFlag.NoNew;
    }

    public override int GetRuntimeSize () => 0;
    public override bool IsReferenceType () => false;
}

internal unsafe class ESC_TypePrototype : ESC_TypeData {
    public override ES_FullyQualifiedName Name { get; }

    public ESC_TypeRef ReturnType { get; init; }
    public ESC_PrototypeArg [] Arguments { get; init; }

    public ESC_TypePrototype (ES_FullyQualifiedName name, ESC_TypeRef retType, ESC_PrototypeArg [] args) {
        Name = name;

        AccessModifier = ES_AccessModifier.Public;
        Flags = ESC_TypeFlag.Analyzed;

        ReturnType = retType;
        Arguments = args;
    }

    public override int GetRuntimeSize () => sizeof (void*);
    public override bool IsReferenceType () => false;
    public override bool IsConstant () => true;
    public override bool IsWritable () => false;

    public override IEnumerable<ESC_TypeMember> GetMembers () => Array.Empty<ESC_TypeMember> ();
    public override IEnumerable<nint> GetGCRefs () => simpleRef;
}
