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
using System.Diagnostics;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.Frontend.Data;

internal class ESC_TypeEnum : ESC_TypeData {
    private List<ESC_TypeMember>? membersList;

    public int RuntimeSize { get; set; }
    public override ES_FullyQualifiedName Name { get; }

    public ESC_TypeData? BaseType { get; set; }
    public List<ESC_TypeMember> Members => membersList ??= new ();

    public ESC_TypeEnum (ES_FullyQualifiedName name, ESC_TypeData? baseType) {
        Debug.Assert (
            baseType is ESC_TypeInt ||
            baseType is ESC_TypeFloat
        );

        Name = name;

        BaseType = baseType;
    }

    public override int GetRuntimeSize () => BaseType?.GetRuntimeSize () ?? 0;
    public override bool IsReferenceType () => false;
    public override bool IsConstant () => false;
    public override bool IsWritable () => true;

    public override IEnumerable<ESC_TypeMember> GetMembers () {
        if (membersList is null)
            return Array.Empty<ESC_TypeMember> ();

        return membersList;
    }
    public override IEnumerable<nint> GetGCRefs () => Array.Empty<nint> ();
}

internal unsafe class ESC_TypeReference : ESC_TypeData {
    public override ES_FullyQualifiedName Name { get; }

    public ESC_TypeRef PointedType { get; private init; }

    public ESC_TypeReference (ES_FullyQualifiedName name, ESC_TypeRef pointedType) {
        Name = name;

        AccessModifier = ES_AccessModifier.Public;
        Flags = ESC_TypeFlag.Analyzed;

        PointedType = pointedType;
    }

    public override int GetRuntimeSize () => sizeof (void*);
    public override bool IsReferenceType () => true;
    public override bool IsConstant () => false;
    public override bool IsWritable () => true;

    public override IEnumerable<ESC_TypeMember> GetMembers () => Array.Empty<ESC_TypeMember> ();
    public override IEnumerable<nint> GetGCRefs () => simpleRef;
}

internal unsafe class ESC_TypeArray : ESC_TypeData {
    private List<ESC_TypeMember>? membersList;
    public override ES_FullyQualifiedName Name { get; }

    public ESC_TypeRef ElementType { get; private init; }
    public int Rank { get; private init; }
    public List<ESC_TypeMember> Members => membersList ??= new ();

    public ESC_TypeArray (ES_FullyQualifiedName name, ESC_TypeRef elemType, int rank) {
        Debug.Assert (rank > 0);

        Name = name;

        AccessModifier = ES_AccessModifier.Public;

        ElementType = elemType;
        Rank = rank;
    }

    public override int GetRuntimeSize () => sizeof (void*);
    public override bool IsReferenceType () => true;
    public override bool IsConstant () => false;
    public override bool IsWritable () => true;

    public override IEnumerable<ESC_TypeMember> GetMembers () {
        if (membersList is null)
            return Array.Empty<ESC_TypeMember> ();

        return membersList;
    }
    public override IEnumerable<nint> GetGCRefs () => simpleRef;
}
