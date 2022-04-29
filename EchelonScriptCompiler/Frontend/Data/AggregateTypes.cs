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

internal abstract class ESC_TypeAggregate : ESC_TypeData {
    private List<ESC_TypeMember>? membersList;

    public int RuntimeSize { get; set; }
    public override ES_FullyQualifiedName Name { get; }

    public List<ESC_TypeMember> Members => membersList ??= new ();
    public nint [] GCRefs { get; set; }

    protected ESC_TypeAggregate (ES_FullyQualifiedName name) {
        Name = name;
        GCRefs = Array.Empty<nint> ();
    }

    public override int GetRuntimeSize () => RuntimeSize;
    public override bool IsReferenceType () => false;
    public override bool IsConstant () => false;
    public override bool IsWritable () => true;

    public override IEnumerable<ESC_TypeMember> GetMembers () {
        if (membersList is null)
            return Array.Empty<ESC_TypeMember> ();

        return membersList;
    }
    public override IEnumerable<nint> GetGCRefs () => GCRefs;
}

internal class ESC_TypeStruct : ESC_TypeAggregate {
    public ESC_TypeInterface [] Interfaces { get; set; }

    public ESC_TypeStruct (ES_FullyQualifiedName name) : base (name) {
        Interfaces = Array.Empty<ESC_TypeInterface> ();
    }
}

internal class ESC_TypeClass : ESC_TypeAggregate {
    public ESC_TypeClass? BaseClass { get; set; }
    public ESC_TypeInterface [] Interfaces { get; set; }

    public ESC_TypeClass (ES_FullyQualifiedName name) : base (name) {
        BaseClass = null;
        Interfaces = Array.Empty<ESC_TypeInterface> ();
    }
}

internal unsafe class ESC_TypeInterface : ESC_TypeData {
    private List<ESC_TypeMember>? membersList;

    public override ES_FullyQualifiedName Name { get; }

    public ESC_TypeClass? BaseType { get; set; }
    public ESC_TypeInterface [] ParentInterfaces { get; set; }
    public List<ESC_TypeMember> Members => membersList ??= new ();

    public ESC_TypeInterface (ES_FullyQualifiedName name) {
        Name = name;

        BaseType = null;
        ParentInterfaces = Array.Empty<ESC_TypeInterface> ();
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
