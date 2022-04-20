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
using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;

namespace EchelonScriptCompiler.Frontend.Data;

internal enum ESC_TypeFlag {
    None = 0,

    Analyzed = 1 << 0,
    NoNew = 1 << 2,
    NoRefs = 1 << 3,
}

internal enum ESC_Constness {
    Mutable,
    Const,
    Immutable,
}

internal static partial class ESC_Utils {
    public static ESC_Constness InheritConstness (ESC_Constness baseConst, ESC_Constness thisConst) {
        return baseConst switch {
            ESC_Constness.Mutable => thisConst,
            ESC_Constness.Const => (thisConst == ESC_Constness.Immutable) ? thisConst : baseConst,
            ESC_Constness.Immutable => ESC_Constness.Immutable,

            _ => throw new NotImplementedException ("Constness type not implemented."),
        };
    }
}

internal struct ESC_TypeRef {
    public ESC_Constness Constness { get; private init; }
    public ESC_TypeData? Type { get; private init; }

    public ESC_TypeRef (ESC_Constness constness, ESC_TypeData? type) {
        Constness = constness;
        Type = type;
    }

    #region ================== Static methods

    public static ESC_TypeRef Null (ESC_Constness constness = ESC_Constness.Mutable) => new () {
        Constness = constness,
        Type = null,
    };

    #endregion

    #region ================== Instance methods

    public ESC_TypeRef WithType (ESC_TypeData type) => new (Constness, type);
    public ESC_TypeRef WithConst (ESC_Constness constness) => new (constness, Type);
    public ESC_TypeRef WithInheritedConst (ESC_Constness constness)
        => WithConst (ESC_Utils.InheritConstness (constness, Constness));

    public bool IsConstant () => Constness != ESC_Constness.Mutable && Type?.IsConstant () == true;
    public bool IsWritable () => Constness == ESC_Constness.Mutable && Type?.IsWritable () == true;

    #region Equality

    public bool Equals (ESC_TypeRef other) => Type == other.Type && Constness == other.Constness;

    public override bool Equals (object? obj) {
        if (obj is ESC_TypeRef other)
            return Equals (other);

        return false;
    }

    public override int GetHashCode () => HashCode.Combine (Constness, Type);

    #endregion

    #endregion

    #region ================== Operators

    public static bool operator == (ESC_TypeRef lhs, ESC_TypeRef rhs) => lhs.Equals (rhs);
    public static bool operator != (ESC_TypeRef lhs, ESC_TypeRef rhs) => !lhs.Equals (rhs);

    #endregion
}

internal abstract class ESC_TypeData {
    protected static nint [] simpleRef = { 0 };

    public ES_AccessModifier AccessModifier { get; set; }
    public ESC_TypeFlag Flags { get; set; }
    public abstract ES_FullyQualifiedName Name { get; }
    public ES_Identifier SourceUnit { get; set; }

    public abstract int GetRuntimeSize ();
    public abstract bool IsReferenceType ();
    public abstract bool IsConstant ();
    public abstract bool IsWritable ();

    public abstract IEnumerable<nint> GetGCRefs ();
    public abstract IEnumerable<ESC_TypeMember> GetMembers ();

    public virtual void GetNiceName (ref StructPooledList<char> charsList, bool fullyQualified) {
        var includeNS = (
            fullyQualified &&
            !Name.NamespaceName.GetCharsSpan ().Equals (ES_Constants.GlobalsNamespace, StringComparison.Ordinal) &&
            !Name.NamespaceName.GetCharsSpan ().Equals (ES_Constants.GeneratedNamespace, StringComparison.Ordinal)
        );

        if (includeNS) {
            charsList.AddRange (Name.NamespaceName.GetCharsSpan ());
            charsList.AddRange ("::");
        }
        charsList.AddRange (Name.TypeName.GetCharsSpan ());
    }

    public PooledArray<char> GetNiceName (bool fullyQualified) {
        var charsList = new StructPooledList<char> (CL_ClearMode.Auto);

        try {
            GetNiceName (ref charsList, fullyQualified);
            return charsList.MoveToArray ();
        } finally {
            charsList.Dispose ();
        }
    }
    public string GetNiceNameString (bool fullyQualified) {
        using var chars = GetNiceName (fullyQualified);
        return chars.Span.GetPooledString ();
    }
}

public enum ESC_MemberFlags : int {
    Static = 1 << 0,
}

internal abstract class ESC_TypeMember {
    public ES_Identifier Name { get; set; }
    public ES_AccessModifier AccessModifier { get; set; }
    public ESC_MemberFlags Flags { get; set; }
}

internal class ESC_TypeMember_Field : ESC_TypeMember {
    public ESC_TypeRef FieldType { get; set; }
    public int Offset { get; set; }
}

internal class ESC_TypeMember_Function : ESC_TypeMember {
    public ESC_Function? Function { get; set; }

    internal ESC_TypeMember_Function (ES_Identifier name, ESC_Function? func = null) {
        Name = name;
        Function = func;
    }
}

internal abstract class ESC_TypeOpaque : ESC_TypeData {
    public override ES_FullyQualifiedName Name { get; }
    private nint [] gcRefs;

    public ESC_TypeOpaque (ES_FullyQualifiedName name, nint []? refs) {
        Name = name;
        gcRefs = refs ?? Array.Empty<nint> ();
    }

    public override bool IsConstant () => false;
    public override bool IsWritable () => true;

    public override IEnumerable<ESC_TypeMember> GetMembers () => Array.Empty<ESC_TypeMember> ();
    public override IEnumerable<nint> GetGCRefs () => gcRefs;
}
