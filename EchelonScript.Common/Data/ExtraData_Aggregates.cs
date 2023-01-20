/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Common.Utilities;

namespace EchelonScript.Common.Data.Types;

public readonly unsafe struct ES_StructInfo {
    /// <summary>The struct's fields.</summary>
    public readonly ArrayPointer<ES_FieldInfo> Fields { get; init; }
    /// <summary>The struct's instance methods.</summary>
    public readonly ArrayPointer<ES_MethodInfo> Methods { get; init; }
    /// <summary>The struct's static methods.</summary>
    public readonly ArrayPointer<ES_FunctionInfo> Functions { get; init; }
}

public readonly unsafe struct ES_ClassInfo {
    /// <summary>The class' fields.</summary>
    public readonly ArrayPointer<ES_FieldInfo> Fields { get; init; }
    /// <summary>The class' instance methods.</summary>
    public readonly ArrayPointer<ES_MethodInfo> Methods { get; init; }
    /// <summary>The class' static methods.</summary>
    public readonly ArrayPointer<ES_FunctionInfo> Functions { get; init; }
}

public readonly unsafe struct ES_FieldInfo {
    /// <summary>The field's access modifier.</summary>
    public readonly ES_AccessModifier AccessModifier { get; init; }
    /// <summary>The field's name.</summary>
    public readonly ES_Utf8String Name { get; init; }
    /// <summary>The field's constness.</summary>
    public readonly ES_Constness Constness { get; init; }
    /// <summary>The field's type.</summary>
    public readonly ES_MethodTable* Type { get; init; }
    /// <summary>The field's offset in the type.</summary>
    public readonly nint Offset { get; init; }
}

public readonly unsafe struct ES_MethodInfo {
    /// <summary>The function's name.</summary>
    public readonly ES_Utf8String Name { get; init; }
    /// <summary>The method's access modifier.</summary>
    public readonly ES_AccessModifier AccessModifier;
    /// <summary>The function's arguments.</summary>
    public readonly ArrayPointer<ES_FunctionArgumentInfo> Arguments { get; init; }
}

public readonly unsafe struct ES_FunctionInfo {
    /// <summary>The function's name.</summary>
    public readonly ES_Utf8String Name { get; init; }
    /// <summary>The method's access modifier.</summary>
    public readonly ES_AccessModifier AccessModifier;
    /// <summary>The function's arguments.</summary>
    public readonly ArrayPointer<ES_FunctionArgumentInfo> Arguments { get; init; }
}

public readonly unsafe struct ES_FunctionArgumentInfo {
    /// <summary>The argument's name.</summary>
    public readonly ES_Utf8String Name { get; init; }
}
