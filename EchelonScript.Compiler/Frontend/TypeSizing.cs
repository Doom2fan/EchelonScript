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
using ChronosLib.Pooled;
using EchelonScript.Common.Utilities;

namespace EchelonScript.Compiler.Frontend;

internal static partial class Compiler_TypeSizing {
#if false
    public static void SizeTypes (ref CompileData compileData) {
        foreach (var nmData in compileData.Namespaces.Values) {
            foreach (var type in nmData.Types.Values) {
                if (TypeSizing_AnalyzeCycles (ref compileData, type))
                    continue;

                TypeSizing_SizeType (ref compileData, type);
            }
        }
    }

    private static void TypeSizing_SizeType (ref CompileData compileData, ESC_TypeData type) {
        if (type.Flags.HasFlag (ESC_TypeFlag.Analyzed))
            return;

        var hasRefs = !type.Flags.HasFlag (ESC_TypeFlag.NoRefs);

        switch (type) {
            case ESC_TypeArray:
            case ESC_TypeReference:
            case ESC_TypeInterface: {
                type.Flags &= ~ESC_TypeFlag.NoRefs;
                type.Flags |= ESC_TypeFlag.Analyzed;
                return;
            }

            case ESC_TypeVoid:
            case ESC_TypeBool:
            case ESC_TypeInt:
            case ESC_TypeFloat:
            case ESC_TypeEnum:
                type.Flags |= ESC_TypeFlag.NoRefs;
                type.Flags |= ESC_TypeFlag.Analyzed;
                return;

            case ESC_TypePrototype:
                type.Flags |= ESC_TypeFlag.NoNew;
                type.Flags |= ESC_TypeFlag.Analyzed;
                return;

            case ESC_TypeStruct:
            case ESC_TypeClass:
                break;

            default:
                throw new NotImplementedException ("Type not implemented yet.");
        }

        using var refsList = new StructPooledList<nint> (CL_ClearMode.Auto);
        var curOffs = 0;
        foreach (var member in type.GetMembers ()) {
            switch (member) {
                case ESC_TypeMember_Field memberField: {
                    var fieldType = memberField.FieldType.Type;
                    Debug.Assert (fieldType is not null);

                    TypeSizing_SizeType (ref compileData, fieldType);

                    if (memberField.Flags.HasFlag (ESC_MemberFlags.Static))
                        continue;

                    memberField.Offset = curOffs;
                    curOffs += fieldType.GetRuntimeSize ();

                    foreach (var refOffs in fieldType.GetGCRefs ())
                        refsList.Add (memberField.Offset + refOffs);

                    hasRefs |= !fieldType.Flags.HasFlag (ESC_TypeFlag.NoRefs);
                    break;
                }

                case ESC_TypeMember_Function:
                    break;

                default:
                    throw new NotImplementedException ("Member type not implemented.");
            }
        }

        switch (type) {
            case ESC_TypeAggregate typeAggregate: {
                typeAggregate.RuntimeSize = curOffs;
                typeAggregate.GCRefs = refsList.ToArray ();
                break;
            }

            default:
                throw new NotImplementedException ("Type not implemented.");
        }

        type.Flags &= ~ESC_TypeFlag.NoRefs;
        if (!hasRefs)
            type.Flags |= ESC_TypeFlag.NoRefs;

        type.Flags |= ESC_TypeFlag.Analyzed;
    }

    private static bool TypeSizing_AnalyzeCycles (ref CompileData compileData, ESC_TypeData type) {
        switch (type) {
            case ESC_TypeStruct:
            case ESC_TypeClass:
                break;

            case ESC_TypeVoid:
            case ESC_TypeBool:
            case ESC_TypeInt:
            case ESC_TypeFloat:
            case ESC_TypePrototype:
            case ESC_TypeEnum:
            case ESC_TypeInterface:
            case ESC_TypeReference:
            case ESC_TypeArray:
                return false;

            default:
                throw new NotImplementedException ("Type not implemented yet.");
        }

        var hasCycles = false;
        var typeStack = new StructPooledList<ESC_TypeData> (CL_ClearMode.Auto);
        try {
            typeStack.Add (type);

            foreach (var member in type.GetMembers ()) {
                switch (member) {
                    case ESC_TypeMember_Field memberField: {
                        if (member.Flags.HasFlag (ESC_MemberFlags.Static))
                            continue;

                        var fieldType = memberField.FieldType.Type;
                        Debug.Assert (fieldType is not null);

                        if (TypeSizing_AnalyzeCycles_Traverse (ref compileData, fieldType, ref typeStack)) {
                            compileData.ErrorList.Add (ES_FrontendErrors.GenFieldCausesCycle (
                                member.Name.GetCharsSpan ().GetPooledString (),
                                compileData.GetNiceNameString (new (ESC_Constness.Mutable, type), true),
                                type.SourceUnit.GetCharsSpan ()
                            ));

                            hasCycles = true;
                        }

                        break;
                    }

                    case ESC_TypeMember_Function:
                        break;

                    default:
                        throw new NotImplementedException ("Member type not implemented.");
                }
            }
        } finally {
            typeStack.Dispose ();
        }

        return hasCycles;
    }

    private static bool TypeSizing_AnalyzeCycles_Traverse (ref CompileData compileData, ESC_TypeData innerType, ref StructPooledList<ESC_TypeData> typeStack) {
        switch (innerType) {
            case ESC_TypeStruct:
            case ESC_TypeClass:
                break;

            case ESC_TypeArray:
            case ESC_TypeVoid:
            case ESC_TypeBool:
            case ESC_TypeInt:
            case ESC_TypeFloat:
            case ESC_TypePrototype:
            case ESC_TypeEnum:
            case ESC_TypeInterface:
            case ESC_TypeReference:
                return false;

            default:
                throw new NotImplementedException ("Type not implemented yet.");
        }

        foreach (var type in typeStack) {
            if (innerType == type)
                return true;
        }

        typeStack.Add (innerType);

        foreach (var member in innerType.GetMembers ()) {
            switch (member) {
                case ESC_TypeMember_Field memberField: {
                    if (member.Flags.HasFlag (ESC_MemberFlags.Static))
                        continue;

                    var fieldType = memberField.FieldType.Type;
                    Debug.Assert (fieldType is not null);

                    if (TypeSizing_AnalyzeCycles_Traverse (ref compileData, fieldType, ref typeStack)) {
                        typeStack.RemoveEnd (1);
                        return true;
                    }

                    break;
                }

                case ESC_TypeMember_Function:
                    break;

                default:
                    throw new NotImplementedException ("Member type not implemented.");
            }
        }

        typeStack.RemoveEnd (1);

        return false;
    }
#endif
}
