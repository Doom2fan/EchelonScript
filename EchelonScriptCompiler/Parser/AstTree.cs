/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

using System;

namespace EchelonScriptCompiler.Parser {
    public abstract class ES_AstTreeNode { }

    public class ES_AstTree : ES_AstTreeNode {
        public bool Valid;

        public ES_AstImportStatement [] ImportStatements;
        public ES_AstTypeAlias [] TypeAliases;
        public ES_AstNamespace [] Namespaces;
    }

    public class ES_AstNamespace : ES_AstTreeNode {
        public ES_AstDottableIdentifier NamespaceName;

        public ES_AstTreeNode [] Contents;
    }

    public class ES_AstDottableIdentifier : ES_AstTreeNode {
        public EchelonScriptToken [] Parts;

        public int GetStringLength () {
            int strLen = 0;

            for (int i = 0; i < Parts.Length; i++) {
                if (i > 0) // Account for the period
                    strLen++;

                strLen += Parts [i].Text.Length;
            }

            return strLen;
        }

        public override string ToString () {
            Span<char> idText = stackalloc char [GetStringLength ()];

            int strLen = 0;
            for (int i = 0; i < Parts.Length; i++) {
                if (i > 0) { // Account for the period
                    idText [strLen] = '.';
                    strLen++;
                }

                var partSpan = Parts [i].Text.Span;
                partSpan.CopyTo (idText.Slice (strLen, partSpan.Length));
                strLen += partSpan.Length;
            }

            return new string (idText);
        }

        public void ToString (Span<char> idText) {
            if (idText.Length < GetStringLength ())
                throw new ArgumentException ("The specified span is not long enough to contain the identifier", nameof (idText));

            int strLen = 0;
            for (int i = 0; i < Parts.Length; i++) {
                if (i > 0) { // Account for the period
                    idText [strLen] = '.';
                    strLen++;
                }

                var partSpan = Parts [i].Text.Span;
                partSpan.CopyTo (idText.Slice (strLen, partSpan.Length));
                strLen += partSpan.Length;
            }
        }
    }

    #region Type declaration

    public abstract class ES_AstTypeDeclaration : ES_AstTreeNode { }

    public class ES_AstTypeDeclaration_TypeName : ES_AstTypeDeclaration {
        public ES_AstDottableIdentifier TypeName;

        public override string ToString () {
            return TypeName.ToString ();
        }
    }

    public class ES_AstTypeDeclaration_Basic : ES_AstTypeDeclaration {
        public enum DeclType {
            Nullable,
            Pointer,
            Const,
            Immutable,
        }

        public DeclType Type;
        public ES_AstTypeDeclaration Inner;

        public override string ToString () {
            switch (Type) {
                case DeclType.Const:
                    return $"const ({Inner})";
                case DeclType.Immutable:
                    return $"immutable ({Inner})";
                case DeclType.Pointer:
                    return $"{Inner}*";
                case DeclType.Nullable:
                    return $"{Inner}?";

                default:
                    throw new NotImplementedException ();
            }
        }
    }

    public class ES_AstTypeDeclaration_Array : ES_AstTypeDeclaration {
        public ES_AstTypeDeclaration Inner;
        public ES_AstExpression [] Dimensions;

        public override string ToString () {
            var innerStr = Inner.ToString ();

            Span<char> str = stackalloc char [innerStr.Length + 3 + Dimensions.Length];

            innerStr.AsSpan ().CopyTo (str.Slice (0, innerStr.Length));
            str [innerStr.Length    ] = ' ';
            str [innerStr.Length + 1] = '[';

            for (int i = 0; i < Dimensions.Length; i++)
                str [innerStr.Length + 1 + i] = ',';

            str [innerStr.Length + 2 + Dimensions.Length] = ']';

            return new string (str);
        }
    }

    #endregion

    #region Aggregates

    public abstract class ES_AstAggregateDefinition : ES_AstTreeNode {
        public ES_AccessModifier AccessModifier;

        public ES_AstTreeNode [] Contents;
    }

    public class ES_AstClassDefinition : ES_AstAggregateDefinition {
        public EchelonScriptToken? DocComment;
        public bool Static;

        public EchelonScriptToken Name;
        public ES_AstDottableIdentifier [] InheritanceList;
    }

    public class ES_AstStructDefinition : ES_AstAggregateDefinition {
        public EchelonScriptToken? DocComment;
        public bool Static;

        public EchelonScriptToken Name;
        public ES_AstDottableIdentifier [] InterfacesList;
    }

    public class ES_AstMemberVarDefinition : ES_AstTreeNode {
        public ES_AccessModifier AccessModifier;
        public EchelonScriptToken? DocComment;

        public bool Static;

        public EchelonScriptToken Name;
        public ES_AstTypeDeclaration ValueType;
        public ES_AstExpression InitializationExpression;
    }

    #endregion

    public class ES_AstEnumDefinition : ES_AstTreeNode {
        public ES_AccessModifier AccessModifier;
        public EchelonScriptToken? DocComment;

        public EchelonScriptToken Name;
        public EchelonScriptToken? BaseType;

        public (EchelonScriptToken, ES_AstExpression) [] MembersList;
    }

    #region Functions

    public class ES_AstFunctionDefinition : ES_AstTreeNode {
        public ES_AccessModifier AccessModifier;
        public EchelonScriptToken? DocComment;

        public bool Static;
        public bool Const;

        public EchelonScriptToken Name;
        public ES_AstTypeDeclaration ReturnType;
        public ES_AstFunctionArgument [] ArgumentsList;

        public ES_AstStatement [] StatementsList;

    }

    public class ES_AstFunctionArgument : ES_AstTreeNode {
        public enum ArgumentMode {
            /// <summary>Passed normally.</summary>
            Normal,
            /// <summary>Passed by reference.</summary>
            Ref,
            /// <summary>Passed as const.</summary>
            In,
            /// <summary>Passed by reference, and requires setting before returning.</summary>
            Out,
        }

        public ArgumentMode Mode;
        public ES_AstTypeDeclaration ValueType;
        public EchelonScriptToken Name;
        public ES_AstExpression DefaultExpression;
    }

    #endregion

    #region Expressions

    public abstract class ES_AstExpression : ES_AstTreeNode {
    }

    #endregion

    #region Statements

    public class ES_AstStatement : ES_AstTreeNode {
    }

    public class ES_AstImportStatement : ES_AstStatement {
        public ES_AstDottableIdentifier NamespaceName;
        public EchelonScriptToken [] ImportedNames;
    }

    public class ES_AstTypeAlias : ES_AstStatement {
        public EchelonScriptToken AliasName;
        public ES_AstDottableIdentifier OriginalName;
    }

    #endregion
}
