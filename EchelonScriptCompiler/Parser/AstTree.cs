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
    public enum ES_ArgumentType {
        /// <summary>Passed normally.</summary>
        Normal,
        /// <summary>Passed by reference.</summary>
        Ref,
        /// <summary>Passed as const.</summary>
        In,
        /// <summary>Passed by reference, and requires setting before returning.</summary>
        Out,
    }

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
            str [innerStr.Length] = ' ';
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
        public ES_AstFunctionArgumentDefinition [] ArgumentsList;

        public ES_AstStatement [] StatementsList;

    }

    public class ES_AstFunctionArgumentDefinition : ES_AstTreeNode {
        public ES_ArgumentType ArgType;
        public ES_AstTypeDeclaration ValueType;
        public EchelonScriptToken Name;
        public ES_AstExpression DefaultExpression;
    }

    #endregion

    #region Statements

    public class ES_AstStatement : ES_AstTreeNode {
    }

    public class ES_AstLabeledStatement : ES_AstStatement {
        public EchelonScriptToken LabelName;
        public ES_AstStatement Statement;
    }

    public class ES_AstBlockStatement : ES_AstStatement {
        public ES_AstStatement [] Statements;
    }

    #region Symbol definition

    public class ES_AstImportStatement : ES_AstStatement {
        public ES_AstDottableIdentifier NamespaceName;
        public EchelonScriptToken [] ImportedNames;
    }

    public class ES_AstTypeAlias : ES_AstStatement {
        public EchelonScriptToken AliasName;
        public ES_AstDottableIdentifier OriginalName;
    }

    #endregion

    public class ES_AstLocalVarDefinition : ES_AstStatement {
        public bool UsingVar;
        public ES_AstTypeDeclaration ValueType;

        public (EchelonScriptToken Name, ES_AstExpression InitializationExpression) [] Variables;
    }

    #region Jump statements

    public class ES_AstConditionalStatement : ES_AstStatement {
        public ES_AstExpression ConditionExpression;
        public ES_AstStatement ThenStatement;
        public ES_AstStatement ElseStatement;
    }

    public class ES_AstSwitchStatement : ES_AstStatement {
        public ES_AstExpression ValueExpression;

        public (ES_AstExpression [] Expressions, ES_AstStatement [] StatementsBlock) [] Sections;
    }

    public class ES_AstBreakStatement : ES_AstStatement {
        public EchelonScriptToken? LabelName;
    }

    public class ES_AstContinueStatement : ES_AstStatement {
        public EchelonScriptToken? LabelName;
    }

    public class ES_AstGotoLabelStatement : ES_AstStatement {
        public EchelonScriptToken LabelName;
    }

    public class ES_AstGotoCaseStatement : ES_AstStatement {
        public ES_AstExpression CaseExpression;
    }

    public class ES_AstReturnStatement : ES_AstStatement {
        public ES_AstExpression ReturnExpression;
    }

    #endregion

    #region Loops

    public class ES_AstLoopStatement : ES_AstStatement {
        public ES_AstStatement InitializationStatement;
        public ES_AstExpression ConditionExpression;
        public ES_AstExpression [] IterationExpressions;

        public ES_AstStatement LoopBody;
    }

    #endregion

    public class ES_AstExpressionStatement : ES_AstStatement {
        public ES_AstExpression Expression;
    }

    public class ES_AstExpressionListStatement : ES_AstStatement {
        public ES_AstExpression [] Expressions;
    }

    #endregion

    #region Expressions

    public enum SimpleBinaryExprType {
        MemberAccess,

        Concatenation,

        Power,

        Multiply,
        Divide,
        Modulo,

        Add,
        Subtract,

        ShiftLeft,
        ShiftRight,
        ShiftRightUnsigned,

        LesserThan,
        GreaterThan,
        LesserThanEqual,
        GreaterThanEqual,

        Equals,
        NotEquals,

        BitAnd,
        BitXor,
        BitOr,

        LogicalAnd,
        LogicalOr,

        #region Assignment

        Assign,

        AssignAdd,
        AssignSubtract,

        AssignMultiply,
        AssignDivide,
        AssignModulo,
        AssignPower,

        AssignBitAnd,
        AssignBitOr,
        AssignXor,

        AssignTilde,

        AssignShiftLeft,
        AssignShiftRight,
        AssignShiftRightUnsigned,

        #endregion
    }

    public enum SimpleUnaryExprType {
        Positive,
        Negative,
        LogicalNot,
        BitNot,
    }

    public abstract class ES_AstExpression : ES_AstTreeNode {
    }

    public abstract class ES_AstBinaryExpression : ES_AstExpression {
        public ES_AstExpression Left;
        public ES_AstExpression Right;
    }

    #region Primary expressions

    public class ES_AstParenthesisExpression : ES_AstExpression {
        public ES_AstExpression Inner;
    }

    public class ES_AstIntegerLiteralExpression : ES_AstExpression {
        public bool Unsigned;
        public bool Long;
        public ulong Value;

        public EchelonScriptToken Token;
    }

    public class ES_AstFloatLiteralExpression : ES_AstExpression {
        public bool IsFloat;
        public float ValueFloat;
        public double ValueDouble;

        public EchelonScriptToken Token;
    }

    public class ES_AstStringLiteralExpression : ES_AstExpression {
        public bool Verbatim;
        public string ValueUTF16;
        public int [] ValueUTF32;

        public EchelonScriptToken Token;
    }

    public class ES_AstCharLiteralExpression : ES_AstExpression {
        public int Value;

        public EchelonScriptToken Token;
    }

    public class ES_AstNameExpression : ES_AstExpression {
        public EchelonScriptToken Value;
    }

    public class ES_AstFunctionCallExpression : ES_AstExpression {
        public ES_AstExpression FunctionExpression;

        public ES_AstFunctionCallArgument [] Arguments;
    }

    public class ES_AstFunctionCallArgument : ES_AstTreeNode {
        public ES_ArgumentType ArgType;
        public ES_AstExpression ValueExpression;
    }

    public class ES_AstIndexingExpression : ES_AstExpression {
        public ES_AstExpression IndexedExpression;

        public ES_AstExpression [] RankExpressions;
    }

    public class ES_AstNewExpression : ES_AstExpression {
        public ES_AstTypeDeclaration TypeDeclaration;

        public ES_AstFunctionCallArgument [] Arguments;
    }

    #endregion

    public class ES_AstIncDecExpression : ES_AstExpression {
        public bool Decrement;
        public bool Postfix;

        public ES_AstExpression Inner;
    }

    #region Unary expressions

    public class ES_AstSimpleUnaryExpression : ES_AstExpression {
        public SimpleUnaryExprType ExpressionType;
        public ES_AstExpression Inner;
    }

    public class ES_AstCastExpression : ES_AstExpression {
        public ES_AstTypeDeclaration DestinationType;

        public ES_AstExpression InnerExpression;
    }

    #endregion

    public class ES_AstSimpleBinaryExpression : ES_AstBinaryExpression {
        public SimpleBinaryExprType ExpressionType;
    }

    #endregion
}
