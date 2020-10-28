/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
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

    public struct ES_AstNodeBounds {
        public int StartPos;
        public int EndPos;

        public ES_AstNodeBounds (int start, int end) {
            StartPos = start;
            EndPos = end;
        }
    }

    public abstract class ES_AstNode {
        public abstract ES_AstNodeBounds NodeBounds { get; }

        public ES_AstNode (int nothing) { }
    }

    public class ES_AbstractSyntaxTree : ES_AstNode {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public bool Valid;
        public ES_AstImportStatement [] ImportStatements;
        public ES_AstTypeAlias [] TypeAliases;
        public ES_AstNamespace [] Namespaces;

        public ES_AbstractSyntaxTree (
            ES_AstImportStatement [] imports, ES_AstTypeAlias [] aliases, ES_AstNamespace [] namespaces,
            ES_AstNodeBounds bounds
        ) : base (1) {
            ImportStatements = imports;
            TypeAliases = aliases;
            Namespaces = namespaces;

            this.bounds = bounds;
        }
    }

    public class ES_AstNamespace : ES_AstNode {
        public override ES_AstNodeBounds NodeBounds { get => bounds; }
        protected ES_AstNodeBounds bounds;

        public ES_AstDottableIdentifier NamespaceName;
        public ES_AstNode [] Contents;

        public ES_AstNamespace (
            ES_AstDottableIdentifier name, ES_AstNode [] contents,
            EchelonScriptToken namespaceTk, EchelonScriptToken closeBraceTk
        ) : base (1) {
            NamespaceName = name;
            Contents = contents;

            bounds = new ES_AstNodeBounds {
                StartPos = namespaceTk.TextStartPos,
                EndPos = closeBraceTk.TextEndPos,
            };
        }
    }

    public class ES_AstDottableIdentifier : ES_AstNode {
        public override ES_AstNodeBounds NodeBounds {
            get {
                if (Parts.Length == 0)
                    return new ES_AstNodeBounds ();

                return new ES_AstNodeBounds {
                    StartPos = Parts [0].TextStartPos,
                    EndPos = (Parts [^1].TextEndPos),
                };
            }
        }

        public readonly EchelonScriptToken [] Parts;

        public ES_AstDottableIdentifier (EchelonScriptToken [] parts) : base (1) {
            Parts = parts;
        }

        #region ================== Instance methods

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

        #endregion
    }

    #region Type declaration

    public abstract class ES_AstTypeDeclaration : ES_AstNode {
        public ES_AstTypeDeclaration (int nothing) : base (1) { }
    }

    public class ES_AstTypeDeclaration_TypeName : ES_AstTypeDeclaration {
        public override ES_AstNodeBounds NodeBounds => TypeName.NodeBounds;

        public ES_AstDottableIdentifier TypeName;

        public ES_AstTypeDeclaration_TypeName (ES_AstDottableIdentifier name) : base (1) {
            TypeName = name;
        }

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

        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public DeclType Type;
        public ES_AstTypeDeclaration Inner;

        public ES_AstTypeDeclaration_Basic (DeclType type, ES_AstTypeDeclaration inner, ES_AstNodeBounds bounds) : base (1) {
            Type = type;
            Inner = inner;

            this.bounds = bounds;
        }

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
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstTypeDeclaration Inner;
        public ES_AstExpression [] Dimensions;

        public ES_AstTypeDeclaration_Array (
            ES_AstTypeDeclaration inner, ES_AstExpression [] dims,
            EchelonScriptToken closeBracketToken
        ) : base (1) {
            Inner = inner;
            Dimensions = dims;

            bounds = new ES_AstNodeBounds (inner.NodeBounds.StartPos, closeBracketToken.TextEndPos);
        }

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

    public abstract class ES_AstAggregateDefinition : ES_AstNode {
        public ES_AccessModifier AccessModifier;
        public ES_AstNode [] Contents;

        public ES_AstAggregateDefinition (ES_AccessModifier accessMod, ES_AstNode [] contents) : base (1) {
            AccessModifier = accessMod;
            Contents = contents;
        }
    }

    public class ES_AstClassDefinition : ES_AstAggregateDefinition {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public EchelonScriptToken? DocComment;
        public bool Static;

        public EchelonScriptToken Name;
        public ES_AstDottableIdentifier [] InheritanceList;

        public ES_AstClassDefinition (
            EchelonScriptToken? docCom, ES_AccessModifier accessMod, bool staticMod,
            EchelonScriptToken name, ES_AstDottableIdentifier [] inheritance, ES_AstNode [] contents,
            EchelonScriptToken startToken, EchelonScriptToken endToken
        ) : base (accessMod, contents) {
            DocComment = docCom;
            Static = staticMod;

            Name = name;
            InheritanceList = inheritance;

            bounds = new ES_AstNodeBounds {
                StartPos = startToken.TextStartPos,
                EndPos = endToken.TextEndPos,
            };
        }
    }

    public class ES_AstStructDefinition : ES_AstAggregateDefinition {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public EchelonScriptToken? DocComment;
        public bool Static;

        public EchelonScriptToken Name;
        public ES_AstDottableIdentifier [] InterfacesList;

        public ES_AstStructDefinition (
            EchelonScriptToken? docCom, ES_AccessModifier accessMod, bool staticMod,
            EchelonScriptToken name, ES_AstDottableIdentifier [] interfaces, ES_AstNode [] contents,
            EchelonScriptToken startToken, EchelonScriptToken endToken
        ) : base (accessMod, contents) {
            DocComment = docCom;
            Static = staticMod;

            Name = name;
            InterfacesList = interfaces;

            bounds = new ES_AstNodeBounds {
                StartPos = startToken.TextStartPos,
                EndPos = endToken.TextEndPos,
            };
        }
    }

    public class ES_AstMemberVarDefinition : ES_AstNode {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AccessModifier AccessModifier;
        public EchelonScriptToken? DocComment;

        public bool Static;

        public EchelonScriptToken Name;
        public ES_AstTypeDeclaration ValueType;
        public ES_AstExpression InitializationExpression;

        public ES_AstMemberVarDefinition (
            ES_AccessModifier accessMod, EchelonScriptToken? docCom, bool staticMod,
            EchelonScriptToken name, ES_AstTypeDeclaration valueType, ES_AstExpression initExpr,
            EchelonScriptToken semicolonTk
        ) : base (1) {
            AccessModifier = accessMod;
            DocComment = docCom;

            Static = staticMod;

            Name = name;
            ValueType = valueType;
            InitializationExpression = initExpr;

            bounds = new ES_AstNodeBounds (valueType.NodeBounds.StartPos, semicolonTk.TextEndPos);
        }
    }

    #endregion

    public class ES_AstEnumDefinition : ES_AstNode {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AccessModifier AccessModifier;
        public EchelonScriptToken? DocComment;

        public EchelonScriptToken Name;
        public EchelonScriptToken? BaseType;

        public (EchelonScriptToken, ES_AstExpression) [] MembersList;

        public ES_AstEnumDefinition (
            ES_AccessModifier accessMod, EchelonScriptToken? docCom,
            EchelonScriptToken name, EchelonScriptToken? baseType, (EchelonScriptToken, ES_AstExpression) [] membersList,
            EchelonScriptToken startTk, EchelonScriptToken closeBraceTk
        ) : base (1) {
            AccessModifier = accessMod;
            DocComment = docCom;

            Name = name;
            BaseType = baseType;

            MembersList = membersList;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, closeBraceTk.TextEndPos);
        }
    }

    #region Functions

    public class ES_AstFunctionDefinition : ES_AstNode {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AccessModifier AccessModifier;
        public EchelonScriptToken? DocComment;

        public bool Static;
        public bool Const;

        public EchelonScriptToken Name;
        public ES_AstTypeDeclaration ReturnType;
        public ES_AstFunctionArgumentDefinition [] ArgumentsList;

        public ES_AstStatement [] StatementsList;

        public ES_AstFunctionDefinition (
            ES_AccessModifier accessMod, EchelonScriptToken? docCom, bool staticMod, bool constMod,
            EchelonScriptToken name, ES_AstTypeDeclaration retType, ES_AstFunctionArgumentDefinition [] argsList,
            ES_AstStatement [] statements,
            EchelonScriptToken closeBraceTk
        ) : base (1) {
            AccessModifier = accessMod;
            DocComment = docCom;

            Static = staticMod;
            Const = constMod;

            Name = name;
            ReturnType = retType;
            ArgumentsList = argsList;

            StatementsList = statements;

            bounds = new ES_AstNodeBounds (retType.NodeBounds.StartPos, closeBraceTk.TextEndPos);
        }
    }

    public class ES_AstFunctionArgumentDefinition : ES_AstNode {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_ArgumentType ArgType;
        public ES_AstTypeDeclaration ValueType;
        public EchelonScriptToken Name;
        public ES_AstExpression DefaultExpression;

        public ES_AstFunctionArgumentDefinition (
            ES_ArgumentType argType, ES_AstTypeDeclaration valueType, EchelonScriptToken name, ES_AstExpression defaultExpr,
            EchelonScriptToken startTk
        ) : base (1) {
            ArgType = argType;
            ValueType = valueType;
            Name = name;
            DefaultExpression = defaultExpr;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, defaultExpr?.NodeBounds.EndPos ?? name.TextEndPos);
        }
    }

    #endregion

    #region Statements

    public abstract class ES_AstStatement : ES_AstNode {
        public ES_AstStatement (int nothing) : base (1) { }
    }

    public class ES_AstLabeledStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = LabelName.TextStartPos,
                    EndPos = Statement?.NodeBounds.EndPos ?? LabelName.TextEndPos,
                };
            }
        }

        public EchelonScriptToken LabelName;
        public ES_AstStatement Statement;

        public ES_AstLabeledStatement (EchelonScriptToken label, ES_AstStatement statement) : base (1) {
            LabelName = label;
            Statement = statement;
        }
    }

    public class ES_AstBlockStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstStatement [] Statements;

        public ES_AstBlockStatement (
            ES_AstStatement [] statements, EchelonScriptToken openTk, EchelonScriptToken closeTk
        ) : base (1) {
            Statements = statements;

            bounds = new ES_AstNodeBounds (openTk.TextStartPos, closeTk.TextEndPos);
        }
    }

    public class ES_AstEmptyStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds
            => new ES_AstNodeBounds (Semicolon.TextStartPos, Semicolon.TextEndPos);

        protected EchelonScriptToken Semicolon;

        public ES_AstEmptyStatement (EchelonScriptToken semicolon) : base (1) {
            Semicolon = semicolon;
        }
    }

    #region Symbol definition

    public class ES_AstImportStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstDottableIdentifier NamespaceName;
        public EchelonScriptToken [] ImportedNames;

        public ES_AstImportStatement (
            ES_AstDottableIdentifier name, EchelonScriptToken [] importedNames,
            EchelonScriptToken startTk, EchelonScriptToken endTk
        ) : base (1) {
            NamespaceName = name;
            ImportedNames = importedNames;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, endTk.TextEndPos);
        }
    }

    public class ES_AstTypeAlias : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public EchelonScriptToken AliasName;
        public ES_AstDottableIdentifier OriginalName;

        public ES_AstTypeAlias (
            EchelonScriptToken aliasName, ES_AstDottableIdentifier origName,
            EchelonScriptToken startTk, EchelonScriptToken endTk
        ) : base (1) {
            AliasName = aliasName;
            OriginalName = origName;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, endTk.TextEndPos);
        }
    }

    public class ES_AstLocalVarDefinition : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public bool UsingVar;
        public ES_AstTypeDeclaration ValueType;

        public (EchelonScriptToken Name, ES_AstExpression InitializationExpression) [] Variables;

        public ES_AstLocalVarDefinition (
            bool usingVar, ES_AstTypeDeclaration valType, (EchelonScriptToken, ES_AstExpression) [] varsList,
            EchelonScriptToken varStartTk, EchelonScriptToken? endTk
        ) : base (1) {
            UsingVar = usingVar;
            ValueType = valType;

            Variables = varsList;

            int endPos;
            if (endTk != null)
                endPos = endTk.Value.TextEndPos;
            else if (Variables != null && Variables.Length > 0) {
                var lastVar = Variables [^0];

                endPos = lastVar.InitializationExpression?.NodeBounds.EndPos ?? lastVar.Name.TextEndPos;
            } else
                throw new Exception ();

            bounds = new ES_AstNodeBounds (varStartTk.TextStartPos, endPos);
        }
    }

    #endregion

    #region Jumps

    public class ES_AstConditionalStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression ConditionExpression;
        public ES_AstStatement ThenStatement;
        public ES_AstStatement ElseStatement;

        public ES_AstConditionalStatement (
            ES_AstExpression condExpr, ES_AstStatement thenStatement, ES_AstStatement elseStatement,
            EchelonScriptToken startTk
        ) : base (1) {
            ConditionExpression = condExpr;
            ThenStatement = thenStatement;
            ElseStatement = elseStatement;

            int endPos = elseStatement?.NodeBounds.EndPos ?? thenStatement.NodeBounds.EndPos;
            bounds = new ES_AstNodeBounds (startTk.TextStartPos, endPos);
        }
    }

    public class ES_AstSwitchStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression ValueExpression;
        public (ES_AstExpression [] Expressions, ES_AstStatement [] StatementsBlock) [] Sections;

        public ES_AstSwitchStatement (
            ES_AstExpression valExpr, (ES_AstExpression [], ES_AstStatement []) [] sectionsList,
            EchelonScriptToken switchStartTk, EchelonScriptToken closeBraceTk
        ) : base (1) {
            ValueExpression = valExpr;
            Sections = sectionsList;

            bounds = new ES_AstNodeBounds (switchStartTk.TextStartPos, closeBraceTk.TextEndPos);
        }
    }

    public class ES_AstBreakStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public EchelonScriptToken? LabelName;

        public ES_AstBreakStatement (
            EchelonScriptToken? label, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
        ) : base (1) {
            LabelName = label;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, semicolonTk.TextEndPos);
        }
    }

    public class ES_AstContinueStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public EchelonScriptToken? LabelName;

        public ES_AstContinueStatement (
            EchelonScriptToken? label, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
        ) : base (1) {
            LabelName = label;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, semicolonTk.TextEndPos);
        }
    }

    public class ES_AstGotoLabelStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public EchelonScriptToken LabelName;

        public ES_AstGotoLabelStatement (
            EchelonScriptToken label, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
        ) : base (1) {
            LabelName = label;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, semicolonTk.TextEndPos);
        }
    }

    public class ES_AstGotoCaseStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression CaseExpression;

        public ES_AstGotoCaseStatement (
            ES_AstExpression caseExpr, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
        ) : base (1) {
            CaseExpression = caseExpr;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, semicolonTk.TextEndPos);
        }
    }

    public class ES_AstReturnStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression ReturnExpression;

        public ES_AstReturnStatement (
            ES_AstExpression retExpr, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
        ) : base (1) {
            ReturnExpression = retExpr;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, semicolonTk.TextEndPos);
        }
    }

    #endregion

    #region Loops

    public class ES_AstLoopStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstStatement InitializationStatement;
        public ES_AstExpression ConditionExpression;
        public ES_AstExpression [] IterationExpressions;

        public ES_AstStatement LoopBody;

        public ES_AstLoopStatement (
            ES_AstStatement initStatement, ES_AstExpression condExpr, ES_AstExpression [] iterExprList,
            ES_AstStatement loopBody,
            EchelonScriptToken startToken
        ) : base (1) {
            InitializationStatement = initStatement;
            ConditionExpression = condExpr;
            IterationExpressions = iterExprList;

            LoopBody = loopBody;

            bounds = new ES_AstNodeBounds (startToken.TextStartPos, loopBody.NodeBounds.EndPos);
        }
    }

    #endregion

    public class ES_AstExpressionStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression Expression;

        public ES_AstExpressionStatement (ES_AstExpression expr, EchelonScriptToken semicolonTk) : base (1) {
            Expression = expr;

            bounds = new ES_AstNodeBounds (expr?.NodeBounds.StartPos ?? semicolonTk.TextStartPos, semicolonTk.TextEndPos);
        }
    }

    public class ES_AstExpressionListStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds {
            get {
                ES_AstExpression firstExpr = null;
                ES_AstExpression lastExpr = null;

                for (int i = 0; i < Expressions.Length; i++) {
                    if (Expressions [i] != null) {
                        firstExpr = Expressions [i];
                        break;
                    }
                }
                for (int i = Expressions.Length - 1; i >= 0; i--) {
                    if (Expressions [i] != null) {
                        lastExpr = Expressions [i];
                        break;
                    }
                }

                if (firstExpr == null || lastExpr == null)
                    return new ES_AstNodeBounds ();

                return new ES_AstNodeBounds () {
                    StartPos = firstExpr.NodeBounds.StartPos,
                    EndPos = lastExpr.NodeBounds.EndPos,
                };
            }
        }

        public ES_AstExpression [] Expressions;

        public ES_AstExpressionListStatement (ES_AstExpression [] exprList) : base (1) {
            Expressions = exprList;
        }
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

        TAG_AssignExpr_Start,

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

        TAG_AssignExpr_End

        #endregion
    }

    public enum SimpleUnaryExprType {
        Positive,
        Negative,
        LogicalNot,
        BitNot,
    }

    public abstract class ES_AstExpression : ES_AstNode {
        public ES_AstExpression (int nothing) : base (1) { }
    }

    public abstract class ES_AstBinaryExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = Left?.NodeBounds.StartPos ?? Right?.NodeBounds.StartPos ?? 0,
                    EndPos = Left?.NodeBounds.EndPos ?? Right?.NodeBounds.EndPos ?? 0,
                };
            }
        }

        public ES_AstExpression Left;
        public ES_AstExpression Right;

        public ES_AstBinaryExpression (ES_AstExpression left, ES_AstExpression right) : base (1) {
            Left = left;
            Right = right;
        }
    }

    #region Primary expressions

    public class ES_AstParenthesisExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression Inner;

        public ES_AstParenthesisExpression (
            ES_AstExpression inner,
            EchelonScriptToken openTk, EchelonScriptToken closeTk
        ) : base (1) {
            Inner = inner;

            bounds = new ES_AstNodeBounds (openTk.TextStartPos, closeTk.TextEndPos);
        }
    }

    public class ES_AstIntegerLiteralExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = Token.TextStartPos,
                    EndPos = Token.TextEndPos,
                };
            }
        }

        public readonly bool Unsigned;
        public readonly bool Long;
        public readonly ulong Value;

        public readonly EchelonScriptToken Token;

        public ES_AstIntegerLiteralExpression (bool isUnsigned, bool isLong, ulong value, EchelonScriptToken tk) : base (1) {
            Unsigned = isUnsigned;
            Long = isLong;
            Value = value;

            Token = tk;
        }
    }

    public class ES_AstFloatLiteralExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = Token.TextStartPos,
                    EndPos = Token.TextEndPos,
                };
            }
        }

        public readonly bool IsFloat;
        public readonly float ValueFloat;
        public readonly double ValueDouble;

        public readonly EchelonScriptToken Token;

        public ES_AstFloatLiteralExpression (float value, EchelonScriptToken tk) : base (1) {
            IsFloat = true;
            ValueFloat = value;

            Token = tk;
        }

        public ES_AstFloatLiteralExpression (double value, EchelonScriptToken tk) : base (1) {
            IsFloat = false;
            ValueDouble = value;

            Token = tk;
        }
    }

    public class ES_AstStringLiteralExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = Token.TextStartPos,
                    EndPos = Token.TextEndPos,
                };
            }
        }

        public readonly bool Verbatim;
        public readonly string ValueUTF16;
        public readonly int [] ValueUTF32;

        public readonly EchelonScriptToken Token;

        public ES_AstStringLiteralExpression (bool verbatim, EchelonScriptToken tk) : base (1) {
            Verbatim = verbatim;
            ValueUTF16 = tk.DecodedStringUTF16;
            ValueUTF32 = tk.DecodedStringUTF32;

            Token = tk;
        }
    }

    public class ES_AstCharLiteralExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = Token.TextStartPos,
                    EndPos = Token.TextEndPos,
                };
            }
        }

        public readonly int Value;
        public readonly EchelonScriptToken Token;

        public ES_AstCharLiteralExpression (int value, EchelonScriptToken tk) : base (1) {
            Value = value;

            Token = tk;
        }
    }

    public class ES_AstNameExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = Value.TextStartPos,
                    EndPos = Value.TextEndPos,
                };
            }
        }

        public readonly EchelonScriptToken Value;

        public ES_AstNameExpression (EchelonScriptToken value) : base (1) {
            Value = value;
        }
    }

    public class ES_AstFunctionCallExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression FunctionExpression;
        public ES_AstFunctionCallArgument [] Arguments;

        public ES_AstFunctionCallExpression (
            ES_AstExpression innerExpr, ES_AstFunctionCallArgument [] args,
            EchelonScriptToken closeParenTk
        ) : base (1) {
            FunctionExpression = innerExpr;
            Arguments = args;

            bounds = new ES_AstNodeBounds (innerExpr.NodeBounds.StartPos, closeParenTk.TextEndPos);
        }
    }

    public class ES_AstFunctionCallArgument : ES_AstNode {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_ArgumentType ArgType;
        public ES_AstExpression ValueExpression;

        public ES_AstFunctionCallArgument (
            ES_ArgumentType type, ES_AstExpression valExpr,
            EchelonScriptToken startTk
        ) : base (1) {
            ArgType = type;
            ValueExpression = valExpr;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, valExpr?.NodeBounds.EndPos ?? startTk.TextEndPos);
        }
    }

    public class ES_AstIndexingExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression IndexedExpression;
        public ES_AstExpression [] RankExpressions;

        public ES_AstIndexingExpression (
            ES_AstExpression indexedExpr, ES_AstExpression [] ranks, EchelonScriptToken closeBracketTk
        ) : base (1) {
            IndexedExpression = indexedExpr;
            RankExpressions = ranks;

            bounds = new ES_AstNodeBounds (indexedExpr.NodeBounds.StartPos, closeBracketTk.TextEndPos);
        }
    }

    public class ES_AstNewExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstTypeDeclaration TypeDeclaration;
        public ES_AstFunctionCallArgument [] Arguments;

        public ES_AstNewExpression (
            ES_AstTypeDeclaration typeDecl, ES_AstFunctionCallArgument [] args,
            EchelonScriptToken newStartTk, EchelonScriptToken closeParenTk
        ) : base (1) {
            TypeDeclaration = typeDecl;
            Arguments = args;

            bounds = new ES_AstNodeBounds (newStartTk.TextStartPos, closeParenTk.TextEndPos);
        }
    }

    #endregion

    public class ES_AstIncDecExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                if (Postfix) {
                    return new ES_AstNodeBounds {
                        StartPos = Inner?.NodeBounds.StartPos ?? OperatorToken.TextStartPos,
                        EndPos = OperatorToken.TextEndPos,
                    };
                } else {
                    return new ES_AstNodeBounds {
                        StartPos = OperatorToken.TextStartPos,
                        EndPos = Inner?.NodeBounds.EndPos ?? OperatorToken.TextEndPos,
                    };
                }
            }
        }

        public readonly EchelonScriptToken OperatorToken;
        public readonly bool Decrement;
        public readonly bool Postfix;

        public ES_AstExpression Inner;

        public ES_AstIncDecExpression (
            EchelonScriptToken opToken,
            bool decrement, bool postfix, ES_AstExpression inner
        ) : base (1) {
            OperatorToken = opToken;
            Decrement = decrement;
            Postfix = postfix;

            Inner = inner;
        }
    }

    #region Unary expressions

    public class ES_AstSimpleUnaryExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = OperatorToken.TextStartPos,
                    EndPos = Inner?.NodeBounds.EndPos ?? OperatorToken.TextEndPos,
                };
            }
        }

        public readonly EchelonScriptToken OperatorToken;
        public readonly SimpleUnaryExprType ExpressionType;
        public ES_AstExpression Inner;

        public ES_AstSimpleUnaryExpression (
            EchelonScriptToken opToken, SimpleUnaryExprType exprType,
            ES_AstExpression inner
        ) : base (1) {
            OperatorToken = opToken;
            ExpressionType = exprType;
            Inner = inner;
        }
    }

    public class ES_AstCastExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstTypeDeclaration DestinationType;
        public ES_AstExpression InnerExpression;

        public ES_AstCastExpression (
            ES_AstTypeDeclaration destType, ES_AstExpression innerExpr,
            EchelonScriptToken castStartTk, EchelonScriptToken closeParenTk
        ) : base (1) {
            DestinationType = destType;
            InnerExpression = innerExpr;

            bounds = new ES_AstNodeBounds (castStartTk.TextStartPos, closeParenTk.TextEndPos);
        }
    }

    #endregion

    public class ES_AstSimpleBinaryExpression : ES_AstBinaryExpression {
        public readonly SimpleBinaryExprType ExpressionType;
        public readonly EchelonScriptToken OperatorToken;

        public ES_AstSimpleBinaryExpression (
            SimpleBinaryExprType exprType, ES_AstExpression left, ES_AstExpression right,
            EchelonScriptToken opToken
        ) : base (left, right) {
            ExpressionType = exprType;
            OperatorToken = opToken;
        }
    }

    #endregion
}
