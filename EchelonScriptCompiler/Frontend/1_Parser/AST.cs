/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using ChronosLib.Pooled;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Frontend {
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
        public readonly ReadOnlyMemory<char> Source;

        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public bool Valid;
        public ES_AstImportStatement [] ImportStatements;
        public ES_AstTypeAlias [] TypeAliases;
        public ES_AstNamespace [] Namespaces;

        public ES_AbstractSyntaxTree (
            ReadOnlyMemory<char> source,
            ES_AstImportStatement [] imports, ES_AstTypeAlias [] aliases, ES_AstNamespace [] namespaces,
            ES_AstNodeBounds bounds
        ) : base (1) {
            Source = source;

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
        public ES_AstNode? [] Contents;

        public ES_AstNamespace (
            ES_AstDottableIdentifier name, ES_AstNode? [] contents,
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

            foreach (var part in Parts)
                strLen += part.Text.Length;
            strLen += Math.Max (Parts.Length - 1, 0);

            return strLen;
        }

        public PooledArray<char> ToPooledChars () {
            var str = PooledArray<char>.GetArray (GetStringLength ());

            ToString (str);

            return str;
        }

        public string ToPooledString () {
            using var str = PooledArray<char>.GetArray (GetStringLength ());

            ToString (str);

            return str.Span.GetPooledString ();
        }

        public override string ToString () {
            Span<char> idText = stackalloc char [GetStringLength ()];

            ToString (idText);

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
        public const string NullInnerName = "INVALID";

        public ES_AstTypeDeclaration (int nothing) : base (1) { }

        public abstract int GetStringLength ();

        public abstract void ToString (Span<char> chars);

        public override string ToString () {
            using var chars = PooledArray<char>.GetArray (GetStringLength ());
            ToString (chars);

            return new string (chars.Span);
        }
    }

    public unsafe class ES_AstTypeDeclaration_TypeReference : ES_AstTypeDeclaration {
        public override ES_AstNodeBounds NodeBounds => Declaration.NodeBounds;

        public ES_AstTypeDeclaration Declaration;
        public ES_TypeInfo* Reference;

        public ES_AstTypeDeclaration_TypeReference (ES_AstTypeDeclaration decl, ES_TypeInfo* typeRef) : base (1) {
            Declaration = decl;
            Reference = typeRef;
        }

        public override int GetStringLength ()
            => Declaration.GetStringLength ();

        public override void ToString (Span<char> chars)
            => Declaration.ToString (chars);
    }

    public class ES_AstTypeDeclaration_TypeName : ES_AstTypeDeclaration {
        public override ES_AstNodeBounds NodeBounds => TypeName.NodeBounds;

        public ES_AstDottableIdentifier? Namespace;
        public ES_AstDottableIdentifier TypeName;

        public ES_AstTypeDeclaration_TypeName (ES_AstDottableIdentifier name) : base (1) {
            Namespace = null;
            TypeName = name;
        }

        public ES_AstTypeDeclaration_TypeName (ES_AstDottableIdentifier nm, ES_AstDottableIdentifier name) : base (1) {
            Namespace = nm;
            TypeName = name;
        }

        public override int GetStringLength () {
            if (Namespace == null)
                return TypeName.GetStringLength ();
            else
                return Namespace.GetStringLength () + TypeName.GetStringLength () + 2;
        }

        public override void ToString (Span<char> chars) {
            if (chars.Length < GetStringLength ())
                throw new ArgumentException ("The chars span is too small.", nameof (chars));

            if (Namespace == null)
                TypeName.ToString (chars);
            else {
                int len = 0;

                Namespace.ToString (chars.Slice (len));
                len += Namespace.GetStringLength ();

                chars [len++] = ':';
                chars [len++] = ':';

                TypeName.ToString (chars.Slice (len));
            }
        }
    }

    public class ES_AstTypeDeclaration_Basic : ES_AstTypeDeclaration {
        private const string constStartStr = "const (";
        private const string immutableStartStr = "immutable (";

        public enum DeclType {
            Nullable,
            Pointer,
            Const,
            Immutable,
        }

        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public DeclType Type;
        public ES_AstTypeDeclaration? Inner;

        public ES_AstTypeDeclaration_Basic (DeclType type, ES_AstTypeDeclaration? inner, ES_AstNodeBounds bounds) : base (1) {
            Type = type;
            Inner = inner;

            this.bounds = bounds;
        }

        public override int GetStringLength () {
            int innerLen = (Inner?.GetStringLength () ?? NullInnerName.Length);
            switch (Type) {
                case DeclType.Const:
                    return constStartStr.Length + innerLen + 1;

                case DeclType.Immutable:
                    return immutableStartStr.Length + innerLen + 1;

                case DeclType.Pointer:
                case DeclType.Nullable:
                    return innerLen + 1;

                default:
                    throw new NotImplementedException ();
            }
        }

        public override void ToString (Span<char> chars) {
            if (chars.Length < GetStringLength ())
                throw new ArgumentException ("The chars array is too small.", nameof (chars));

            int innerLen = Inner?.GetStringLength () ?? NullInnerName.Length;

            switch (Type) {
                case DeclType.Const: {
                    int len = 0;
                    constStartStr.AsSpan ().CopyTo (chars);
                    len += constStartStr.Length;

                    if (Inner != null)
                        Inner!.ToString (chars.Slice (len));
                    else
                        NullInnerName.AsSpan ().CopyTo (chars.Slice (len));
                    len += innerLen;

                    chars [len] = ')';

                    break;
                }
                case DeclType.Immutable: {
                    int len = 0;
                    immutableStartStr.AsSpan ().CopyTo (chars);
                    len += immutableStartStr.Length;

                    if (Inner != null)
                        Inner!.ToString (chars.Slice (len));
                    else
                        NullInnerName.AsSpan ().CopyTo (chars.Slice (len));
                    len += innerLen;

                    chars [len] = ')';

                    break;
                }

                case DeclType.Pointer: {
                    Inner?.ToString (chars);
                    chars [^0] = '*';

                    break;
                }
                case DeclType.Nullable: {
                    Inner?.ToString (chars);
                    chars [^0] = '?';

                    break;
                }

                default:
                    throw new NotImplementedException ();
            }
        }
    }

    public class ES_AstTypeDeclaration_Array : ES_AstTypeDeclaration {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstTypeDeclaration Inner;
        public ES_AstExpression? [] Dimensions;

        public ES_AstTypeDeclaration_Array (
            ES_AstTypeDeclaration inner, ES_AstExpression? [] dims,
            EchelonScriptToken closeBracketToken
        ) : base (1) {
            Inner = inner;
            Dimensions = dims;

            bounds = new ES_AstNodeBounds (inner.NodeBounds.StartPos, closeBracketToken.TextEndPos);
        }

        public override int GetStringLength () {
            int len = Inner?.GetStringLength () ?? NullInnerName.Length;
            len++; // Space
            len++; // Opening bracket

            for (int i = 1; i < Dimensions.Length; i++)
                len++;

            len++; // Closing bracket

            return len;
        }

        public override void ToString (Span<char> chars) {
            if (chars.Length < GetStringLength ())
                throw new ArgumentException ("The chars array is too small.", nameof (chars));

            int len = 0;

            if (Inner != null)
                Inner.ToString (chars);
            else
                NullInnerName.AsSpan ().CopyTo (chars);
            len += Inner?.GetStringLength () ?? NullInnerName.Length;

            chars [len++] = ' ';
            chars [len++] = '[';

            for (int i = 1; i < Dimensions.Length; i++)
                chars [len++] = ',';

            chars [len] = ']';
        }
    }

    #endregion

    #region Aggregates

    public abstract class ES_AstAggregateDefinition : ES_AstNode {
        public ES_AccessModifier AccessModifier;
        public ES_AstNode? [] Contents;

        public EchelonScriptToken Name;

        public ES_AstAggregateDefinition (ES_AccessModifier accessMod, ES_AstNode? [] contents) : base (1) {
            AccessModifier = accessMod;
            Contents = contents;
        }
    }

    public class ES_AstClassDefinition : ES_AstAggregateDefinition {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public EchelonScriptToken? DocComment;
        public bool Static;
        public bool Abstract;

        public ES_AstTypeDeclaration_TypeName [] InheritanceList;

        public ES_AstClassDefinition (
            EchelonScriptToken? docCom, ES_AccessModifier accessMod, bool staticMod, bool abstractMod,
            EchelonScriptToken name, ES_AstTypeDeclaration_TypeName [] inheritance, ES_AstNode? [] contents,
            EchelonScriptToken startToken, EchelonScriptToken endToken
        ) : base (accessMod, contents) {
            DocComment = docCom;
            Static = staticMod;
            Abstract = abstractMod;

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

        public ES_AstTypeDeclaration_TypeName [] InterfacesList;

        public ES_AstStructDefinition (
            EchelonScriptToken? docCom, ES_AccessModifier accessMod, bool staticMod,
            EchelonScriptToken name, ES_AstTypeDeclaration_TypeName [] interfaces, ES_AstNode? [] contents,
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
        public ES_AstTypeDeclaration? ValueType;
        public ES_AstExpression? InitializationExpression;

        public ES_AstMemberVarDefinition (
            ES_AccessModifier accessMod, EchelonScriptToken? docCom, bool staticMod,
            EchelonScriptToken name, ES_AstTypeDeclaration? valueType, ES_AstExpression? initExpr,
            EchelonScriptToken semicolonTk
        ) : base (1) {
            AccessModifier = accessMod;
            DocComment = docCom;

            Static = staticMod;

            Name = name;
            ValueType = valueType;
            InitializationExpression = initExpr;

            bounds = new ES_AstNodeBounds (valueType?.NodeBounds.StartPos ?? name.TextStartPos, semicolonTk.TextEndPos);
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

        public (EchelonScriptToken, ES_AstExpression?) [] MembersList;

        public ES_AstEnumDefinition (
            ES_AccessModifier accessMod, EchelonScriptToken? docCom,
            EchelonScriptToken name, EchelonScriptToken? baseType, (EchelonScriptToken, ES_AstExpression?) [] membersList,
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
        public ES_VirtualnessModifier VirtualMod;

        public EchelonScriptToken Name;
        public ES_AstTypeDeclaration ReturnType;
        public ES_AstFunctionArgumentDefinition [] ArgumentsList;

        public bool ExpressionBody;
        public ES_AstStatement? Statement;

        public ES_AstFunctionDefinition (
            ES_AccessModifier accessMod, EchelonScriptToken? docCom, bool staticMod, bool constMod,
            ES_VirtualnessModifier virtualMod, EchelonScriptToken name, ES_AstTypeDeclaration retType,
            ES_AstFunctionArgumentDefinition [] argsList,
            bool exprBody, ES_AstStatement? statements,
            EchelonScriptToken closeBraceTk
        ) : base (1) {
            AccessModifier = accessMod;
            DocComment = docCom;

            Static = staticMod;
            Const = constMod;
            VirtualMod = virtualMod;

            Name = name;
            ReturnType = retType;
            ArgumentsList = argsList;

            ExpressionBody = exprBody;
            Statement = statements;

            bounds = new ES_AstNodeBounds (retType?.NodeBounds.StartPos ?? name.TextStartPos, closeBraceTk.TextEndPos);
        }
    }

    public class ES_AstFunctionArgumentDefinition : ES_AstNode {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_ArgumentType ArgType;
        public ES_AstTypeDeclaration? ValueType;
        public EchelonScriptToken Name;
        public ES_AstExpression? DefaultExpression;

        public ES_AstFunctionArgumentDefinition (
            ES_ArgumentType argType, ES_AstTypeDeclaration? valueType, EchelonScriptToken name, ES_AstExpression? defaultExpr,
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
        public ES_AstStatement? Endpoint { get; set; }

        public ES_AstStatement (int nothing) : base (1) { }
    }

    public class ES_AstEmptyErrorStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        public ES_AstNodeBounds bounds;

        public ES_AstEmptyErrorStatement (EchelonScriptToken tk) : base (1) {
            bounds = new ES_AstNodeBounds (tk.TextStartPos, tk.TextStartPos);
        }
    }

    public class ES_AstLabeledStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = LabelName.TextStartPos,
                    EndPos = LabelName.TextEndPos,
                };
            }
        }

        public EchelonScriptToken LabelName;

        public ES_AstLabeledStatement (EchelonScriptToken label) : base (1) {
            LabelName = label;
        }
    }

    public class ES_AstBlockStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstStatement? Statement;

        public ES_AstBlockStatement (
            ES_AstStatement? statement, EchelonScriptToken openTk, EchelonScriptToken closeTk
        ) : base (1) {
            Statement = statement;

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
        public ES_AstTypeDeclaration? OriginalName;

        public ES_AstTypeAlias (
            EchelonScriptToken aliasName, ES_AstTypeDeclaration? origName,
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
        public ES_AstTypeDeclaration? ValueType;

        public (EchelonScriptToken Name, ES_AstExpression? InitializationExpression) [] Variables;

        public ES_AstLocalVarDefinition (
            bool usingVar, ES_AstTypeDeclaration? valType, (EchelonScriptToken, ES_AstExpression?) [] varsList,
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
        public ES_AstStatement? ElseStatement;

        public ES_AstConditionalStatement (
            ES_AstExpression condExpr, ES_AstStatement thenStatement, ES_AstStatement? elseStatement,
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
        public (ES_AstExpression? [] Expressions, ES_AstStatement StatementsBlock) [] Sections;

        public ES_AstSwitchStatement (
            ES_AstExpression valExpr, (ES_AstExpression? [], ES_AstStatement) [] sectionsList,
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

        public ES_AstExpression? CaseExpression;

        public ES_AstGotoCaseStatement (
            ES_AstExpression? caseExpr, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
        ) : base (1) {
            CaseExpression = caseExpr;

            bounds = new ES_AstNodeBounds (startTk.TextStartPos, semicolonTk.TextEndPos);
        }
    }

    public class ES_AstReturnStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression? ReturnExpression;

        public ES_AstReturnStatement (
            ES_AstExpression? retExpr, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
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

        public ES_AstStatement? InitializationStatement;
        public ES_AstExpression? ConditionExpression;
        public ES_AstExpression? []? IterationExpressions;
        public bool PostLoop;

        public ES_AstStatement LoopBody;

        public ES_AstLoopStatement (
            ES_AstStatement? initStatement, ES_AstExpression? condExpr, ES_AstExpression? []? iterExprList,
            ES_AstStatement loopBody, bool postLoop,
            EchelonScriptToken startToken, EchelonScriptToken? endToken
        ) : base (1) {
            InitializationStatement = initStatement;
            ConditionExpression = condExpr;
            IterationExpressions = iterExprList;
            PostLoop = postLoop;

            LoopBody = loopBody;

            bounds = new ES_AstNodeBounds (startToken.TextStartPos, endToken?.TextEndPos ?? loopBody?.NodeBounds.EndPos ?? startToken.TextEndPos);
        }
    }

    #endregion

    public class ES_AstExpressionStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression Expression;

        public ES_AstExpressionStatement (ES_AstExpression expr, EchelonScriptToken semicolonTk) : base (1) {
            Expression = expr;

            if (!(expr is ES_AstEmptyErrorExpression))
                bounds = new ES_AstNodeBounds (expr.NodeBounds.StartPos, semicolonTk.TextEndPos);
            else
                bounds = new ES_AstNodeBounds (semicolonTk.TextStartPos, semicolonTk.TextEndPos);
        }
    }

    public class ES_AstExpressionListStatement : ES_AstStatement {
        public override ES_AstNodeBounds NodeBounds {
            get {
                ES_AstExpression? firstExpr = null;
                ES_AstExpression? lastExpr = null;

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

        AssignConcatenate,

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

    public static class ES_AstExtensions {
        public static bool IsAssignment (this SimpleBinaryExprType op) {
            return op >= SimpleBinaryExprType.TAG_AssignExpr_Start && op <= SimpleBinaryExprType.TAG_AssignExpr_End;
        }

        public static bool IsBitShift (this SimpleBinaryExprType op) {
            switch (op) {
                case SimpleBinaryExprType.ShiftLeft:
                case SimpleBinaryExprType.ShiftRight:
                case SimpleBinaryExprType.ShiftRightUnsigned:
                case SimpleBinaryExprType.AssignShiftLeft:
                case SimpleBinaryExprType.AssignShiftRight:
                case SimpleBinaryExprType.AssignShiftRightUnsigned:
                    return true;

                default:
                    return false;
            }
        }

        public static bool IsComparison (this SimpleBinaryExprType op) {
            switch (op) {
                case SimpleBinaryExprType.LesserThan:
                case SimpleBinaryExprType.GreaterThan:
                case SimpleBinaryExprType.LesserThanEqual:
                case SimpleBinaryExprType.GreaterThanEqual:
                case SimpleBinaryExprType.Equals:
                case SimpleBinaryExprType.NotEquals:
                    return true;

                default:
                    return false;
            }
        }
    }

    public abstract class ES_AstExpression : ES_AstNode {
        public ES_AstExpression (int nothing) : base (1) { }
    }

    public class ES_AstEmptyErrorExpression : ES_AstExpression {
        protected int errPos;
        public override ES_AstNodeBounds NodeBounds => new ES_AstNodeBounds (errPos, errPos);

        public ES_AstEmptyErrorExpression (int pos) : base (1) {
            errPos = pos;
        }
    }

    public abstract class ES_AstBinaryExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = Left?.NodeBounds.StartPos ?? Right?.NodeBounds.StartPos ?? 0,
                    EndPos = Right?.NodeBounds.EndPos ?? Left?.NodeBounds.EndPos ?? 0,
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
        public readonly bool HexBin;
        public readonly ulong Value;

        public readonly EchelonScriptToken Token;

        public ES_AstIntegerLiteralExpression (bool isUnsigned, bool isLong, bool hexBin, ulong value, EchelonScriptToken tk) : base (1) {
            Unsigned = isUnsigned;
            Long = isLong;
            HexBin = hexBin;
            Value = value;

            Token = tk;
        }
    }

    public class ES_AstBooleanLiteralExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = Token.TextStartPos,
                    EndPos = Token.TextEndPos,
                };
            }
        }

        public readonly bool Value;

        public readonly EchelonScriptToken Token;

        public ES_AstBooleanLiteralExpression (bool value, EchelonScriptToken tk) : base (1) {
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

    public class ES_AstMemberAccessExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = Parent?.NodeBounds.StartPos ?? OpToken.TextStartPos,
                    EndPos = Member?.TextEndPos ?? OpToken.TextEndPos,
                };
            }
        }

        public ES_AstExpression Parent;
        public EchelonScriptToken? Member;
        public EchelonScriptToken OpToken;

        public ES_AstMemberAccessExpression (ES_AstExpression parent, EchelonScriptToken? member, EchelonScriptToken opToken) : base (1) {
            Parent = parent;
            Member = member;

            OpToken = opToken;
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
        public ES_AstExpression? [] RankExpressions;

        public ES_AstIndexingExpression (
            ES_AstExpression indexedExpr, ES_AstExpression? [] ranks, EchelonScriptToken closeBracketTk
        ) : base (1) {
            IndexedExpression = indexedExpr;
            RankExpressions = ranks;

            bounds = new ES_AstNodeBounds (indexedExpr.NodeBounds.StartPos, closeBracketTk.TextEndPos);
        }
    }

    public class ES_AstNewExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstTypeDeclaration? TypeDeclaration;
        public ES_AstFunctionCallArgument [] Arguments;

        public ES_AstNewExpression (
            ES_AstTypeDeclaration? typeDecl, ES_AstFunctionCallArgument [] args,
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
        public override ES_AstNodeBounds NodeBounds {
            get {
                return new ES_AstNodeBounds {
                    StartPos = bounds.StartPos,
                    EndPos = InnerExpression?.NodeBounds.EndPos ?? bounds.EndPos,
                };
            }
        }
        protected ES_AstNodeBounds bounds;

        public ES_AstNodeBounds CastBounds => bounds;

        public ES_AstTypeDeclaration? DestinationType;
        public ES_AstExpression InnerExpression;

        public ES_AstCastExpression (
            ES_AstTypeDeclaration? destType, ES_AstExpression innerExpr,
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

    public class ES_AstConditionalExpression : ES_AstExpression {
        public override ES_AstNodeBounds NodeBounds => bounds;
        protected ES_AstNodeBounds bounds;

        public ES_AstExpression Condition;
        public ES_AstExpression Then;
        public ES_AstExpression Else;

        public ES_AstConditionalExpression (
            ES_AstExpression condExpr, ES_AstExpression thenExpr, ES_AstExpression elseExpr
        ) : base (1) {
            Condition = condExpr;
            Then = thenExpr;
            Else = elseExpr;

            bounds = new ES_AstNodeBounds (Condition.NodeBounds.StartPos, Else.NodeBounds.EndPos);
        }
    }

    #endregion
}
