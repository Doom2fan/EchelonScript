﻿/*
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
using System.Reflection;
using ChronosLib.Pooled;
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Frontend.Parser;

public struct ES_AggregateModifiers {
    public ES_AccessModifier? AccessModifier;
    public bool? Static;
    public bool? Const;
    public ES_VirtualnessModifier? VirtualnessModifier;

    public EchelonScriptToken? DocComment;

    public void ResetToNull () {
        AccessModifier = null;
        Static = null;
        Const = null;
        VirtualnessModifier = null;

        DocComment = null;
    }

    public void CopyDefaultsToUndefined (ES_AggregateModifiers defaults) {
        if (AccessModifier == null)
            AccessModifier = defaults.AccessModifier;

        if (Static == null)
            Static = defaults.Static;

        if (Const == null)
            Const = defaults.Const;

        if (VirtualnessModifier == null)
            VirtualnessModifier = defaults.VirtualnessModifier;
    }

    public bool AnyUndefined ()
        => !AccessModifier.HasValue || !Static.HasValue || !Const.HasValue || !VirtualnessModifier.HasValue;

    public bool AnySet ()
        => AccessModifier.HasValue || Static.HasValue || Const.HasValue || VirtualnessModifier.HasValue;
}

public class EchelonScriptParser : IDisposable {
    #region ================== Expression parsing

    protected enum ExpressionPrecedence {
        NO_PRECEDENCE = 0,

        Assignment,
        ConditionalExpression,
        ConditionalOr,
        ConditionalAnd,
        BitOr,
        BitXor,
        BitAnd,
        Equality,
        Comparison,
        Shift,
        Additive,
        Multiplicative,
        Exponentiation,
        Concatenation,
        Primary,
        Unary,
    }

    protected interface IPrefixExpressionParselet {
        public ExpressionPrecedence PrefixPrecedence { get; }

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token);

        public ES_AstExpression? Parse (EchelonScriptParser parser, EchelonScriptToken token);
    }

    protected interface IPostfixExpressionParselet {
        public ExpressionPrecedence PostfixPrecedence { get; }

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token);

        public ES_AstExpression? Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token);
    }

    protected class SimpleBinaryExpressionParselet : IPostfixExpressionParselet {
        #region ================== Instance fields

        protected ExpressionPrecedence opPrecedence;
        protected EchelonScriptTokenType tokenType;
        protected SimpleBinaryExprType expressionType;

        #endregion

        #region ================== Instance properties

        public ExpressionPrecedence PostfixPrecedence => opPrecedence;

        #endregion

        public SimpleBinaryExpressionParselet (ExpressionPrecedence precedence, EchelonScriptTokenType type, SimpleBinaryExprType exprType) {
            opPrecedence = precedence;
            tokenType = type;
            expressionType = exprType;
        }

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token)
            => token.Type == tokenType;

        public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            var right = parser.ParseExpression (opPrecedence);

            return new ES_AstSimpleBinaryExpression (expressionType, left, right, token);
        }

        #endregion
    }

    protected class AssignExpressionParselet : IPostfixExpressionParselet {
        #region ================== Instance fields

        protected ExpressionPrecedence opPrecedence;
        protected EchelonScriptTokenType tokenType;
        protected SimpleBinaryExprType expressionType;

        #endregion

        #region ================== Instance properties

        public ExpressionPrecedence PostfixPrecedence => opPrecedence;

        #endregion

        public AssignExpressionParselet (ExpressionPrecedence precedence, EchelonScriptTokenType type, SimpleBinaryExprType exprType) {
            opPrecedence = precedence;
            tokenType = type;
            expressionType = exprType;
        }

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token)
            => token.Type == tokenType;

        public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            var right = parser.ParseExpression (opPrecedence - 1);

            return new ES_AstSimpleBinaryExpression (expressionType, left, right, token);
        }

        #endregion
    }

    protected class SimpleUnaryExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance fields

        protected ExpressionPrecedence opPrecedence;
        protected EchelonScriptTokenType tokenType;
        protected SimpleUnaryExprType expressionType;

        #endregion

        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => opPrecedence;

        #endregion

        public SimpleUnaryExpressionParselet (ExpressionPrecedence precedence, EchelonScriptTokenType type, SimpleUnaryExprType exprType) {
            opPrecedence = precedence;
            tokenType = type;
            expressionType = exprType;
        }

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) => token.Type == tokenType;

        public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            var inner = parser.ParseExpression (opPrecedence);

            return new ES_AstSimpleUnaryExpression (token, expressionType, inner);
        }

        #endregion
    }

    protected class DoubleDereferenceExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance fields

        protected ExpressionPrecedence opPrecedence;

        #endregion

        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => opPrecedence;

        #endregion

        public DoubleDereferenceExpressionParselet (ExpressionPrecedence precedence) => opPrecedence = precedence;

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token)
            => token.Type == EchelonScriptTokenType.PowerOp;

        public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            var inner = parser.ParseExpression (opPrecedence);

            var asteriskLeft = token;
            var asteriskRight = token;
            asteriskLeft.Type = asteriskRight.Type = EchelonScriptTokenType.Asterisk;

            asteriskLeft.Text = token.Text [..1];
            asteriskRight.Text = token.Text [1..2];

            asteriskRight.TextStartPos++;
            asteriskRight.TextColumn++;

            inner = new ES_AstSimpleUnaryExpression (asteriskRight, SimpleUnaryExprType.Dereference, inner);
            return new ES_AstSimpleUnaryExpression (asteriskLeft, SimpleUnaryExprType.Dereference, inner);
        }

        #endregion
    }

    #region Primary expressions

    protected class ParenthesisExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token)
            => token.Type == EchelonScriptTokenType.ParenOpen;

        public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            var expr = parser.ParseExpression ();

            var tkPair = parser.tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                parser.errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("')'", tkPair.tk));
            else
                parser.tokenizer.NextToken ();

            return new ES_AstParenthesisExpression (expr, token, tkPair.tk);
        }

        #endregion
    }

    protected class MemberAccessExpressionParselet : IPostfixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PostfixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token)
            => token.Type == EchelonScriptTokenType.Dot;

        public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            var member = parser.ParseIdentifier ();

            return new ES_AstMemberAccessExpression (left, member, token);
        }

        #endregion
    }

    protected class LiteralExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) {
            var tkText = token.Text.Span;

            return (
                token.Type == EchelonScriptTokenType.DecIntegerLiteral ||
                token.Type == EchelonScriptTokenType.HexIntegerLiteral ||
                token.Type == EchelonScriptTokenType.BinIntegerLiteral ||
                token.Type == EchelonScriptTokenType.FloatLiteral ||
                token.Type == EchelonScriptTokenType.RegularStringLiteral ||
                token.Type == EchelonScriptTokenType.VerbatimStringLiteral ||
                token.Type == EchelonScriptTokenType.CharacterLiteral ||
                token.CheckIdentifier (ES_Keywords.True) ||
                token.CheckIdentifier (ES_Keywords.False)
            );
        }

        public ES_AstExpression? Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            ES_AstExpression? expr = null;

            switch (token.Type) {
                case EchelonScriptTokenType.DecIntegerLiteral: {
                    bool? isSigned = null;
                    ES_IntSize? size = null;

                    var tokenChars = token.Text.Span;
                    Span<char> chars = stackalloc char [token.Text.Length];

                    var charsCount = 0;
                    foreach (var c in tokenChars) {
                        if (EchelonScriptTokenizer.IsIntegerDigit (c))
                            chars [charsCount++] = c;
                        else if (IsSignSuffix (c)) {
                            ParseSuffix (parser, token, tokenChars [charsCount..^0], out isSigned, out size);
                            break;
                        }
                    }

                    if (ulong.TryParse (chars [..charsCount], System.Globalization.NumberStyles.None, null, out var resultULong))
                        expr = new ES_AstIntegerLiteralExpression (isSigned, size, false, resultULong, token);
                    else
                        parser.errorsList.Add (new EchelonScriptErrorMessage (token, ES_FrontendErrors.IntLiteralTooBig));

                    break;
                }
                case EchelonScriptTokenType.HexIntegerLiteral: {
                    bool? isSigned = null;
                    ES_IntSize? size = null;

                    var tokenChars = token.Text.Span [2..];
                    Span<char> chars = stackalloc char [tokenChars.Length];

                    var charsCount = 0;
                    foreach (var c in tokenChars) {
                        if (EchelonScriptTokenizer.IsHexDigit (c))
                            chars [charsCount++] = c;
                        else if (IsSignSuffix (c)) {
                            ParseSuffix (parser, token, tokenChars [charsCount..^0], out isSigned, out size);
                            break;
                        }
                    }

                    if (ulong.TryParse (chars [..charsCount], System.Globalization.NumberStyles.AllowHexSpecifier, null, out var resultULong))
                        expr = new ES_AstIntegerLiteralExpression (isSigned, size, true, resultULong, token);
                    else
                        parser.errorsList.Add (new EchelonScriptErrorMessage (token, ES_FrontendErrors.IntLiteralTooBig));

                    break;
                }
                case EchelonScriptTokenType.BinIntegerLiteral: {
                    bool? isSigned = null;
                    ES_IntSize? size = null;

                    var tokenChars = token.Text.Span [2..];
                    Span<char> chars = stackalloc char [tokenChars.Length];

                    var charsCount = 0;
                    foreach (var c in tokenChars) {
                        if (EchelonScriptTokenizer.IsBinaryDigit (c))
                            chars [charsCount++] = c;
                        else if (IsSignSuffix (c)) {
                            ParseSuffix (parser, token, tokenChars [charsCount..^0], out isSigned, out size);
                            break;
                        }
                    }

                    if (charsCount > 64)
                        parser.errorsList.Add (new EchelonScriptErrorMessage (token, ES_FrontendErrors.IntLiteralTooBig));

                    var value = 0uL;
                    var bitOffs = 0;
                    foreach (var c in chars [..charsCount]) {
                        if (c == '1')
                            value |= 1uL << bitOffs;

                        bitOffs++;
                    }

                    expr = new ES_AstIntegerLiteralExpression (isSigned, size, true, value, token);

                    break;
                }

                case EchelonScriptTokenType.FloatLiteral: {
                    var isFloat = false;

                    var chars = token.Text.Span;
                    {
                        var lastChar = chars [^1];
                        if (lastChar == 'f' || lastChar == 'F') {
                            isFloat = true;
                            chars = chars [0..^1];
                        } else if (lastChar == 'd' || lastChar == 'D')
                            chars = chars [0..^1];
                    }

                    if (isFloat && float.TryParse (chars, System.Globalization.NumberStyles.Float, null, out var resultFloat))
                        expr = new ES_AstFloatLiteralExpression (resultFloat, token);
                    else if (!isFloat && double.TryParse (chars, System.Globalization.NumberStyles.Float, null, out var resultDouble))
                        expr = new ES_AstFloatLiteralExpression (resultDouble, token);
                    else
                        parser.errorsList.Add (new EchelonScriptErrorMessage (token, ES_FrontendErrors.InvalidFloatLiteral));

                    break;
                }

                case EchelonScriptTokenType.RegularStringLiteral: {
                    expr = new ES_AstStringLiteralExpression (false, token);
                    break;
                }

                case EchelonScriptTokenType.VerbatimStringLiteral: {
                    expr = new ES_AstStringLiteralExpression (true, token);
                    break;
                }

                case EchelonScriptTokenType.CharacterLiteral: {
                    if (token.DecodedStringUTF32 == null || token.DecodedStringUTF32.Length != 1)
                        expr = new ES_AstCharLiteralExpression (0, token);
                    else
                        expr = new ES_AstCharLiteralExpression (token.DecodedStringUTF32 [0], token);

                    break;
                }

                case EchelonScriptTokenType.Identifier: {
                    var tkText = token.Text.Span;

                    if (tkText.Equals (ES_Keywords.True, StringComparison.Ordinal))
                        expr = new ES_AstBooleanLiteralExpression (true, token);
                    else if (tkText.Equals (ES_Keywords.False, StringComparison.Ordinal))
                        expr = new ES_AstBooleanLiteralExpression (false, token);
                    else
                        return null;

                    break;
                }

                default:
                    throw new Exception ();
            }

            parser.tokenizer.NextToken ();

            return expr;
        }

        private static bool IsSignSuffix (char c) => c == 's' || c == 'S' || c == 'u' || c == 'u';

        private static void ParseSuffix (
            EchelonScriptParser parser, EchelonScriptToken tk,
            ReadOnlySpan<char> suffixChars, out bool? isSigned, out ES_IntSize? size
        ) {
            Debug.Assert (suffixChars.Length > 0);
            Debug.Assert (IsSignSuffix (suffixChars [0]));

            var c = suffixChars [0];
            if (c == 's' || c == 'S')
                isSigned = true;
            else if (c == 'u' || c == 'U')
                isSigned = false;
            else // Unreachable, only here so it doesn't complain.
                isSigned = null;

            if (suffixChars.Length == 1) {
                size = null;
                return;
            }

            var sizeChars = suffixChars [1 .. ^0];

            if (sizeChars.Equals ("8", StringComparison.Ordinal))
                size = ES_IntSize.Int8;
            else if (sizeChars.Equals ("16", StringComparison.Ordinal))
                size = ES_IntSize.Int16;
            else if (sizeChars.Equals ("32", StringComparison.Ordinal))
                size = ES_IntSize.Int32;
            else if (sizeChars.Equals ("64", StringComparison.Ordinal))
                size = ES_IntSize.Int64;
            else {
                parser.errorsList.Add (new EchelonScriptErrorMessage (tk, ES_FrontendErrors.InvalidIntLiteralSize));
                size = null;
            }
        }

        #endregion
    }

    protected class NameExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) {
            return
                token.Type == EchelonScriptTokenType.Identifier &&
                !token.CheckIdentifier (ES_Keywords.New) &&
                !token.CheckIdentifier (ES_Keywords.Null) &&
                !token.CheckIdentifier (ES_Keywords.Cast);
        }

        public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            return new ES_AstNameExpression (token);
        }

        #endregion
    }

    protected class FunctionCallExpressionParselet : IPostfixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PostfixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token)
            => token.Type == EchelonScriptTokenType.ParenOpen;

        public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token)
            => new ES_AstFunctionCallExpression (left, parser.ParseArgumentsList (out var endToken), endToken);

        #endregion
    }

    protected class IndexingExpressionParselet : IPostfixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PostfixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token)
            => token.Type == EchelonScriptTokenType.BracketOpen;

        public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
            var tkPair = parser.tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BracketOpen, null) != EnsureTokenResult.Correct)
                parser.errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'['", tkPair.tk));

            if (parser.tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.BracketClose) {
                tkPair = parser.tokenizer.NextToken ();
                parser.errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.ValueExpected));

                return new ES_AstIndexingExpression (left, new ES_AstExpression? [] { null }, tkPair.tk.TextEndPos);
            }

            var dims = parser.ParseArrayDimensions (out _, out var endPos, true);

            return new ES_AstIndexingExpression (left, dims, endPos);
        }

        #endregion
    }

    protected class NewExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token)
            => token.CheckIdentifier (ES_Keywords.New);

        public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            var newTk = parser.tokenizer.NextToken ().tk;
            var typeDecl = parser.ParseTypeDeclaration ();

            if (typeDecl is null) {
                parser.errorsList.Add (ES_FrontendErrors.GenExpectedXGotY (
                    "a type", parser.tokenizer.PeekNextToken ().tk
                ));
            }

            var tkPeek = parser.tokenizer.PeekNextToken ().tk;

            if (tkPeek.Type == EchelonScriptTokenType.ParenOpen)
                return ParseNewObject (parser, newTk, typeDecl);
            else if (tkPeek.Type == EchelonScriptTokenType.BracketOpen)
                return ParseNewArray (parser, newTk, typeDecl);

            // Invalid token.
            parser.errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("\"[\" or \"(\"", tkPeek));
            return new ES_AstEmptyErrorExpression (newTk.TextStartPos, tkPeek.TextStartPos);
        }

        private static ES_AstExpression ParseNewObject (EchelonScriptParser parser, EchelonScriptToken newTk, ES_AstTypeDeclaration? typeDecl)
            => new ES_AstNewObjectExpression (typeDecl, parser.ParseArgumentsList (out var endTk), newTk, endTk);

        private static ES_AstExpression ParseNewArray (EchelonScriptParser parser, EchelonScriptToken newTk, ES_AstTypeDeclaration? typeDecl) {
            Debug.Assert (parser.EnsureTokenPeek (EchelonScriptTokenType.BracketOpen, null) == EnsureTokenResult.Correct);

            var dims = parser.ParseArrayDimensions (out _, out var endPos, false);

            return new ES_AstNewArrayExpression (typeDecl, dims, newTk, endPos);
        }

        #endregion
    }

    protected class NullExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token)
            => token.CheckIdentifier (ES_Keywords.Null);

        public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            return new ES_AstNullLiteralExpression (token);
        }

        #endregion
    }

    #endregion

    protected class IncDecExpressionParselet : IPrefixExpressionParselet, IPostfixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Unary;
        public ExpressionPrecedence PostfixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        #region Prefix

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token)
            => token.Type == EchelonScriptTokenType.PlusPlus || token.Type == EchelonScriptTokenType.MinusMinus;

        public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            var innerExpr = parser.ParseExpression ();

            return new ES_AstIncDecExpression (token,
                token.Type == EchelonScriptTokenType.MinusMinus,
                false, innerExpr
            );
        }

        #endregion

        #region Postfix

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token)
            => token.Type == EchelonScriptTokenType.PlusPlus || token.Type == EchelonScriptTokenType.MinusMinus;

        public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            return new ES_AstIncDecExpression (token,
                token.Type == EchelonScriptTokenType.MinusMinus,
                true, left
            );
        }

        #endregion

        #endregion
    }

    #region Unary expressions

    protected class CastExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Unary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token)
            => token.CheckIdentifier (ES_Keywords.Cast);

        public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            var tkPair = parser.tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
                parser.errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));
            else
                parser.tokenizer.NextToken ();

            var typeDecl = parser.ParseTypeDeclaration ();

            if (typeDecl is null) {
                parser.errorsList.Add (ES_FrontendErrors.GenExpectedXGotY (
                    "a type", parser.tokenizer.PeekNextToken ().tk
                ));
            }

            tkPair = parser.tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                parser.errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("')'", tkPair.tk));
            else
                parser.tokenizer.NextToken ();

            var innerExpr = parser.ParseExpression (PrefixPrecedence);

            return new ES_AstCastExpression (typeDecl, innerExpr, token, tkPair.tk);
        }

        #endregion
    }

    #endregion

    protected class ConditionalExpressionParselet : IPostfixExpressionParselet {
        #region ================== Instance fields

        #endregion

        #region ================== Instance properties

        public ExpressionPrecedence PostfixPrecedence => ExpressionPrecedence.ConditionalExpression;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression cond, EchelonScriptToken token)
            => token.Type == EchelonScriptTokenType.Question;

        public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression cond, EchelonScriptToken token) {
            parser.tokenizer.NextToken ();

            var thenExpr = parser.ParseExpression (PostfixPrecedence);

            if (parser.EnsureTokenPeek (EchelonScriptTokenType.Colon, null) != EnsureTokenResult.Correct)
                parser.errorsList.Add (ES_FrontendErrors.GenExpectedXGotY (":", parser.tokenizer.PeekNextToken ().tk));
            else
                parser.tokenizer.NextToken ();

            var elseExpr = parser.ParseExpression (PostfixPrecedence);

            return new ES_AstConditionalExpression (cond, thenExpr, elseExpr);
        }

        #endregion
    }

    #endregion

    #region ================== Static properties

    public static string [] PrimitiveTypes { get; private set; }
    public static string [] Keywords { get; private set; }

    protected static Dictionary<EchelonScriptTokenType, List<IPrefixExpressionParselet>> PrefixExprParsers { get; private set; }
    protected static Dictionary<EchelonScriptTokenType, List<IPostfixExpressionParselet>> PostfixExprParsers { get; private set; }

    #endregion

    #region ================== Instance fields

    protected List<EchelonScriptErrorMessage> errorsList;
    protected EchelonScriptTokenizer tokenizer;
    protected ReadOnlyMemory<char> fileName;
    protected ReadOnlyMemory<char> sourceText;

    #endregion

    #region ================== Instance properties

    public IReadOnlyList<EchelonScriptErrorMessage> Errors => errorsList;

    #endregion

    #region ================== Constructors

    #region Util functions

    static void AddPrefixParselet (EchelonScriptTokenType tokenType, IPrefixExpressionParselet parselet) {
        if (!PrefixExprParsers.ContainsKey (tokenType))
            PrefixExprParsers.Add (tokenType, new List<IPrefixExpressionParselet> ());

        PrefixExprParsers [tokenType].Add (parselet);
    }

    static void AddPostfixParselet (EchelonScriptTokenType tokenType, IPostfixExpressionParselet parselet) {
        if (!PostfixExprParsers.ContainsKey (tokenType))
            PostfixExprParsers.Add (tokenType, new List<IPostfixExpressionParselet> ());

        PostfixExprParsers [tokenType].Add (parselet);
    }

    static void AddSimpleBinaryExpr (ExpressionPrecedence precedence, EchelonScriptTokenType tokenType, SimpleBinaryExprType exprType) {
        Debug.Assert (!exprType.IsAssignment ());

        if (!PostfixExprParsers.ContainsKey (tokenType))
            PostfixExprParsers.Add (tokenType, new List<IPostfixExpressionParselet> ());

        PostfixExprParsers [tokenType].Add (new SimpleBinaryExpressionParselet (precedence, tokenType, exprType));
    }

    static void AddAssignExpr (ExpressionPrecedence precedence, EchelonScriptTokenType tokenType, SimpleBinaryExprType exprType) {
        Debug.Assert (exprType.IsAssignment ());

        if (!PostfixExprParsers.ContainsKey (tokenType))
            PostfixExprParsers.Add (tokenType, new List<IPostfixExpressionParselet> ());

        PostfixExprParsers [tokenType].Add (new AssignExpressionParselet (precedence, tokenType, exprType));
    }

    static void AddSimpleUnaryExpr (ExpressionPrecedence precedence, EchelonScriptTokenType tokenType, SimpleUnaryExprType exprType) {
        if (!PrefixExprParsers.ContainsKey (tokenType))
            PrefixExprParsers.Add (tokenType, new List<IPrefixExpressionParselet> ());

        PrefixExprParsers [tokenType].Add (new SimpleUnaryExpressionParselet (precedence, tokenType, exprType));
    }

    #endregion

    static EchelonScriptParser () {
        // Primitive types
        using var primitiveTypesList = new StructPooledList<string> (CL_ClearMode.Auto);
        var primitiveTypesConstants = typeof (ES_PrimitiveTypes).GetFields (BindingFlags.Public | BindingFlags.Static);

        foreach (var primitiveConst in primitiveTypesConstants) {
            if (primitiveConst.IsLiteral && !primitiveConst.IsInitOnly && primitiveConst.FieldType == typeof (string))
                primitiveTypesList.Add ((primitiveConst.GetValue (null) as string)!);
        }

        PrimitiveTypes = primitiveTypesList.ToArray ();

        // Keywords
        using var keywordsList = new StructPooledList<string> (CL_ClearMode.Auto);
        var keywordsConstants = typeof (ES_Keywords).GetFields (BindingFlags.Public | BindingFlags.Static);

        foreach (var keywordConst in keywordsConstants) {
            if (keywordConst.IsLiteral && !keywordConst.IsInitOnly && keywordConst.FieldType == typeof (string))
                keywordsList.Add ((keywordConst.GetValue (null) as string)!);
        }

        Keywords = keywordsList.ToArray ();

        // Parselets
        PrefixExprParsers = new Dictionary<EchelonScriptTokenType, List<IPrefixExpressionParselet>> ();
        PostfixExprParsers = new Dictionary<EchelonScriptTokenType, List<IPostfixExpressionParselet>> ();

        var incDecParselet = new IncDecExpressionParselet ();

        #region Primary expressions

        AddPrefixParselet (EchelonScriptTokenType.ParenOpen, new ParenthesisExpressionParselet ());
        var literalExprParselet = new LiteralExpressionParselet ();
        {
            AddPrefixParselet (EchelonScriptTokenType.DecIntegerLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.HexIntegerLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.BinIntegerLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.FloatLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.RegularStringLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.VerbatimStringLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.CharacterLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.Identifier, literalExprParselet);
        }
        AddPrefixParselet (EchelonScriptTokenType.Identifier, new NewExpressionParselet ());
        AddPrefixParselet (EchelonScriptTokenType.Identifier, new NullExpressionParselet ());
        AddPrefixParselet (EchelonScriptTokenType.Identifier, new NameExpressionParselet ());
        AddPostfixParselet (EchelonScriptTokenType.Dot, new MemberAccessExpressionParselet ());
        AddPostfixParselet (EchelonScriptTokenType.ParenOpen, new FunctionCallExpressionParselet ());
        AddPostfixParselet (EchelonScriptTokenType.BracketOpen, new IndexingExpressionParselet ());
        AddPostfixParselet (EchelonScriptTokenType.PlusPlus, incDecParselet);
        AddPostfixParselet (EchelonScriptTokenType.MinusMinus, incDecParselet);

        #endregion

        #region Unary expressions

        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Plus, SimpleUnaryExprType.Positive);
        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Minus, SimpleUnaryExprType.Negative);
        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Bang, SimpleUnaryExprType.LogicalNot);
        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Tilde, SimpleUnaryExprType.BitNot);
        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Asterisk, SimpleUnaryExprType.Dereference);
        AddPrefixParselet (EchelonScriptTokenType.PowerOp, new DoubleDereferenceExpressionParselet (ExpressionPrecedence.Unary));
        AddPrefixParselet (EchelonScriptTokenType.PlusPlus, incDecParselet);
        AddPrefixParselet (EchelonScriptTokenType.MinusMinus, incDecParselet);
        AddPrefixParselet (EchelonScriptTokenType.Identifier, new CastExpressionParselet ());

        #endregion

        #region Binary ops

        AddSimpleBinaryExpr (ExpressionPrecedence.Concatenation, EchelonScriptTokenType.DotDot, SimpleBinaryExprType.Concatenation);

        AddSimpleBinaryExpr (ExpressionPrecedence.Exponentiation, EchelonScriptTokenType.PowerOp, SimpleBinaryExprType.Power);

        AddSimpleBinaryExpr (ExpressionPrecedence.Multiplicative, EchelonScriptTokenType.Asterisk, SimpleBinaryExprType.Multiply);
        AddSimpleBinaryExpr (ExpressionPrecedence.Multiplicative, EchelonScriptTokenType.Divide, SimpleBinaryExprType.Divide);
        AddSimpleBinaryExpr (ExpressionPrecedence.Multiplicative, EchelonScriptTokenType.Modulo, SimpleBinaryExprType.Modulo);

        AddSimpleBinaryExpr (ExpressionPrecedence.Additive, EchelonScriptTokenType.Plus, SimpleBinaryExprType.Add);
        AddSimpleBinaryExpr (ExpressionPrecedence.Additive, EchelonScriptTokenType.Minus, SimpleBinaryExprType.Subtract);

        AddSimpleBinaryExpr (ExpressionPrecedence.Shift, EchelonScriptTokenType.ShiftLeft, SimpleBinaryExprType.ShiftLeft);
        AddSimpleBinaryExpr (ExpressionPrecedence.Shift, EchelonScriptTokenType.ShiftRight, SimpleBinaryExprType.ShiftRight);
        AddSimpleBinaryExpr (ExpressionPrecedence.Shift, EchelonScriptTokenType.ShiftRightU, SimpleBinaryExprType.ShiftRightUnsigned);

        AddSimpleBinaryExpr (ExpressionPrecedence.Comparison, EchelonScriptTokenType.LesserThan, SimpleBinaryExprType.LesserThan);
        AddSimpleBinaryExpr (ExpressionPrecedence.Comparison, EchelonScriptTokenType.GreaterThan, SimpleBinaryExprType.GreaterThan);
        AddSimpleBinaryExpr (ExpressionPrecedence.Comparison, EchelonScriptTokenType.LesserThanEq, SimpleBinaryExprType.LesserThanEqual);
        AddSimpleBinaryExpr (ExpressionPrecedence.Comparison, EchelonScriptTokenType.GreaterThanEq, SimpleBinaryExprType.GreaterThanEqual);

        AddSimpleBinaryExpr (ExpressionPrecedence.Equality, EchelonScriptTokenType.EqualsEquals, SimpleBinaryExprType.Equals);
        AddSimpleBinaryExpr (ExpressionPrecedence.Equality, EchelonScriptTokenType.NotEquals, SimpleBinaryExprType.NotEquals);

        AddSimpleBinaryExpr (ExpressionPrecedence.BitAnd, EchelonScriptTokenType.And, SimpleBinaryExprType.BitAnd);

        AddSimpleBinaryExpr (ExpressionPrecedence.BitXor, EchelonScriptTokenType.Xor, SimpleBinaryExprType.BitXor);

        AddSimpleBinaryExpr (ExpressionPrecedence.BitOr, EchelonScriptTokenType.BitOr, SimpleBinaryExprType.BitOr);

        AddSimpleBinaryExpr (ExpressionPrecedence.ConditionalAnd, EchelonScriptTokenType.AndAnd, SimpleBinaryExprType.LogicalAnd);

        AddSimpleBinaryExpr (ExpressionPrecedence.ConditionalOr, EchelonScriptTokenType.OrOr, SimpleBinaryExprType.LogicalOr);

        #region Assignments

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.Equals, SimpleBinaryExprType.Assign);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.PlusEq, SimpleBinaryExprType.AssignAdd);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.MinusEq, SimpleBinaryExprType.AssignSubtract);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.MultiplyEq, SimpleBinaryExprType.AssignMultiply);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.DivideEq, SimpleBinaryExprType.AssignDivide);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ModuloEq, SimpleBinaryExprType.AssignModulo);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.PowerOpEq, SimpleBinaryExprType.AssignPower);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.AndEq, SimpleBinaryExprType.AssignBitAnd);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.BitOrEq, SimpleBinaryExprType.AssignBitOr);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.XorEq, SimpleBinaryExprType.AssignXor);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ConcatEq, SimpleBinaryExprType.AssignConcatenate);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ShiftLeftEq, SimpleBinaryExprType.AssignShiftLeft);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ShiftRightEq, SimpleBinaryExprType.AssignShiftRight);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ShiftRightUEq, SimpleBinaryExprType.AssignShiftRightUnsigned);

        #endregion

        #endregion

        AddPostfixParselet (EchelonScriptTokenType.Question, new ConditionalExpressionParselet ());

        // Shrink the lists to fit
        foreach (var kvp in PrefixExprParsers)
            kvp.Value.Capacity = kvp.Value.Count;
        foreach (var kvp in PostfixExprParsers)
            kvp.Value.Capacity = kvp.Value.Count;
    }

    public EchelonScriptParser (List<EchelonScriptErrorMessage> errList) {
        errorsList = errList;
        tokenizer = new EchelonScriptTokenizer (errorsList);
    }

    #endregion

    #region ================== Instance methods

    #region Public methods

    public void Reset () {
        CheckDisposed ();

        tokenizer.Reset ();
        fileName = null;
        sourceText = null;
    }

    public ES_AbstractSyntaxTree ParseCode (ReadOnlyMemory<char> name, ReadOnlyMemory<char> codeData) {
        CheckDisposed ();

        Reset ();
        fileName = name;
        sourceText = codeData;
        tokenizer.SetSource (name, codeData);

        var astTree = ParseCodeUnit ();

        return astTree;
    }

    #endregion

    #region Parsing methods

    #region Utilities

    protected enum EnsureTokenResult {
        Correct,
        WrongType,
        WrongText,
    }

    protected static EnsureTokenResult EnsureToken (EchelonScriptToken tk, EchelonScriptTokenType type, string? text) {
        if (tk.Type != type)
            return EnsureTokenResult.WrongType;

        if (text != null && !tk.Text.Span.Equals (text, StringComparison.Ordinal))
            return EnsureTokenResult.WrongText;

        return EnsureTokenResult.Correct;
    }

    protected EnsureTokenResult EnsureTokenPeek (EchelonScriptTokenType type, string? text) {
        var tkPair = tokenizer.PeekNextToken ();
        return EnsureToken (tkPair.tk, type, text);
    }

    protected EnsureTokenResult EnsureTokenPeek (int offset, EchelonScriptTokenType type, string? text) {
        var tkPair = tokenizer.PeekNextToken (offset);
        return EnsureToken (tkPair.tk, type, text);
    }

    protected bool ParseSemicolon (out EchelonScriptToken tk) {
        tk = tokenizer.PeekNextToken ().tk;

        if (EnsureToken (tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct) {
            errorsList.Add (new (tk, ES_FrontendErrors.MissingSemicolon));
            return false;
        }

        tokenizer.NextToken ();
        return true;
    }

    protected bool CheckNoAccessModifierErrors (EchelonScriptToken tk, ES_AggregateModifiers currentModifiers) {
        if (currentModifiers.Static == true) {
            errorsList.Add (new EchelonScriptErrorMessage (tk, ES_FrontendErrors.AccessBeforeStorage));
            return false;
        }
        if (currentModifiers.VirtualnessModifier != null) {
            errorsList.Add (new EchelonScriptErrorMessage (tk, ES_FrontendErrors.VirtualnessBeforeStorage));
            return false;
        }
        if (currentModifiers.AccessModifier != null) {
            errorsList.Add (new EchelonScriptErrorMessage (tk, ES_FrontendErrors.MultipleAccessMods));
            return false;
        }

        return true;
    }

    protected void ParseAggregate_SetDocComment (ref ES_AggregateModifiers curMods, EchelonScriptToken? newDocCom) {
        if (newDocCom == null)
            return;

        if (curMods.AnySet ()) {
            errorsList.Add (new EchelonScriptErrorMessage (newDocCom.Value, ES_FrontendErrors.UnexpectedDocComment));
            return;
        }

        curMods.DocComment = newDocCom;
    }

    protected static bool IsIntegerLiteral (EchelonScriptToken tk) {
        return (
            tk.Type == EchelonScriptTokenType.DecIntegerLiteral ||
            tk.Type == EchelonScriptTokenType.BinIntegerLiteral ||
            tk.Type == EchelonScriptTokenType.HexIntegerLiteral
        );
    }

    protected static bool IsFloatLiteral (EchelonScriptToken tk) {
        return (
            tk.Type == EchelonScriptTokenType.FloatLiteral ||
            tk.Type == EchelonScriptTokenType.DecIntegerLiteral
        );
    }

    protected static bool IsKeyword (EchelonScriptToken tk) {
        var idSpan = tk.Text.Span;

        foreach (var keyword in Keywords) {
            if (idSpan.Equals (keyword, StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    protected static bool IsPrimitiveType (EchelonScriptToken tk) {
        var idSpan = tk.Text.Span;

        foreach (var type in PrimitiveTypes) {
            if (idSpan.Equals (type, StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    protected ES_AstExpression? [] ParseArrayDimensions (out EchelonScriptToken startTk, out int endPos, bool ignoreOpenTk) {
        using var dimsList = new StructPooledList<ES_AstExpression?> (CL_ClearMode.Auto);

        if (!ignoreOpenTk) {
            startTk = tokenizer.NextToken ().tk;
            if (startTk.Type != EchelonScriptTokenType.BracketOpen)
                throw new Exception ("The calling function must check for the correct initial token first.");
        } else
            startTk = tokenizer.PeekNextToken ().tk;

        while (true) {
            var tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                endPos = tkPair.tk.TextStartPos;
                break;
            }

            var expr = ParseExpression ();
            dimsList.Add (expr);

            tkPair = tokenizer.PeekNextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.BracketClose) {
                tokenizer.NextToken ();
                endPos = tkPair.tk.TextEndPos;

                break;
            } else if (tkPair.tk.Type != EchelonScriptTokenType.Comma) {
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("',' or ']'", tkPair.tk));
                endPos = tkPair.tk.TextStartPos;

                break;
            } else
                tokenizer.NextToken ();
        }

        return dimsList.ToArray ();
    }

    #endregion

    #region Basic data

    protected EchelonScriptToken? ParseIdentifier () {
        var tkPair = tokenizer.NextToken ();

        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            return null;
        }

        return tkPair.tk;
    }

    protected bool ParseUserIdentifier (out EchelonScriptToken token) {
        var tkPair = tokenizer.NextToken ();
        token = tkPair.tk;

        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            return false;
        }

        if (IsKeyword (tkPair.tk)) {
            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.InvalidUserIdentifier));
            return false;
        }

        if (IsPrimitiveType (tkPair.tk)) {
            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.InvalidUserIdentifier));
            return false;
        }

        return true;
    }

    protected ES_AstDottableIdentifier ParseDottableIdentifier () {
        if (EnsureTokenPeek (EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        using var partsList = new StructPooledList<EchelonScriptToken> (CL_ClearMode.Auto);

        var id = ParseIdentifier ();
        if (id != null)
            partsList.Add (id.Value);

        var tkPair = tokenizer.PeekNextToken ();
        while (tkPair.tk.Type == EchelonScriptTokenType.Dot) {
            tokenizer.NextToken ();

            id = ParseIdentifier ();
            if (id != null)
                partsList.Add (id.Value);

            tkPair = tokenizer.PeekNextToken ();
        }

        return new ES_AstDottableIdentifier (partsList.ToArray ());
    }

    protected ES_AstDottableIdentifier ParseDottableUserIdentifier () {
        var ret = ParseDottableIdentifier ();

        foreach (var part in ret.Parts) {
            if (IsKeyword (part))
                errorsList.Add (new EchelonScriptErrorMessage (part, ES_FrontendErrors.InvalidUserIdentifier));

            if (IsPrimitiveType (part))
                errorsList.Add (new EchelonScriptErrorMessage (part, ES_FrontendErrors.InvalidUserIdentifier));
        }

        return ret;
    }

    protected ES_AstTypeDeclaration? ParseTypeDeclaration () {
        ES_AstTypeDeclaration? innerDecl = null;

        while (true) {
            var tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                break;
            }

            if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                var isConst = tkPair.tk.Text.Span.Equals (ES_Keywords.Const, StringComparison.Ordinal);
                var isImmutable = tkPair.tk.Text.Span.Equals (ES_Keywords.Immutable, StringComparison.Ordinal);

                if (innerDecl != null)
                    break;
                else if (isConst || isImmutable) {
                    var startTk = tokenizer.NextToken ();

                    tkPair = tokenizer.NextToken ();
                    if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
                        errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));

                    var innerType = ParseTypeDeclaration ();

                    if (innerType is null)
                        errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a type", tokenizer.PeekNextToken ().tk));

                    var parenCloseTk = tokenizer.PeekNextToken ();
                    if (EnsureToken (parenCloseTk.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                        errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("')'", parenCloseTk.tk));
                    else
                        tokenizer.NextToken ();

                    var declType = ES_AstTypeDeclaration_Basic.DeclType.Const;
                    if (isConst) declType = ES_AstTypeDeclaration_Basic.DeclType.Const;
                    else if (isImmutable) declType = ES_AstTypeDeclaration_Basic.DeclType.Immutable;

                    var bounds = new ES_AstNodeBounds (startTk.tk.TextStartPos, innerType?.NodeBounds.EndPos ?? parenCloseTk.tk.TextEndPos);
                    innerDecl = new ES_AstTypeDeclaration_Basic (declType, innerType, bounds);
                } else {
                    var dottableId = ParseDottableIdentifier ();

                    if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.NamespaceOp) {
                        tokenizer.NextToken ();

                        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
                        else {
                            var typeName = ParseDottableIdentifier ();
                            innerDecl = new ES_AstTypeDeclaration_TypeName (dottableId, typeName);
                        }
                    } else
                        innerDecl = new ES_AstTypeDeclaration_TypeName (dottableId);
                }
            } else if (tkPair.tk.Type == EchelonScriptTokenType.And) {
                tokenizer.NextToken ();
                if (innerDecl == null) {
                    errorsList.Add (ES_FrontendErrors.GenUnexpectedToken (tkPair.tk));
                    break;
                }

                var bounds = new ES_AstNodeBounds (innerDecl?.NodeBounds.StartPos ?? tkPair.tk.TextStartPos, tkPair.tk.TextEndPos);
                innerDecl = new ES_AstTypeDeclaration_Basic (ES_AstTypeDeclaration_Basic.DeclType.Reference, innerDecl!, bounds);
            } else if (tkPair.tk.Type == EchelonScriptTokenType.AndAnd) {
                tokenizer.NextToken ();
                if (innerDecl == null) {
                    var errToken = tkPair.tk;
                    errToken.Text = errToken.Text [..1];
                    errorsList.Add (ES_FrontendErrors.GenUnexpectedToken (errToken));
                    break;
                }

                var boundsInner = new ES_AstNodeBounds (innerDecl?.NodeBounds.StartPos ?? tkPair.tk.TextStartPos, tkPair.tk.TextEndPos - 1);
                innerDecl = new ES_AstTypeDeclaration_Basic (ES_AstTypeDeclaration_Basic.DeclType.Reference, innerDecl!, boundsInner);

                var boundsOuter = new ES_AstNodeBounds (innerDecl?.NodeBounds.StartPos ?? tkPair.tk.TextStartPos, tkPair.tk.TextEndPos);
                innerDecl = new ES_AstTypeDeclaration_Basic (ES_AstTypeDeclaration_Basic.DeclType.Reference, innerDecl!, boundsOuter);
            } else if (tkPair.tk.Type == EchelonScriptTokenType.Question) {
                tokenizer.NextToken ();
                if (innerDecl == null) {
                    errorsList.Add (ES_FrontendErrors.GenUnexpectedToken (tkPair.tk));
                    break;
                }

                var bounds = new ES_AstNodeBounds (innerDecl?.NodeBounds.StartPos ?? tkPair.tk.TextStartPos, tkPair.tk.TextEndPos);
                innerDecl = new ES_AstTypeDeclaration_Basic (ES_AstTypeDeclaration_Basic.DeclType.Nullable, innerDecl!, bounds);
            } else if (tkPair.tk.Type == EchelonScriptTokenType.BracketOpen) {
                var peekNext = tokenizer.PeekNextToken (1).tk;

                if (peekNext.Type != EchelonScriptTokenType.Comma && peekNext.Type != EchelonScriptTokenType.BracketClose)
                    break;

                tokenizer.NextToken ();
                if (innerDecl == null) {
                    errorsList.Add (ES_FrontendErrors.GenUnexpectedToken (tkPair.tk));
                    break;
                }

                var rank = 0;

                int endPos;
                while (true) {
                    tkPair = tokenizer.PeekNextToken ();

                    if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                        endPos = tkPair.tk.TextEndPos;

                        break;
                    } else if (tkPair.tk.Type == EchelonScriptTokenType.BracketClose) {
                        endPos = tokenizer.NextToken ().tk.TextEndPos;
                        rank++;

                        break;
                    } else if (tkPair.tk.Type == EchelonScriptTokenType.Comma) {
                        tokenizer.NextToken ();
                        rank++;
                    } else {
                        errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a comma or \"]\"", tkPair.tk));
                        endPos = tkPair.tk.TextStartPos;

                        break;
                    }
                }

                innerDecl = new ES_AstTypeDeclaration_Array (innerDecl, rank, endPos);
            } else
                break;
        }

        return innerDecl;
    }

    #endregion

    protected ES_AbstractSyntaxTree ParseCodeUnit () {
        using var importsList = new StructPooledList<ES_AstImportStatement> (CL_ClearMode.Auto);
        using var aliasesList = new StructPooledList<ES_AstTypeAlias> (CL_ClearMode.Auto);
        using var namespacesList = new StructPooledList<ES_AstNamespace> (CL_ClearMode.Auto);

        while (true) {
            var tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF)
                break;

            if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
                tkPair = tokenizer.NextToken ();
                var tokenText = StringPool.Shared.GetOrAdd (tkPair.tk.Text.Span);
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.CodeUnit_UnexpectedToken.Replace ("{0}", tokenText)));
                continue;
            }

            var textSpan = tkPair.tk.Text.Span;
            if (textSpan.Equals (ES_Keywords.Using, StringComparison.Ordinal)) {
                if (namespacesList.Count > 0)
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.ImportAfterNamespace));

                var importStatement = ParseStatement_Import (out _);
                if (importStatement != null)
                    importsList.Add (importStatement);
            } else if (textSpan.Equals (ES_Keywords.Alias, StringComparison.Ordinal)) {
                if (namespacesList.Count > 0)
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.AliasAfterNamespace));

                var aliasStatement = ParseStatement_Alias (out _);
                if (aliasStatement != null)
                    aliasesList.Add (aliasStatement);
            } else if (textSpan.Equals (ES_Keywords.Namespace, StringComparison.Ordinal)) {
                var namespaceNode = ParseNamespace ();
                if (namespaceNode != null)
                    namespacesList.Add (namespaceNode);
            } else {
                tokenizer.NextToken ();
                var tokenText = StringPool.Shared.GetOrAdd (tkPair.tk.Text.Span);
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.CodeUnit_UnexpectedToken.Replace ("{0}", tokenText)));
            }
        }

        var startPos = int.MaxValue;
        var endPos = int.MinValue;

        foreach (var node in importsList) {
            startPos = Math.Max (startPos, node.NodeBounds.StartPos);
            endPos = Math.Max (endPos, node.NodeBounds.EndPos);
        }
        foreach (var node in aliasesList) {
            startPos = Math.Max (startPos, node.NodeBounds.StartPos);
            endPos = Math.Max (endPos, node.NodeBounds.EndPos);
        }
        foreach (var node in namespacesList) {
            startPos = Math.Max (startPos, node.NodeBounds.StartPos);
            endPos = Math.Max (endPos, node.NodeBounds.EndPos);
        }

        var codeUnit = new ES_AbstractSyntaxTree (
            sourceText, fileName,
            importsList.ToArray (),
            aliasesList.ToArray (),
            namespacesList.ToArray (),
            new ES_AstNodeBounds (startPos, endPos)
        ) {
            Valid = errorsList.Count == 0
        };

        return codeUnit;
    }

    protected ES_AstNamespace? ParseNamespace () {
        var namespaceTk = tokenizer.NextToken ().tk;
        if (EnsureToken (namespaceTk, EchelonScriptTokenType.Identifier, ES_Keywords.Namespace) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            return null;
        }

        var defaultModifiers = new ES_AggregateModifiers {
            AccessModifier = ES_AccessModifier.Private,
            Static = false,
            Const = false,
            VirtualnessModifier = ES_VirtualnessModifier.None,
        };

        var namespaceName = ParseDottableUserIdentifier ();
        var namespaceContents = ParseNamespaceContents (defaultModifiers, out var endTk);

        return new ES_AstNamespace (namespaceName, namespaceContents, namespaceTk, endTk);
    }

    protected ES_AstNode? [] ParseNamespaceContents (ES_AggregateModifiers defaultModifiers, out EchelonScriptToken endTk) {
        var tkPair = tokenizer.NextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'{'", tkPair.tk));
            endTk = new EchelonScriptToken { TextStartPos = tkPair.tk.TextStartPos, };
            return Array.Empty<ES_AstNode?> ();
        }

        using var contents = new StructPooledList<ES_AstNode?> (CL_ClearMode.Auto);
        var currentModifiers = new ES_AggregateModifiers ();
        currentModifiers.ResetToNull ();

        while (true) {
            tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                endTk = tkPair.tk;
                break;
            }

            if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                endTk = tokenizer.NextToken ().tk;
                if (currentModifiers.AnySet ())
                    errorsList.Add (ES_FrontendErrors.GenExpectedAggregateContent (tkPair.tk));

                break;
            } else if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                var textSpan = tkPair.tk.Text.Span;
                var textString = StringPool.Shared.GetOrAdd (textSpan);

                switch (textString) {
                    /* Access modifiers */
                    case ES_Keywords.Public:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers))
                            currentModifiers.AccessModifier = ES_AccessModifier.Public;
                        break;
                    case ES_Keywords.Internal:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers))
                            currentModifiers.AccessModifier = ES_AccessModifier.Internal;
                        break;
                    case ES_Keywords.Private:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers))
                            currentModifiers.AccessModifier = ES_AccessModifier.Private;
                        break;
                    case ES_Keywords.Protected:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.InvalidAccessModForNamespaceContent));
                        break;

                    /* Virtualness modifiers */
                    case ES_Keywords.Abstract:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (currentModifiers.Static == true)
                            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("abstract", tkPair.tk));

                        currentModifiers.VirtualnessModifier = ES_VirtualnessModifier.Abstract;
                        break;
                    case ES_Keywords.Virtual:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        errorsList.Add (ES_FrontendErrors.GenInvalidModifierForContext ("virtual", tkPair.tk));
                        break;
                    case ES_Keywords.Override:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        errorsList.Add (ES_FrontendErrors.GenInvalidModifierForContext ("override", tkPair.tk));
                        break;

                    /* Other modifiers */
                    case ES_Keywords.Static:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (currentModifiers.Static == true)
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.ElementAlreadyStatic));

                        currentModifiers.Static = true;
                        break;
                    case ES_Keywords.Const:
                        if (tokenizer.PeekNextToken (1).tk.Type != EchelonScriptTokenType.ParenOpen) {
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                            tokenizer.NextToken ();

                            if (currentModifiers.Const == true)
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.ElementAlreadyConst));

                            currentModifiers.Const = true;
                            break;
                        } else
                            goto default;

                    case ES_Keywords.Namespace:
                        endTk = new EchelonScriptToken { TextStartPos = tkPair.tk.TextStartPos, };
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.NamespaceMissingBrace));
                        return contents.ToArray ();

                    case ES_Keywords.Class:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                        currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                        contents.Add (ParseClass (currentModifiers));

                        currentModifiers.ResetToNull ();
                        break;

                    case ES_Keywords.Struct:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                        currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                        contents.Add (ParseStruct (currentModifiers));

                        currentModifiers.ResetToNull ();
                        break;

                    case ES_Keywords.Enum:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                        currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                        contents.Add (ParseEnum (currentModifiers));

                        currentModifiers.ResetToNull ();
                        break;

                    case ES_Keywords.Immutable:
                        goto default;

                    default:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                        currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                        contents.Add (ParseFunction (currentModifiers, null, false));

                        currentModifiers.ResetToNull ();
                        break;
                }
            } else {
                if (tkPair.tk.Type == EchelonScriptTokenType.ParenOpen) {
                    ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                    currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                    var funcDef = ParseFunction (currentModifiers, null, false);

                    if (funcDef is not null)
                        contents.Add (funcDef);
                    else
                        tokenizer.NextToken ();

                    currentModifiers.ResetToNull ();
                } else {
                    tokenizer.NextToken ();

                    errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a keyword, function definition or '}'", tkPair.tk));
                }
            }
        }

        return contents.ToArray ();
    }

    #region Aggregates

    protected ES_AstNode? [] ParseAggregate (
        ES_AggregateModifiers? baseModifiersArg, ES_AggregateModifiers defaultModifiers,
        bool abstractsAllowed, bool virtualsAllowed, bool overridesAllowed,
        out EchelonScriptToken endTk
    ) {
        ES_AggregateModifiers baseModifiers;
        if (baseModifiersArg == null) {
            baseModifiers = new ES_AggregateModifiers ();
            baseModifiers.ResetToNull ();
        } else
            baseModifiers = baseModifiersArg.Value;

        var tkPair = tokenizer.NextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'{'", tkPair.tk));
            endTk = new EchelonScriptToken { TextStartPos = tkPair.tk.TextStartPos, };
            return Array.Empty<ES_AstNode?> ();
        }

        using var contents = new StructPooledList<ES_AstNode?> (CL_ClearMode.Auto);
        var currentModifiers = new ES_AggregateModifiers ();
        currentModifiers.ResetToNull ();

        while (true) {
            tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                endTk = tkPair.tk;
                break;
            }

            if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                endTk = tokenizer.NextToken ().tk;
                if (currentModifiers.AnySet ())
                    errorsList.Add (ES_FrontendErrors.GenExpectedAggregateContent (tkPair.tk));

                break;
            } else if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                var textSpan = tkPair.tk.Text.Span;
                var textString = StringPool.Shared.GetOrAdd (textSpan);

                switch (textString) {
                    /* Access modifiers */
                    case ES_Keywords.Public:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                            currentModifiers.AccessModifier = ES_AccessModifier.Public;
                        break;
                    case ES_Keywords.Protected:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (EnsureTokenPeek (EchelonScriptTokenType.Identifier, ES_Keywords.Internal) == EnsureTokenResult.Correct) {
                            tokenizer.NextToken ();

                            if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                                currentModifiers.AccessModifier = ES_AccessModifier.ProtectedInternal;
                        } else {
                            if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                                currentModifiers.AccessModifier = ES_AccessModifier.Protected;
                        }
                        break;
                    case ES_Keywords.Internal:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                            currentModifiers.AccessModifier = ES_AccessModifier.Internal;
                        break;
                    case ES_Keywords.Private:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                            currentModifiers.AccessModifier = ES_AccessModifier.Private;
                        break;

                    /* Virtualness modifiers */
                    case ES_Keywords.Abstract:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (currentModifiers.Static == true)
                            errorsList.Add (ES_FrontendErrors.GenInvalidModifier (ES_Keywords.Abstract, tkPair.tk));
                        if (currentModifiers.VirtualnessModifier != null)
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.MultipleVirtualnessMods));

                        if (abstractsAllowed)
                            currentModifiers.VirtualnessModifier = ES_VirtualnessModifier.Abstract;
                        else
                            errorsList.Add (ES_FrontendErrors.GenInvalidModifierForContext (ES_Keywords.Abstract, tkPair.tk));
                        break;
                    case ES_Keywords.Virtual:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (currentModifiers.Static == true)
                            errorsList.Add (ES_FrontendErrors.GenInvalidModifier (ES_Keywords.Virtual, tkPair.tk));
                        if (currentModifiers.VirtualnessModifier != null)
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.MultipleVirtualnessMods));

                        if (virtualsAllowed)
                            currentModifiers.VirtualnessModifier = ES_VirtualnessModifier.Virtual;
                        else
                            errorsList.Add (ES_FrontendErrors.GenInvalidModifierForContext (ES_Keywords.Virtual, tkPair.tk));
                        break;
                    case ES_Keywords.Override:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (currentModifiers.Static == true)
                            errorsList.Add (ES_FrontendErrors.GenInvalidModifier (ES_Keywords.Override, tkPair.tk));
                        if (currentModifiers.VirtualnessModifier != null)
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.MultipleVirtualnessMods));

                        if (overridesAllowed)
                            currentModifiers.VirtualnessModifier = ES_VirtualnessModifier.Override;
                        else
                            errorsList.Add (ES_FrontendErrors.GenInvalidModifierForContext (ES_Keywords.Override, tkPair.tk));
                        break;

                    /* Other modifiers */
                    case ES_Keywords.Static:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                        tokenizer.NextToken ();

                        if (currentModifiers.Static == true)
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.ElementAlreadyStatic));

                        currentModifiers.Static = true;
                        break;
                    case ES_Keywords.Const:
                        if (tokenizer.PeekNextToken (1).tk.Type != EchelonScriptTokenType.ParenOpen) {
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                            tokenizer.NextToken ();

                            if (currentModifiers.Const == true)
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.ElementAlreadyConst));

                            currentModifiers.Const = true;
                            break;
                        } else
                            goto ParseFunctionOrVariable;

                    case ES_Keywords.This:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                        var isStatic = defaultModifiers.Static == true || baseModifiers.Static == true || currentModifiers.Static == true;

                        if (isStatic && (baseModifiers.AccessModifier.HasValue || currentModifiers.AccessModifier.HasValue)) {
                            errorsList.Add (new EchelonScriptErrorMessage (
                                tkPair.tk, ES_FrontendErrors.NoAccessModsOnStaticConstructors
                            ));
                        }

                        currentModifiers.CopyDefaultsToUndefined (baseModifiers);
                        currentModifiers.CopyDefaultsToUndefined (defaultModifiers);

                        contents.Add (ParseConstructor (currentModifiers));

                        currentModifiers.ResetToNull ();
                        break;

                    case ES_Keywords.Immutable:
                        goto ParseFunctionOrVariable;

                    default:
                        goto ParseFunctionOrVariable;

                    ParseFunctionOrVariable:
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                        currentModifiers.CopyDefaultsToUndefined (baseModifiers);
                        currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                        contents.AddRange (ParseFunctionOrVariable (currentModifiers));

                        currentModifiers.ResetToNull ();
                        break;
                }
            } else {
                if (tkPair.tk.Type == EchelonScriptTokenType.ParenOpen) {
                    ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                    currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                    contents.AddRange (ParseFunctionOrVariable (currentModifiers));

                    currentModifiers.ResetToNull ();
                } else {
                    if (tkPair.tk.Type == EchelonScriptTokenType.BraceOpen && currentModifiers.AnySet ()) {
                        currentModifiers.CopyDefaultsToUndefined (baseModifiers);

                        var newContents = ParseAggregate (
                            currentModifiers, defaultModifiers,
                            abstractsAllowed, virtualsAllowed, overridesAllowed,
                            out var _
                        );
                        contents.AddRange (newContents);

                        currentModifiers.ResetToNull ();
                    } else {
                        tokenizer.NextToken ();

                        errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a keyword, function definition, variable definition or '}'", tkPair.tk));
                    }
                }
            }
        }

        return contents.ToArray ();
    }

    protected ES_AstTypeDeclaration_TypeName []? ParseInheritanceList () {
        var tkPair = tokenizer.PeekNextToken ();

        if (tkPair.tk.Type != EchelonScriptTokenType.Colon)
            return null;
        tokenizer.NextToken ();

        using var listContents = new StructPooledList<ES_AstTypeDeclaration_TypeName> (CL_ClearMode.Auto);

        while (true) {
            tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                break;
            }

            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            else {
                var id = ParseDottableIdentifier ();

                if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.NamespaceOp) {
                    tokenizer.NextToken ();

                    if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                        errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
                    else {
                        var typeName = ParseDottableIdentifier ();
                        listContents.Add (new ES_AstTypeDeclaration_TypeName (id, typeName));
                    }
                } else
                    listContents.Add (new ES_AstTypeDeclaration_TypeName (id));
            }

            tkPair = tokenizer.PeekNextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.Comma)
                tokenizer.NextToken ();
            else
                break;
        }

        return listContents.ToArray ();
    }

    protected ES_AstMemberVarDefinition [] ParseMemberVar (ES_AggregateModifiers varModifiers, ES_AstTypeDeclaration? valueType) {
        // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
        if (varModifiers.AnyUndefined ())
            throw new ArgumentException ("Modifiers set contains undefined values.", nameof (varModifiers));

        if (valueType == null) {
            var newValType = ParseTypeDeclaration ();

            if (valueType is null)
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a type", tokenizer.PeekNextToken ().tk));

            valueType = newValType;
        }

        using var varDataList = new StructPooledList<(EchelonScriptToken Name, ES_AstExpression? Expr)> (CL_ClearMode.Auto);

        while (true) {
            var tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                break;
            }

            ParseUserIdentifier (out var functionName);
            ES_AstExpression? expression = null;

            if (varModifiers.Const == true)
                errorsList.Add (new EchelonScriptErrorMessage (functionName, ES_FrontendErrors.ConstOnlyOnFunctions));

            if (varModifiers.VirtualnessModifier == ES_VirtualnessModifier.Abstract)
                errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("abstract", functionName));
            else if (varModifiers.VirtualnessModifier == ES_VirtualnessModifier.Virtual)
                errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("virtual", functionName));
            else if (varModifiers.VirtualnessModifier == ES_VirtualnessModifier.Override)
                errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("override", functionName));

            varModifiers.Const = false;
            varModifiers.VirtualnessModifier = ES_VirtualnessModifier.None;

            tkPair = tokenizer.PeekNextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.Equals) {
                tokenizer.NextToken ();

                expression = ParseExpression ();
            }

            varDataList.Add ((functionName, expression));

            tkPair = tokenizer.PeekNextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.Comma)
                tokenizer.NextToken ();
            else
                break;
        }

        ParseSemicolon (out var semicolonTk);

        using var memberVarsList = new StructPooledList<ES_AstMemberVarDefinition> (CL_ClearMode.Auto);
        foreach (var memberVar in varDataList) {
            var memberVarDef = new ES_AstMemberVarDefinition (
                varModifiers.AccessModifier!.Value,
                varModifiers.DocComment,

                varModifiers.Static!.Value,

                memberVar.Name,
                valueType,
                memberVar.Expr,
                semicolonTk
            );
            memberVarsList.Add (memberVarDef);
        }

        return memberVarsList.ToArray ();
    }

    protected ES_AstNode? [] ParseFunctionOrVariable (ES_AggregateModifiers modifiers) {
        var typeDecl = ParseTypeDeclaration ();

        var nextTkPair = tokenizer.PeekNextToken (1);
        if (nextTkPair.tk.Type == EchelonScriptTokenType.ParenOpen)
            return new ES_AstNode? [] { ParseFunction (modifiers, typeDecl, true) };
        else
            return ParseMemberVar (modifiers, typeDecl);
    }

    protected ES_AstConstructorDefinition ParseConstructor (ES_AggregateModifiers consModifiers) {
        // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
        if (consModifiers.AnyUndefined ())
            throw new ArgumentException ("Modifiers set contains undefined values.", nameof (consModifiers));

        var startTk = tokenizer.NextToken ();
        Debug.Assert (EnsureToken (startTk.tk, EchelonScriptTokenType.Identifier, ES_Keywords.This) == EnsureTokenResult.Correct);

        if (consModifiers.VirtualnessModifier == ES_VirtualnessModifier.Abstract)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("abstract", startTk.tk));
        else if (consModifiers.VirtualnessModifier == ES_VirtualnessModifier.Virtual)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("virtual", startTk.tk));
        else if (consModifiers.VirtualnessModifier == ES_VirtualnessModifier.Override)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("override", startTk.tk));

        var isStatic = consModifiers.Static!.Value;

        var argumentsList = ParseArgumentsDefinitionList ();

        if (isStatic && argumentsList.Length > 0)
            errorsList.Add (new EchelonScriptErrorMessage (startTk.tk, ES_FrontendErrors.NoArgsOnStaticConstructors));

        bool exprBody;
        ES_AstStatement? statements;
        EchelonScriptToken endTk;
        if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.LambdaArrow) {
            tokenizer.NextToken ();

            var expr = ParseExpression ();

            endTk = tokenizer.PeekNextToken ().tk;

            ParseSemicolon (out _);

            statements = new ES_AstExpressionStatement (expr, endTk);

            exprBody = true;
        } else {
            statements = ParseStatementsBlock (out endTk);
            exprBody = false;
        }

        var functionDef = new ES_AstConstructorDefinition (
            consModifiers.AccessModifier!.Value,
            consModifiers.DocComment,

            isStatic,

            startTk.tk,
            argumentsList,

            exprBody,
            statements,
            endTk
        );

        return functionDef;
    }

    protected ES_AstClassDefinition ParseClass (ES_AggregateModifiers classModifiers) {
        // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
        if (classModifiers.AnyUndefined ())
            throw new ArgumentException ("Modifiers set contains undefined values.", nameof (classModifiers));

        var defaultModifiers = new ES_AggregateModifiers {
            AccessModifier = ES_AccessModifier.Private,
            Static = false,
            Const = false,
            VirtualnessModifier = ES_VirtualnessModifier.None,
        };

        // Parse the 'class' keyword.
        var startTkPair = tokenizer.NextToken ();
        if (EnsureToken (startTkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.Class) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ($"\"{ES_Keywords.Class}\"", startTkPair.tk));

        // Parse the class' name.
        var nameTkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (nameTkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (nameTkPair.tk));
        else
            tokenizer.NextToken ();

        if (classModifiers.Static == true)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("static", nameTkPair.tk));
        if (classModifiers.Const == true)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("const", nameTkPair.tk));

        if (classModifiers.VirtualnessModifier == ES_VirtualnessModifier.Virtual)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("virtual", nameTkPair.tk));
        else if (classModifiers.VirtualnessModifier == ES_VirtualnessModifier.Override)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("override", nameTkPair.tk));

        classModifiers.Static = false;
        classModifiers.Const = false;
        classModifiers.VirtualnessModifier = ES_VirtualnessModifier.None;

        // Error out if this is a static class and it has an inheritance list.
        var colonTkPair = tokenizer.PeekNextToken ();
        if (classModifiers.Static == true && colonTkPair.tk.Type == EchelonScriptTokenType.Colon)
            errorsList.Add (new EchelonScriptErrorMessage (colonTkPair.tk, ES_FrontendErrors.InheritanceOnStaticClass));

        // Parse the inheritance list.
        var inheritanceList = ParseInheritanceList ();
        if (inheritanceList == null)
            inheritanceList = Array.Empty<ES_AstTypeDeclaration_TypeName> ();

        // Parse the class' contents.
        var contents = ParseAggregate (null, defaultModifiers, true, true, true, out var endTk);

        // Create the class AST node.
        var classDef = new ES_AstClassDefinition (
            classModifiers.DocComment,

            classModifiers.AccessModifier!.Value,
            classModifiers.Static!.Value,
            classModifiers.VirtualnessModifier == ES_VirtualnessModifier.Abstract,

            nameTkPair.tk,
            inheritanceList,

            contents,

            startTkPair.tk,
            endTk
        );

        return classDef;
    }

    protected ES_AstStructDefinition ParseStruct (ES_AggregateModifiers structModifiers) {
        // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
        if (structModifiers.AnyUndefined ())
            throw new ArgumentException ("Modifiers set contains undefined values.", nameof (structModifiers));

        var defaultModifiers = new ES_AggregateModifiers {
            AccessModifier = ES_AccessModifier.Private,
            Static = false,
            Const = false,
            VirtualnessModifier = ES_VirtualnessModifier.None,
        };

        // Parse the 'struct' keyword.
        var startTkPair = tokenizer.NextToken ();
        if (EnsureToken (startTkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.Struct) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ($"\"{ES_Keywords.Struct}\"", startTkPair.tk));

        // Parse the struct's name.
        var nameTkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (nameTkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (nameTkPair.tk));
        else
            tokenizer.NextToken ();

        if (structModifiers.Static == true)
            errorsList.Add (new EchelonScriptErrorMessage (nameTkPair.tk, ES_FrontendErrors.StaticOnStruct));
        if (structModifiers.Const == true)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("const", nameTkPair.tk));

        if (structModifiers.VirtualnessModifier == ES_VirtualnessModifier.Abstract)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("abstract", nameTkPair.tk));
        else if (structModifiers.VirtualnessModifier == ES_VirtualnessModifier.Virtual)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("virtual", nameTkPair.tk));
        else if (structModifiers.VirtualnessModifier == ES_VirtualnessModifier.Override)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("override", nameTkPair.tk));

        structModifiers.Static = false;
        structModifiers.Const = false;
        structModifiers.VirtualnessModifier = ES_VirtualnessModifier.None;

        // Parse the inheritance list.
        var interfacesList = ParseInheritanceList ();
        if (interfacesList == null)
            interfacesList = Array.Empty<ES_AstTypeDeclaration_TypeName> ();

        // Parse the struct's contents
        var contents = ParseAggregate (null, defaultModifiers, false, false, false, out var endTk);

        // Create the struct AST node.
        var structDef = new ES_AstStructDefinition (
            structModifiers.DocComment,

            structModifiers.AccessModifier!.Value,

            nameTkPair.tk,
            interfacesList,

            contents,
            startTkPair.tk,
            endTk
        );

        return structDef;
    }

    #endregion

    protected ES_AstEnumDefinition ParseEnum (ES_AggregateModifiers enumModifiers) {
        // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
        if (enumModifiers.AnyUndefined ())
            throw new ArgumentException ("Modifiers set contains undefined values.", nameof (enumModifiers));

        // Parse the 'enum' keyword.
        var startTkPair = tokenizer.NextToken ();
        if (EnsureToken (startTkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.Enum) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ($"\"{ES_Keywords.Enum}\"", startTkPair.tk));

        // Parse the enum's name.
        ParseUserIdentifier (out var nameToken);

        if (enumModifiers.Static == true)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("static", nameToken));
        if (enumModifiers.Const == true)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("const", nameToken));

        if (enumModifiers.VirtualnessModifier == ES_VirtualnessModifier.Abstract)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("abstract", nameToken));
        else if (enumModifiers.VirtualnessModifier == ES_VirtualnessModifier.Virtual)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("virtual", nameToken));
        else if (enumModifiers.VirtualnessModifier == ES_VirtualnessModifier.Override)
            errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("override", nameToken));

        enumModifiers.Static = false;
        enumModifiers.Const = false;
        enumModifiers.VirtualnessModifier = ES_VirtualnessModifier.None;

        // Parse the enum's type, if any.
        EchelonScriptToken? baseType = null;
        if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.Colon) {
            tokenizer.NextToken ();

            baseType = ParseIdentifier ();
        }

        // Parse the opening brace.
        {
            var openBraceTkPair = tokenizer.NextToken ();
            if (EnsureToken (openBraceTkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'{'", openBraceTkPair.tk));
        }

        // Parse the enum's members
        using var membersList = new StructPooledList<(EchelonScriptToken, ES_AstExpression?)> (CL_ClearMode.Auto);

        EchelonScriptToken endTk;
        var expectingEnd = false;
        while (true) {
            var tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                endTk = tkPair.tk;
                break;
            }

            if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                endTk = tokenizer.NextToken ().tk;
                break;
            } else if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                ParseUserIdentifier (out var memberName);
                ES_AstExpression? assignExpr = null;

                // Error out if the previous member had no comma.
                if (expectingEnd)
                    errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'}'", tkPair.tk));

                // Read the value expression
                tkPair = tokenizer.PeekNextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.Equals) {
                    tokenizer.NextToken ();

                    assignExpr = ParseExpression ();
                }

                // Check for a comma.
                tkPair = tokenizer.PeekNextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.Comma)
                    tokenizer.NextToken ();
                else
                    expectingEnd = true;

                membersList.Add ((memberName, assignExpr));
            } else {
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a keyword or '}'", tkPair.tk));
                tokenizer.NextToken ();
            }
        }

        // Create the enum AST node.
        var enumDef = new ES_AstEnumDefinition (
            enumModifiers.AccessModifier!.Value,
            enumModifiers.DocComment,

            nameToken,
            baseType,

            membersList.ToArray (),
            startTkPair.tk,
            endTk
        );

        return enumDef;
    }

    protected ES_AstFunctionDefinition? ParseFunction (ES_AggregateModifiers funcModifiers, ES_AstTypeDeclaration? returnType, bool virtualsValid) {
        // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
        if (funcModifiers.AnyUndefined ())
            throw new ArgumentException ("Modifiers set contains undefined values.", nameof (funcModifiers));

        if (returnType == null) {
            var newRetType = ParseTypeDeclaration ();
            if (newRetType == null)
                return null;
            returnType = newRetType;
        }

        ParseUserIdentifier (out var functionName);

        var argumentsList = ParseArgumentsDefinitionList ();

        if (!virtualsValid) {
            if (funcModifiers.VirtualnessModifier == ES_VirtualnessModifier.Abstract)
                errorsList.Add (ES_FrontendErrors.GenInvalidModifier ("abstract", functionName));
            else if (funcModifiers.VirtualnessModifier == ES_VirtualnessModifier.Virtual)
                errorsList.Add (ES_FrontendErrors.GenInvalidModifierForContext ("virtual", functionName));
            else if (funcModifiers.VirtualnessModifier == ES_VirtualnessModifier.Override)
                errorsList.Add (ES_FrontendErrors.GenInvalidModifierForContext ("override", functionName));

            funcModifiers.VirtualnessModifier = ES_VirtualnessModifier.None;
        }

        bool exprBody;
        ES_AstStatement? statements;
        EchelonScriptToken endTk;
        if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.LambdaArrow) {
            tokenizer.NextToken ();

            var expr = ParseExpression ();

            endTk = tokenizer.PeekNextToken ().tk;

            ParseSemicolon (out _);

            statements = new ES_AstExpressionStatement (expr, endTk);

            exprBody = true;
        } else {
            statements = ParseStatementsBlock (out endTk);
            exprBody = false;
        }

        var functionDef = new ES_AstFunctionDefinition (
            funcModifiers.AccessModifier!.Value,
            funcModifiers.DocComment,

            funcModifiers.Static!.Value,
            funcModifiers.Const!.Value,
            funcModifiers.VirtualnessModifier!.Value,

            functionName,
            returnType,
            argumentsList,

            exprBody,
            statements,
            endTk
        );

        return functionDef;
    }

    protected ES_AstFunctionArgumentDefinition [] ParseArgumentsDefinitionList () {
        var tkPair = tokenizer.NextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));

        if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.ParenClose) {
            tokenizer.NextToken ();
            return Array.Empty<ES_AstFunctionArgumentDefinition> ();
        }

        using var argsList = new StructPooledList<ES_AstFunctionArgumentDefinition> (CL_ClearMode.Auto);

        while (true) {
            tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                break;
            }

            var argStartTk = tkPair.tk;
            var mode = ES_ArgumentType.Normal;

            if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                var textSpan = tkPair.tk.Text.Span;
                if (textSpan.Equals (ES_Keywords.Ref, StringComparison.Ordinal))
                    mode = ES_ArgumentType.Ref;
                else if (textSpan.Equals (ES_Keywords.In, StringComparison.Ordinal))
                    mode = ES_ArgumentType.In;
                else if (textSpan.Equals (ES_Keywords.Out, StringComparison.Ordinal))
                    mode = ES_ArgumentType.Out;

                if (mode != ES_ArgumentType.Normal) {
                    tokenizer.NextToken ();
                    tkPair = tokenizer.PeekNextToken ();

                    if (tkPair.tk.Type != EchelonScriptTokenType.Identifier)
                        errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
                }
            }

            var argumentType = ParseTypeDeclaration ();
            if (argumentType is null)
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a type", tokenizer.PeekNextToken ().tk));

            ParseUserIdentifier (out var argumentName);
            ES_AstExpression? argumentExpr = null;

            tkPair = tokenizer.PeekNextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.Equals) {
                tokenizer.NextToken ();

                argumentExpr = ParseExpression ();
            }

            var functionArgDef = new ES_AstFunctionArgumentDefinition (
                mode,
                argumentType,
                argumentName,
                argumentExpr,
                argStartTk
            );
            argsList.Add (functionArgDef);

            tkPair = tokenizer.NextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.ParenClose)
                break;
            else if (tkPair.tk.Type != EchelonScriptTokenType.Comma)
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("',' or ')'", tkPair.tk));
        }

        return argsList.ToArray ();
    }

    #region Statements

    protected ES_AstStatement ParseStatement () {
        var tkPair = tokenizer.PeekNextToken ();

        if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
            var tkTextStr = StringPool.Shared.GetOrAdd (tkPair.tk.Text.Span);

            var nextTkPair = tokenizer.PeekNextToken (1);
            if (nextTkPair.tk.Type == EchelonScriptTokenType.Colon) {
                // Labeled statement
                var labelName = tokenizer.NextToken ().tk;
                tokenizer.NextToken (); // Read the colon

                var ret = new ES_AstLabeledStatement (labelName) {
                    Endpoint = ParseStatement ()
                };

                if (ret.Endpoint is ES_AstEmptyStatement) {
                    errorsList.Add (new EchelonScriptErrorMessage (
                        sourceText.Span, fileName, ret.Endpoint.NodeBounds, ES_FrontendErrors.LabelOnEmptyStatement
                    ));
                }

                return ret;
            }

            switch (tkTextStr) {
                case ES_Keywords.Var: {
                    var retVar = ParseStatement_Variable (false);
                    ParseSemicolon (out _);
                    return retVar;
                }

                case ES_Keywords.Using: {
                    if (EnsureTokenPeek (1, EchelonScriptTokenType.Identifier, ES_Keywords.Var) == EnsureTokenResult.Correct) {
                        tokenizer.NextToken ();

                        var retVar = ParseStatement_Variable (true);
                        ParseSemicolon (out _);
                        return retVar;
                    } else {
                        var import = ParseStatement_Import (out var errTok);

                        if (import is null)
                            return new ES_AstEmptyErrorStatement (errTok!.Value);

                        return import;
                    }
                }
                case ES_Keywords.Alias: {
                    var alias = ParseStatement_Alias (out var errTok);

                    if (alias is null)
                        return new ES_AstEmptyErrorStatement (errTok!.Value);

                    return alias;
                }
            }
        }

        var retStmt = ParseEmbeddedStatementInternal ();
        return retStmt;
    }

    protected ES_AstStatement ParseEmbeddedStatementInternal () {
        var tkPair = tokenizer.PeekNextToken ();

        if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
            tokenizer.NextToken ();
            return new ES_AstEmptyStatement (tkPair.tk);
        } if (tkPair.tk.Type == EchelonScriptTokenType.BraceOpen) {
            return ParseStatementsBlock (out _);
        } else if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
            var tkTextStr = StringPool.Shared.GetOrAdd (tkPair.tk.Text.Span);

            switch (tkTextStr) {
                case ES_Keywords.If:
                    return ParseStatement_IfElse ();
                case ES_Keywords.Switch:
                    return ParseStatement_Switch ();

                case ES_Keywords.Break:
                    return ParseStatement_Break ();
                case ES_Keywords.Continue:
                    return ParseStatement_Continue ();
                case ES_Keywords.Goto:
                    return ParseStatement_Goto ();

                case ES_Keywords.While:
                    return ParseStatement_While ();
                case ES_Keywords.Do:
                    return ParseStatement_DoWhile ();
                case ES_Keywords.For:
                    return ParseStatement_For ();

                case ES_Keywords.Return:
                    return ParseStatement_Return ();
            }
        }

        var expr = ParseStatement_Expression ();

        var isAllowedExpression = (
            expr.Expression is ES_AstFunctionCallExpression ||
            expr.Expression is ES_AstIncDecExpression
        );

        if (expr.Expression is ES_AstSimpleBinaryExpression binExpr) {
            if (binExpr.ExpressionType >= SimpleBinaryExprType.TAG_AssignExpr_Start &&
                binExpr.ExpressionType <= SimpleBinaryExprType.TAG_AssignExpr_End)
                isAllowedExpression = true;
        }

        if (!isAllowedExpression)
            errorsList.Add (new EchelonScriptErrorMessage (sourceText.Span, fileName, expr.NodeBounds, ES_FrontendErrors.IllegalExpressionStatement));

        return expr;
    }

    protected ES_AstStatement ParseActualEmbeddedStatement () {
        // We use ParseStatement here so we can give better error messages.
        var statement = ParseStatement ();

        if (
            statement is ES_AstLocalVarDefinition ||
            statement is ES_AstImportStatement ||
            statement is ES_AstTypeAlias ||
            statement is ES_AstLabeledStatement
        ) {
            errorsList.Add (new EchelonScriptErrorMessage (sourceText.Span, fileName, statement.NodeBounds, ES_FrontendErrors.IllegalEmbeddedStatement));
        }

        return statement;
    }

    protected ES_AstStatement ParseStatementsBlock (out EchelonScriptToken endTk) {
        var tkPair = tokenizer.NextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'{'", tkPair.tk));
            endTk = new EchelonScriptToken ();
            return new ES_AstEmptyErrorStatement (tkPair.tk);
        }

        ES_AstStatement? firstStatement = null;
        ES_AstStatement? prevStatement = null;

        while (true) {
            tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                endTk = tkPair.tk;
                break;
            }

            if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                endTk = tokenizer.NextToken ().tk;
                break;
            }

            var curStatement = ParseStatement ();

            if (prevStatement is not null)
                prevStatement.Endpoint = curStatement;

            if (firstStatement is null)
                firstStatement = curStatement;

            prevStatement = curStatement;

            while (prevStatement.Endpoint is not null)
                prevStatement = prevStatement.Endpoint;
        }

        return new ES_AstBlockStatement (firstStatement, tkPair.tk, endTk);
    }

    #region Symbol definition statements

    protected ES_AstImportStatement? ParseStatement_Import (out EchelonScriptToken? errToken) {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Using) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            errToken = tkPair.tk;
            return null;
        }

        var namespaceName = ParseDottableUserIdentifier ();
        using var importedNamesList = new StructPooledList<EchelonScriptToken> (CL_ClearMode.Auto);
        EchelonScriptToken endTk;

        tkPair = tokenizer.PeekNextToken ();
        endTk = tkPair.tk;
        if (tkPair.tk.Type == EchelonScriptTokenType.Colon) {
            tokenizer.NextToken ();

            if (ParseUserIdentifier (out var name))
                importedNamesList.Add (name);

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                    endTk = tkPair.tk;
                    break;
                }

                if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
                    endTk = tokenizer.NextToken ().tk;
                    break;
                } else if (tkPair.tk.Type != EchelonScriptTokenType.Comma) {
                    errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("',' or ';'", tkPair.tk));
                    endTk = new EchelonScriptToken { TextStartPos = tkPair.tk.TextStartPos, };
                    break;
                } else
                    tokenizer.NextToken ();

                if (ParseUserIdentifier (out name))
                    importedNamesList.Add (name);
            }
        } else if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
            tokenizer.NextToken ();
        } else
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("',' or ';'", tkPair.tk));

        errToken = null;
        return new ES_AstImportStatement (namespaceName, importedNamesList.ToArray (), startTk, endTk);
    }

    protected ES_AstTypeAlias? ParseStatement_Alias (out EchelonScriptToken? errToken) {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Alias) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        if (!ParseUserIdentifier (out var aliasName)) {
            errToken = tokenizer.PeekNextToken ().tk;
            return null;
        }

        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Equals, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'='", tkPair.tk));
        } else
            tokenizer.NextToken ();

        tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            errToken = tkPair.tk;
            return null;
        }

        var originalName = ParseTypeDeclaration ();
        if (originalName is null)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a type", tokenizer.PeekNextToken ().tk));

        ParseSemicolon (out var endTk);

        errToken = null;
        return new ES_AstTypeAlias (aliasName, originalName, startTk, endTk);
    }

    protected ES_AstLocalVarDefinition ParseStatement_Variable (bool usingVar) {
        var varStartTk = tokenizer.NextToken ().tk;
        if (EnsureToken (varStartTk, EchelonScriptTokenType.Identifier, ES_Keywords.Var) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        ES_AstTypeDeclaration? valueType;
        using var variables = new StructPooledList<(EchelonScriptToken, ES_AstExpression?)> (CL_ClearMode.Auto);
        bool initRequired;

        if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.Identifier &&
            tokenizer.PeekNextToken (1).tk.Type == EchelonScriptTokenType.Equals
        ) {
            initRequired = true;
            valueType = null;
        } else {
            initRequired = false;
            valueType = ParseTypeDeclaration ();

            if (valueType is null)
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a type", tokenizer.PeekNextToken ().tk));
        }

        if (usingVar)
            initRequired = true;

        EchelonScriptToken endTk;
        while (true) {
            var varName = new EchelonScriptToken ();
            ES_AstExpression? initializationExpr = null;

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            else {
                varName = tkPair.tk;
                tokenizer.NextToken ();
            }

            tkPair = tokenizer.PeekNextToken ();
            if (!initRequired) {
                if (tkPair.tk.Type == EchelonScriptTokenType.Equals) {
                    tokenizer.NextToken ();

                    initializationExpr = ParseExpression ();
                }
            } else if (initRequired) {
                if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Equals, null) != EnsureTokenResult.Correct)
                    errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'='", tkPair.tk));
                else
                    tokenizer.NextToken ();

                initializationExpr = ParseExpression ();
            }

            variables.Add ((varName, initializationExpr));

            tkPair = tokenizer.PeekNextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.Comma)
                tokenizer.NextToken ();
            else {
                endTk = varName;
                break;
            }
        }

        return new ES_AstLocalVarDefinition (
            usingVar,
            valueType,

            variables.ToArray (),
            varStartTk,
            endTk
        );
    }

    #endregion

    #region Jump statements

    protected ES_AstStatement ParseStatement_IfElse () {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.If) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));
            return new ES_AstEmptyErrorStatement (tkPair.tk);
        } else
            tokenizer.NextToken ();

        var conditionExpr = ParseExpression ();

        tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("')'", tkPair.tk));
        else
            tokenizer.NextToken ();

        var thenStatement = ParseActualEmbeddedStatement ();
        ES_AstStatement? elseStatement = null;

        if (EnsureTokenPeek (EchelonScriptTokenType.Identifier, ES_Keywords.Else) == EnsureTokenResult.Correct) {
            tokenizer.NextToken ();

            elseStatement = ParseActualEmbeddedStatement ();
        }

        return new ES_AstConditionalStatement (
            conditionExpr,
            thenStatement,
            elseStatement,
            startTk
        );
    }

    protected ES_AstStatement ParseStatement_Switch () {
        var switchStartTk = tokenizer.NextToken ().tk;
        if (EnsureToken (switchStartTk, EchelonScriptTokenType.Identifier, ES_Keywords.Switch) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));
            return new ES_AstEmptyErrorStatement (tkPair.tk);
        }

        var valueExpr = ParseExpression ();

        tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("')'", tkPair.tk));
        else
            tokenizer.NextToken ();

        tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'{'", tkPair.tk));
        else
            tokenizer.NextToken ();

        EchelonScriptToken endTK;
        using var sections = new StructPooledList<(ES_AstExpression? [], ES_AstStatement)> (CL_ClearMode.Auto);
        using var sectionExpressions = new StructPooledList<ES_AstExpression?> (CL_ClearMode.Auto);
        while (true) {
            tkPair = tokenizer.NextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                endTK = tkPair.tk;
                break;
            } else if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                endTK = tkPair.tk;
                break;
            }

            if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
                errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            }

            if (tkPair.tk.Text.Span.Equals (ES_Keywords.Default, StringComparison.Ordinal))
                sectionExpressions.Add (null);
            else if (tkPair.tk.Text.Span.Equals (ES_Keywords.Case, StringComparison.Ordinal))
                sectionExpressions.Add (ParseExpression ());
            else
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ($"\"{ES_Keywords.Case}\" or \"{ES_Keywords.Default}\"", tkPair.tk));

            tkPair = tokenizer.PeekNextToken ();
            if (
                EnsureTokenPeek (EchelonScriptTokenType.Identifier, ES_Keywords.Case   ) == EnsureTokenResult.Correct ||
                EnsureTokenPeek (EchelonScriptTokenType.Identifier, ES_Keywords.Default) == EnsureTokenResult.Correct
            ) {
                continue;
            }

            var statementsBlock = ParseStatementsBlock (out var _);

            sections.Add ((sectionExpressions.ToArray (), statementsBlock));
            sectionExpressions.Clear ();
        }

        return new ES_AstSwitchStatement (
            valueExpr,
            sections.ToArray (),
            switchStartTk,
            endTK
        );
    }

    protected ES_AstBreakStatement ParseStatement_Break () {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Break) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        EchelonScriptToken? labelName = null;
        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) == EnsureTokenResult.Correct)
            labelName = tokenizer.NextToken ().tk;

        ParseSemicolon (out var endTk);

        return new ES_AstBreakStatement (labelName, startTk, endTk);
    }

    protected ES_AstContinueStatement ParseStatement_Continue () {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Continue) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        EchelonScriptToken? labelName = null;
        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) == EnsureTokenResult.Correct)
            labelName = tokenizer.NextToken ().tk;

        ParseSemicolon (out var endTk);

        return new ES_AstContinueStatement (labelName, startTk, endTk);
    }

    protected ES_AstStatement ParseStatement_Goto () {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Goto) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        ES_AstStatement statement;

        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));

        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) == EnsureTokenResult.Correct) {
            if (tkPair.tk.Text.Span.Equals (ES_Keywords.Case, StringComparison.Ordinal)) {
                tokenizer.NextToken ();

                var expr = ParseExpression ();

                ParseSemicolon (out var endTk);

                statement = new ES_AstGotoCaseStatement (expr, startTk, endTk);
            } else if (tkPair.tk.Text.Span.Equals (ES_Keywords.Default, StringComparison.Ordinal)) {
                tokenizer.NextToken ();

                ParseSemicolon (out var endTk);

                statement = new ES_AstGotoCaseStatement (null, startTk, endTk);
            } else {
                tokenizer.NextToken ();

                ParseSemicolon (out var endTk);

                statement = new ES_AstGotoLabelStatement (tkPair.tk, startTk, endTk);
            }
        } else
            statement = new ES_AstEmptyErrorStatement (tkPair.tk);

        return statement;
    }

    protected ES_AstReturnStatement ParseStatement_Return () {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Return) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        ES_AstExpression? retExpr = null;

        if (EnsureTokenPeek (EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
            retExpr = ParseExpression ();

        ParseSemicolon (out var endTk);

        return new ES_AstReturnStatement (retExpr, startTk, endTk);
    }

    #endregion

    #region Loop statements

    protected ES_AstStatement ParseStatement_While () {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.While) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));
            return new ES_AstEmptyErrorStatement (tkPair.tk);
        } else
            tokenizer.NextToken ();

        var conditionExpr = ParseExpression ();

        var foundCloseBrace = false;
        var closeBraceTk = tokenizer.PeekNextToken ().tk;
        if (EnsureToken (closeBraceTk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("')'", closeBraceTk));
        else
            tokenizer.NextToken ();

        var bodyStatement = ParseActualEmbeddedStatement ();

        EchelonScriptToken? endTk;
        if (bodyStatement is ES_AstEmptyErrorStatement)
            endTk = null;
        else if (foundCloseBrace)
            endTk = closeBraceTk;
        else
            endTk = startTk;

        return new ES_AstLoopStatement (
            null, conditionExpr, null,
            bodyStatement, false,
            startTk, endTk
        );
    }

    protected ES_AstLoopStatement ParseStatement_DoWhile () {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Do) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        var bodyStatement = ParseActualEmbeddedStatement ();

        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.While) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ($"\"{ES_Keywords.While}\"", tkPair.tk));
        else
            tokenizer.NextToken ();

        tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));
        else
            tokenizer.NextToken ();

        var conditionExpr = ParseExpression ();

        var foundCloseParen = false;
        var closeParenTk = tokenizer.PeekNextToken ().tk;
        if (EnsureToken (closeParenTk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("')'", closeParenTk));
        else {
            tokenizer.NextToken ();
            foundCloseParen = true;
        }

        var foundSemicolon = ParseSemicolon (out var semicolonTk);

        EchelonScriptToken? endTk;
        if (foundSemicolon)
            endTk = semicolonTk;
        else if (foundCloseParen)
            endTk = closeParenTk;
        else
            endTk = null;

        return new ES_AstLoopStatement (
            null, conditionExpr, null,
            bodyStatement, true,
            startTk, endTk
        );
    }

    protected ES_AstStatement ParseStatement_For () {
        var startTk = tokenizer.NextToken ().tk;
        if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.For) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        ES_AstStatement? initStatement;
        ES_AstExpression? conditionExpr;
        ES_AstExpression? []? iterExpressions;

        var tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));
            return new ES_AstEmptyErrorStatement (tkPair.tk);
        } else
            tokenizer.NextToken ();

        // Initialization statement
        tkPair = tokenizer.PeekNextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.Var) == EnsureTokenResult.Correct) {
            initStatement = ParseStatement_Variable (false);
        } else {
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                initStatement = new ES_AstExpressionListStatement (ParseExpressionList ());
            else
                initStatement = null;
        }

        ParseSemicolon (out _);

        // Condition expression
        if (EnsureTokenPeek (EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
            conditionExpr = ParseExpression ();
        else
            conditionExpr = null;

        ParseSemicolon (out _);

        // Iteration expressions
        if (EnsureTokenPeek (EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
            iterExpressions = ParseExpressionList ();
        else
            iterExpressions = null;

        // Closing parenthesis
        var endTk = tokenizer.PeekNextToken ().tk;
        if (EnsureToken (endTk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("')'", endTk));
        else
            tokenizer.NextToken ();

        // Loop body statement
        var bodyStatement = ParseActualEmbeddedStatement ();

        return new ES_AstLoopStatement (
            initStatement, conditionExpr, iterExpressions,
            bodyStatement, false,
            startTk, bodyStatement == null ? endTk : null
        );
    }

    #endregion

    protected ES_AstExpressionStatement ParseStatement_Expression () {
        var expr = ParseExpression ();

        if (expr is ES_AstEmptyErrorExpression)
            tokenizer.NextToken ();

        ParseSemicolon (out var endTk);

        return new ES_AstExpressionStatement (expr, endTk);
    }

    #endregion

    #region Expressions

    protected ES_AstExpression [] ParseExpressionList () {
        using var exprList = new StructPooledList<ES_AstExpression> (CL_ClearMode.Auto);

        while (true) {
            var tkPair = tokenizer.PeekNextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                break;
            }

            exprList.Add (ParseExpression ());

            if (EnsureTokenPeek (EchelonScriptTokenType.Comma, null) != EnsureTokenResult.Correct)
                break;
        }

        return exprList.ToArray ();
    }

    protected ES_AstExpression ParseExpression (ExpressionPrecedence precedence = ExpressionPrecedence.NO_PRECEDENCE) {
        var tkPair = tokenizer.PeekNextToken ();

        if (!PrefixExprParsers.TryGetValue (tkPair.tk.Type, out var prefixParsers)) {
            errorsList.Add (ES_FrontendErrors.GenUnexpectedToken (tkPair.tk));
            return new ES_AstEmptyErrorExpression (tkPair.tk.TextStartPos);
        }

        ES_AstExpression? left = null;

        foreach (var parser in prefixParsers) {
            if (parser.CheckCanParse (this, tkPair.tk)) {
                left = parser.Parse (this, tkPair.tk);
                break;
            }
        }

        if (left == null) {
            var tok = tokenizer.PeekNextToken ();
            return new ES_AstEmptyErrorExpression (tok.tk.TextStartPos);
        }

        while (true) {
            tkPair = tokenizer.PeekNextToken ();

            if (!PostfixExprParsers.TryGetValue (tkPair.tk.Type, out var postfixParsers))
                return left!;

            var parsed = false;
            foreach (var parser in postfixParsers) {
                if (precedence < parser.PostfixPrecedence && parser.CheckCanParse (this, left!, tkPair.tk)) {
                    left = parser.Parse (this, left!, tkPair.tk);
                    parsed = true;
                    break;
                }
            }

            if (!parsed)
                return left!;
        }
    }

    protected ES_AstFunctionCallArgument [] ParseArgumentsList (out EchelonScriptToken endToken) {
        var tkPair = tokenizer.NextToken ();
        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));

        if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.ParenClose) {
            endToken = tokenizer.NextToken ().tk;
            return Array.Empty<ES_AstFunctionCallArgument> ();
        }

        using var argsList = new StructPooledList<ES_AstFunctionCallArgument> (CL_ClearMode.Auto);

        while (true) {
            tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_FrontendErrors.UnexpectedEOF));
                endToken = tkPair.tk;
                break;
            }

            var mode = ES_ArgumentType.Normal;
            if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                var textSpan = tkPair.tk.Text.Span;
                if (tkPair.tk.CheckIdentifier (ES_Keywords.Ref))
                    mode = ES_ArgumentType.Ref;
                else if (tkPair.tk.CheckIdentifier (ES_Keywords.In))
                    errorsList.Add (ES_FrontendErrors.GenUnexpectedIdentifier (tkPair.tk));
                else if (tkPair.tk.CheckIdentifier (ES_Keywords.Out))
                    mode = ES_ArgumentType.Out;

                if (mode != ES_ArgumentType.Normal)
                    tokenizer.NextToken ();
            }

            ES_AstExpression argumentExpr;

            var peekTkPair = tokenizer.PeekNextToken ();
            if (peekTkPair.tk.Type == EchelonScriptTokenType.Comma ||
                peekTkPair.tk.Type == EchelonScriptTokenType.ParenClose) {
                errorsList.Add (new EchelonScriptErrorMessage (
                    peekTkPair.tk, ES_FrontendErrors.EmptyArgument
                ));

                argumentExpr = new ES_AstEmptyErrorExpression (peekTkPair.tk.TextStartPos);
            } else
                argumentExpr = ParseExpression ();

            var functionCallArgDef = new ES_AstFunctionCallArgument (mode, argumentExpr, tkPair.tk);
            argsList.Add (functionCallArgDef);

            tkPair = tokenizer.NextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.ParenClose) {
                endToken = tkPair.tk;
                break;
            } else if (tkPair.tk.Type != EchelonScriptTokenType.Comma)
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("',' or ')'", tkPair.tk));
        }

        return argsList.ToArray ();
    }

    #endregion

    #endregion

    #region Protected methods

    protected void CheckDisposed () {
        if (IsDisposed)
            throw new ObjectDisposedException (nameof (EchelonScriptParser));
    }

    #endregion

    #endregion

    #region ================== IDisposable Support

    public bool IsDisposed { get; private set; }

    protected virtual void DoDispose () {
        if (IsDisposed)
            return;

        tokenizer?.Dispose ();

        sourceText = null;

        IsDisposed = true;
    }

    public void Dispose () {
        GC.SuppressFinalize (this);
        DoDispose ();
    }

    #endregion
}
