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
using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;
using EchelonScript.Compiler.Frontend.Parser.Tokenizer;

namespace EchelonScript.Compiler.Frontend.Parser;

public partial class EchelonScriptParser {
#if false
    private enum ExpressionPrecedence {
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

    private interface IPrefixExpressionParselet {
        public ExpressionPrecedence PrefixPrecedence { get; }

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token);

        public ES_AstExpression? Parse (EchelonScriptParser parser, EchelonScriptToken token);
    }

    private interface IPostfixExpressionParselet {
        public ExpressionPrecedence PostfixPrecedence { get; }

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token);

        public ES_AstExpression? Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token);
    }

    private class SimpleBinaryExpressionParselet : IPostfixExpressionParselet {
        #region ================== Instance fields

        private ExpressionPrecedence opPrecedence;
        private EchelonScriptTokenType tokenType;
        private SimpleBinaryExprType expressionType;

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

    private class AssignExpressionParselet : IPostfixExpressionParselet {
        #region ================== Instance fields

        private ExpressionPrecedence opPrecedence;
        private EchelonScriptTokenType tokenType;
        private SimpleBinaryExprType expressionType;

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

    private class SimpleUnaryExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance fields

        private ExpressionPrecedence opPrecedence;
        private EchelonScriptTokenType tokenType;
        private SimpleUnaryExprType expressionType;

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

    private class DoubleDereferenceExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance fields

        private ExpressionPrecedence opPrecedence;

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

    private class ParenthesisExpressionParselet : IPrefixExpressionParselet {
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
            parser.GuaranteeToken (tkPair.tk, EchelonScriptTokenType.ParenClose, "')'");

            return new ES_AstParenthesisExpression (expr, token, tkPair.tk);
        }

        #endregion
    }

    private class MemberAccessExpressionParselet : IPostfixExpressionParselet {
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

    private class LiteralExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) {
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
                    // TODO: FIXME
                    /*if (token.DecodedStringUTF32 == null || token.DecodedStringUTF32.Length != 1)
                        expr = new ES_AstCharLiteralExpression (0, token);
                    else
                        expr = new ES_AstCharLiteralExpression (token.DecodedStringUTF32 [0], token);*/

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

    private class NameExpressionParselet : IPrefixExpressionParselet {
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

    private class FullyQualifiedNameExpressionParselet : IPrefixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) {
            return
                token.Type == EchelonScriptTokenType.Identifier &&
                !token.CheckIdentifier (ES_Keywords.New) &&
                !token.CheckIdentifier (ES_Keywords.Null) &&
                !token.CheckIdentifier (ES_Keywords.Cast) &&
                parser.tokenizer.PeekNextToken (1).tk.Type == EchelonScriptTokenType.NamespaceOp;
        }

        public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
            var ns = parser.ParseNamespaceIdentifier (true);
            parser.tokenizer.NextToken ();
            var id = parser.tokenizer.NextToken ().tk;

            return new ES_AstFullyQualifiedNameExpression (ns, id);
        }

        #endregion
    }

    private class FunctionCallExpressionParselet : IPostfixExpressionParselet {
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

    private class IndexingExpressionParselet : IPostfixExpressionParselet {
        #region ================== Instance properties

        public ExpressionPrecedence PostfixPrecedence => ExpressionPrecedence.Primary;

        #endregion

        #region ================== Instance methods

        public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token)
            => token.Type == EchelonScriptTokenType.BracketOpen;

        public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
            var tkPair = parser.tokenizer.NextToken ();
            if (tkPair.tk.Type != EchelonScriptTokenType.BracketOpen)
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

    private class NewExpressionParselet : IPrefixExpressionParselet {
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

    private class NullExpressionParselet : IPrefixExpressionParselet {
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

    private class IncDecExpressionParselet : IPrefixExpressionParselet, IPostfixExpressionParselet {
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

    private class CastExpressionParselet : IPrefixExpressionParselet {
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

    private class ConditionalExpressionParselet : IPostfixExpressionParselet {
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
#endif
}
