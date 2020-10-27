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
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Reflection;
using EchelonScriptCompiler.Utilities;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Parser {
    public struct ES_AggregateModifiers {
        public ES_AccessModifier? AccessModifier;
        public bool? Static;
        public bool? Const;

        public EchelonScriptToken? DocComment;

        public void ResetToNull () {
            AccessModifier = null;
            Static = null;
            Const = null;

            DocComment = null;
        }

        public void CopyDefaultsToUndefined (ES_AggregateModifiers defaults) {
            if (AccessModifier == null)
                AccessModifier = defaults.AccessModifier;

            if (Static == null)
                Static = defaults.Static;

            if (Const == null)
                Const = defaults.Const;
        }

        public bool AnyUndefined () {
            return !AccessModifier.HasValue || !Static.HasValue || !Const.HasValue;
        }

        public bool AnySet () {
            return AccessModifier.HasValue || Static.HasValue || Const.HasValue;
        }
    }

    public static class ES_PrimitiveTypes {
        public const string Object = "object";
        public const string Bool = "bool";

        public const string Int64 = "int64";
        public const string Int32 = "int32";
        public const string Int16 = "int16";
        public const string Int8  = "int8";

        public const string UInt64 = "int64";
        public const string UInt32 = "int32";
        public const string UInt16 = "int16";
        public const string UInt8  = "int8";

        public const string Float32 = "float32";
        public const string Float64 = "float64";

        public const string String = "string";
        public const string Char = "char";
    }

    public static class ES_Keywords {
        public const string Using = "using";
        public const string Alias = "alias";
        public const string Namespace = "namespace";

        public const string Var = "var";
        public const string New = "new";
        public const string Cast = "cast";

        public const string Void = "void";
        public const string Const = "const";
        public const string Immutable = "immutable";

        public const string Public = "public";
        public const string Protected = "protected";
        public const string Internal = "internal";
        public const string Private = "private";

        public const string Static = "static";

        public const string Class = "class";
        public const string Struct = "struct";
        public const string Enum = "enum";

        public const string Ref = "ref";
        public const string In = "in";
        public const string Out = "out";

        public const string If = "if";
        public const string Else = "else";
        public const string Switch = "switch";
        public const string Break = "break";
        public const string Continue = "continue";
        public const string Goto = "goto";
        public const string Return = "return";

        public const string While = "while";
        public const string For = "for";

        public const string Case = "case";
        public const string Default = "default";
    }

    public class EchelonScriptParser : IDisposable {
        #region ================== Expression parsing

        protected enum ExpressionPrecedence {
            NO_PRECEDENCE = 0,

            Assignment,
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
            Unary,
            Primary,
        }

        protected interface IPrefixExpressionParselet {
            public ExpressionPrecedence PrefixPrecedence { get; }

            public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token);

            public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token);
        }

        protected interface IPostfixExpressionParselet {
            public ExpressionPrecedence PostfixPrecedence { get; }

            public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token);

            public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token);
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

            #region ================== Constructors

            public SimpleBinaryExpressionParselet (ExpressionPrecedence precedence, EchelonScriptTokenType type, SimpleBinaryExprType exprType) {
                opPrecedence = precedence;
                tokenType = type;
                expressionType = exprType;
            }

            #endregion

            #region ================== Instance methods

            public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
                return token.Type == tokenType;
            }

            public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
                parser.tokenizer.NextToken ();

                var right = parser.ParseExpression (opPrecedence);

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

            #region ================== Constructors

            public SimpleUnaryExpressionParselet (ExpressionPrecedence precedence, EchelonScriptTokenType type, SimpleUnaryExprType exprType) {
                opPrecedence = precedence;
                tokenType = type;
                expressionType = exprType;
            }

            #endregion

            #region ================== Instance methods

            public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) {
                return token.Type == tokenType;
            }

            public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
                parser.tokenizer.NextToken ();

                var inner = parser.ParseExpression ();

                return new ES_AstSimpleUnaryExpression (token, expressionType, inner);
            }

            #endregion
        }

        #region Primary expressions

        protected class ParenthesisExpressionParselet : IPrefixExpressionParselet {
            #region ================== Instance properties

            public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Primary;

            #endregion

            #region ================== Instance methods

            public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) {
                return token.Type == EchelonScriptTokenType.ParenOpen;
            }

            public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
                parser.tokenizer.NextToken ();

                var expr = parser.ParseExpression ();

                var tkPair = parser.tokenizer.PeekNextToken ();
                if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                    parser.errorsList.Add (ES_Errors.GenExpectedXGotY ("')'", tkPair.tk));
                else
                    parser.tokenizer.NextToken ();

                return new ES_AstParenthesisExpression (expr, token, tkPair.tk);
            }

            #endregion
        }

        protected class LiteralExpressionParselet : IPrefixExpressionParselet {
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
                    token.Type == EchelonScriptTokenType.CharacterLiteral
                );
            }

            public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
                ES_AstExpression expr = null;

                switch (token.Type) {
                    case EchelonScriptTokenType.DecIntegerLiteral: {
                        bool isLong = false;
                        bool isUnsigned = false;

                        Span<char> chars = stackalloc char [token.Text.Length];
                        int charsCount = 0;
                        foreach (var c in token.Text.Span) {
                            if (EchelonScriptTokenizer.IsIntegerDigit (c))
                                chars [charsCount++] = c;
                            else if (c == 'L')
                                isLong = true;
                            else if (c == 'u' || c == 'U')
                                isUnsigned = true;
                        }

                        if (long.TryParse (chars.Slice (0, charsCount), System.Globalization.NumberStyles.None, null, out var resultLong))
                            expr = new ES_AstIntegerLiteralExpression (isUnsigned, isLong, (ulong) resultLong, token);
                        else if (ulong.TryParse (chars.Slice (0, charsCount), System.Globalization.NumberStyles.None, null, out var resultULong))
                            expr = new ES_AstIntegerLiteralExpression (isUnsigned, isLong, resultULong, token);
                        else
                            parser.errorsList.Add (new EchelonScriptErrorMessage (token, ES_Errors.IntLiteralTooBig));

                        break;
                    }
                    case EchelonScriptTokenType.HexIntegerLiteral: {
                        bool isLong = false;
                        bool isUnsigned = false;

                        Span<char> chars = stackalloc char [token.Text.Length];
                        int charsCount = 0;
                        foreach (var c in token.Text.Span.Slice (2)) {
                            if (EchelonScriptTokenizer.IsHexDigit (c))
                                chars [charsCount++] = c;
                            else if (c == 'L')
                                isLong = true;
                            else if (c == 'u' || c == 'U')
                                isUnsigned = true;
                        }

                        if (ulong.TryParse (chars.Slice (0, charsCount), System.Globalization.NumberStyles.AllowHexSpecifier, null, out var resultULong))
                            expr = new ES_AstIntegerLiteralExpression (isUnsigned, isLong, resultULong, token);
                        else
                            parser.errorsList.Add (new EchelonScriptErrorMessage (token, ES_Errors.IntLiteralTooBig));

                        break;
                    }
                    case EchelonScriptTokenType.BinIntegerLiteral: {
                        bool isLong = false;
                        bool isUnsigned = false;

                        Span<char> chars = stackalloc char [token.Text.Length];
                        int charsCount = 0;
                        foreach (var c in token.Text.Span.Slice (2)) {
                            if (EchelonScriptTokenizer.IsBinaryDigit (c))
                                chars [charsCount++] = c;
                            else if (c == 'L')
                                isLong = true;
                            else if (c == 'u' || c == 'U')
                                isUnsigned = true;
                        }

                        if (charsCount > 64)
                            parser.errorsList.Add (new EchelonScriptErrorMessage (token, ES_Errors.IntLiteralTooBig));

                        ulong value = 0;
                        int bitOffs = 0;
                        foreach (var c in chars.Slice (0, charsCount)) {
                            if (c == '1')
                                value |= 1uL << bitOffs;

                            bitOffs++;
                        }

                        expr = new ES_AstIntegerLiteralExpression (isUnsigned, isLong, value, token);

                        break;
                    }

                    case EchelonScriptTokenType.FloatLiteral: {
                        bool isFloat = false;

                        ReadOnlySpan<char> chars = token.Text.Span;
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
                            parser.errorsList.Add (new EchelonScriptErrorMessage (token, ES_Errors.InvalidFloatLiteral));

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

                    default:
                        throw new Exception ();
                }

                parser.tokenizer.NextToken ();

                return expr;
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
                    !token.Text.Span.Equals (ES_Keywords.New, StringComparison.Ordinal) &&
                    !token.Text.Span.Equals (ES_Keywords.Cast, StringComparison.Ordinal);
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

            public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
                return token.Type == EchelonScriptTokenType.ParenOpen;
            }

            public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
                var args = parser.ParseArgumentsList (out var endToken);

                return new ES_AstFunctionCallExpression (left, args, endToken);
            }

            #endregion
        }

        protected class IndexingExpressionParselet : IPostfixExpressionParselet {
            #region ================== Instance properties

            public ExpressionPrecedence PostfixPrecedence => ExpressionPrecedence.Primary;

            #endregion

            #region ================== Instance methods

            public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
                return token.Type == EchelonScriptTokenType.BracketOpen;
            }

            public ES_AstExpression Parse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
                var tkPair = parser.tokenizer.NextToken ();
                if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BracketOpen, null) != EnsureTokenResult.Correct)
                    parser.errorsList.Add (ES_Errors.GenExpectedXGotY ("'['", tkPair.tk));

                if (parser.tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.BracketClose) {
                    tkPair = parser.tokenizer.NextToken ();
                    return new ES_AstIndexingExpression (left, Array.Empty<ES_AstExpression> (), tkPair.tk);
                }

                using var ranksList = new StructPooledList<ES_AstExpression> (ClearMode.Auto);

                EchelonScriptToken endTk;
                while (true) {
                    tkPair = parser.tokenizer.PeekNextToken ();

                    if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                        parser.errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                        endTk = tkPair.tk;
                        break;
                    }

                    var argumentExpr = parser.ParseExpression ();
                    ranksList.Add (argumentExpr);

                    tkPair = parser.tokenizer.NextToken ();
                    if (tkPair.tk.Type == EchelonScriptTokenType.BracketClose) {
                        endTk = tkPair.tk;
                        break;
                    }  else if (tkPair.tk.Type != EchelonScriptTokenType.Comma)
                        parser.errorsList.Add (ES_Errors.GenExpectedXGotY ("',' or ']'", tkPair.tk));
                }

                return new ES_AstIndexingExpression (left, ranksList.ToArray (), endTk);
            }

            #endregion
        }

        protected class NewExpressionParselet : IPrefixExpressionParselet {
            #region ================== Instance properties

            public ExpressionPrecedence PrefixPrecedence => ExpressionPrecedence.Primary;

            #endregion

            #region ================== Instance methods

            public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) {
                return token.Type == EchelonScriptTokenType.Identifier && token.Text.Span.Equals (ES_Keywords.New, StringComparison.Ordinal);
            }

            public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
                parser.tokenizer.NextToken ();

                var typeDecl = parser.ParseTypeDeclaration ();
                var args = parser.ParseArgumentsList (out var endTk);

                return new ES_AstNewExpression (typeDecl, args, token, endTk);
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

            public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) {
                return token.Type == EchelonScriptTokenType.PlusPlus || token.Type == EchelonScriptTokenType.MinusMinus;
            }

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

            public bool CheckCanParse (EchelonScriptParser parser, ES_AstExpression left, EchelonScriptToken token) {
                return token.Type == EchelonScriptTokenType.PlusPlus || token.Type == EchelonScriptTokenType.MinusMinus;
            }

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

            public bool CheckCanParse (EchelonScriptParser parser, EchelonScriptToken token) {
                return token.Type == EchelonScriptTokenType.Identifier && token.Text.Span.Equals (ES_Keywords.Cast, StringComparison.Ordinal);
            }

            public ES_AstExpression Parse (EchelonScriptParser parser, EchelonScriptToken token) {
                parser.tokenizer.NextToken ();

                var tkPair = parser.tokenizer.PeekNextToken ();
                if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
                    parser.errorsList.Add (ES_Errors.GenExpectedXGotY ("'('", tkPair.tk));
                else
                    parser.tokenizer.NextToken ();

                var typeDecl = parser.ParseTypeDeclaration ();

                tkPair = parser.tokenizer.PeekNextToken ();
                if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                    parser.errorsList.Add (ES_Errors.GenExpectedXGotY ("')'", tkPair.tk));
                else
                    parser.tokenizer.NextToken ();

                var innerExpr = parser.ParseExpression ();

                return new ES_AstCastExpression (typeDecl, innerExpr, token, tkPair.tk);
            }

            #endregion
        }

        #endregion

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
        protected ReadOnlyMemory<char> sourceText;

        #endregion

        #region ================== Instance properties

        public IReadOnlyList<EchelonScriptErrorMessage> Errors { get => errorsList; }

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
            if (!PostfixExprParsers.ContainsKey (tokenType))
                PostfixExprParsers.Add (tokenType, new List<IPostfixExpressionParselet> ());

            PostfixExprParsers [tokenType].Add (new SimpleBinaryExpressionParselet (precedence, tokenType, exprType));
        }

        static void AddSimpleUnaryExpr (ExpressionPrecedence precedence, EchelonScriptTokenType tokenType, SimpleUnaryExprType exprType) {
            if (!PrefixExprParsers.ContainsKey (tokenType))
                PrefixExprParsers.Add (tokenType, new List<IPrefixExpressionParselet> ());

            PrefixExprParsers [tokenType].Add (new SimpleUnaryExpressionParselet (precedence, tokenType, exprType));
        }

        #endregion

        static EchelonScriptParser () {
            // Primitive types
            using var primitiveTypesList = new StructPooledList<string> (ClearMode.Auto);
            var primitiveTypesConstants = typeof (ES_PrimitiveTypes).GetFields (BindingFlags.Public | BindingFlags.Static);

            foreach (var primitiveConst in primitiveTypesConstants) {
                if (primitiveConst.IsLiteral && !primitiveConst.IsInitOnly)
                    primitiveTypesList.Add (primitiveConst.GetValue (null) as string);
            }

            PrimitiveTypes = primitiveTypesList.ToArray ();

            // Keywords
            using var keywordsList = new StructPooledList<string> (ClearMode.Auto);
            var keywordsConstants = typeof (ES_Keywords).GetFields (BindingFlags.Public | BindingFlags.Static);

            foreach (var keywordConst in keywordsConstants) {
                if (keywordConst.IsLiteral && !keywordConst.IsInitOnly)
                    keywordsList.Add (keywordConst.GetValue (null) as string);
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
            }
            AddPrefixParselet (EchelonScriptTokenType.Identifier, new NameExpressionParselet ());
            AddSimpleBinaryExpr (ExpressionPrecedence.Primary, EchelonScriptTokenType.Dot, SimpleBinaryExprType.MemberAccess);
            AddPostfixParselet (EchelonScriptTokenType.ParenOpen, new FunctionCallExpressionParselet ());
            AddPostfixParselet (EchelonScriptTokenType.BracketOpen, new IndexingExpressionParselet ());
            AddPostfixParselet (EchelonScriptTokenType.PlusPlus, incDecParselet);
            AddPostfixParselet (EchelonScriptTokenType.MinusMinus, incDecParselet);
            AddPrefixParselet (EchelonScriptTokenType.Identifier, new NewExpressionParselet ());

            #endregion

            #region Unary expressions

            AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Plus, SimpleUnaryExprType.Positive);
            AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Minus, SimpleUnaryExprType.Negative);
            AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Bang, SimpleUnaryExprType.LogicalNot);
            AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Tilde, SimpleUnaryExprType.BitNot);
            AddPostfixParselet (EchelonScriptTokenType.PlusPlus, incDecParselet);
            AddPostfixParselet (EchelonScriptTokenType.MinusMinus, incDecParselet);
            AddPrefixParselet (EchelonScriptTokenType.Identifier, new CastExpressionParselet ());

            #endregion

            #region Binary ops

            AddSimpleBinaryExpr (ExpressionPrecedence.Concatenation, EchelonScriptTokenType.Tilde, SimpleBinaryExprType.Concatenation);

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

            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.Equals, SimpleBinaryExprType.Assign);

            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.PlusEq, SimpleBinaryExprType.AssignAdd);
            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.MinusEq, SimpleBinaryExprType.AssignSubtract);

            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.MultiplyEq, SimpleBinaryExprType.AssignMultiply);
            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.DivideEq, SimpleBinaryExprType.AssignDivide);
            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ModuloEq, SimpleBinaryExprType.AssignModulo);
            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.PowerOpEq, SimpleBinaryExprType.AssignPower);

            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.AndEq, SimpleBinaryExprType.AssignBitAnd);
            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.BitOrEq, SimpleBinaryExprType.AssignBitOr);
            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.XorEq, SimpleBinaryExprType.AssignXor);

            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.TildeEq, SimpleBinaryExprType.AssignTilde);

            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ShiftLeftEq, SimpleBinaryExprType.AssignShiftLeft);
            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ShiftRightEq, SimpleBinaryExprType.AssignShiftRight);
            AddSimpleBinaryExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ShiftRightUEq, SimpleBinaryExprType.AssignShiftRightUnsigned);

            #endregion

            #endregion

            // Shrink the lists to fit
            foreach (var kvp in PrefixExprParsers)
                kvp.Value.Capacity = kvp.Value.Count;
            foreach (var kvp in PostfixExprParsers)
                kvp.Value.Capacity = kvp.Value.Count;
        }

        public EchelonScriptParser () {
            errorsList = new List<EchelonScriptErrorMessage> ();
            tokenizer = new EchelonScriptTokenizer (errorsList);
        }

        #endregion

        #region ================== Instance methods

        #region Public methods

        public void Reset () {
            CheckDisposed ();

            errorsList.Clear ();
            tokenizer.Reset ();
            sourceText = null;
        }

        public ES_AbstractSyntaxTree ParseCode (ReadOnlyMemory<char> codeData) {
            CheckDisposed ();

            Reset ();
            sourceText = codeData;
            tokenizer.SetSource (codeData);

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

        protected static EnsureTokenResult EnsureToken (EchelonScriptToken tk, EchelonScriptTokenType type, string text) {
            if (tk.Type != type)
                return EnsureTokenResult.WrongType;

            if (text != null && !tk.Text.Span.Equals (text, StringComparison.Ordinal))
                return EnsureTokenResult.WrongText;

            return EnsureTokenResult.Correct;
        }

        protected EnsureTokenResult EnsureTokenPeek (EchelonScriptTokenType type, string text) {
            var tkPair = tokenizer.PeekNextToken ();
            return EnsureToken (tkPair.tk, type, text);
        }

        protected EnsureTokenResult EnsureTokenPeek (int offset, EchelonScriptTokenType type, string text) {
            var tkPair = tokenizer.PeekNextToken (offset);
            return EnsureToken (tkPair.tk, type, text);
        }

        protected bool CheckNoAccessModifierErrors (EchelonScriptToken tk, ES_AggregateModifiers currentModifiers) {
            if (currentModifiers.Static == true) {
                errorsList.Add (new EchelonScriptErrorMessage (tk, ES_Errors.AccessBeforeStorage));
                return false;
            }
            if (currentModifiers.AccessModifier != null) {
                errorsList.Add (new EchelonScriptErrorMessage (tk, ES_Errors.MultipleAccessMods));
                return false;
            }

            return true;
        }

        protected void ParseAggregate_SetDocComment (ref ES_AggregateModifiers curMods, EchelonScriptToken? newDocCom) {
            if (newDocCom == null)
                return;

            if (curMods.AnySet ()) {
                errorsList.Add (new EchelonScriptErrorMessage (newDocCom.Value, ES_Errors.UnexpectedDocComment));
                return;
            }

            curMods.DocComment = newDocCom;
        }

        protected bool IsIntegerLiteral (EchelonScriptToken tk) {
            return (
                tk.Type == EchelonScriptTokenType.DecIntegerLiteral ||
                tk.Type == EchelonScriptTokenType.BinIntegerLiteral ||
                tk.Type == EchelonScriptTokenType.HexIntegerLiteral
            );
        }

        protected bool IsFloatLiteral (EchelonScriptToken tk) {
            return (
                tk.Type == EchelonScriptTokenType.FloatLiteral ||
                tk.Type == EchelonScriptTokenType.DecIntegerLiteral
            );
        }

        protected bool IsKeyword (EchelonScriptToken tk) {
            var idSpan = tk.Text.Span;

            foreach (var keyword in Keywords) {
                if (idSpan.Equals (keyword, StringComparison.Ordinal))
                    return true;
            }

            return false;
        }

        protected bool IsPrimitiveType (EchelonScriptToken tk) {
            var idSpan = tk.Text.Span;

            foreach (var type in PrimitiveTypes) {
                if (idSpan.Equals (type, StringComparison.Ordinal))
                    return true;
            }

            return false;
        }

        #endregion

        #region Basic data

        protected EchelonScriptToken? ParseIdentifier () {
            var tkPair = tokenizer.NextToken ();

            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                return null;
            }

            return tkPair.tk;
        }

        protected bool ParseUserIdentifier (out EchelonScriptToken token) {
            var tkPair = tokenizer.NextToken ();
            token = tkPair.tk;

            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                return false;
            }

            if (IsKeyword (tkPair.tk)) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.InvalidUserIdentifier));
                return false;
            }

            if (IsPrimitiveType (tkPair.tk)) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.InvalidUserIdentifier));
                return false;
            }

            return true;
        }

        protected ES_AstDottableIdentifier ParseDottableIdentifier () {
            if (EnsureTokenPeek (EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            using var partsList = new StructPooledList<EchelonScriptToken> (ClearMode.Auto);

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
                    errorsList.Add (new EchelonScriptErrorMessage (part, ES_Errors.InvalidUserIdentifier));

                if (IsPrimitiveType (part))
                    errorsList.Add (new EchelonScriptErrorMessage (part, ES_Errors.InvalidUserIdentifier));
            }

            return ret;
        }

        protected ES_AstTypeDeclaration ParseTypeDeclaration () {
            ES_AstTypeDeclaration innerDecl = null;

            while (true) {
                var tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                    bool isConst = tkPair.tk.Text.Span.Equals (ES_Keywords.Const, StringComparison.Ordinal);
                    bool isImmutable = tkPair.tk.Text.Span.Equals (ES_Keywords.Immutable, StringComparison.Ordinal);

                    if (isConst || isImmutable) {
                        var startTk = tokenizer.NextToken ();

                        tkPair = tokenizer.NextToken ();
                        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
                            errorsList.Add (ES_Errors.GenExpectedXGotY ("'('", tkPair.tk));

                        var innerType = ParseTypeDeclaration ();

                        var parenCloseTk = tokenizer.PeekNextToken ();
                        if (EnsureToken (parenCloseTk.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                            errorsList.Add (ES_Errors.GenExpectedXGotY ("')'", parenCloseTk.tk));
                        else
                            tokenizer.NextToken ();

                        ES_AstTypeDeclaration_Basic.DeclType declType = ES_AstTypeDeclaration_Basic.DeclType.Const;
                        if (isConst) declType = ES_AstTypeDeclaration_Basic.DeclType.Const;
                        else if (isImmutable) declType = ES_AstTypeDeclaration_Basic.DeclType.Immutable;

                        var bounds = new ES_AstNodeBounds (startTk.tk.TextStartPos, innerDecl?.NodeBounds.EndPos ?? parenCloseTk.tk.TextEndPos);
                        innerDecl = new ES_AstTypeDeclaration_Basic (declType, innerDecl, bounds);
                    } else if (innerDecl == null) {
                        var dottableId = ParseDottableIdentifier ();
                        innerDecl = new ES_AstTypeDeclaration_TypeName (dottableId);
                    } else
                        break;
                } else if (innerDecl == null && tkPair.tk.Type == EchelonScriptTokenType.ParenOpen) {
                    tokenizer.NextToken ();

                    var innerType = ParseTypeDeclaration ();

                    tkPair = tokenizer.PeekNextToken ();
                    if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                        errorsList.Add (ES_Errors.GenExpectedXGotY ("')'", tkPair.tk));
                    else
                        tokenizer.NextToken ();

                    innerDecl = innerType;
                } else if (tkPair.tk.Type == EchelonScriptTokenType.Asterisk) {
                    if (innerDecl == null) {
                        errorsList.Add (ES_Errors.GenUnexpectedToken (tkPair.tk));
                        break;
                    }

                    var bounds = new ES_AstNodeBounds (innerDecl?.NodeBounds.StartPos ?? tkPair.tk.TextStartPos, tkPair.tk.TextEndPos);
                    innerDecl = new ES_AstTypeDeclaration_Basic (ES_AstTypeDeclaration_Basic.DeclType.Pointer, innerDecl, bounds);
                } else if (tkPair.tk.Type == EchelonScriptTokenType.Question) {
                    if (innerDecl == null) {
                        errorsList.Add (ES_Errors.GenUnexpectedToken (tkPair.tk));
                        break;
                    }

                    var bounds = new ES_AstNodeBounds (innerDecl?.NodeBounds.StartPos ?? tkPair.tk.TextStartPos, tkPair.tk.TextEndPos);
                    innerDecl = new ES_AstTypeDeclaration_Basic (ES_AstTypeDeclaration_Basic.DeclType.Nullable, innerDecl, bounds);
                } else if (tkPair.tk.Type == EchelonScriptTokenType.BracketOpen) {
                    if (innerDecl == null) {
                        errorsList.Add (ES_Errors.GenUnexpectedToken (tkPair.tk));
                        break;
                    }

                    using var ranks = new StructPooledList<ES_AstExpression> (ClearMode.Auto);

                    EchelonScriptToken endTk;
                    ES_AstExpression lastDim = null;
                    while (true) {
                        tkPair = tokenizer.PeekNextToken ();

                        if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                            endTk = tkPair.tk;
                            break;
                        }

                        if (tkPair.tk.Type == EchelonScriptTokenType.BracketClose) {
                            endTk = tokenizer.NextToken ().tk;

                            ranks.Add (lastDim);
                            lastDim = null;

                            break;
                        } else if (tkPair.tk.Type == EchelonScriptTokenType.Comma) {
                            tokenizer.NextToken ();

                            ranks.Add (lastDim);
                            lastDim = null;
                        } else
                            lastDim = ParseExpression ();
                    }

                    innerDecl = new ES_AstTypeDeclaration_Array (innerDecl, ranks.ToArray (), endTk);
                } else
                    break;
            }

            return innerDecl;
        }

        #endregion

        protected ES_AbstractSyntaxTree ParseCodeUnit () {
            using var importsList = new StructPooledList<ES_AstImportStatement> (ClearMode.Auto);
            using var aliasesList = new StructPooledList<ES_AstTypeAlias> (ClearMode.Auto);
            using var namespacesList = new StructPooledList<ES_AstNamespace> (ClearMode.Auto);

            while (true) {
                var tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF)
                    break;

                if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
                    tkPair = tokenizer.NextToken ();
                    var tokenText = StringPool.Shared.GetOrAdd (tkPair.tk.Text.Span);
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.CodeUnit_UnexpectedToken.Replace ("{0}", tokenText)));
                    continue;
                }

                var textSpan = tkPair.tk.Text.Span;
                if (textSpan.Equals (ES_Keywords.Using, StringComparison.Ordinal)) {
                    if (namespacesList.Count > 0)
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.ImportAfterNamespace));

                    var importStatement = ParseStatement_Import ();
                    if (importStatement != null)
                        importsList.Add (importStatement);
                } else if (textSpan.Equals (ES_Keywords.Alias, StringComparison.Ordinal)) {
                    if (namespacesList.Count > 0)
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.AliasAfterNamespace));

                    var aliasStatement = ParseStatement_Alias ();
                    if (aliasStatement != null)
                        aliasesList.Add (aliasStatement);
                } else if (textSpan.Equals (ES_Keywords.Namespace, StringComparison.Ordinal)) {
                    var namespaceNode = ParseNamespace ();
                    if (namespaceNode != null)
                        namespacesList.Add (namespaceNode);
                } else {
                    tokenizer.NextToken ();
                    var tokenText = StringPool.Shared.GetOrAdd (tkPair.tk.Text.Span);
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.CodeUnit_UnexpectedToken.Replace ("{0}", tokenText)));
                }
            }

            int startPos = int.MaxValue;
            int endPos = int.MinValue;

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
                importsList.ToArray (),
                aliasesList.ToArray (),
                namespacesList.ToArray (),
                new ES_AstNodeBounds (startPos, endPos)
            );
            codeUnit.Valid = errorsList.Count == 0;

            return codeUnit;
        }

        protected ES_AstNamespace ParseNamespace () {
            var namespaceTk = tokenizer.NextToken ().tk;
            if (EnsureToken (namespaceTk, EchelonScriptTokenType.Identifier, ES_Keywords.Namespace) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                return null;
            }

            var defaultModifiers = new ES_AggregateModifiers {
                AccessModifier = ES_AccessModifier.Private,
                Static = false,
                Const = false,
            };

            var namespaceName = ParseDottableUserIdentifier ();
            var namespaceContents = ParseAggregateOrNamespace (null, defaultModifiers, true, out var endTk);

            return new ES_AstNamespace (namespaceName, namespaceContents, namespaceTk, endTk);
        }

        #region Aggregates

        protected ES_AstNode [] ParseAggregateOrNamespace (
            ES_AggregateModifiers? baseModifiersArg, ES_AggregateModifiers defaultModifiers, bool isNamespace,
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
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'{'", tkPair.tk));
                endTk = new EchelonScriptToken { TextStartPos = tkPair.tk.TextStartPos, };
                return Array.Empty<ES_AstNode> ();
            }

            using var contents = new StructPooledList<ES_AstNode> (ClearMode.Auto);
            ES_AggregateModifiers currentModifiers = new ES_AggregateModifiers ();
            currentModifiers.ResetToNull ();

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    endTk = tkPair.tk;
                    break;
                }

                if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                    endTk = tokenizer.NextToken ().tk;
                    if (currentModifiers.AnySet ())
                        errorsList.Add (ES_Errors.GenExpectedAggregateContent (tkPair.tk));

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
                            if (isNamespace) {
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.InvalidAccessModForNamespaceContent));
                                break;
                            }

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
                            if (isNamespace) {
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.InvalidAccessModForNamespaceContent));
                                break;
                            } else if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                                currentModifiers.AccessModifier = ES_AccessModifier.Private;
                            break;

                        /* Other modifiers */
                        case ES_Keywords.Static:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                            tokenizer.NextToken ();

                            if (currentModifiers.Static == true)
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.ElementAlreadyStatic));

                            currentModifiers.Static = true;
                            break;
                        case ES_Keywords.Const:
                            if (tokenizer.PeekNextToken (1).tk.Type != EchelonScriptTokenType.ParenOpen) {
                                ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                                tokenizer.NextToken ();

                                if (currentModifiers.Const == true)
                                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.ElementAlreadyConst));

                                currentModifiers.Const = true;
                                break;
                            } else
                                goto ParseFunctionOrVariable;

                        case ES_Keywords.Namespace:
                            endTk = new EchelonScriptToken { TextStartPos = tkPair.tk.TextStartPos, };
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.NamespaceMissingBrace));
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
                            goto ParseFunctionOrVariable;

                        default:
                            goto ParseFunctionOrVariable;

                        ParseFunctionOrVariable:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                            currentModifiers.CopyDefaultsToUndefined (baseModifiers);
                            currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                            if (!isNamespace)
                                contents.AddRange (ParseFunctionOrVariable (currentModifiers));
                            else
                                contents.Add (ParseFunction (currentModifiers, null));

                            currentModifiers.ResetToNull ();
                            break;
                    }
                } else {
                    if (tkPair.tk.Type == EchelonScriptTokenType.ParenOpen) {
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                        currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                        if (!isNamespace)
                            contents.AddRange (ParseFunctionOrVariable (currentModifiers));
                        else
                            contents.Add (ParseFunction (currentModifiers, null));

                        currentModifiers.ResetToNull ();
                    } else {
                        if (!isNamespace && tkPair.tk.Type == EchelonScriptTokenType.BraceOpen && currentModifiers.AnySet ()) {
                            currentModifiers.CopyDefaultsToUndefined (baseModifiers);

                            var newContents = ParseAggregateOrNamespace (currentModifiers, defaultModifiers, false, out var _);
                            contents.AddRange (newContents);

                            currentModifiers.ResetToNull ();
                        } else {
                            tokenizer.NextToken ();

                            if (!isNamespace)
                                errorsList.Add (ES_Errors.GenExpectedXGotY ("a keyword, function definition, variable definition or '}'", tkPair.tk));
                            else
                                errorsList.Add (ES_Errors.GenExpectedXGotY ("a keyword, function definition or '}'", tkPair.tk));
                        }
                    }
                }
            }

            return contents.ToArray ();
        }

        protected ES_AstDottableIdentifier [] ParseInheritanceList () {
            var tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type != EchelonScriptTokenType.Colon)
                return null;
            tokenizer.NextToken ();

            using var listContents = new StructPooledList<ES_AstDottableIdentifier> (ClearMode.Auto);

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                    errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                else
                    listContents.Add (ParseDottableUserIdentifier ());

                tkPair = tokenizer.PeekNextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.Comma)
                    tokenizer.NextToken ();
                else
                    break;
            }

            return listContents.ToArray ();
        }

        protected ES_AstMemberVarDefinition [] ParseMemberVar (ES_AggregateModifiers varModifiers, ES_AstTypeDeclaration valueType) {
            // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
            if (varModifiers.AnyUndefined ())
                throw new ArgumentException ("Modifiers set contains undefined values.", nameof (varModifiers));

            if (valueType == null)
                valueType = ParseTypeDeclaration ();

            using var varDataList = new StructPooledList<(EchelonScriptToken Name, ES_AstExpression Expr)> (ClearMode.Auto);

            while (true) {
                var tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                ParseUserIdentifier (out var functionName);
                ES_AstExpression expression = null;

                if (varModifiers.Const == true)
                    errorsList.Add (new EchelonScriptErrorMessage (functionName, ES_Errors.ConstOnlyOnFunctions));
                varModifiers.Const = false;

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

            var semicolonTkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (semicolonTkPair.tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", semicolonTkPair.tk));
            else
                tokenizer.NextToken ();

            using var memberVarsList = new StructPooledList<ES_AstMemberVarDefinition> (ClearMode.Auto);
            foreach (var memberVar in varDataList) {
                var memberVarDef = new ES_AstMemberVarDefinition (
                    varModifiers.AccessModifier.Value,
                    varModifiers.DocComment,

                    varModifiers.Static.Value,

                    memberVar.Name,
                    valueType,
                    memberVar.Expr,
                    semicolonTkPair.tk
                );
            }

            return memberVarsList.ToArray ();
        }

        protected ES_AstNode [] ParseFunctionOrVariable (ES_AggregateModifiers modifiers) {
            var typeDecl = ParseTypeDeclaration ();

            var nextTkPair = tokenizer.PeekNextToken (1);
            if (nextTkPair.tk.Type == EchelonScriptTokenType.ParenOpen)
                return new ES_AstNode [] { ParseFunction (modifiers, typeDecl) };
            else
                return ParseMemberVar (modifiers, typeDecl);
        }

        protected ES_AstClassDefinition ParseClass (ES_AggregateModifiers classModifiers) {
            // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
            if (classModifiers.AnyUndefined ())
                throw new ArgumentException ("Modifiers set contains undefined values.", nameof (classModifiers));

            var defaultModifiers = new ES_AggregateModifiers {
                AccessModifier = ES_AccessModifier.Private,
                Static = false,
                Const = false,
            };

            // Parse the 'class' keyword.
            var startTkPair = tokenizer.NextToken ();
            if (EnsureToken (startTkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.Class) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ($"\"{ES_Keywords.Class}\"", startTkPair.tk));

            // Parse the class' name.
            var nameTkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (nameTkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedIdentifier (nameTkPair.tk));
            else
                tokenizer.NextToken ();

            if (classModifiers.Const == true)
                errorsList.Add (new EchelonScriptErrorMessage (nameTkPair.tk, ES_Errors.ConstOnlyOnFunctions));
            classModifiers.Const = false;

            // Error out if this is a static class and it has an inheritance list.
            var colonTkPair = tokenizer.PeekNextToken ();
            if (classModifiers.Static == true && colonTkPair.tk.Type == EchelonScriptTokenType.Colon)
                errorsList.Add (new EchelonScriptErrorMessage (colonTkPair.tk, ES_Errors.InheritanceOnStaticClass));

            // Parse the inheritance list.
            var inheritanceList = ParseInheritanceList ();
            if (inheritanceList == null)
                inheritanceList = Array.Empty<ES_AstDottableIdentifier> ();

            // Parse the class' contents.
            var contents = ParseAggregateOrNamespace (null, defaultModifiers, false, out var endTk);

            // Create the class AST node.
            var classDef = new ES_AstClassDefinition (
                classModifiers.DocComment,

                classModifiers.AccessModifier.Value,
                classModifiers.Static.Value,

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
            };

            // Parse the 'struct' keyword.
            var startTkPair = tokenizer.NextToken ();
            if (EnsureToken (startTkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.Struct) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ($"\"{ES_Keywords.Struct}\"", startTkPair.tk));

            // Parse the struct's name.
            var nameTkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (nameTkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedIdentifier (nameTkPair.tk));
            else
                tokenizer.NextToken ();

            if (structModifiers.Const == true)
                errorsList.Add (new EchelonScriptErrorMessage (nameTkPair.tk, ES_Errors.ConstOnlyOnFunctions));
            structModifiers.Const = false;

            // Error out if this is a static struct and it has an inheritance list.
            var colonTkPair = tokenizer.PeekNextToken ();
            if (structModifiers.Static == true && colonTkPair.tk.Type == EchelonScriptTokenType.Colon)
                errorsList.Add (new EchelonScriptErrorMessage (colonTkPair.tk, ES_Errors.InheritanceOnStaticStruct));

            // Parse the inheritance list.
            var interfacesList = ParseInheritanceList ();
            if (interfacesList == null)
                interfacesList = Array.Empty<ES_AstDottableIdentifier> ();

            // Parse the struct's contents
            var contents = ParseAggregateOrNamespace (null, defaultModifiers, false, out var endTk);

            // Create the struct AST node.
            var structDef = new ES_AstStructDefinition (
                structModifiers.DocComment,

                structModifiers.AccessModifier.Value,
                structModifiers.Static.Value,

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
                errorsList.Add (ES_Errors.GenExpectedXGotY ($"\"{ES_Keywords.Enum}\"", startTkPair.tk));

            // Parse the enum's name.
            ParseUserIdentifier (out var nameToken);

            if (enumModifiers.Const == true)
                errorsList.Add (new EchelonScriptErrorMessage (nameToken, ES_Errors.ConstOnlyOnFunctions));
            enumModifiers.Const = false;

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
                    errorsList.Add (ES_Errors.GenExpectedXGotY ("'{'", openBraceTkPair.tk));
            }

            // Parse the enum's members
            using var membersList = new StructPooledList<(EchelonScriptToken, ES_AstExpression)> (ClearMode.Auto);

            EchelonScriptToken endTk;
            bool expectingEnd = false;
            while (true) {
                var tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    endTk = tkPair.tk;
                    break;
                }

                if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                    endTk = tokenizer.NextToken ().tk;
                    break;
                } else if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                    ParseUserIdentifier (out var memberName);
                    ES_AstExpression assignExpr = null;

                    // Error out if the previous member had no comma.
                    if (expectingEnd)
                        errorsList.Add (ES_Errors.GenExpectedXGotY ("'}'", tkPair.tk));

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
                    errorsList.Add (ES_Errors.GenExpectedXGotY ("a keyword or '}'", tkPair.tk));
                    tokenizer.NextToken ();
                }
            }

            // Create the enum AST node.
            var enumDef = new ES_AstEnumDefinition (
                enumModifiers.AccessModifier.Value,
                enumModifiers.DocComment,

                nameToken,
                baseType,

                membersList.ToArray (),
                startTkPair.tk,
                endTk
            );

            return enumDef;
        }

        protected ES_AstFunctionDefinition ParseFunction (ES_AggregateModifiers funcModifiers, ES_AstTypeDeclaration returnType) {
            // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
            if (funcModifiers.AnyUndefined ())
                throw new ArgumentException ("Modifiers set contains undefined values.", nameof (funcModifiers));

            if (returnType == null)
                returnType = ParseTypeDeclaration ();

            ParseUserIdentifier (out var functionName);

            var argumentsList = ParseArgumentsDefinitionList ();

            var statements = ParseStatementsBlock (out var endTk);

            var functionDef = new ES_AstFunctionDefinition (
                funcModifiers.AccessModifier.Value,
                funcModifiers.DocComment,

                funcModifiers.Static.Value,
                funcModifiers.Const.Value,

                functionName,
                returnType,
                argumentsList,

                statements,
                endTk
            );

            return functionDef;
        }

        protected ES_AstFunctionArgumentDefinition [] ParseArgumentsDefinitionList () {
            var tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'('", tkPair.tk));

            if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.ParenClose) {
                tokenizer.NextToken ();
                return Array.Empty<ES_AstFunctionArgumentDefinition> ();
            }

            using var argsList = new StructPooledList<ES_AstFunctionArgumentDefinition> (ClearMode.Auto);

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
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
                            errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                    }
                }

                var argumentType = ParseTypeDeclaration ();

                ParseUserIdentifier (out var argumentName);
                ES_AstExpression argumentExpr = null;

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
                    errorsList.Add (ES_Errors.GenExpectedXGotY ("',' or ')'", tkPair.tk));
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

                    var statement = ParseStatement ();

                    return new ES_AstLabeledStatement (labelName, statement);
                }

                switch (tkTextStr) {
                    case ES_Keywords.Var:
                        return ParseStatement_Variable (false);
                    case ES_Keywords.Using: {
                        if (EnsureTokenPeek (1, EchelonScriptTokenType.Identifier, ES_Keywords.Var) == EnsureTokenResult.Correct) {
                            tokenizer.NextToken ();
                            return ParseStatement_Variable (true);
                        } else
                            return ParseStatement_Import ();
                    }
                    case ES_Keywords.Alias:
                        return ParseStatement_Alias ();
                }
            }

            return ParseEmbeddedStatement ();
        }

        protected ES_AstStatement ParseEmbeddedStatement () {
            var tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
                tokenizer.NextToken ();
                return new ES_AstEmptyStatement (tkPair.tk);
            } if (tkPair.tk.Type == EchelonScriptTokenType.BraceOpen) {
                var statements = ParseStatementsBlock (out var endTk);
                return new ES_AstBlockStatement (statements, tkPair.tk, endTk);
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
                    case ES_Keywords.For:
                        return ParseStatement_For ();

                    case ES_Keywords.Return:
                        return ParseStatement_Return ();
                }
            }

            var retExpr = ParseStatement_Expression ();

            var isAllowedExpression = (
                retExpr.Expression is ES_AstFunctionCallExpression ||
                retExpr.Expression is ES_AstIncDecExpression
            );

            if (retExpr.Expression is ES_AstSimpleBinaryExpression) {
                var binExpr = retExpr.Expression as ES_AstSimpleBinaryExpression;

                if (binExpr.ExpressionType >= SimpleBinaryExprType.TAG_AssignExpr_Start &&
                    binExpr.ExpressionType <= SimpleBinaryExprType.TAG_AssignExpr_End)
                    isAllowedExpression = true;
            }

            if (!isAllowedExpression)
                errorsList.Add (new EchelonScriptErrorMessage (sourceText.Span, retExpr.NodeBounds, ES_Errors.IllegalExpressionStatement));

            return retExpr;
        }

        protected ES_AstStatement [] ParseStatementsBlock (out EchelonScriptToken endTk) {
            var tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'{'", tkPair.tk));
                endTk = new EchelonScriptToken ();
                return null;
            }

            using var contents = new StructPooledList<ES_AstStatement> (ClearMode.Auto);

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    endTk = tkPair.tk;
                    break;
                }

                if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                    endTk = tokenizer.NextToken ().tk;
                    break;
                } else
                    contents.Add (ParseStatement ());
            }

            return contents.ToArray ();
        }

        #region Symbol definition statements

        protected ES_AstImportStatement ParseStatement_Import () {
            var startTk = tokenizer.NextToken ().tk;
            if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Using) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                return null;
            }

            var namespaceName = ParseDottableUserIdentifier ();
            using var importedNamesList = new StructPooledList<EchelonScriptToken> (ClearMode.Auto);
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
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                        endTk = tkPair.tk;
                        break;
                    }

                    if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
                        endTk = tokenizer.NextToken ().tk;
                        break;
                    } else if (tkPair.tk.Type != EchelonScriptTokenType.Comma) {
                        errorsList.Add (ES_Errors.GenExpectedXGotY ("',' or ';'", tkPair.tk));
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
                errorsList.Add (ES_Errors.GenExpectedXGotY ("',' or ';'", tkPair.tk));

            return new ES_AstImportStatement (namespaceName, importedNamesList.ToArray (), startTk, endTk);
        }

        protected ES_AstTypeAlias ParseStatement_Alias () {
            var startTk = tokenizer.NextToken ().tk;
            if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Alias) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            if (!ParseUserIdentifier (out var aliasName))
                return null;

            var tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Equals, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'='", tkPair.tk));
                return null;
            }

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                return null;
            }
            var originalName = ParseDottableUserIdentifier ();

            var endTk = tokenizer.NextToken ().tk;
            if (EnsureToken (endTk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", endTk));

            return new ES_AstTypeAlias (aliasName, originalName, startTk, endTk);
        }

        protected ES_AstLocalVarDefinition ParseStatement_Variable (bool usingVar) {
            var varStartTk = tokenizer.NextToken ().tk;
            if (EnsureToken (varStartTk, EchelonScriptTokenType.Identifier, ES_Keywords.Var) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            ES_AstTypeDeclaration valueType;
            using var variables = new StructPooledList<(EchelonScriptToken, ES_AstExpression)> (ClearMode.Auto);
            bool initRequired;

            if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.Identifier &&
                tokenizer.PeekNextToken (1).tk.Type == EchelonScriptTokenType.Equals
            ) {
                initRequired = true;
                valueType = null;
            } else {
                initRequired = false;
                valueType = ParseTypeDeclaration ();
            }

            if (usingVar)
                initRequired = true;

            while (true) {
                EchelonScriptToken varName = new EchelonScriptToken ();
                ES_AstExpression initializationExpr = null;

                var tkPair = tokenizer.PeekNextToken ();
                if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                    errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                else
                    varName = tkPair.tk;

                tkPair = tokenizer.PeekNextToken ();
                if (!initRequired) {
                    if (tkPair.tk.Type == EchelonScriptTokenType.Equals) {
                        tokenizer.NextToken ();

                        initializationExpr = ParseExpression ();
                    }
                } else if (initRequired) {
                    if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Equals, null) != EnsureTokenResult.Correct)
                        errorsList.Add (ES_Errors.GenExpectedXGotY ("'='", tkPair.tk));
                    else
                        tokenizer.NextToken ();

                    initializationExpr = ParseExpression ();
                }

                variables.Add ((varName, initializationExpr));

                tkPair = tokenizer.PeekNextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.Comma) {
                    tokenizer.NextToken ();
                } else
                    break;
            }

            var endTk = tokenizer.PeekNextToken ().tk;
            if (EnsureToken (endTk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", endTk));

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

        protected ES_AstConditionalStatement ParseStatement_IfElse () {
            var startTk = tokenizer.NextToken ().tk;
            if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.If) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'('", tkPair.tk));
                return null;
            } else
                tokenizer.NextToken ();

            var conditionExpr = ParseExpression ();

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("')'", tkPair.tk));
            else
                tokenizer.NextToken ();

            var thenStatement = ParseEmbeddedStatement ();
            ES_AstStatement elseStatement = null;

            if (EnsureTokenPeek (EchelonScriptTokenType.Identifier, ES_Keywords.Else) == EnsureTokenResult.Correct) {
                tokenizer.NextToken ();

                elseStatement = ParseEmbeddedStatement ();
            }

            return new ES_AstConditionalStatement (
                conditionExpr,
                thenStatement,
                elseStatement,
                startTk
            );
        }

        protected ES_AstSwitchStatement ParseStatement_Switch () {
            var switchStartTk = tokenizer.NextToken ().tk;
            if (EnsureToken (switchStartTk, EchelonScriptTokenType.Identifier, ES_Keywords.Switch) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'('", tkPair.tk));
                return null;
            }

            var valueExpr = ParseExpression ();

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("')'", tkPair.tk));
            else
                tokenizer.NextToken ();

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'{'", tkPair.tk));
            else
                tokenizer.NextToken ();

            EchelonScriptToken endTK;
            using var sections = new StructPooledList<(ES_AstExpression [], ES_AstStatement [])> (ClearMode.Auto);
            using var sectionExpressions = new StructPooledList<ES_AstExpression> (ClearMode.Auto);
            while (true) {
                tkPair = tokenizer.NextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    endTK = tkPair.tk;
                    break;
                } else if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                    endTK = tkPair.tk;
                    break;
                }

                if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
                    errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                }

                if (tkPair.tk.Text.Span.Equals (ES_Keywords.Default, StringComparison.Ordinal))
                    sectionExpressions.Add (null);
                else if (tkPair.tk.Text.Span.Equals (ES_Keywords.Case, StringComparison.Ordinal))
                    sectionExpressions.Add (ParseExpression ());
                else
                    errorsList.Add (ES_Errors.GenExpectedXGotY ($"\"{ES_Keywords.Case}\" or \"{ES_Keywords.Default}\"", tkPair.tk));

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

            var endTk = tokenizer.PeekNextToken ().tk;
            if (EnsureToken (endTk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", endTk));
            else
                tokenizer.NextToken ();

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

            var endTk = tokenizer.PeekNextToken ().tk;
            if (EnsureToken (endTk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", endTk));
            else
                tokenizer.NextToken ();

            return new ES_AstContinueStatement (labelName, startTk, endTk);
        }

        protected ES_AstStatement ParseStatement_Goto () {
            var startTk = tokenizer.NextToken ().tk;
            if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Goto) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            ES_AstStatement statement = null;

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));

            var endTk = tokenizer.PeekNextToken ().tk;
            if (EnsureToken (endTk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", endTk));
            else
                tokenizer.NextToken ();

            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) == EnsureTokenResult.Correct) {
                if (tkPair.tk.Text.Span.Equals (ES_Keywords.Case, StringComparison.Ordinal)) {
                    tokenizer.NextToken ();

                    statement = new ES_AstGotoCaseStatement (ParseExpression (), startTk, endTk);
                } else if (tkPair.tk.Text.Span.Equals (ES_Keywords.Default, StringComparison.Ordinal)) {
                    tokenizer.NextToken ();

                    statement = new ES_AstGotoCaseStatement (null, startTk, endTk);
                } else {
                    tokenizer.NextToken ();

                    statement = new ES_AstGotoLabelStatement (tkPair.tk, startTk, endTk);
                }
            }

            return statement;
        }

        protected ES_AstReturnStatement ParseStatement_Return () {
            var startTk = tokenizer.NextToken ().tk;
            if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.Return) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            ES_AstExpression retExpr = null;

            if (EnsureTokenPeek (EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                retExpr = ParseExpression ();

            var endTk = tokenizer.PeekNextToken ().tk;
            if (EnsureToken (endTk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", endTk));
            else
                tokenizer.NextToken ();

            return new ES_AstReturnStatement (retExpr, startTk, endTk);
        }

        #endregion

        #region Loop statements

        protected ES_AstLoopStatement ParseStatement_While () {
            var startTk = tokenizer.NextToken ().tk;
            if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.While) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'('", tkPair.tk));
                return null;
            }

            var conditionExpr = ParseExpression ();

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("')'", tkPair.tk));
            else
                tokenizer.NextToken ();

            var bodyStatement = ParseEmbeddedStatement ();

            return new ES_AstLoopStatement (
                null, conditionExpr, null,

                bodyStatement,

                startTk
            );
        }

        protected ES_AstLoopStatement ParseStatement_For () {
            var startTk = tokenizer.NextToken ().tk;
            if (EnsureToken (startTk, EchelonScriptTokenType.Identifier, ES_Keywords.For) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            ES_AstStatement initStatement;
            ES_AstExpression conditionExpr;
            ES_AstExpression [] iterExpressions;

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct) {
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'('", tkPair.tk));
                return null;
            }

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

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", tkPair.tk));
            else
                tokenizer.NextToken ();

            // Condition expression
            if (EnsureTokenPeek (EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                conditionExpr = ParseExpression ();
            else
                conditionExpr = null;

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", tkPair.tk));
            else
                tokenizer.NextToken ();

            // Iteration expressions
            if (EnsureTokenPeek (EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                iterExpressions = ParseExpressionList ();
            else
                iterExpressions = null;

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", tkPair.tk));
            else
                tokenizer.NextToken ();

            // Closing parenthesis
            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("')'", tkPair.tk));
            else
                tokenizer.NextToken ();

            // Loop body statement
            var bodyStatement = ParseEmbeddedStatement ();

            return new ES_AstLoopStatement (
                initStatement, conditionExpr, iterExpressions,

                bodyStatement,

                startTk
            );
        }

        #endregion

        protected ES_AstExpressionStatement ParseStatement_Expression () {
            var expr = ParseExpression ();

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("';'", tkPair.tk));
            else
                tokenizer.NextToken ();

            return new ES_AstExpressionStatement (expr, tkPair.tk);
        }

        #endregion

        #region Expressions

        protected ES_AstExpression [] ParseExpressionList () {
            using var exprList = new StructPooledList<ES_AstExpression> (ClearMode.Auto);

            while (true) {
                var tkPair = tokenizer.PeekNextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
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
                errorsList.Add (ES_Errors.GenUnexpectedToken (tkPair.tk));
                tokenizer.NextToken ();
                return null;
            }

            ES_AstExpression left = null;

            foreach (var parser in prefixParsers) {
                if (parser.CheckCanParse (this, tkPair.tk)) {
                    left = parser.Parse (this, tkPair.tk);
                    break;
                }
            }

            if (left == null)
                return null;

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (!PostfixExprParsers.TryGetValue (tkPair.tk.Type, out var postfixParsers))
                    return left;

                bool parsed = false;
                foreach (var parser in postfixParsers) {
                    if (precedence < parser.PostfixPrecedence && parser.CheckCanParse (this, left, tkPair.tk)) {
                        left = parser.Parse (this, left, tkPair.tk);
                        parsed = true;
                        break;
                    }
                }

                if (!parsed)
                    return left;
            }
        }

        protected ES_AstFunctionCallArgument [] ParseArgumentsList (out EchelonScriptToken endToken) {
            var tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
                errorsList.Add (ES_Errors.GenExpectedXGotY ("'('", tkPair.tk));

            if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.ParenClose) {
                endToken = tokenizer.NextToken ().tk;
                return Array.Empty<ES_AstFunctionCallArgument> ();
            }

            using var argsList = new StructPooledList<ES_AstFunctionCallArgument> (ClearMode.Auto);

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    endToken = tkPair.tk;
                    break;
                }

                var mode = ES_ArgumentType.Normal;
                if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                    var textSpan = tkPair.tk.Text.Span;
                    if (textSpan.Equals (ES_Keywords.Ref, StringComparison.Ordinal))
                        mode = ES_ArgumentType.Ref;
                    else if (textSpan.Equals (ES_Keywords.In, StringComparison.Ordinal))
                        errorsList.Add (ES_Errors.GenUnexpectedIdentifier (tkPair.tk));
                    else if (textSpan.Equals (ES_Keywords.Out, StringComparison.Ordinal))
                        mode = ES_ArgumentType.Out;

                    if (mode != ES_ArgumentType.Normal) {
                        tokenizer.NextToken ();
                        tkPair = tokenizer.PeekNextToken ();

                        if (tkPair.tk.Type != EchelonScriptTokenType.Identifier)
                            errorsList.Add (ES_Errors.GenExpectedIdentifier (tkPair.tk));
                    }
                }

                var argumentExpr = ParseExpression ();

                var functionCallArgDef = new ES_AstFunctionCallArgument (mode, argumentExpr, tkPair.tk);
                argsList.Add (functionCallArgDef);

                tkPair = tokenizer.NextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.ParenClose) {
                    endToken = tkPair.tk;
                    break;
                } else if (tkPair.tk.Type != EchelonScriptTokenType.Comma)
                    errorsList.Add (ES_Errors.GenExpectedXGotY ("',' or ')'", tkPair.tk));
            }

            return argsList.ToArray ();
        }

        #endregion

        #endregion

        #region Protected methods

        protected void CheckDisposed () {
            if (IsDisposed)
                throw new ObjectDisposedException (null);
        }

        #endregion

        #endregion

        #region ================== IDisposable Support

        public bool IsDisposed { get; private set; }

        protected virtual void DoDispose () {
            if (!IsDisposed) {
                tokenizer.Dispose ();

                IsDisposed = true;
            }
        }

        public void Dispose () {
            DoDispose ();
        }

        #endregion
    }
}
