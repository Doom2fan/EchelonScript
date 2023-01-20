/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using ChronosLib.Pooled;
using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;

namespace EchelonScript.Compiler.Frontend.Parser;

public partial class EchelonScriptParser {
    private ES_AstExpression [] ParseExpressionList () {
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

    private ES_AstExpression ParseExpression (ExpressionPrecedence precedence = ExpressionPrecedence.NO_PRECEDENCE) {
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

    private ES_AstFunctionCallArgument [] ParseArgumentsList (out EchelonScriptToken endToken) {
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

            var mode = ES_ArgumentKind.Normal;
            if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                var textSpan = tkPair.tk.Text.Span;
                if (tkPair.tk.CheckIdentifier (ES_Keywords.Ref))
                    mode = ES_ArgumentKind.Ref;
                else if (tkPair.tk.CheckIdentifier (ES_Keywords.In))
                    errorsList.Add (ES_FrontendErrors.GenUnexpectedIdentifier (tkPair.tk));
                else if (tkPair.tk.CheckIdentifier (ES_Keywords.Out))
                    mode = ES_ArgumentKind.Out;

                if (mode != ES_ArgumentKind.Normal)
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
}
