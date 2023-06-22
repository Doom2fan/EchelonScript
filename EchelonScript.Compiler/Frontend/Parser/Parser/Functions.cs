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
#if false
    private ES_AstFunctionDefinition? ParseFunction (ES_AggregateModifiers funcModifiers, ES_AstTypeDeclaration? returnType, bool virtualsValid) {
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

    private ES_AstFunctionArgumentDefinition [] ParseArgumentsDefinitionList () {
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
            var mode = ES_ArgumentKind.Normal;

            if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                var textSpan = tkPair.tk.Text.Span;
                if (textSpan.Equals (ES_Keywords.Ref, StringComparison.Ordinal))
                    mode = ES_ArgumentKind.Ref;
                else if (textSpan.Equals (ES_Keywords.In, StringComparison.Ordinal))
                    mode = ES_ArgumentKind.In;
                else if (textSpan.Equals (ES_Keywords.Out, StringComparison.Ordinal))
                    mode = ES_ArgumentKind.Out;

                if (mode != ES_ArgumentKind.Normal) {
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
#endif
}
