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
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;

namespace EchelonScript.Compiler.Frontend.Parser;

public partial class EchelonScriptParser {
#if false
    private enum EnsureTokenResult {
        Correct,
        WrongType,
        WrongText,
    }

    private bool GuaranteeToken (EchelonScriptToken tk, EchelonScriptTokenType type, string expectedToken) {
        if (tk.Type != type) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY (expectedToken, tk));
            return false;
        }

        tokenizer.NextToken ();
        return true;
    }

    private static EnsureTokenResult EnsureToken (EchelonScriptToken tk, EchelonScriptTokenType type, string? text) {
        if (tk.Type != type)
            return EnsureTokenResult.WrongType;

        if (text != null && !tk.Text.Span.Equals (text, StringComparison.Ordinal))
            return EnsureTokenResult.WrongText;

        return EnsureTokenResult.Correct;
    }

    private EnsureTokenResult EnsureTokenPeek (EchelonScriptTokenType type, string? text) {
        var tkPair = tokenizer.PeekNextToken ();
        return EnsureToken (tkPair.tk, type, text);
    }

    private EnsureTokenResult EnsureTokenPeek (int offset, EchelonScriptTokenType type, string? text) {
        var tkPair = tokenizer.PeekNextToken (offset);
        return EnsureToken (tkPair.tk, type, text);
    }

    private bool TryReadToken (EchelonScriptTokenType type, string? text) => TryReadToken (type, text, out _);

    private bool TryReadToken (EchelonScriptTokenType type, string? text, out EchelonScriptToken tk) {
        tk = tokenizer.PeekNextToken ().tk;
        if (EnsureToken (tk, type, text) != EnsureTokenResult.Correct)
            return false;

        tokenizer.NextToken ();
        return true;
    }

    private bool ParseSemicolon (out EchelonScriptToken tk) {
        tk = tokenizer.PeekNextToken ().tk;

        if (EnsureToken (tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct) {
            errorsList.Add (new (tk, ES_FrontendErrors.MissingSemicolon));
            return false;
        }

        tokenizer.NextToken ();
        return true;
    }

    private bool CheckNoAccessModifierErrors (EchelonScriptToken tk, ES_AggregateModifiers currentModifiers) {
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

    private void ParseAggregate_SetDocComment (ref ES_AggregateModifiers curMods, EchelonScriptToken? newDocCom) {
        if (newDocCom == null)
            return;

        if (curMods.AnySet ()) {
            errorsList.Add (new EchelonScriptErrorMessage (newDocCom.Value, ES_FrontendErrors.UnexpectedDocComment));
            return;
        }

        curMods.DocComment = newDocCom;
    }

    private static bool IsIntegerLiteral (EchelonScriptToken tk) {
        return (
            tk.Type == EchelonScriptTokenType.DecIntegerLiteral ||
            tk.Type == EchelonScriptTokenType.BinIntegerLiteral ||
            tk.Type == EchelonScriptTokenType.HexIntegerLiteral
        );
    }

    private static bool IsFloatLiteral (EchelonScriptToken tk) {
        return (
            tk.Type == EchelonScriptTokenType.FloatLiteral ||
            tk.Type == EchelonScriptTokenType.DecIntegerLiteral
        );
    }

    private static bool IsKeyword (EchelonScriptToken tk) {
        var idSpan = tk.Text.Span;

        foreach (var keyword in Keywords) {
            if (idSpan.Equals (keyword, StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    private static bool IsPrimitiveType (EchelonScriptToken tk) {
        var idSpan = tk.Text.Span;

        foreach (var type in PrimitiveTypes) {
            if (idSpan.Equals (type, StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    private ES_AstExpression? [] ParseArrayDimensions (out EchelonScriptToken startTk, out int endPos, bool ignoreOpenTk) {
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
#endif
}
