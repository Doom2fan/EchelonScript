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
using ChronosLib.Pooled;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;

namespace EchelonScript.Compiler.Frontend.Parser;

public partial class EchelonScriptParser {
#if false
    private EchelonScriptToken? ParseIdentifier () {
        var tkPair = tokenizer.NextToken ();

        if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            return null;
        }

        return tkPair.tk;
    }

    private bool ParseUserIdentifier (out EchelonScriptToken token) {
        var tkPair = tokenizer.NextToken ();
        token = tkPair.tk;

        if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
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

    private ES_AstDottableIdentifier ParseDottableIdentifier () {
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

    private ES_AstDottableIdentifier ParseDottableUserIdentifier () {
        var ret = ParseDottableIdentifier ();

        foreach (var part in ret.Parts) {
            if (IsKeyword (part))
                errorsList.Add (new EchelonScriptErrorMessage (part, ES_FrontendErrors.InvalidUserIdentifier));

            if (IsPrimitiveType (part))
                errorsList.Add (new EchelonScriptErrorMessage (part, ES_FrontendErrors.InvalidUserIdentifier));
        }

        return ret;
    }

    private ES_AstNamespaceIdentifier ParseNamespaceIdentifier (bool ignoreLast) {
        if (EnsureTokenPeek (EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        using var partsList = new StructPooledList<EchelonScriptToken> (CL_ClearMode.Auto);

        var id = ParseIdentifier ();
        if (id != null)
            partsList.Add (id.Value);

        var tkPair = tokenizer.PeekNextToken ();
        while (tkPair.tk.Type == EchelonScriptTokenType.NamespaceOp) {
            if (ignoreLast && tokenizer.PeekNextToken (2).tk.Type != EchelonScriptTokenType.NamespaceOp)
                break;

            tokenizer.NextToken ();

            id = ParseIdentifier ();
            if (id != null)
                partsList.Add (id.Value);

            tkPair = tokenizer.PeekNextToken ();
        }

        return new ES_AstNamespaceIdentifier (partsList.ToArray ());
    }

    private ES_AstTypeDeclaration_TypeName ParseTypeNameDeclaration () {
        if (EnsureTokenPeek (EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        if (tokenizer.PeekNextToken (1).tk.Type != EchelonScriptTokenType.NamespaceOp)
            return new ES_AstTypeDeclaration_TypeName (ParseDottableIdentifier ());

        var nsName = ParseNamespaceIdentifier (true);

        if (tokenizer.NextToken ().tk.Type != EchelonScriptTokenType.NamespaceOp)
            Debug.Fail ("This shouldn't happen.");

        var dottableId = ParseDottableIdentifier ();

        return new ES_AstTypeDeclaration_TypeName (nsName, dottableId);
    }

    private ES_AstTypeDeclaration? ParseTypeDeclaration () {
        ES_AstTypeDeclaration ParseConstDecl () {
            var startTk = tokenizer.NextToken ().tk;

            var tkPair = tokenizer.NextToken ();
            if (tkPair.tk.Type != EchelonScriptTokenType.ParenOpen)
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));

            var innerType = ParseTypeDeclaration ();

            if (innerType is null)
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("a type", tokenizer.PeekNextToken ().tk));

            var parenCloseTk = tokenizer.PeekNextToken ();
            GuaranteeToken (parenCloseTk.tk, EchelonScriptTokenType.ParenClose, "')'");

            var declType = ES_AstTypeDeclaration_Basic.DeclType.Const;
            if (startTk.Text.Span.Equals (ES_Keywords.Const, StringComparison.Ordinal))
                declType = ES_AstTypeDeclaration_Basic.DeclType.Const;
            else if (startTk.Text.Span.Equals (ES_Keywords.Immutable, StringComparison.Ordinal))
                declType = ES_AstTypeDeclaration_Basic.DeclType.Immutable;

            var bounds = new ES_AstNodeBounds (startTk.TextStartPos, innerType?.NodeBounds.EndPos ?? parenCloseTk.tk.TextEndPos);
            return new ES_AstTypeDeclaration_Basic (declType, innerType, bounds);
        }

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
                else if (isConst || isImmutable)
                    innerDecl = ParseConstDecl ();
                else
                    innerDecl = ParseTypeNameDeclaration ();
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
#endif
}
