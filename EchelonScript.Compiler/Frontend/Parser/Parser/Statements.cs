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
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;

namespace EchelonScript.Compiler.Frontend.Parser;

public partial class EchelonScriptParser {
#if false
    private ES_AstStatement ParseStatement () {
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

    private ES_AstStatement ParseEmbeddedStatementInternal () {
        var tkPair = tokenizer.PeekNextToken ();

        if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
            tokenizer.NextToken ();
            return new ES_AstEmptyStatement (tkPair.tk);
        }
        if (tkPair.tk.Type == EchelonScriptTokenType.BraceOpen) {
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

    private ES_AstStatement ParseActualEmbeddedStatement () {
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

    private ES_AstStatement ParseStatementsBlock (out EchelonScriptToken endTk) {
        var tkPair = tokenizer.NextToken ();
        if (tkPair.tk.Type != EchelonScriptTokenType.BraceOpen) {
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

    private ES_AstImportStatement? ParseStatement_Import (out EchelonScriptToken? errToken) {
        var startTk = tokenizer.NextToken ().tk;
        if (!startTk.CheckIdentifier (ES_Keywords.Using))
            throw new Exception ("The calling function must check for the correct initial token first.");

        var tkPair = tokenizer.PeekNextToken ();
        if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            errToken = tkPair.tk;
            return null;
        }

        var namespaceName = ParseNamespaceIdentifier (false);
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

    private ES_AstTypeAlias? ParseStatement_Alias (out EchelonScriptToken? errToken) {
        var startTk = tokenizer.NextToken ().tk;
        if (!startTk.CheckIdentifier (ES_Keywords.Alias))
            throw new Exception ("The calling function must check for the correct initial token first.");

        if (!ParseUserIdentifier (out var aliasName)) {
            errToken = tokenizer.PeekNextToken ().tk;
            return null;
        }

        var tkPair = tokenizer.PeekNextToken ();
        if (tkPair.tk.Type != EchelonScriptTokenType.Equals) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'='", tkPair.tk));
        } else
            tokenizer.NextToken ();

        tkPair = tokenizer.PeekNextToken ();
        if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
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

    private ES_AstLocalVarDefinition ParseStatement_Variable (bool usingVar) {
        var varStartTk = tokenizer.NextToken ().tk;
        if (!varStartTk.CheckIdentifier (ES_Keywords.Var))
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
            if (tkPair.tk.Type != EchelonScriptTokenType.Identifier)
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
                if (tkPair.tk.Type != EchelonScriptTokenType.Equals)
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

    private ES_AstStatement ParseStatement_IfElse () {
        var startTk = tokenizer.NextToken ().tk;
        if (!startTk.CheckIdentifier (ES_Keywords.If))
            throw new Exception ("The calling function must check for the correct initial token first.");

        var tkPair = tokenizer.PeekNextToken ();
        if (tkPair.tk.Type != EchelonScriptTokenType.ParenOpen) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));
            return new ES_AstEmptyErrorStatement (tkPair.tk);
        } else
            tokenizer.NextToken ();

        var conditionExpr = ParseExpression ();

        tkPair = tokenizer.PeekNextToken ();
        if (tkPair.tk.Type != EchelonScriptTokenType.ParenClose)
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("')'", tkPair.tk));
        else
            tokenizer.NextToken ();

        var thenStatement = ParseActualEmbeddedStatement ();
        ES_AstStatement? elseStatement = null;

        if (tokenizer.PeekNextToken ().tk.CheckIdentifier (ES_Keywords.Else)) {
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

    private ES_AstStatement ParseStatement_Switch () {
        var switchStartTk = tokenizer.NextToken ().tk;
        if (EnsureToken (switchStartTk, EchelonScriptTokenType.Identifier, ES_Keywords.Switch) != EnsureTokenResult.Correct)
            throw new Exception ("The calling function must check for the correct initial token first.");

        var tkPair = tokenizer.PeekNextToken ();
        if (tkPair.tk.Type != EchelonScriptTokenType.ParenOpen) {
            errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ("'('", tkPair.tk));
            return new ES_AstEmptyErrorStatement (tkPair.tk);
        }

        var valueExpr = ParseExpression ();

        tkPair = tokenizer.PeekNextToken ();
        GuaranteeToken (tkPair.tk, EchelonScriptTokenType.ParenClose, "')'");

        tkPair = tokenizer.PeekNextToken ();
        GuaranteeToken (tkPair.tk, EchelonScriptTokenType.ParenClose, "'{'");

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

            if (tkPair.tk.Type != EchelonScriptTokenType.Identifier)
                errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));

            if (tkPair.tk.Text.Span.Equals (ES_Keywords.Default, StringComparison.Ordinal))
                sectionExpressions.Add (null);
            else if (tkPair.tk.Text.Span.Equals (ES_Keywords.Case, StringComparison.Ordinal))
                sectionExpressions.Add (ParseExpression ());
            else
                errorsList.Add (ES_FrontendErrors.GenExpectedXGotY ($"\"{ES_Keywords.Case}\" or \"{ES_Keywords.Default}\"", tkPair.tk));

            tkPair = tokenizer.PeekNextToken ();
            if (
                tkPair.tk.CheckIdentifier (ES_Keywords.Case) ||
                tkPair.tk.CheckIdentifier (ES_Keywords.Default)
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

    private ES_AstBreakStatement ParseStatement_Break () {
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

    private ES_AstContinueStatement ParseStatement_Continue () {
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

    private ES_AstStatement ParseStatement_Goto () {
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

    private ES_AstReturnStatement ParseStatement_Return () {
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

    private ES_AstStatement ParseStatement_While () {
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

        return new ES_AstWhileStatement (
            conditionExpr, bodyStatement, false,
            startTk, endTk
        );
    }

    private ES_AstStatement ParseStatement_DoWhile () {
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

        return new ES_AstWhileStatement (
            conditionExpr,
            bodyStatement, true,
            startTk, endTk
        );
    }

    private ES_AstStatement ParseStatement_For () {
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

        return new ES_AstForStatement (
            initStatement, conditionExpr, iterExpressions,
            bodyStatement,
            startTk, bodyStatement == null ? endTk : null
        );
    }

    #endregion

    private ES_AstExpressionStatement ParseStatement_Expression () {
        var expr = ParseExpression ();

        if (expr is ES_AstEmptyErrorExpression)
            tokenizer.NextToken ();

        ParseSemicolon (out var endTk);

        return new ES_AstExpressionStatement (expr, endTk);
    }
#endif
}
