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
using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;

namespace EchelonScript.Compiler.Frontend.Parser;

public partial class EchelonScriptParser {
    private ES_AbstractSyntaxTree ParseCodeUnit () {
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

    private ES_AstNamespace? ParseNamespace () {
        var namespaceTk = tokenizer.NextToken ().tk;
        if (!namespaceTk.CheckIdentifier (ES_Keywords.Namespace))
            throw new Exception ("The calling function must check for the correct initial token first.");

        var tkPair = tokenizer.PeekNextToken ();
        if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
            errorsList.Add (ES_FrontendErrors.GenExpectedIdentifier (tkPair.tk));
            return null;
        }

        var defaultModifiers = new ES_AggregateModifiers {
            AccessModifier = ES_AccessModifier.Private,
            Static = false,
            Const = false,
            VirtualnessModifier = ES_VirtualnessModifier.None,
        };

        var namespaceName = ParseNamespaceIdentifier (false);
        var namespaceContents = ParseNamespaceContents (defaultModifiers, out var endTk);

        return new ES_AstNamespace (namespaceName, namespaceContents, namespaceTk, endTk);
    }

    private ES_AstNode? [] ParseNamespaceContents (ES_AggregateModifiers defaultModifiers, out EchelonScriptToken endTk) {
        var tkPair = tokenizer.NextToken ();
        if (tkPair.tk.Type != EchelonScriptTokenType.BraceOpen) {
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
}
