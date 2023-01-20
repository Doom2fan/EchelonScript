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
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;

namespace EchelonScript.Compiler.Frontend.Parser;

public partial class EchelonScriptParser {
    private ES_AstNode? [] ParseAggregate (
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

    private ES_AstTypeDeclaration_TypeName []? ParseInheritanceList () {
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
            else
                listContents.Add (ParseTypeNameDeclaration ());

            if (!TryReadToken (EchelonScriptTokenType.Comma, null))
                break;
        }

        return listContents.ToArray ();
    }

    private ES_AstMemberVarDefinition [] ParseMemberVar (ES_AggregateModifiers varModifiers, ES_AstTypeDeclaration? valueType) {
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

            if (!TryReadToken (EchelonScriptTokenType.Comma, null))
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

    private ES_AstNode? [] ParseFunctionOrVariable (ES_AggregateModifiers modifiers) {
        var typeDecl = ParseTypeDeclaration ();

        var nextTkPair = tokenizer.PeekNextToken (1);
        if (nextTkPair.tk.Type == EchelonScriptTokenType.ParenOpen)
            return new ES_AstNode? [] { ParseFunction (modifiers, typeDecl, true) };
        else
            return ParseMemberVar (modifiers, typeDecl);
    }

    private ES_AstConstructorDefinition ParseConstructor (ES_AggregateModifiers consModifiers) {
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

    private ES_AstClassDefinition ParseClass (ES_AggregateModifiers classModifiers) {
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

    private ES_AstStructDefinition ParseStruct (ES_AggregateModifiers structModifiers) {
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

    private ES_AstEnumDefinition ParseEnum (ES_AggregateModifiers enumModifiers) {
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
}
