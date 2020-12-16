/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler {
    public static class ES_FrontendErrors {
        #region Tokenizer errors

        public const string InvalidHexLiteral = "Invalid hex literal.";
        public const string InvalidBinaryLiteral = "Invalid binary literal.";
        public const string InvalidFloatLiteral = "Invalid float literal.";

        public const string UnrecognizedEscape = "Unrecognized escape sequence.";

        public const string UnclosedStringLiteral = "Unclosed string literal.";
        public const string NoCRInRegularStrings = "Carriage return characters are not allowed in regular strings.";
        public const string NoLFInRegularStrings = "Newline characters are not allowed in regular strings.";

        public const string EmptyCharLiteral = "Empty character literal.";
        public const string TooLongCharLiteral = "Too many characters in character literal.";
        public const string UnclosedCharLiteral = "Unclosed string literal.";
        public const string NoCRInCharLiterals = "Carriage return characters are not allowed in character literals.";
        public const string NoLFInCharLiterals = "Newline characters are not allowed in character literals.";

        #endregion

        #region Parser errors

        public const string CodeUnit_UnexpectedToken = "Expected \"using\", \"alias\" or \"namespace\", got \"{0}\".";
        public const string ExpectedXGotY = "Expected {0}, got \"{1}\".";
        public const string GotXExpectedY = "Unexpected {0}. Expected {1}.";
        public const string UnexpectedX = "Unexpected {0} \"{1}\"";
        public const string UnrecognizedX = "Unrecognized {0} \"{1}\"";

        public const string UnexpectedEOF = "Unexpected end of file.";
        public const string UnexpectedDocComment = "Unexpected documentation comment.";

        public const string InvalidUserIdentifier = "User identifiers cannot be the same as a keyword or primitive type.";

        public const string InvalidModifier = "The modifier \"{0}\" is not valid for this item.";
        public const string InvalidModifierForContext = "The modifier \"{0}\" is not valid in this context.";

        public const string AccessBeforeStorage = "Access modifiers must come before storage modifiers.";
        public const string VirtualnessBeforeStorage = "Access modifiers must come before virtualness modifiers.";
        public const string MultipleAccessMods = "Cannot use more than one access modifier.";
        public const string MultipleVirtualnessMods = "Element already has a virtualness modifier.";
        public const string ElementAlreadyConst = "Element already declared as const.";
        public const string ElementAlreadyStatic = "Element already declared as static.";
        public const string ConstOnlyOnFunctions = "The const keyword can only be applied to functions.";

        public const string ImportAfterNamespace = "Import declarations must come before any namespaces.";
        public const string AliasAfterNamespace = "Type alias declarations must come before any namespaces.";

        public const string InvalidAccessModForNamespaceContent = "Elements defined in a namespace cannot be declared as private, protected, or protected internal.";
        public const string NamespaceMissingBrace = "Unexpected keyword \"namespace\". Missing '}'?";

        public const string InheritanceOnStaticClass = "Static classes cannot inherit from other classes or implement interfaces.";
        public const string InheritanceOnStaticStruct = "Static structs cannot implement interfaces.";

        public const string NoVarDefsInThisContext = "Variable definitions are not allowed in this context.";
        public const string NoImportsInThisContext = "Import statements are not allowed in this context.";
        public const string NoAliasesInThisContext = "Type aliases are not allowed in this context.";

        public const string TypeDeclExpected = "Syntax error; Type declaration expected.";
        public const string ValueExpected = "Syntax error; value expected.";

        public const string IntLiteralTooBig = "The specified integer literal is larger than 64 bits.";

        public const string IllegalEmbeddedStatement = "Embedded statements cannot be a declaration or labeled statement.";
        public const string IllegalExpressionStatement = "Only assignment, call, increment and decrement expressions can be used as statements.";

        #region Generation functions

        public static EchelonScriptErrorMessage GenExpectedXGotY (string expected, EchelonScriptToken token) {
            var tokenText = StringPool.Shared.GetOrAdd (token.Text.Span);
            var errorMessage = ExpectedXGotY.Replace ("{0}", expected).Replace ("{1}", tokenText);
            return new EchelonScriptErrorMessage (token, errorMessage);
        }

        public static EchelonScriptErrorMessage GenExpectedIdentifier (EchelonScriptToken token) {
            return GenExpectedXGotY ("an identifier", token);
        }

        public static EchelonScriptErrorMessage GenGotXExpectedY (EchelonScriptToken token, string expected) {
            var tokenText = StringPool.Shared.GetOrAdd (token.Text.Span);
            var errorMessage = GotXExpectedY.Replace ("{0}", tokenText).Replace ("{1}", expected);
            return new EchelonScriptErrorMessage (token, errorMessage);
        }

        public static EchelonScriptErrorMessage GenUnrecognizedIdentifier (EchelonScriptToken token) {
            var tokenText = StringPool.Shared.GetOrAdd (token.Text.Span);
            var errorMessage = UnrecognizedX.Replace ("{0}", "identifier").Replace ("{1}", tokenText);
            return new EchelonScriptErrorMessage (token, errorMessage);
        }

        public static EchelonScriptErrorMessage GenUnexpectedToken (EchelonScriptToken token) {
            var tokenText = StringPool.Shared.GetOrAdd (token.Text.Span);
            var errorMessage = UnexpectedX.Replace ("{0}", "token").Replace ("{1}", tokenText);
            return new EchelonScriptErrorMessage (token, errorMessage);
        }

        public static EchelonScriptErrorMessage GenUnexpectedIdentifier (EchelonScriptToken token) {
            var tokenText = StringPool.Shared.GetOrAdd (token.Text.Span);
            var errorMessage = UnexpectedX.Replace ("{0}", "identifier").Replace ("{1}", tokenText);
            return new EchelonScriptErrorMessage (token, errorMessage);
        }

        public static EchelonScriptErrorMessage GenExpectedAggregateContent (EchelonScriptToken token) {
            return GenExpectedXGotY ($"{ES_Keywords.Class}, {ES_Keywords.Struct} or {ES_Keywords.Enum}", token);
        }

        public static EchelonScriptErrorMessage GenInvalidModifier (string modName, EchelonScriptToken token) {
            var errorMessage = InvalidModifier.Replace ("{0}", modName);
            return new EchelonScriptErrorMessage (token, errorMessage);
        }

        public static EchelonScriptErrorMessage GenInvalidModifierForContext (string modName, EchelonScriptToken token) {
            var errorMessage = InvalidModifierForContext.Replace ("{0}", modName);
            return new EchelonScriptErrorMessage (token, errorMessage);
        }

        #endregion

        #endregion

        #region Compilation errors

        public const string TypeAlreadyDefined = "The namespace \"{0}\" already contains a definition for \"{1}\".";
        public const string NamespaceDoesntExist = "The namespace \"{0}\" does not exist.";

        public const string CantFindSymbol = "The symbol \"{0}\" could not be found.";

        public const string InvalidInheritance = "Cannot inherit from type \"{0}\".";

        public const string MultipleBaseClasses = "Classes cannot have multiple base classes.";

        public const string RepeatedInterfaceInList = "Interface \"{0}\" is already in the interfaces list.";

        #region Generation functions

        public static EchelonScriptErrorMessage GenTypeAlreadyDefined (string nmName, string typeName, EchelonScriptToken errorToken) {
            var errorMessage = TypeAlreadyDefined.Replace ("{0}", nmName).Replace ("{1}", typeName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }

        public static EchelonScriptErrorMessage GenNamespaceDoesntExist (string nmName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = NamespaceDoesntExist.Replace ("{0}", nmName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenCantFindSymbol (string symbolName, EchelonScriptToken errorToken) {
            var errorMessage = CantFindSymbol.Replace ("{0}", symbolName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }

        public static EchelonScriptErrorMessage GenCantFindSymbol (string symbolName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = CantFindSymbol.Replace ("{0}", symbolName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenInvalidInheritance (string symbolName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = InvalidInheritance.Replace ("{0}", symbolName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenRepeatedInterfaceInList (string interfaceFqn, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = RepeatedInterfaceInList.Replace ("{0}", interfaceFqn);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        #endregion

        #endregion

        #region Exceptions

        public const string ClashingTypeExists = "A clashing type exists. The caller must check for this before calling this function.";

        #endregion
    }
}
