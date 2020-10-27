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

using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Parser {
    public static class ES_Errors {
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

        public const string AccessBeforeStorage = "Access modifiers must come before storage modifiers.";
        public const string MultipleAccessMods = "Cannot use more than one access modifier.";
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

        public const string IntLiteralTooBig = "The specified integer literal is larger than 64 bits.";

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

        #endregion

        #endregion
    }
}
