/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;

namespace EchelonScriptCompiler.Frontend {
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

        public const string NoArgsOnStaticConstructors = "Static constructors cannot have arguments.";
        public const string NoAccessModsOnStaticConstructors = "Static constructors cannot have access modifiers.";

        public const string ImportAfterNamespace = "Import declarations must come before any namespaces.";
        public const string AliasAfterNamespace = "Type alias declarations must come before any namespaces.";

        public const string InvalidAccessModForNamespaceContent = "Elements defined in a namespace cannot be declared as private, protected, or protected internal.";
        public const string NamespaceMissingBrace = "Unexpected keyword \"namespace\". Missing '}'?";

        public const string StaticOnStruct = "Structs cannot be static.";
        public const string InheritanceOnStaticClass = "Static classes cannot inherit from other classes or implement interfaces.";

        public const string NoVarDefsInThisContext = "Variable definitions are not allowed in this context.";
        public const string NoImportsInThisContext = "Import statements are not allowed in this context.";
        public const string NoAliasesInThisContext = "Type aliases are not allowed in this context.";

        public const string TypeDeclExpected = "Syntax error; Type declaration expected.";
        public const string ValueExpected = "Syntax error; value expected.";

        public const string EmptyArgument = "Argument missing; Arguments cannot be empty.";

        public const string InvalidIntLiteralSize = "The specified integer literal size is not valid.";
        public const string IntLiteralTooBig = "The specified integer literal is larger than 64 bits.";

        public const string LabelOnEmptyStatement = "Labels cannot be put on empty statements.";

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

        public const string TypeAlreadyDefined = "The namespace \"{namespace}\" already contains a definition for \"{typeName}\".";
        public const string NamespaceDoesntExist = "The namespace \"{namespace}\" does not exist.";
        public const string InaccessibleProtectionLevel = "\"{symbolName}\" is inaccessible due to its protection level.";

        public const string CantFindSymbol = "The symbol \"{symbolName}\" could not be found.";
        public const string VarUsedAsType = "Variable \"{symbolName}\" cannot be used as a type.";
        public const string FuncUsedAsType = "Function \"{symbolName}\" cannot be used as a type.";
        public const string DuplicateSymbolDefinition = "Attempted to redefine the symbol \"{symbolName}\".";

        public const string CantInvokeExpr = "Expression cannot be invoked like a function.";
        public const string CantInvokeType = "Type \"{typeName}\" cannot be invoked like a function.";

        public const string TooManyFuncArgs = "Too many arguments in call to function \"{funcName}\".";
        public const string NotEnoughFuncArgs = "Missing required arguments in call to function \"{funcName}\".";
        public const string MissingFuncArg = "There is no argument given that corresponds to required argument \"{argName}\" in call to function \"{funcName}\".";

        public const string WrongArgType = "Argument \"{arg}\" may not be passed with the \"{argType}\" keyword.";
        public const string ArgNeedsType = "Argument \"{arg}\" must be passed with the \"{argType}\" keyword.";

        public const string ArgAlreadyDefined = "Tried to redefine argument \"{argName}\".";
        public const string ReqArgAfterOptional = "Required arguments must come before all optional arguments.";
        public const string ArgTypeCantUseDefExpr = "Argument \"{arg}\" with keyword \"{argType}\" cannot have a default value.";

        public const string InvalidInheritance = "Cannot inherit from type \"{0}\".";
        public const string MultipleBaseClasses = "Classes cannot have multiple base classes.";
        public const string RepeatedInterfaceInList = "Interface \"{0}\" is already in the interfaces list.";
        public const string InstDefValOutsideClass = "Only classes may contain default initializers for instance members.";

        public const string ConstantExprExpected = "A constant value is expected.";

        public const string TempValueInIncDecOp = "The operand of an increment or decrement operator cannot be a temporary value.";

        public const string InvalidExprTerm = "Invalid expression term \"{exprTerm}\".";

        public const string IntLitTooBigForSize = "The integer literal is too big to fit in {sign} {size}-bit integer.";

        public const string NoCast = "Cannot convert type \"{givenType}\" to \"{destType}\".";
        public const string NoImplicitCast = "Cannot implicitly convert type \"{givenType}\" to \"{destType}\".";
        public const string NoExplicitCast = "Cannot type \"{givenType}\" to \"{destType}\".";
        public const string CantApplyBinaryOp = "\"{op}\" cannot be applied to operands of type \"{lhs}\" and \"{rhs}\".";
        public const string CantApplyUnaryOp = "\"{op}\" cannot be applied to operand of type \"{expr}\".";
        public const string CannotAssignExpr = "The expression is not assignable.";

        public const string MemberDoesntExist = "The type \"{typeName}\" has no member named \"{memberName}\".";
        public const string StaticAccessOnInst = "The member \"{memberName}\" cannot be accessed through an instance; use \"{typeName}.{memberName}\" instead.";
        public const string InstAccessOnStatic = "The member \"{memberName}\" is not static; it must be accessed through an instance.";
        public const string NoSuchConstructor = "The type \"{typeName}\" has no constructor that takes the parameter types {signature}.";

        public const string UnexpectedReturnValue = "Unexpected return value.";
        public const string MissingReturnValue = "A return value of or convertible to type \"{retType}\" is required.";
        public const string MissingReturnStatement = "Not all code paths return a value.";

        #region Generation functions

        public static EchelonScriptErrorMessage GenTypeAlreadyDefined (string nmName, string typeName, EchelonScriptToken errorToken) {
            var errorMessage = TypeAlreadyDefined.Replace ("{namespace}", nmName).Replace ("{typeName}", typeName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenNamespaceDoesntExist (string nmName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = NamespaceDoesntExist.Replace ("{namespace}", nmName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenInaccessibleProtectionLevel (string symbolName, EchelonScriptToken errorToken) {
            var errorMessage = InaccessibleProtectionLevel.Replace ("{symbolName}", symbolName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenInaccessibleProtectionLevel (string symbolName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = InaccessibleProtectionLevel.Replace ("{symbolName}", symbolName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenCantFindSymbol (string symbolName, EchelonScriptToken errorToken) {
            var errorMessage = CantFindSymbol.Replace ("{symbolName}", symbolName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenCantFindSymbol (string symbolName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = CantFindSymbol.Replace ("{symbolName}", symbolName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenVarUsedAsType (string symbolName, EchelonScriptToken errorToken) {
            var errorMessage = VarUsedAsType.Replace ("{symbolName}", symbolName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenFuncUsedAsType (string symbolName, EchelonScriptToken errorToken) {
            var errorMessage = FuncUsedAsType.Replace ("{symbolName}", symbolName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenDuplicateSymbolDef (string symbolName, EchelonScriptToken errorToken) {
            var errorMessage = DuplicateSymbolDefinition.Replace ("{symbolName}", symbolName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenDuplicateSymbolDef (string symbolName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = DuplicateSymbolDefinition.Replace ("{symbolName}", symbolName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenCantInvokeType (string typeName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = CantInvokeType.Replace ("{typeName}", typeName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenTooManyFuncArgs (string funcName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = TooManyFuncArgs.Replace ("{funcName}", funcName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenNotEnoughFuncArgs (string funcName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = NotEnoughFuncArgs.Replace ("{funcName}", funcName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenMissingFuncArg (string argName, string funcName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = MissingFuncArg.Replace ("{argName}", argName).Replace ("{funcName}", funcName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenWrongArgType (string argName, string argType, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = WrongArgType.Replace ("{arg}", "\"{argName}\"").Replace ("{argName}", argName).Replace ("{argType}", argType);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenWrongArgType (int argNum, string argType, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = WrongArgType.Replace ("{arg}", argNum.ToString ()).Replace ("{argType}", argType);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenArgNeedsType (string argName, string argType, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = ArgNeedsType.Replace ("{arg}", "\"{argName}\"").Replace ("{argName}", argName).Replace ("{argType}", argType);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenArgNeedsType (int argNum, string argType, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = ArgNeedsType.Replace ("{arg}", argNum.ToString ()).Replace ("{argType}", argType);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenArgAlreadyDefined (string argName, EchelonScriptToken errorToken) {
            var errorMessage = ArgAlreadyDefined.Replace ("{argName}", argName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenArgTypeCantUseDefExpr (string argName, string argType, EchelonScriptToken errorToken) {
            var errorMessage = ArgAlreadyDefined.Replace ("{arg}", argName).Replace ("{argType}", argType);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }

        public static EchelonScriptErrorMessage GenInvalidInheritance (string symbolName, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = InvalidInheritance.Replace ("{0}", symbolName);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenRepeatedInterfaceInList (string interfaceFqn, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = RepeatedInterfaceInList.Replace ("{0}", interfaceFqn);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenInvalidExprTerm (string exprTerm, EchelonScriptToken errorToken) {
            var errorMessage = InvalidExprTerm.Replace ("{exprTerm}", exprTerm);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenInvalidExprTerm (string exprTerm, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = InvalidExprTerm.Replace ("{exprTerm}", exprTerm);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenIntLitTooBig (bool sign, ES_IntSize size, EchelonScriptToken errorToken) {
            string signStr = sign ? "a signed" : "an unsigned";
            string sizeStr;

            switch (size) {
                case ES_IntSize.Int8: sizeStr = "8"; break;
                case ES_IntSize.Int16: sizeStr = "16"; break;
                case ES_IntSize.Int32: sizeStr = "32"; break;
                case ES_IntSize.Int64: sizeStr = "64"; break;

                default:
                    throw new NotImplementedException ("Int size not implemented.");
            }

            var errorMessage = IntLitTooBigForSize.Replace ("{sign}", signStr).Replace ("{size}", sizeStr);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }

        public static EchelonScriptErrorMessage GenNoCast (string destType, string givenType, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = NoCast.Replace ("{givenType}", givenType).Replace ("{destType}", destType);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenNoImplicitCast (string destType, string givenType, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = NoImplicitCast.Replace ("{givenType}", givenType).Replace ("{destType}", destType);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenNoExplicitCast (string destType, string givenType, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = NoExplicitCast.Replace ("{givenType}", givenType).Replace ("{destType}", destType);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenCantApplyBinaryOp (string op, string lhs, string rhs, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = CantApplyBinaryOp.Replace ("{op}", op).Replace ("{lhs}", lhs).Replace ("{rhs}", rhs);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }
        public static EchelonScriptErrorMessage GenCantApplyUnaryOp (string op, string expr, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = CantApplyUnaryOp.Replace ("{op}", op).Replace ("{expr}", expr);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenMemberDoesntExist (string typeName, string memberName, EchelonScriptToken errorToken) {
            var errorMessage = MemberDoesntExist.Replace ("{typeName}", typeName).Replace ("{memberName}", memberName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenStaticAccessOnInst (string typeName, string memberName, EchelonScriptToken errorToken) {
            var errorMessage = StaticAccessOnInst.Replace ("{typeName}", typeName).Replace ("{memberName}", memberName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenInstAccessOnStatic (string memberName, EchelonScriptToken errorToken) {
            var errorMessage = InstAccessOnStatic.Replace ("{memberName}", memberName);
            return new EchelonScriptErrorMessage (errorToken, errorMessage);
        }
        public static EchelonScriptErrorMessage GenNoSuchConstructor (string typeName, string signature, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = NoSuchConstructor.Replace ("{typeName}", typeName).Replace ("{signature}", signature);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        public static EchelonScriptErrorMessage GenMissingReturnValue (string retType, ReadOnlySpan<char> src, ES_AstNodeBounds errorBounds) {
            var errorMessage = MissingReturnValue.Replace ("{retType}", retType);
            return new EchelonScriptErrorMessage (src, errorBounds, errorMessage);
        }

        #endregion

        #endregion

        #region Exceptions

        public const string ClashingTypeExists = "A clashing type exists. The caller must check for this before calling this function.";

        public const string ConstFoldFailure = "The constant folding pass left a literal node untransformed.";

        #endregion
    }

    public static class ES_FrontendWarnings {
        #region Compilation warnings

        public const string UnreachableCode = "Unreachable code detected.";

        #endregion
    }

    public static class ES_FrontendInfoMsg {
        #region Compilation info

        public const string RedundantCast = "Cast is redundant.";

        #endregion
    }
}
