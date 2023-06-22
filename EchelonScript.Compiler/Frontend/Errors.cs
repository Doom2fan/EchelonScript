/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScript.Compiler.Frontend;

internal static class ES_FrontendErrors {
    #region Parser errors

    public const string CodeUnit_UnexpectedToken = "Expected \"using\", \"alias\" or \"namespace\", got \"{0}\".";
    public const string ExpectedXGotY = "Expected {0}, got \"{1}\".";
    public const string UnexpectedX = "Unexpected {0} \"{1}\"";
    public const string UnrecognizedX = "Unrecognized {0} \"{1}\"";
    public const string MissingSemicolon = "Missing semicolon.";

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

#if false

    public static ES_Diagnostic GenExpectedXGotY (string expected, EchelonScriptToken token) {
        var tokenText = StringPool.Shared.GetOrAdd (token.Text.Span);
        var errorMessage = ExpectedXGotY.Replace ("{0}", expected).Replace ("{1}", tokenText);
        return new EchelonScriptDiagnostic (token, errorMessage);
    }

    public static ES_Diagnostic GenExpectedIdentifier (EchelonScriptToken token)
        => GenExpectedXGotY ("an identifier", token);

    public static ES_Diagnostic GenUnrecognizedIdentifier (EchelonScriptToken token) {
        var tokenText = StringPool.Shared.GetOrAdd (token.Text.Span);
        var errorMessage = UnrecognizedX.Replace ("{0}", "identifier").Replace ("{1}", tokenText);
        return new EchelonScriptDiagnostic (token, errorMessage);
    }

    public static ES_Diagnostic GenUnexpectedToken (EchelonScriptToken token) {
        var tokenText = StringPool.Shared.GetOrAdd (token.Text.Span);
        var errorMessage = UnexpectedX.Replace ("{0}", "token").Replace ("{1}", tokenText);
        return new EchelonScriptDiagnostic (token, errorMessage);
    }

    public static ES_Diagnostic GenUnexpectedIdentifier (EchelonScriptToken token) {
        var tokenText = StringPool.Shared.GetOrAdd (token.Text.Span);
        var errorMessage = UnexpectedX.Replace ("{0}", "identifier").Replace ("{1}", tokenText);
        return new EchelonScriptDiagnostic (token, errorMessage);
    }

    public static ES_Diagnostic GenExpectedAggregateContent (EchelonScriptToken token)
        => GenExpectedXGotY ($"{ES_Keywords.Class}, {ES_Keywords.Struct} or {ES_Keywords.Enum}", token);

    public static ES_Diagnostic GenInvalidModifier (string modName, EchelonScriptToken token) {
        var errorMessage = InvalidModifier.Replace ("{0}", modName);
        return new EchelonScriptDiagnostic (token, errorMessage);
    }

    public static ES_Diagnostic GenInvalidModifierForContext (string modName, EchelonScriptToken token) {
        var errorMessage = InvalidModifierForContext.Replace ("{0}", modName);
        return new EchelonScriptDiagnostic (token, errorMessage);
    }

#endif

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

    public const string InvalidEnumBaseType = "Unsupported base type for enum: {baseType}";

    public const string ConstantExprExpected = "A constant value is expected.";

    public const string TempValueInIncDecOp = "The operand of an increment or decrement operator cannot be a temporary value.";

    public const string InvalidExprTerm = "Invalid expression term \"{exprTerm}\".";

    public const string IntLitTooBigForSize = "The integer literal is too big to fit in {sign} {size}-bit integer.";
    public const string TypeNotNullable = "Type \"{typeName}\" is not nullable.";
    public const string NoImplicitNullVars = "Cannot assign [null] to an implicitly-typed variable.";

    public const string NoCast = "Cannot convert type \"{givenType}\" to \"{destType}\".";
    public const string NoImplicitCast = "Cannot implicitly convert type \"{givenType}\" to \"{destType}\".";
    public const string NotValueExpression = "Expression is not a value.";
    public const string NoExplicitCast = "Cannot type \"{givenType}\" to \"{destType}\".";
    public const string CantApplyBinaryOp = "\"{op}\" cannot be applied to operands of type \"{lhs}\" and \"{rhs}\".";
    public const string CantApplyUnaryOp = "\"{op}\" cannot be applied to operand of type \"{expr}\".";
    public const string CannotAssignExpr = "The expression is not assignable.";

    public const string MemberDoesntExist = "The type \"{typeName}\" has no member named \"{memberName}\".";
    public const string StaticAccessOnInst = "The member \"{memberName}\" cannot be accessed through an instance; use \"{typeName}.{memberName}\" instead.";
    public const string InstAccessOnStatic = "The member \"{memberName}\" is not static; it must be accessed through an instance.";
    public const string NoTypeNew = "The type \"{typeName}\" cannot be constructed with the \"new\" keyword.";
    public const string NoSuchConstructor = "The type \"{typeName}\" has no constructor that takes the parameter types {signature}.";
    public const string CantApplyIndexingToType = "Cannot index expression of type \"{typeName}\".";
    public const string CantApplyIndexing = "Indexed expression must return a type.";
    public const string IndexingBadRank = "Incorrect number of dimensions.";

    public const string UnexpectedReturnValue = "Unexpected return value.";
    public const string MissingReturnValue = "A return value of or convertible to type \"{retType}\" is required.";
    public const string MissingReturnStatement = "Not all code paths return a value.";

    public const string TypeAlreadyConst = "Cannot apply {newConst}, type is already {oldConst}.";

    public const string FieldCausesCycle = "Field \"{fieldName}\" causes a cycle in type {typeName}.";

    #region Generation functions

#if false

    public static ES_Diagnostic GenTypeAlreadyDefined (string nmName, string typeName, EchelonScriptToken errorToken) {
        var errorMessage = TypeAlreadyDefined.Replace ("{namespace}", nmName).Replace ("{typeName}", typeName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenNamespaceDoesntExist (string nmName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = NamespaceDoesntExist.Replace ("{namespace}", nmName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenInaccessibleProtectionLevel (string symbolName, EchelonScriptToken errorToken) {
        var errorMessage = InaccessibleProtectionLevel.Replace ("{symbolName}", symbolName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenInaccessibleProtectionLevel (string symbolName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = InaccessibleProtectionLevel.Replace ("{symbolName}", symbolName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenCantFindSymbol (string symbolName, EchelonScriptToken errorToken) {
        var errorMessage = CantFindSymbol.Replace ("{symbolName}", symbolName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenCantFindSymbol (string symbolName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = CantFindSymbol.Replace ("{symbolName}", symbolName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenVarUsedAsType (string symbolName, EchelonScriptToken errorToken) {
        var errorMessage = VarUsedAsType.Replace ("{symbolName}", symbolName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenFuncUsedAsType (string symbolName, EchelonScriptToken errorToken) {
        var errorMessage = FuncUsedAsType.Replace ("{symbolName}", symbolName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenDuplicateSymbolDef (string symbolName, EchelonScriptToken errorToken) {
        var errorMessage = DuplicateSymbolDefinition.Replace ("{symbolName}", symbolName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenDuplicateSymbolDef (string symbolName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = DuplicateSymbolDefinition.Replace ("{symbolName}", symbolName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenCantInvokeType (string typeName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = CantInvokeType.Replace ("{typeName}", typeName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenTooManyFuncArgs (string funcName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = TooManyFuncArgs.Replace ("{funcName}", funcName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenNotEnoughFuncArgs (string funcName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = NotEnoughFuncArgs.Replace ("{funcName}", funcName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenMissingFuncArg (string argName, string funcName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = MissingFuncArg.Replace ("{argName}", argName).Replace ("{funcName}", funcName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenWrongArgType (string argName, string argType, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = WrongArgType.Replace ("{arg}", "\"{argName}\"").Replace ("{argName}", argName).Replace ("{argType}", argType);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenWrongArgType (int argNum, string argType, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = WrongArgType.Replace ("{arg}", argNum.ToString ()).Replace ("{argType}", argType);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenArgNeedsType (string argName, string argType, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = ArgNeedsType.Replace ("{arg}", "\"{argName}\"").Replace ("{argName}", argName).Replace ("{argType}", argType);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenArgNeedsType (int argNum, string argType, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = ArgNeedsType.Replace ("{arg}", argNum.ToString ()).Replace ("{argType}", argType);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenArgAlreadyDefined (string argName, EchelonScriptToken errorToken) {
        var errorMessage = ArgAlreadyDefined.Replace ("{argName}", argName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenArgTypeCantUseDefExpr (string argName, string argType, EchelonScriptToken errorToken) {
        var errorMessage = ArgTypeCantUseDefExpr.Replace ("{arg}", argName).Replace ("{argType}", argType);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }

    public static ES_Diagnostic GenInvalidInheritance (string symbolName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = InvalidInheritance.Replace ("{0}", symbolName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenRepeatedInterfaceInList (string interfaceFqn, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = RepeatedInterfaceInList.Replace ("{0}", interfaceFqn);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenInvalidEnumBaseType (string baseType, EchelonScriptToken errorToken) {
        var errorMessage = InvalidEnumBaseType.Replace ("{baseType}", baseType);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }

    public static ES_Diagnostic GenInvalidExprTerm (string exprTerm, EchelonScriptToken errorToken) {
        var errorMessage = InvalidExprTerm.Replace ("{exprTerm}", exprTerm);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenInvalidExprTerm (string exprTerm, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = InvalidExprTerm.Replace ("{exprTerm}", exprTerm);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenIntLitTooBig (bool sign, ES_IntSize size, EchelonScriptToken errorToken) {
        var signStr = sign ? "a signed" : "an unsigned";

        var sizeStr = size switch {
            ES_IntSize.Int8 => "8",
            ES_IntSize.Int16 => "16",
            ES_IntSize.Int32 => "32",
            ES_IntSize.Int64 => "64",

            _ => throw new NotImplementedException ("Int size not implemented."),
        };

        var errorMessage = IntLitTooBigForSize.Replace ("{sign}", signStr).Replace ("{size}", sizeStr);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenTypeNotNullable (string typeName, EchelonScriptToken errorToken) {
        var errorMessage = TypeNotNullable.Replace ("{typeName}", typeName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenTypeNotNullable (string typeName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = TypeNotNullable.Replace ("{typeName}", typeName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenNoCast (string destType, string givenType, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = NoCast.Replace ("{givenType}", givenType).Replace ("{destType}", destType);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenNoImplicitCast (string destType, string givenType, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = NoImplicitCast.Replace ("{givenType}", givenType).Replace ("{destType}", destType);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenNoExplicitCast (string destType, string givenType, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = NoExplicitCast.Replace ("{givenType}", givenType).Replace ("{destType}", destType);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenCantApplyBinaryOp (string op, string lhs, string rhs, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = CantApplyBinaryOp.Replace ("{op}", op).Replace ("{lhs}", lhs).Replace ("{rhs}", rhs);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenCantApplyUnaryOp (string op, string expr, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = CantApplyUnaryOp.Replace ("{op}", op).Replace ("{expr}", expr);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenCantApplyIndexingToType (string typeName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = CantApplyIndexingToType.Replace ("{typeName}", typeName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenMemberDoesntExist (string typeName, string memberName, EchelonScriptToken errorToken) {
        var errorMessage = MemberDoesntExist.Replace ("{typeName}", typeName).Replace ("{memberName}", memberName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenStaticAccessOnInst (string typeName, string memberName, EchelonScriptToken errorToken) {
        var errorMessage = StaticAccessOnInst.Replace ("{typeName}", typeName).Replace ("{memberName}", memberName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenInstAccessOnStatic (string memberName, EchelonScriptToken errorToken) {
        var errorMessage = InstAccessOnStatic.Replace ("{memberName}", memberName);
        return new EchelonScriptDiagnostic (errorToken, errorMessage);
    }
    public static ES_Diagnostic GenNoTypeNew (string typeName, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = NoTypeNew.Replace ("{typeName}", typeName);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }
    public static ES_Diagnostic GenNoSuchConstructor (string typeName, string signature, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = NoSuchConstructor.Replace ("{typeName}", typeName).Replace ("{signature}", signature);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    public static ES_Diagnostic GenMissingReturnValue (string retType, SourceData src, ES_AstNodeBounds errorBounds) {
        var errorMessage = MissingReturnValue.Replace ("{retType}", retType);
        return new EchelonScriptDiagnostic (src, errorBounds, errorMessage);
    }

    private static string GenTypeAlreadyConst (bool newImmut, bool oldImmut) {
        var newConst = newImmut ? "immutable" : "const";
        var oldConst = oldImmut ? "immutable" : "const";

        return TypeAlreadyConst.Replace ("{newConst}", newConst).Replace ("{oldConst}", oldConst);
    }
    public static ES_Diagnostic GenTypeAlreadyConst (bool newImmut, bool oldImmut, EchelonScriptToken tk)
        => new (tk, GenTypeAlreadyConst (newImmut, oldImmut));
    public static ES_Diagnostic GenTypeAlreadyConst (bool newImmut, bool oldImmut, SourceData src, ES_AstNodeBounds errorBounds)
        => new (src, errorBounds, GenTypeAlreadyConst (newImmut, oldImmut));

    public static ES_Diagnostic GenFieldCausesCycle (string fieldName, string typeName, ReadOnlySpan<char> fileName) {
        var errorMessage = FieldCausesCycle.Replace ("{fieldName}", fieldName).Replace ("{typeName}", typeName);
        return new EchelonScriptDiagnostic (errorMessage, fileName);
    }

#endif

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
