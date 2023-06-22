/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */


using EchelonScript.Compiler.Data;

namespace EchelonScript.Compiler.Frontend.Parser.Internal;

internal static partial class DiagnosticDescriptors {
    /*
     * Tokenizer
     */
    public static ES_DiagnosticDescriptor UnclosedBlockComment => new (
        id: GetExportErrorId (DiagnosticId.UnclosedBlockComment),
        title: "Unclosed block comment",
        messageFormat: "Unclosed block comment",
        category: ES_Constants.DiagnosticCategoryCompiler,
        severity: ES_DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static ES_DiagnosticDescriptor UnclosedDocComment => new (
        id: GetExportErrorId (DiagnosticId.UnclosedDocComment),
        title: "Unclosed doc comment",
        messageFormat: "Unclosed doc comment",
        category: ES_Constants.DiagnosticCategoryCompiler,
        severity: ES_DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static ES_DiagnosticDescriptor InvalidNumber => new (
        id: GetExportErrorId (DiagnosticId.InvalidNumber),
        title: "Invalid number",
        messageFormat: "Invalid number",
        category: ES_Constants.DiagnosticCategoryCompiler,
        severity: ES_DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static ES_DiagnosticDescriptor UnrecognizedEscape => new (
        id: GetExportErrorId (DiagnosticId.UnrecognizedEscape),
        title: "Unrecognized escape sequence",
        messageFormat: "Unrecognized escape sequence",
        category: ES_Constants.DiagnosticCategoryCompiler,
        severity: ES_DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static ES_DiagnosticDescriptor UnclosedStringLiteral => new (
        id: GetExportErrorId (DiagnosticId.UnclosedStringLiteral),
        title: "Unclosed string literal",
        messageFormat: "Unclosed string literal",
        category: ES_Constants.DiagnosticCategoryCompiler,
        severity: ES_DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static ES_DiagnosticDescriptor NewlineInConstant => new (
        id: GetExportErrorId (DiagnosticId.NewlineInConstant),
        title: "Newline in constant",
        messageFormat: "Newline in constant",
        category: ES_Constants.DiagnosticCategoryCompiler,
        severity: ES_DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static ES_DiagnosticDescriptor EmptyCharLiteral => new (
        id: GetExportErrorId (DiagnosticId.EmptyCharLiteral),
        title: "Empty character literal",
        messageFormat: "Empty character literal",
        category: ES_Constants.DiagnosticCategoryCompiler,
        severity: ES_DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static ES_DiagnosticDescriptor TooLongCharLiteral => new (
        id: GetExportErrorId (DiagnosticId.TooLongCharLiteral),
        title: "Too many characters in character literal",
        messageFormat: "Too many characters in character literal",
        category: ES_Constants.DiagnosticCategoryCompiler,
        severity: ES_DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static ES_DiagnosticDescriptor UnclosedCharLiteral => new (
        id: GetExportErrorId (DiagnosticId.UnclosedCharLiteral),
        title: "Unclosed character literal",
        messageFormat: "Unclosed character literal",
        category: ES_Constants.DiagnosticCategoryCompiler,
        severity: ES_DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );
}
