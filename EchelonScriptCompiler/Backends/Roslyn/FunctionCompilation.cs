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
using System.Globalization;
using ChronosLib.Pooled;
using EchelonScriptCommon;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler.CompilerCommon.IR;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EchelonScriptCompiler.Backends.RoslynBackend;

public unsafe sealed partial class RoslynCompilerBackend {
    private struct StatementData : IDisposable {
        public bool AlwaysReturns;

        public PooledArray<StatementSyntax> Statements;
        public PooledArray<int> Registers;

        public StatementData (StatementSyntax statement, bool alwaysReturns = false) {
            AlwaysReturns = alwaysReturns;

            Statements = PooledArray<StatementSyntax>.GetArray (1);
            Statements.Span [0] = statement;
            Registers = PooledArray<int>.Empty ();
        }

        public StatementData (ReadOnlySpan<StatementSyntax> statements, bool alwaysReturns = false) {
            AlwaysReturns = alwaysReturns;

            Statements = PooledArray<StatementSyntax>.GetArray (statements.Length);
            statements.CopyTo (Statements.Span);
            Registers = PooledArray<int>.Empty ();
        }

        public StatementData (StatementSyntax statement, ReadOnlySpan<int> registers, bool alwaysReturns = false) {
            AlwaysReturns = alwaysReturns;

            Statements = PooledArray<StatementSyntax>.GetArray (1);
            Statements.Span [0] = statement;

            Registers = PooledArray<int>.GetArray (registers.Length);
            registers.CopyTo (Registers.Span);
        }

        public StatementData (ReadOnlySpan<StatementSyntax> statements, ReadOnlySpan<int> registers, bool alwaysReturns = false) {
            AlwaysReturns = alwaysReturns;

            Statements = PooledArray<StatementSyntax>.GetArray (statements.Length);
            statements.CopyTo (Statements.Span);

            Registers = PooledArray<int>.GetArray (registers.Length);
            registers.CopyTo (Registers.Span);
        }

        public StatementData (bool alwaysReturns) {
            AlwaysReturns = alwaysReturns;
            Statements = PooledArray<StatementSyntax>.Empty ();
            Registers = PooledArray<int>.Empty ();
        }

        public void Dispose () {
            Statements.Dispose ();
            Registers.Dispose ();
        }
    }

    internal struct LabelSymbol {
        public ES_Identifier? Name { get; init; }
        public string JumpTarget { get; init; }
        public string? BreakTarget { get; init; }
        public string? ContinueTarget { get; init; }

        public LabelSymbol (ES_Identifier? name, string jumpTarget, string? breakTarget, string? continueTarget) {
            Name = name;
            JumpTarget = jumpTarget;
            BreakTarget = breakTarget;
            ContinueTarget = continueTarget;
        }

        public LabelSymbol (ES_Identifier? name, string jumpTarget) {
            Name = name;
            JumpTarget = jumpTarget;
            BreakTarget = null;
            ContinueTarget = null;
        }
    }

    private ref struct FunctionData {
        public ES_TypeInfo* ReturnType { get; init; }

        public ReadOnlySpan<ESIR_ArgumentDefinition> Args;
        public ReadOnlySpan<ESIR_TypeNode> Locals;

        public int LabelIndex;
        public string? CurBreakTarget;
        public string? CurContinueTarget;
    }

    private static ExpressionSyntax GetDefaultValue (ES_TypeInfo* varType) {
        switch (varType->TypeTag) {
            case ES_TypeTag.Bool:
                return BoolLiteral (false);

            case ES_TypeTag.Int:
            case ES_TypeTag.Float:
                return NumericLiteral (0);

            case ES_TypeTag.Reference:
            case ES_TypeTag.Array:
                return LiteralExpression (SyntaxKind.NullLiteralExpression);

            case ES_TypeTag.Struct: {
                return InvocationExpression (MemberAccessExpression (
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName (MangleTypeName (varType)),
                    IdentifierName (DefaultValueFuncName)
                ));
            }

            case ES_TypeTag.Const:
            case ES_TypeTag.Immutable: {
                var constData = (ES_ConstData*) varType;
                return GetDefaultValue (constData->InnerType);
            }

            default:
                throw new NotImplementedException ("Variable type not implemented.");
        }
    }

    private static string GetIndexedName (ReadOnlySpan<char> prefix, int idx) {
        const int intLen = 15;
        var formatProv = NumberFormatInfo.InvariantInfo;

        using var chars = PooledArray<char>.GetArray (prefix.Length + intLen);

        prefix.CopyTo (chars);
        if (!idx.TryFormat (chars.Span [prefix.Length..], out var intCharsWritten, provider: formatProv))
            Debug.Fail ("This shouldn't happen.");

        return chars.Span [..^(intLen - intCharsWritten)].GetPooledString ();
    }

    private static string GetPrefixedName (ReadOnlySpan<char> prefix, ReadOnlySpan<char> name) {
        using var chars = PooledArray<char>.GetArray (prefix.Length + name.Length);

        prefix.CopyTo (chars);
        name.CopyTo (chars.Span [prefix.Length..]);

        return chars.GetPooledString ();
    }

    private static string GetLocalVarName (int idx) => GetIndexedName ("esLocal_", idx);

    private static string GetArgName (int idx) => GetIndexedName ("esArg_", idx);

    private static string GetIndexedLabel (int idx) => GetIndexedName ("esLabel_", idx);

    private static string GetLabelNameLoopBreak (ReadOnlySpan<char> name) => GetPrefixedName (name, "_LoopBreak");

    private static string GetLabelNameLoopContinue (ReadOnlySpan<char> name) => GetPrefixedName (name, "_LoopContinue");

    private static void CompileFunction (ref PassData passData, ESIR_Function func) {
        var roslynFuncName = func.Name.GetCharsSpan ().GetPooledString ();
        using var argsList = new StructPooledList<ParameterSyntax> (CL_ClearMode.Auto);

        passData.LabelStack.Push ();

        var argCount = 0;
        foreach (var arg in func.Arguments.Elements) {
            var argNum = argCount++;
            var argName = GetArgName (argNum);
            var argValType = GetRoslynType (arg.ValueType.Pointer);

            var parameter = Parameter (Identifier (argName)).WithType (argValType);
            switch (arg.ArgType) {
                case ES_ArgumentType.Normal: break;

                case ES_ArgumentType.Ref:
                    parameter = parameter.WithModifiers (TokenList (Token (SyntaxKind.RefKeyword)));
                    break;

                case ES_ArgumentType.Out:
                    throw new NotImplementedException ("[TODO] Argument type not implemented yet.");

                default:
                    throw new NotImplementedException ("Argument type not implemented.");
            }

            argsList.Add (parameter);
        }

        var funcData = new FunctionData {
            ReturnType = func.ReturnType.Pointer,

            Args = func.Arguments.Elements,
            Locals = func.LocalValues.Elements,
        };

        using var attrsList = new StructPooledList<AttributeSyntax> (CL_ClearMode.Auto);

        var hasTraceData = false;
        var hasFuncData = false;
        foreach (var attr in func.Attributes.Elements) {
            if (attr is ESIR_TraceDataAttribute traceData) {
                Debug.Assert (!hasTraceData);
                hasTraceData = true;

                var parentTypeExpr = LiteralExpression (SyntaxKind.NullLiteralExpression);
                var fileNameExpr = LiteralExpression (SyntaxKind.NullLiteralExpression);

                if (traceData.ParentType.HasValue)
                    parentTypeExpr = StringLiteral (traceData.ParentType.Value.GetCharsSpan ().GetPooledString ());
                if (traceData.FileName is not null)
                    fileNameExpr = StringLiteral (traceData.FileName);

                attrsList.Add (Attribute (
                    IdentifierName (nameof (ES_MethodTraceDataAttribute)),
                    SimpleAttributeArgumentList (
                        AttributeArgument (StringLiteral (traceData.Namespace.GetCharsSpan ().GetPooledString ())),
                        AttributeArgument (StringLiteral (traceData.Name.GetCharsSpan ().GetPooledString ())),

                        AttributeArgument (
                            NameEquals (IdentifierName (nameof (ES_MethodTraceDataAttribute.ParentType))), null,
                            parentTypeExpr
                        ),
                        AttributeArgument (
                            NameEquals (IdentifierName (nameof (ES_MethodTraceDataAttribute.FileName))), null,
                            fileNameExpr
                        )
                    )
                ));
            } else if (attr is ESIR_FunctionDataAttribute funcDataAttr) {
                Debug.Assert (!hasFuncData);
                hasFuncData = true;

                passData.Mappings.Add (BackendMapping.Function (funcDataAttr.FunctionData, roslynFuncName));
            }
        }

        var stmtsList = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);
        try {
            var localsCount = 0;
            foreach (var local in func.LocalValues.Elements) {
                var localIdx = localsCount++;

                var varDeclarator = VariableDeclarator (GetLocalVarName (localIdx));
                var declStmt = LocalDeclarationStatement (
                    VariableDeclaration (GetRoslynType (local.Pointer))
                        .WithVariables (SingletonSeparatedList (varDeclarator))
                );
                stmtsList.Add (declStmt);
            }

            CompileStatements (ref passData, ref funcData, ref stmtsList, func.Statements.Elements);

            passData.GlobalMembers.Add (
                MethodDeclaration (
                    GetRoslynType (func.ReturnType.Pointer),
                    roslynFuncName
                ).AddAttributeLists (AttributeList (
                    SimpleSeparatedList (attrsList.Span, Token (SyntaxKind.CommaToken))
                )).WithParameterList (ParameterList (
                    SimpleSeparatedList (
                        argsList.Span,
                        Token (SyntaxKind.CommaToken)
                    )
                )).WithModifiers (TokenList (
                    Token (SyntaxKind.PublicKeyword),
                    Token (SyntaxKind.StaticKeyword)
                )).WithBody (BlockSpan (stmtsList.Span))
            );
        } finally {
            stmtsList.Dispose ();
        }

        passData.LabelStack.Pop ();
    }

    private static PooledArray<StatementSyntax> CompileStatements (
        ref PassData passData,
        ref FunctionData funcData,
        ReadOnlySpan<ESIR_Statement> statements
    ) {
        var stmtsList = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);
        try {
            CompileStatements (ref passData, ref funcData, ref stmtsList, statements);
            return stmtsList.MoveToArray ();
        } finally {
            stmtsList.Dispose ();
        }
    }

    private static void CompileStatements (
        ref PassData passData,
        ref FunctionData funcData,
        ref StructPooledList<StatementSyntax> stmtsList,
        ReadOnlySpan<ESIR_Statement> statements
    ) {
        foreach (var stmt in statements)
            CompileStatement (ref passData, ref funcData, ref stmtsList, stmt);
    }

    private static StatementSyntax CompileStatementOptionalBlock (
        ref PassData passData,
        ref FunctionData funcData,
        ESIR_Statement stmt
    ) {
        var stmtsList = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);

        try {
            CompileStatement (ref passData, ref funcData, ref stmtsList, stmt);

            if (stmtsList.Count < 1)
                return EmptyStatement ();
            else if (stmtsList.Count == 1)
                return stmtsList [0];
            else
                return BlockSpan (stmtsList.Span);
        } finally {
            stmtsList.Dispose ();
        }
    }

    private static PooledArray<StatementSyntax> CompileStatementUnwrapBlock (
        ref PassData passData,
        ref FunctionData funcData,
        ESIR_Statement stmt
    ) {
        var stmtsList = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);

        try {
            if (stmt is ESIR_BlockStatement blockStmt) {
                foreach (var innerStmt in blockStmt.Statements.Elements)
                    CompileStatement (ref passData, ref funcData, ref stmtsList, innerStmt);
            } else
                CompileStatement (ref passData, ref funcData, ref stmtsList, stmt);

            return stmtsList.MoveToArray ();
        } finally {
            stmtsList.Dispose ();
        }
    }

    private static void CompileStatement (
        ref PassData passData,
        ref FunctionData funcData,
        ref StructPooledList<StatementSyntax> stmtsList,
        ESIR_Statement stmt
    ) {
        switch (stmt.Kind) {
            case ESIR_NodeKind.BlockStatement when stmt is ESIR_BlockStatement blockStmt:
                CompileStatements (ref passData, ref funcData, ref stmtsList, blockStmt.Statements.Elements);
                break;

            case ESIR_NodeKind.LabeledStatement when stmt is ESIR_LabeledStatement labelStmt:
                CompileStatement_Label (ref passData, ref funcData, ref stmtsList, labelStmt);
                break;

            case ESIR_NodeKind.ConditionalStatement when stmt is ESIR_ConditionalStatement condStmt: {
                var condExpr = CompileExpression (ref passData, ref funcData, condStmt.Condition);
                var thenStmt = CompileStatementOptionalBlock (ref passData, ref funcData, condStmt.Then);

                if (condStmt.Else is not null) {
                    var elseStmt = CompileStatementOptionalBlock (ref passData, ref funcData, condStmt.Else);
                    stmtsList.Add (IfStatement (condExpr.Value!, thenStmt, ElseClause (elseStmt)));
                } else
                    stmtsList.Add (IfStatement (condExpr.Value!, thenStmt));

                break;
            }

            // TODO: case ESIR_NodeKind.SwitchStatement:

            case ESIR_NodeKind.BreakStatement when stmt is ESIR_BreakStatement breakStmt: {
                string targetLabel;

                if (breakStmt.Label is not null) {
                    var label = passData.LabelStack.GetSymbol (breakStmt.Label.Value);

                    if (!label.Name.HasValue)
                        throw new CompilationException ("Break label doesn't exist.");
                    else if (label.BreakTarget is null)
                        throw new CompilationException ("Break label has no continue target.");

                    targetLabel = label.BreakTarget;
                } else {
                    if (funcData.CurBreakTarget is null)
                        throw new CompilationException ("No continue target");

                    targetLabel = funcData.CurBreakTarget;
                }

                stmtsList.Add (GotoStatement (SyntaxKind.GotoStatement, IdentifierName (targetLabel)));

                break;
            }
            case ESIR_NodeKind.ContinueStatement when stmt is ESIR_ContinueStatement continueStmt: {
                string targetLabel;

                if (continueStmt.Label is not null) {
                    var label = passData.LabelStack.GetSymbol (continueStmt.Label.Value);

                    if (!label.Name.HasValue)
                        throw new CompilationException ("Continue label doesn't exist.");
                    else if (label.ContinueTarget is null)
                        throw new CompilationException ("Continue label has no continue target.");

                    targetLabel = label.ContinueTarget;
                } else {
                    if (funcData.CurContinueTarget is null)
                        throw new CompilationException ("No continue target");

                    targetLabel = funcData.CurContinueTarget;
                }

                stmtsList.Add (GotoStatement (SyntaxKind.GotoStatement, IdentifierName (targetLabel)));

                break;
            }

            case ESIR_NodeKind.GotoLabelStatement when stmt is ESIR_GotoLabelStatement gotoLabelStmt: {
                var label = passData.LabelStack.GetSymbol (gotoLabelStmt.Label);

                if (!label.Name.HasValue)
                    throw new CompilationException ("Goto label doesn't exist.");

                stmtsList.Add (GotoStatement (SyntaxKind.GotoStatement, IdentifierName (label.JumpTarget)));

                break;
            }
            // TODO: case ESIR_NodeKind.GotoCaseStatement:

            case ESIR_NodeKind.ReturnStatement when stmt is ESIR_ReturnStatement retStmt: {
                if (retStmt.ReturnExpr is not null) {
                    var exprData = CompileExpression (ref passData, ref funcData, retStmt.ReturnExpr);
                    Debug.Assert (exprData.Value is not null);

                    stmtsList.Add (ReturnStatement (exprData.Value));
                } else
                    stmtsList.Add (ReturnStatement ());

                break;
            }

            case ESIR_NodeKind.LoopStatement when stmt is ESIR_LoopStatement loopStmt:
                CompileStatement_Loop (ref passData, ref funcData, ref stmtsList, loopStmt, null);
                break;

            case ESIR_NodeKind.ExpressionStatement when stmt is ESIR_ExpressionStatement exprStmt: {
                var exprData = CompileExpression (ref passData, ref funcData, exprStmt.Expression);
                Debug.Assert (exprData.Value is not null);

                stmtsList.Add (ExpressionStatement (exprData.Value));

                break;
            }

            default:
                throw new NotImplementedException ("Statement type not implemented.");
        }
    }

    private static void CompileStatement_Label (
        ref PassData passData,
        ref FunctionData funcData,
        ref StructPooledList<StatementSyntax> stmtsList,
        ESIR_LabeledStatement labelStmt
    ) {
        const string alreadyInUseError = "Name already in use in label stack?";

        var labelName = labelStmt.Label;
        var labelNameStr = labelName.GetCharsSpan ().GetPooledString ();

        if (labelStmt.Statement is ESIR_LoopStatement labeledLoop) {
            var labelSymbol = new LabelSymbol (
                labelName,
                labelNameStr,
                GetLabelNameLoopBreak (labelNameStr),
                GetLabelNameLoopContinue (labelNameStr)
            );

            if (!passData.LabelStack.AddSymbol (labelName, labelSymbol))
                throw new CompilationException (alreadyInUseError);

            var loopStmts = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);
            try {
                CompileStatement_Loop (ref passData, ref funcData, ref loopStmts, labeledLoop, labelNameStr);
                Debug.Assert (loopStmts.Count > 0);
                stmtsList.Add (LabeledStatement (labelNameStr, loopStmts [0]));
                stmtsList.AddRange (loopStmts.Span [1..]);
            } finally {
                loopStmts.Dispose ();
            }
        } else {
            if (!passData.LabelStack.AddSymbol (labelName, new (labelName, labelNameStr)))
                throw new CompilationException (alreadyInUseError);

            var labelStmts = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);
            try {
                CompileStatement (ref passData, ref funcData, ref labelStmts, labelStmt.Statement);
                Debug.Assert (labelStmts.Count > 0);

                if (labelStmts.Count == 0)
                    stmtsList.Add (LabeledStatement (labelNameStr, EmptyStatement ()));
                else
                    stmtsList.Add (LabeledStatement (labelNameStr, labelStmts [0]));

                stmtsList.AddRange (labelStmts.Span [1..]);
            } finally {
                labelStmts.Dispose ();
            }
        }
    }

    private static void CompileStatement_Loop (
        ref PassData passData,
        ref FunctionData funcData,
        ref StructPooledList<StatementSyntax> stmtsList,
        ESIR_LoopStatement loopStmt,
        string? labelName
    ) {
        if (labelName is null)
            labelName = GetIndexedLabel (funcData.LabelIndex++);

        var breakLabel = GetLabelNameLoopBreak (labelName);
        var continueLabel = GetLabelNameLoopContinue (labelName);

        var prevBreakTarget = funcData.CurBreakTarget;
        var prevContinueTarget = funcData.CurContinueTarget;
        funcData.CurBreakTarget = breakLabel;
        funcData.CurContinueTarget = continueLabel;

        using var bodyStmts = new StructPooledList<StatementSyntax> (CL_ClearMode.Auto);
        using var condExprs = PooledArray<ExpressionSyntax>.GetArray (loopStmt.ConditionExpr.Elements.Length);
        using var iterExprs = PooledArray<ExpressionSyntax>.GetArray (loopStmt.IterationExpr?.Elements.Length ?? 0);

        // Compile the condition expressions.
        var condNum = 0;
        foreach (var expr in loopStmt.ConditionExpr.Elements)
            condExprs.Span [condNum++] = CompileExpression (ref passData, ref funcData, expr).Value!;

        // Compile the iteration expressions.
        if (loopStmt.IterationExpr is not null) {
            var iterNum = 0;
            foreach (var expr in loopStmt.IterationExpr.Elements)
                iterExprs.Span [iterNum++] = CompileExpression (ref passData, ref funcData, expr).Value!;
        }

        // Compile the loop body.
        using var loopBody = CompileStatementUnwrapBlock (ref passData, ref funcData, loopStmt.Body);

        // Add the conditions.
        if (condExprs.RealLength > 0) {
            foreach (var expr in condExprs.Span [..^1])
                bodyStmts.Add (ExpressionStatement (expr));
            bodyStmts.Add (IfStatement (
                PrefixUnaryExpression (SyntaxKind.LogicalNotExpression, condExprs.Span [^1]),
                GotoStatement (SyntaxKind.GotoStatement, IdentifierName (breakLabel))
            ));
        }

        // Add the loop body.
        bodyStmts.AddRange (loopBody.Span);
        bodyStmts.Add (GotoStatement (SyntaxKind.GotoStatement, IdentifierName (continueLabel)));

        // Add the break section.
        bodyStmts.Add (LabeledStatement (breakLabel, BreakStatement ()));

        // Add the continue/iterator section.
        if (iterExprs.RealLength > 0) {
            bodyStmts.Add (LabeledStatement (continueLabel, ExpressionStatement (iterExprs.Span [0])));
            foreach (var expr in iterExprs.Span [1..])
                bodyStmts.Add (ExpressionStatement (expr));
        } else
            bodyStmts.Add (LabeledStatement (continueLabel, EmptyStatement ()));

        // Emit the while loop.
        stmtsList.Add (WhileStatement (LiteralExpression (SyntaxKind.TrueLiteralExpression), BlockSpan (bodyStmts.Span)));

        funcData.CurBreakTarget = prevBreakTarget;
        funcData.CurContinueTarget = prevContinueTarget;
    }
}
