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
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Frontend.Data;
using static EchelonScriptCompiler.CompilerCommon.IR.ESIR_Factory;

namespace EchelonScriptCompiler.Frontend;

internal unsafe static partial class Compiler_TypeChecking {
    private static void FinishExpression (ref PassData passData, ref ExpressionData exprData) {
        var irWriter = passData.IRWriter;
        using var exprList = exprData.Expressions;

        foreach (var expr in exprList.Expressions)
            irWriter.AddStatement (ExpressionStatement (expr));
        irWriter.AddStatement (ExpressionStatement (exprData.Value));

        exprList.ReturnRegisters (irWriter);
        irWriter.ReturnRegister (exprData.ValueRegister);

        exprData.Expressions = new ExpressionList (null, null);
    }

    private static void CheckFunction (
        ref CompileData compileData, ref PassData passData,
        ESC_TypeData? parentType, ESC_Namespace? nsData,
        ES_AstFunctionDefinition funcDef
    ) {
        Debug.Assert (parentType is not null || nsData is not null);

        var idPool = compileData.IdPool;
        var symbols = compileData.Symbols;
        var irWriter = passData.IRWriter;

        var funcName = idPool.GetIdentifier (funcDef.Name.Text.Span);
        var retType = GetTypeRef (funcDef.ReturnType);
        Debug.Assert (retType.Type is not null);

        ESC_Function? funcData;
        if (nsData is not null) {
            if (!nsData.Functions.TryGetValue (funcName, out funcData))
                Debug.Fail ("This shouldn't ever be reached.");
        } else
            throw new NotImplementedException ("[TODO] Member functions not implemented yet.");

        symbols.Push ();

        irWriter.StartFunction (MangleFunctionName (ref compileData, funcData), TypeNode (ref compileData, retType));

        foreach (var arg in funcDef.ArgumentsList) {
            var argName = idPool.GetIdentifier (arg.Name.Text.Span);
            var argValType = GetTypeRef (arg.ValueType);

            var flags = (FrontendSymbolFlags) 0;

            var argType = arg.ArgType;
            switch (argType) {
                case ES_ArgumentType.Normal:
                    break;

                case ES_ArgumentType.Ref:
                    flags |= FrontendSymbolFlags.RefVar;
                    break;

                case ES_ArgumentType.In:
                    if (argValType.Constness != ESC_Constness.Mutable) {
                        compileData.ErrorList.Add (ES_FrontendErrors.GenTypeAlreadyConst (
                            false, argValType.Constness == ESC_Constness.Immutable,
                            arg.Name
                        ));
                        break;
                    }

                    argType = ES_ArgumentType.Normal;
                    argValType = argValType.WithConst (ESC_Constness.Const);
                    break;

                case ES_ArgumentType.Out:
                    throw new NotImplementedException ("[TODO] Argument type not implemented yet.");

                default:
                    throw new NotImplementedException ("Argument type not implemented yet.");
            }

            if (argValType.IsWritable ())
                flags |= FrontendSymbolFlags.Writable;

            var argIdx = irWriter.AddArgument (ArgumentDefinition (argType, TypeNode (ref compileData, argValType)));
            if (!symbols.AddSymbol (argName, FrontendSymbol.NewVariable (new (argValType, ArgumentExpression (argIdx)), flags)))
                Debug.Fail ("This shouldn't be reachable.");

            if (arg.DefaultExpression is not null) {
                var argDefExpr = CheckExpression (ref compileData, ref passData, arg.DefaultExpression, argValType);
                EnsureCompat (ref compileData, ref passData, ref argDefExpr, argValType, arg.DefaultExpression.NodeBounds);
            }
        }

        // Emit the function body.
        Debug.Assert (funcDef.Statement is not null);
        Debug.Assert (funcDef.Statement.Endpoint is null);
        if (funcDef.ExpressionBody) {
            Debug.Assert (funcDef.Statement is ES_AstExpressionStatement);

            var exprExpType = retType.Type is not ESC_TypeVoid ? retType : compileData.GetUnknownType (ESC_Constness.Mutable);
            var exprStmt = (funcDef.Statement as ES_AstExpressionStatement)!;
            var exprData = CheckExpression (ref compileData, ref passData, exprStmt.Expression, exprExpType);

            if (retType.Type is not ESC_TypeVoid) {
                if (!EnsureCompat (ref compileData, ref passData, ref exprData, retType, exprData.Expr.NodeBounds))
                    compileData.ErrorList.Add (new (funcDef.Name, ES_FrontendErrors.MissingReturnStatement));
            }

            using var exprList = exprData.Expressions;

            foreach (var expr in exprList.Expressions)
                irWriter.AddStatement (ExpressionStatement (expr));

            if (retType.Type is not ESC_TypeVoid)
                irWriter.AddStatement (ReturnStatement (exprData.Value));
            else
                irWriter.AddStatement (ExpressionStatement (exprData.Value));

            irWriter.AddScopeRegisters (exprList.Registers);
            irWriter.AddScopeRegister (exprData.ValueRegister);
        } else {
            var stmtData = CheckStatement (ref compileData, ref passData, retType, funcDef.Statement);

            if (!stmtData.AlwaysReturns && retType.Type is not ESC_TypeVoid)
                compileData.ErrorList.Add (new (funcDef.Name, ES_FrontendErrors.MissingReturnStatement));
        }

        symbols.Pop ();

        var funcInfo = compileData.ToFunctionInfo (funcData);

        ESIR_TraceDataAttribute traceDataAttr;
        ESIR_FunctionDataAttribute funcDataAttr;
        if (parentType is not null) {
            traceDataAttr = TraceDataAttribute (
                parentType.Name.NamespaceName,
                funcName,
                parentType.Name.TypeName,
                funcDef.Name.FileName.Span.GetPooledString ()
            );

            funcDataAttr = FunctionDataAttribute (funcInfo, compileData.ToTypeInfo (parentType));
        } else {
            traceDataAttr = TraceDataAttribute (
                nsData.NamespaceName,
                funcName,
                funcDef.Name.FileName.Span.GetPooledString ()
            );

            funcDataAttr = FunctionDataAttribute (funcInfo);
        }

        irWriter.EndFunction (List<ESIR_Attribute> (traceDataAttr, funcDataAttr));
    }
}
