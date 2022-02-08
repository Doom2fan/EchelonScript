/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Data;

using static EchelonScriptCompiler.CompilerCommon.IR.ESIR_Factory;

namespace EchelonScriptCompiler.Frontend {
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
            ref PassData passData,
            ES_TypeInfo* parentType, ES_NamespaceData? namespaceData,
            ES_AstFunctionDefinition funcDef
        ) {
            Debug.Assert (parentType is not null || namespaceData is not null);

            var idPool = passData.Env.IdPool;
            var symbols = passData.Symbols;
            var irWriter = passData.IRWriter;

            var funcName = idPool.GetIdentifier (funcDef.Name.Text.Span);
            var retType = GetTypeRef (funcDef.ReturnType);
            Debug.Assert (retType is not null);

            ES_FunctionData* funcData;
            if (namespaceData is not null) {
                if (!namespaceData.Functions.TryGetValue (funcName, out var funcDataPtr))
                    Debug.Fail ("This shouldn't ever be reached.");

                funcData = funcDataPtr.Address;
            } else
                throw new NotImplementedException ("[TODO] Member functions not implemented yet.");

            symbols.Push ();

            irWriter.StartFunction (MangleFunctionName (ref passData, funcData), TypeNode (retType));

            foreach (var arg in funcDef.ArgumentsList) {
                var argName = idPool.GetIdentifier (arg.Name.Text.Span);
                var argValType = GetTypeRef (arg.ValueType);

                var flags = (SymbolFlags) 0;

                var argType = arg.ArgType;
                switch (argType) {
                    case ES_ArgumentType.Normal:
                        break;

                    case ES_ArgumentType.Ref:
                        flags |= SymbolFlags.RefVar;
                        break;

                    case ES_ArgumentType.In:
                        argValType = passData.EnvBuilder.CreateConstType (argValType);
                        break;

                    case ES_ArgumentType.Out:
                        throw new NotImplementedException ("[TODO] Argument type not implemented yet.");

                    default:
                        throw new NotImplementedException ("Argument type not implemented yet.");
                }

                if (argValType->IsConstant ())
                    flags |= SymbolFlags.Constant;
                if (argValType->IsWritable ())
                    flags |= SymbolFlags.Writable;

                var argIdx = irWriter.AddArgument (ArgumentDefinition (argType, TypeNode (argValType)));
                if (!symbols.AddSymbol (argName, TCSymbol.NewVariable (argValType, ArgumentExpression (argIdx), flags)))
                    Debug.Fail ("This shouldn't be reachable.");

                if (arg.DefaultExpression is not null) {
                    var argDefExpr = CheckExpression (ref passData, arg.DefaultExpression, argValType);
                    EnsureCompat (ref argDefExpr, argValType, ref passData, arg.DefaultExpression.NodeBounds);
                }
            }

            // Emit the function body.
            Debug.Assert (funcDef.Statement is not null);
            Debug.Assert (funcDef.Statement.Endpoint is null);
            if (funcDef.ExpressionBody) {
                Debug.Assert (funcDef.Statement is ES_AstExpressionStatement);

                var exprExpType = retType->TypeTag != ES_TypeTag.Void ? retType : passData.Env.TypeUnknownValue;
                var exprStmt = (funcDef.Statement as ES_AstExpressionStatement)!;
                var exprData = CheckExpression (ref passData, exprStmt.Expression, exprExpType);

                if (retType->TypeTag != ES_TypeTag.Void) {
                    if (!EnsureCompat (ref exprData, retType, ref passData, exprData.Expr.NodeBounds))
                        passData.ErrorList.Add (new (funcDef.Name, ES_FrontendErrors.MissingReturnStatement));
                }

                using var exprList = exprData.Expressions;

                foreach (var expr in exprList.Expressions)
                    irWriter.AddStatement (ExpressionStatement (expr));

                if (retType->TypeTag != ES_TypeTag.Void)
                    irWriter.AddStatement (ReturnStatement (exprData.Value));
                else
                    irWriter.AddStatement (ExpressionStatement (exprData.Value));

                irWriter.AddScopeRegisters (exprList.Registers);
                irWriter.AddScopeRegister (exprData.ValueRegister);
            } else {
                var stmtData = CheckStatement (ref passData, retType, funcDef.Statement);

                if (!stmtData.AlwaysReturns && retType->TypeTag != ES_TypeTag.Void)
                    passData.ErrorList.Add (new (funcDef.Name, ES_FrontendErrors.MissingReturnStatement));
            }

            symbols.Pop ();

            ESIR_TraceDataAttribute traceDataAttr;
            ESIR_FunctionDataAttribute funcDataAttr;
            if (parentType is not null) {
                traceDataAttr = TraceDataAttribute (
                    parentType->Name.NamespaceName,
                    funcName,
                    parentType->Name.TypeName,
                    funcDef.Name.FileName.Span.GetPooledString ()
                );

                funcDataAttr = FunctionDataAttribute (funcData, parentType);
            } else {
                traceDataAttr = TraceDataAttribute (
                    namespaceData.NamespaceName,
                    funcName,
                    funcDef.Name.FileName.Span.GetPooledString ()
                );

                funcDataAttr = FunctionDataAttribute (funcData);
            }

            irWriter.EndFunction (List<ESIR_Attribute> (traceDataAttr, funcDataAttr));
        }
    }
}
