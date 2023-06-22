/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Common;
using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.CompilerCommon;

namespace EchelonScript.Compiler.Frontend.AST;

#if false
public sealed class ES_AstFunctionDefinition : ES_AstNode {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AccessModifier AccessModifier;
    public EchelonScriptToken? DocComment;

    public bool Static;
    public bool Const;
    public ES_VirtualnessModifier VirtualMod;

    public EchelonScriptToken Name;
    public ES_AstTypeDeclaration ReturnType;
    public ES_AstFunctionArgumentDefinition [] ArgumentsList;

    public bool ExpressionBody;
    public ES_AstStatement? Statement;

    public ES_AstFunctionDefinition (
        ES_AccessModifier accessMod, EchelonScriptToken? docCom, bool staticMod, bool constMod,
        ES_VirtualnessModifier virtualMod, EchelonScriptToken name, ES_AstTypeDeclaration retType,
        ES_AstFunctionArgumentDefinition [] argsList,
        bool exprBody, ES_AstStatement? statements,
        EchelonScriptToken closeBraceTk
    ) : base (1) {
        AccessModifier = accessMod;
        DocComment = docCom;

        Static = staticMod;
        Const = constMod;
        VirtualMod = virtualMod;

        Name = name;
        ReturnType = retType;
        ArgumentsList = argsList;

        ExpressionBody = exprBody;
        Statement = statements;

        bounds = new (retType?.NodeBounds.StartPos ?? name.TextStartPos, closeBraceTk.TextEndPos);
    }
}

public sealed class ES_AstFunctionArgumentDefinition : ES_AstNode {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_ArgumentKind ArgType;
    public ES_AstTypeDeclaration? ValueType;
    public EchelonScriptToken Name;
    public ES_AstExpression? DefaultExpression;

    public ES_AstFunctionArgumentDefinition (
        ES_ArgumentKind argType, ES_AstTypeDeclaration? valueType, EchelonScriptToken name, ES_AstExpression? defaultExpr,
        EchelonScriptToken startTk
    ) : base (1) {
        ArgType = argType;
        ValueType = valueType;
        Name = name;
        DefaultExpression = defaultExpr;

        bounds = new (startTk.TextStartPos, defaultExpr?.NodeBounds.EndPos ?? name.TextEndPos);
    }
}
#endif
