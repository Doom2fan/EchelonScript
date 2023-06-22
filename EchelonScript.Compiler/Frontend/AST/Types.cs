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
public abstract class ES_AstAggregateDefinition : ES_AstNode {
    public ES_AccessModifier AccessModifier;
    public ES_AstNode? [] Contents;

    public EchelonScriptToken Name;

    public ES_AstAggregateDefinition (ES_AccessModifier accessMod, ES_AstNode? [] contents) : base (1) {
        AccessModifier = accessMod;
        Contents = contents;
    }
}

public sealed class ES_AstClassDefinition : ES_AstAggregateDefinition {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public EchelonScriptToken? DocComment;
    public bool Static;
    public bool Abstract;

    public ES_AstTypeDeclaration_TypeName [] InheritanceList;

    public ES_AstClassDefinition (
        EchelonScriptToken? docCom, ES_AccessModifier accessMod, bool staticMod, bool abstractMod,
        EchelonScriptToken name, ES_AstTypeDeclaration_TypeName [] inheritance, ES_AstNode? [] contents,
        EchelonScriptToken startToken, EchelonScriptToken endToken
    ) : base (accessMod, contents) {
        DocComment = docCom;
        Static = staticMod;
        Abstract = abstractMod;

        Name = name;
        InheritanceList = inheritance;

        bounds = new ES_AstNodeBounds {
            StartPos = startToken.TextStartPos,
            EndPos = endToken.TextEndPos,
        };
    }
}

public sealed class ES_AstStructDefinition : ES_AstAggregateDefinition {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public EchelonScriptToken? DocComment;

    public ES_AstTypeDeclaration_TypeName [] InterfacesList;

    public ES_AstStructDefinition (
        EchelonScriptToken? docCom, ES_AccessModifier accessMod,
        EchelonScriptToken name, ES_AstTypeDeclaration_TypeName [] interfaces, ES_AstNode? [] contents,
        EchelonScriptToken startToken, EchelonScriptToken endToken
    ) : base (accessMod, contents) {
        DocComment = docCom;

        Name = name;
        InterfacesList = interfaces;

        bounds = new (startToken.TextStartPos, endToken.TextEndPos);
    }
}

public sealed class ES_AstMemberVarDefinition : ES_AstNode {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AccessModifier AccessModifier;
    public EchelonScriptToken? DocComment;

    public bool Static;

    public EchelonScriptToken Name;
    public ES_AstTypeDeclaration? ValueType;
    public ES_AstExpression? InitializationExpression;

    public ES_AstMemberVarDefinition (
        ES_AccessModifier accessMod, EchelonScriptToken? docCom, bool staticMod,
        EchelonScriptToken name, ES_AstTypeDeclaration? valueType, ES_AstExpression? initExpr,
        EchelonScriptToken semicolonTk
    ) : base (1) {
        AccessModifier = accessMod;
        DocComment = docCom;

        Static = staticMod;

        Name = name;
        ValueType = valueType;
        InitializationExpression = initExpr;

        bounds = new (valueType?.NodeBounds.StartPos ?? name.TextStartPos, semicolonTk.TextEndPos);
    }
}

public sealed class ES_AstConstructorDefinition : ES_AstNode {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AccessModifier AccessModifier;
    public EchelonScriptToken? DocComment;

    public bool Static;

    public EchelonScriptToken ThisToken;

    public ES_AstFunctionArgumentDefinition [] ArgumentsList;

    public bool ExpressionBody;
    public ES_AstStatement? Statement;

    public ES_AstConstructorDefinition (
        ES_AccessModifier accessMod, EchelonScriptToken? docCom, bool staticMod, EchelonScriptToken startTk,
        ES_AstFunctionArgumentDefinition [] argsList,
        bool exprBody, ES_AstStatement? statements,
        EchelonScriptToken closeBraceTk
    ) : base (1) {
        AccessModifier = accessMod;
        DocComment = docCom;

        Static = staticMod;

        ThisToken = startTk;

        ArgumentsList = argsList;

        ExpressionBody = exprBody;
        Statement = statements;

        bounds = new (ThisToken.TextStartPos, closeBraceTk.TextEndPos);
    }
}

public sealed class ES_AstEnumDefinition : ES_AstNode {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AccessModifier AccessModifier;
    public EchelonScriptToken? DocComment;

    public EchelonScriptToken Name;
    public EchelonScriptToken? BaseType;

    public (EchelonScriptToken, ES_AstExpression?) [] MembersList;

    public ES_AstEnumDefinition (
        ES_AccessModifier accessMod, EchelonScriptToken? docCom,
        EchelonScriptToken name, EchelonScriptToken? baseType, (EchelonScriptToken, ES_AstExpression?) [] membersList,
        EchelonScriptToken startTk, EchelonScriptToken closeBraceTk
    ) : base (1) {
        AccessModifier = accessMod;
        DocComment = docCom;

        Name = name;
        BaseType = baseType;

        MembersList = membersList;

        bounds = new (startTk.TextStartPos, closeBraceTk.TextEndPos);
    }
}
#endif
