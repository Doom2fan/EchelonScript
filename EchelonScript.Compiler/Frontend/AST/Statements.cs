/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using EchelonScript.Compiler.CompilerCommon;

namespace EchelonScript.Compiler.Frontend.AST;

public abstract class ES_AstStatement : ES_AstNode {
    public ES_AstStatement? Endpoint { get; set; }

    public ES_AstStatement (int nothing) : base (1) { }
}

public sealed class ES_AstEmptyErrorStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    public ES_AstNodeBounds bounds;

    public ES_AstEmptyErrorStatement (EchelonScriptToken tk) : base (1) => bounds = new (tk.TextStartPos);
}

public sealed class ES_AstLabeledStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = LabelName.TextStartPos,
        EndPos = LabelName.TextEndPos,
    };

    public EchelonScriptToken LabelName;

    public ES_AstLabeledStatement (EchelonScriptToken label) : base (1) => LabelName = label;
}

public sealed class ES_AstBlockStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstStatement? Statement;

    public ES_AstBlockStatement (
        ES_AstStatement? statement, EchelonScriptToken openTk, EchelonScriptToken closeTk
    ) : base (1) {
        Statement = statement;

        bounds = new (openTk.TextStartPos, closeTk.TextEndPos);
    }
}

public sealed class ES_AstEmptyStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => new (semicolon.TextStartPos, semicolon.TextEndPos);

    private EchelonScriptToken semicolon;

    public ES_AstEmptyStatement (EchelonScriptToken semicol) : base (1) => semicolon = semicol;
}

#region Symbol definition

public sealed class ES_AstImportStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstNamespaceIdentifier NamespaceName;
    public EchelonScriptToken [] ImportedNames;

    public ES_AstImportStatement (
        ES_AstNamespaceIdentifier name, EchelonScriptToken [] importedNames,
        EchelonScriptToken startTk, EchelonScriptToken endTk
    ) : base (1) {
        NamespaceName = name;
        ImportedNames = importedNames;

        bounds = new (startTk.TextStartPos, endTk.TextEndPos);
    }
}

public sealed class ES_AstTypeAlias : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public EchelonScriptToken AliasName;
    public ES_AstTypeDeclaration? OriginalName;

    public ES_AstTypeAlias (
        EchelonScriptToken aliasName, ES_AstTypeDeclaration? origName,
        EchelonScriptToken startTk, EchelonScriptToken endTk
    ) : base (1) {
        AliasName = aliasName;
        OriginalName = origName;

        bounds = new (startTk.TextStartPos, endTk.TextEndPos);
    }
}

public sealed class ES_AstLocalVarDefinition : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public bool UsingVar;
    public ES_AstTypeDeclaration? ValueType;

    public (EchelonScriptToken Name, ES_AstExpression? InitializationExpression) [] Variables;

    public ES_AstLocalVarDefinition (
        bool usingVar, ES_AstTypeDeclaration? valType, (EchelonScriptToken, ES_AstExpression?) [] varsList,
        EchelonScriptToken varStartTk, EchelonScriptToken? endTk
    ) : base (1) {
        UsingVar = usingVar;
        ValueType = valType;

        Variables = varsList;

        int endPos;
        if (endTk != null)
            endPos = endTk.Value.TextEndPos;
        else if (Variables != null && Variables.Length > 0) {
            var lastVar = Variables [^0];

            endPos = lastVar.InitializationExpression?.NodeBounds.EndPos ?? lastVar.Name.TextEndPos;
        } else
            throw new Exception ();

        bounds = new (varStartTk.TextStartPos, endPos);
    }
}

#endregion

#region Jumps

public sealed class ES_AstConditionalStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression ConditionExpression;
    public ES_AstStatement ThenStatement;
    public ES_AstStatement? ElseStatement;

    public ES_AstConditionalStatement (
        ES_AstExpression condExpr, ES_AstStatement thenStatement, ES_AstStatement? elseStatement,
        EchelonScriptToken startTk
    ) : base (1) {
        ConditionExpression = condExpr;
        ThenStatement = thenStatement;
        ElseStatement = elseStatement;

        var endPos = elseStatement?.NodeBounds.EndPos ?? thenStatement.NodeBounds.EndPos;
        bounds = new (startTk.TextStartPos, endPos);
    }
}

public sealed class ES_AstSwitchStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression ValueExpression;
    public (ES_AstExpression? [] Expressions, ES_AstStatement StatementsBlock) [] Sections;

    public ES_AstSwitchStatement (
        ES_AstExpression valExpr, (ES_AstExpression? [], ES_AstStatement) [] sectionsList,
        EchelonScriptToken switchStartTk, EchelonScriptToken closeBraceTk
    ) : base (1) {
        ValueExpression = valExpr;
        Sections = sectionsList;

        bounds = new (switchStartTk.TextStartPos, closeBraceTk.TextEndPos);
    }
}

public sealed class ES_AstBreakStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public EchelonScriptToken? LabelName;

    public ES_AstBreakStatement (
        EchelonScriptToken? label, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
    ) : base (1) {
        LabelName = label;

        bounds = new (startTk.TextStartPos, semicolonTk.TextEndPos);
    }
}

public sealed class ES_AstContinueStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public EchelonScriptToken? LabelName;

    public ES_AstContinueStatement (
        EchelonScriptToken? label, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
    ) : base (1) {
        LabelName = label;

        bounds = new (startTk.TextStartPos, semicolonTk.TextEndPos);
    }
}

public sealed class ES_AstGotoLabelStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public EchelonScriptToken LabelName;

    public ES_AstGotoLabelStatement (
        EchelonScriptToken label, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
    ) : base (1) {
        LabelName = label;

        bounds = new (startTk.TextStartPos, semicolonTk.TextEndPos);
    }
}

public sealed class ES_AstGotoCaseStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression? CaseExpression;

    public ES_AstGotoCaseStatement (
        ES_AstExpression? caseExpr, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
    ) : base (1) {
        CaseExpression = caseExpr;

        bounds = new (startTk.TextStartPos, semicolonTk.TextEndPos);
    }
}

public sealed class ES_AstReturnStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression? ReturnExpression;

    public ES_AstReturnStatement (
        ES_AstExpression? retExpr, EchelonScriptToken startTk, EchelonScriptToken semicolonTk
    ) : base (1) {
        ReturnExpression = retExpr;

        bounds = new (startTk.TextStartPos, semicolonTk.TextEndPos);
    }
}

#endregion

#region Loops

public sealed class ES_AstForStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstStatement? InitializationStatement;
    public ES_AstExpression? ConditionExpression;
    public ES_AstExpression? []? IterationExpressions;

    public ES_AstStatement LoopBody;

    public ES_AstForStatement (
        ES_AstStatement? initStatement, ES_AstExpression? condExpr, ES_AstExpression? []? iterExprList,
        ES_AstStatement loopBody,
        EchelonScriptToken startToken, EchelonScriptToken? endToken
    ) : base (1) {
        InitializationStatement = initStatement;
        ConditionExpression = condExpr;
        IterationExpressions = iterExprList;

        LoopBody = loopBody;

        bounds = new (
            startToken.TextStartPos,
            endToken?.TextEndPos ?? loopBody?.NodeBounds.EndPos ?? startToken.TextEndPos
        );
    }
}

public sealed class ES_AstWhileStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression ConditionExpression;
    public bool PostLoop;

    public ES_AstStatement LoopBody;

    public ES_AstWhileStatement (
        ES_AstExpression condExpr, ES_AstStatement loopBody, bool postLoop,
        EchelonScriptToken startToken, EchelonScriptToken? endToken
    ) : base (1) {
        ConditionExpression = condExpr;
        PostLoop = postLoop;

        LoopBody = loopBody;

        bounds = new (
            startToken.TextStartPos,
            endToken?.TextEndPos ?? loopBody?.NodeBounds.EndPos ?? startToken.TextEndPos
        );
    }
}

#endregion

public sealed class ES_AstExpressionStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression Expression;

    public ES_AstExpressionStatement (ES_AstExpression expr, EchelonScriptToken semicolonTk) : base (1) {
        Expression = expr;

        bounds = new (
            expr is not ES_AstEmptyErrorExpression ? expr.NodeBounds.StartPos : semicolonTk.TextStartPos,
            semicolonTk.TextEndPos
        );
    }
}

public sealed class ES_AstExpressionListStatement : ES_AstStatement {
    public override ES_AstNodeBounds NodeBounds {
        get {
            ES_AstExpression? firstExpr = null;
            ES_AstExpression? lastExpr = null;

            for (var i = 0; i < Expressions.Length; i++) {
                if (Expressions [i] != null) {
                    firstExpr = Expressions [i];
                    break;
                }
            }
            for (var i = Expressions.Length - 1; i >= 0; i--) {
                if (Expressions [i] != null) {
                    lastExpr = Expressions [i];
                    break;
                }
            }

            if (firstExpr == null || lastExpr == null)
                return new ES_AstNodeBounds ();

            return new ES_AstNodeBounds () {
                StartPos = firstExpr.NodeBounds.StartPos,
                EndPos = lastExpr.NodeBounds.EndPos,
            };
        }
    }

    public ES_AstExpression [] Expressions;

    public ES_AstExpressionListStatement (ES_AstExpression [] exprList) : base (1) => Expressions = exprList;
}
