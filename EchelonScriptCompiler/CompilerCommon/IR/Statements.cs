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
using EchelonScriptCommon.Data;

namespace EchelonScriptCompiler.CompilerCommon.IR;

public abstract class ESIR_Statement : ESIR_Node { }

public class ESIR_BlockStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.BlockStatement;
    internal override int ChildrenCount => 1;

    private readonly ESIR_List<ESIR_Statement> statementsNode;

    public ESIR_List<ESIR_Statement> Statements => statementsNode;

    internal ESIR_BlockStatement (ESIR_List<ESIR_Statement> statement) => statementsNode = statement;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return statementsNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_LabeledStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.LabeledStatement;
    internal override int ChildrenCount => 2;

    private readonly ESIR_ValueNode labelNode;
    private readonly ESIR_Statement statementNode;

    public ES_Identifier Label => labelNode.GetIdentifier ()!.Value;
    public ESIR_Statement Statement => statementNode;

    internal ESIR_LabeledStatement (ESIR_ValueNode label, ESIR_Statement statement) {
        labelNode = label;
        statementNode = statement;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return labelNode;
            case 1: return statementNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_ConditionalStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ConditionalStatement;
    internal override int ChildrenCount => 3;

    private readonly ESIR_Expression conditionNode;
    private readonly ESIR_Statement thenNode;
    private readonly ESIR_Statement? elseNode;

    public ESIR_Expression Condition => conditionNode;
    public ESIR_Statement Then => thenNode;
    public ESIR_Statement? Else => elseNode;

    internal ESIR_ConditionalStatement (ESIR_Expression condition, ESIR_Statement thenStmt, ESIR_Statement? elseStmt) {
        conditionNode = condition;
        thenNode = thenStmt;
        elseNode = elseStmt;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return conditionNode;
            case 1: return thenNode;
            case 2: return elseNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_BreakStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.BreakStatement;
    internal override int ChildrenCount => 1;

    private readonly ESIR_ValueNode? labelNode;

    public ES_Identifier? Label => labelNode?.GetIdentifier ();

    internal ESIR_BreakStatement (ESIR_ValueNode? label) => labelNode = label;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return labelNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_ContinueStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ContinueStatement;
    internal override int ChildrenCount => 1;

    private readonly ESIR_ValueNode? labelNode;

    public ES_Identifier? Label => labelNode?.GetIdentifier ();

    internal ESIR_ContinueStatement (ESIR_ValueNode? label) => labelNode = label;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return labelNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_GotoLabelStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.GotoLabelStatement;
    internal override int ChildrenCount => 1;

    private readonly ESIR_ValueNode labelNode;

    public ES_Identifier Label => labelNode.GetIdentifier ()!.Value;

    internal ESIR_GotoLabelStatement (ESIR_ValueNode label) => labelNode = label;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return labelNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_GotoCaseStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.GotoCaseStatement;
    internal override int ChildrenCount => 1;

    private readonly ESIR_Expression? caseExprNode;

    public ESIR_Expression? CaseExpr => caseExprNode;

    internal ESIR_GotoCaseStatement (ESIR_Expression? caseExpr) => caseExprNode = caseExpr;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return caseExprNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_ReturnStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ReturnStatement;
    internal override int ChildrenCount => 1;

    private readonly ESIR_Expression? retExprNode;

    public ESIR_Expression? ReturnExpr => retExprNode;

    internal ESIR_ReturnStatement (ESIR_Expression? retExpr) => retExprNode = retExpr;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return retExprNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_LoopStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.LoopStatement;
    internal override int ChildrenCount => 3;

    private readonly ESIR_List<ESIR_Expression> condNode;
    private readonly ESIR_List<ESIR_Expression>? iterNode;
    private readonly ESIR_Statement bodyNode;

    public ESIR_List<ESIR_Expression> ConditionExpr => condNode;
    public ESIR_List<ESIR_Expression>? IterationExpr => iterNode;
    public ESIR_Statement Body => bodyNode;

    internal ESIR_LoopStatement (
        ESIR_List<ESIR_Expression> condExpr,
        ESIR_List<ESIR_Expression>? iterExpr,
        ESIR_Statement bodyStmt
    ) {
        condNode = condExpr;
        iterNode = iterExpr;
        bodyNode = bodyStmt;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return condNode;
            case 1: return iterNode;
            case 2: return bodyNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_ExpressionStatement : ESIR_Statement {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ExpressionStatement;
    internal override int ChildrenCount => 1;

    private readonly ESIR_Expression exprNode;

    public ESIR_Expression Expression => exprNode;

    internal ESIR_ExpressionStatement (ESIR_Expression label) => exprNode = label;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return exprNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public static partial class ESIR_Factory {
    public static ESIR_BlockStatement BlockStatement (ESIR_List<ESIR_Statement> statements) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.BlockStatement, statements, out var hash);
        if (node is not null)
            return (ESIR_BlockStatement) node;

        var ret = new ESIR_BlockStatement (statements);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_Statement OptionalBlockStatement (ReadOnlySpan<ESIR_Statement> statements) {
        if (statements.Length == 1)
            return statements [0];

        return BlockStatement (List (statements));
    }

    public static ESIR_LabeledStatement LabeledStatement (ES_Identifier label, ESIR_Statement statement)
        => LabeledStatement (ValueNode (label), statement);
    private static ESIR_LabeledStatement LabeledStatement (ESIR_ValueNode label, ESIR_Statement statement) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.LabeledStatement, label, statement, out var hash);
        if (node is not null)
            return (ESIR_LabeledStatement) node;

        var ret = new ESIR_LabeledStatement (label, statement);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_ConditionalStatement ConditionalStatement (ESIR_Expression condition, ESIR_Statement thenStmt, ESIR_Statement? elseStmt) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ConditionalStatement, condition, thenStmt, elseStmt, out var hash);
        if (node is not null)
            return (ESIR_ConditionalStatement) node;

        var ret = new ESIR_ConditionalStatement (condition, thenStmt, elseStmt);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_BreakStatement BreakStatement () => BreakStatement (null);
    public static ESIR_BreakStatement BreakStatement (ES_Identifier label) => BreakStatement (ValueNode (label));
    private static ESIR_BreakStatement BreakStatement (ESIR_ValueNode? label) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.BreakStatement, label, out var hash);
        if (node is not null)
            return (ESIR_BreakStatement) node;

        var ret = new ESIR_BreakStatement (label);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_ContinueStatement ContinueStatement () => ContinueStatement (null);
    public static ESIR_ContinueStatement ContinueStatement (ES_Identifier label)
        => ContinueStatement (ValueNode (label));
    private static ESIR_ContinueStatement ContinueStatement (ESIR_ValueNode? label) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ContinueStatement, label, out var hash);
        if (node is not null)
            return (ESIR_ContinueStatement) node;

        var ret = new ESIR_ContinueStatement (label);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_GotoLabelStatement GotoLabelStatement (ES_Identifier label)
        => GotoLabelStatement (ValueNode (label));
    private static ESIR_GotoLabelStatement GotoLabelStatement (ESIR_ValueNode label) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.GotoLabelStatement, label, out var hash);
        if (node is not null)
            return (ESIR_GotoLabelStatement) node;

        var ret = new ESIR_GotoLabelStatement (label);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_GotoCaseStatement GotoDefaultStatement () => GotoCaseStatementInternal (null);
    public static ESIR_GotoCaseStatement GotoCaseStatement (ESIR_Expression caseExpr)
        => GotoCaseStatementInternal (caseExpr);
    private static ESIR_GotoCaseStatement GotoCaseStatementInternal (ESIR_Expression? caseExpr) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.GotoCaseStatement, caseExpr, out var hash);
        if (node is not null)
            return (ESIR_GotoCaseStatement) node;

        var ret = new ESIR_GotoCaseStatement (caseExpr);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_ReturnStatement ReturnStatement (ESIR_Expression? retExpr = null) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ReturnStatement, retExpr, out var hash);
        if (node is not null)
            return (ESIR_ReturnStatement) node;

        var ret = new ESIR_ReturnStatement (retExpr);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_LoopStatement LoopStatement (
        ESIR_List<ESIR_Expression> condExpr,
        ESIR_List<ESIR_Expression>? iterExprs,
        ESIR_Statement bodyStmt
    ) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.LoopStatement, condExpr, iterExprs, bodyStmt, out var hash);
        if (node is not null)
            return (ESIR_LoopStatement) node;

        var ret = new ESIR_LoopStatement (condExpr, iterExprs, bodyStmt);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_ExpressionStatement ExpressionStatement (ESIR_Expression expr) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ExpressionStatement, expr, out var hash);
        if (node is not null)
            return (ESIR_ExpressionStatement) node;

        var ret = new ESIR_ExpressionStatement (expr);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }
}
