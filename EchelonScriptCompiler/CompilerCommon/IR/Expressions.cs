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
using EchelonScriptCommon.Utilities;

namespace EchelonScriptCompiler.CompilerCommon.IR;

public abstract class ESIR_Expression : ESIR_Node { }

public class ESIR_ErrorExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ErrorExpression;
    internal override int ChildrenCount => 0;

    internal ESIR_ErrorExpression () { }

    internal override ESIR_Node? GetChild (int slot) {
        Debug.Fail ("Invalid slot num");
        return null;
    }
}

public class ESIR_SimpleBinaryExpression : ESIR_Expression {
    private ESIR_NodeKind kind;
    public override ESIR_NodeKind Kind => kind;
    internal override int ChildrenCount => 2;

    private readonly ESIR_Expression leftNode;
    private readonly ESIR_Expression rightNode;

    public ESIR_Expression ExprLeft => leftNode;
    public ESIR_Expression ExprRight => rightNode;

    internal ESIR_SimpleBinaryExpression (ESIR_NodeKind op, ESIR_Expression left, ESIR_Expression right) {
        kind = op;
        leftNode = left;
        rightNode = right;
    }

    public bool IsComparison () => Kind.IsBinaryComparison ();

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return leftNode;
            case 1: return rightNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_AssignExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.AssignExpression;
    internal override int ChildrenCount => 2;

    private readonly ESIR_Expression assigneeNode;
    private readonly ESIR_Expression valueNode;

    public ESIR_Expression Assignee => assigneeNode;
    public ESIR_Expression Value => valueNode;

    internal ESIR_AssignExpression (ESIR_Expression assignee, ESIR_Expression value) {
        assigneeNode = assignee;
        valueNode = value;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return assigneeNode;
            case 1: return valueNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_UnaryExpression : ESIR_Expression {
    private ESIR_NodeKind kind;
    public override ESIR_NodeKind Kind => kind;
    internal override int ChildrenCount => 1;

    private readonly ESIR_Expression innerNode;

    public ESIR_Expression ExprInner => innerNode;

    internal ESIR_UnaryExpression (ESIR_NodeKind op, ESIR_Expression inner) {
        kind = op;
        innerNode = inner;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return innerNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_LiteralExpression : ESIR_Expression {
    private ESIR_NodeKind kind;
    public override ESIR_NodeKind Kind => kind;
    internal override int ChildrenCount => 1;

    private readonly ESIR_ValueNode? valueNode;

    public ESIR_ValueNode? Value => valueNode;

    internal ESIR_LiteralExpression (ESIR_NodeKind op, ESIR_ValueNode? value) {
        kind = op;
        valueNode = value;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return valueNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_NullLiteralExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.LiteralNull;
    internal override int ChildrenCount => 1;

    private readonly ESIR_TypeNode typeNode;

    public ESIR_TypeNode Type => typeNode;

    internal ESIR_NullLiteralExpression (ESIR_TypeNode type) => typeNode = type;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return typeNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_StringConstantExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.StringConstant;
    internal override int ChildrenCount => 1;

    private readonly ESIR_ValueNode indexNode;

    public int Index => (int) indexNode.GetInt ()!.Value;

    internal ESIR_StringConstantExpression (ESIR_ValueNode index) => indexNode = index;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return indexNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_StaticVariableExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.StaticVariableExpression;
    internal override int ChildrenCount => 1;

    private readonly ESIR_ValueNode nameNode;

    public ArrayPointer<byte> Name => nameNode.GetIdentifier ()!.Value;

    internal ESIR_StaticVariableExpression (ESIR_ValueNode index) => nameNode = index;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return nameNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_ArgumentExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ArgumentExpression;
    internal override int ChildrenCount => 1;

    private readonly ESIR_ValueNode indexNode;

    public int Index => (int) indexNode.GetInt ()!.Value;

    internal ESIR_ArgumentExpression (ESIR_ValueNode index) => indexNode = index;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return indexNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_LocalValueExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.LocalValueExpression;
    internal override int ChildrenCount => 1;

    private readonly ESIR_ValueNode indexNode;

    public int Index => (int) indexNode.GetInt ()!.Value;

    internal ESIR_LocalValueExpression (ESIR_ValueNode index) => indexNode = index;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return indexNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_DefaultValueExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.DefaultValueExpression;
    internal override int ChildrenCount => 1;

    private readonly ESIR_TypeNode typeNode;

    public ESIR_TypeNode Type => typeNode;

    internal ESIR_DefaultValueExpression (ESIR_TypeNode type) => typeNode = type;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return typeNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_MemberAccessExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.MemberAccessExpression;
    internal override int ChildrenCount => 2;

    private readonly ESIR_Expression parentNode;
    private readonly ESIR_ValueNode nameNode;

    public ESIR_Expression ExprParent => parentNode;
    public ArrayPointer<byte> Name => nameNode.GetIdentifier ()!.Value;

    internal ESIR_MemberAccessExpression (ESIR_Expression parent, ESIR_ValueNode name) {
        parentNode = parent;
        nameNode = name;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return parentNode;
            case 1: return nameNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_FunctionCallExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.FunctionCallExpression;
    internal override int ChildrenCount => 2;

    private readonly ESIR_ValueNode nameNode;
    private readonly ESIR_List<ESIR_ArgumentValue> argumentsNode;

    public ArrayPointer<byte> Name => nameNode.GetIdentifier ()!.Value;
    public ESIR_List<ESIR_ArgumentValue> Arguments => argumentsNode;

    internal ESIR_FunctionCallExpression (ESIR_ValueNode name, ESIR_List<ESIR_ArgumentValue> arguments) {
        nameNode = name;
        argumentsNode = arguments;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return nameNode;
            case 1: return argumentsNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_IndexingExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.IndexingExpression;
    internal override int ChildrenCount => 2;

    private readonly ESIR_Expression indexedNode;
    private readonly ESIR_List<ESIR_Expression> indicesNode;

    public ESIR_Expression IndexedExpr => indexedNode;
    public ESIR_List<ESIR_Expression> Indices => indicesNode;

    internal ESIR_IndexingExpression (ESIR_Expression indexed, ESIR_List<ESIR_Expression> indices) {
        indexedNode = indexed;
        indicesNode = indices;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return indexedNode;
            case 1: return indicesNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_NewObjectExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.NewObjectExpression;
    internal override int ChildrenCount => 1;

    private readonly ESIR_TypeNode typeNode;

    public ESIR_TypeNode Type => typeNode;

    internal ESIR_NewObjectExpression (ESIR_TypeNode type) => typeNode = type;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return typeNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_NewArrayExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.NewArrayExpression;
    internal override int ChildrenCount => 2;

    private readonly ESIR_TypeNode elemTypeNode;
    private readonly ESIR_List<ESIR_Expression> ranksNode;

    public ESIR_TypeNode ElementType => elemTypeNode;
    public ESIR_List<ESIR_Expression> Ranks => ranksNode;

    internal ESIR_NewArrayExpression (ESIR_TypeNode elemType, ESIR_List<ESIR_Expression> ranks) {
        elemTypeNode = elemType;
        ranksNode = ranks;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return elemTypeNode;
            case 1: return ranksNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_CastExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.CastExpression;
    internal override int ChildrenCount => 2;

    private readonly ESIR_Expression exprNode;
    private readonly ESIR_TypeNode destTypeNode;

    public ESIR_Expression Expression => exprNode;
    public ESIR_TypeNode DestType => destTypeNode;

    internal ESIR_CastExpression (ESIR_Expression expr, ESIR_TypeNode elemType) {
        destTypeNode = elemType;
        exprNode = expr;
    }

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return exprNode;
            case 1: return destTypeNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public class ESIR_ConditionalExpression : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ConditionalExpression;
    internal override int ChildrenCount => 3;

    private readonly ESIR_Expression conditionNode;
    private readonly ESIR_Expression thenNode;
    private readonly ESIR_Expression elseNode;

    public ESIR_Expression Condition => conditionNode;
    public ESIR_Expression ThenExpression => thenNode;
    public ESIR_Expression ElseExpression => elseNode;

    internal ESIR_ConditionalExpression (ESIR_Expression condition, ESIR_Expression thenExpr, ESIR_Expression elseExpr) {
        conditionNode = condition;
        thenNode = thenExpr;
        elseNode = elseExpr;
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

public class ESIR_ExpressionList : ESIR_Expression {
    public override ESIR_NodeKind Kind => ESIR_NodeKind.ExpressionList;
    internal override int ChildrenCount => 1;

    private readonly ESIR_List<ESIR_Expression> exprsNode;

    public ESIR_List<ESIR_Expression> Expressions => exprsNode;

    internal ESIR_ExpressionList (ESIR_List<ESIR_Expression> exprs) => exprsNode = exprs;

    internal override ESIR_Node? GetChild (int slot) {
        switch (slot) {
            case 0: return exprsNode;

            default:
                Debug.Fail ("Invalid slot num");
                return null;
        }
    }
}

public static partial class ESIR_Factory {
    public static ESIR_ErrorExpression ErrorExpression () {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ErrorExpression, out var hash);
        if (node is not null)
            return (ESIR_ErrorExpression) node;

        var ret = new ESIR_ErrorExpression ();
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_SimpleBinaryExpression SimpleBinaryExpression (ESIR_NodeKind kind, ESIR_Expression left, ESIR_Expression right) {
        if (kind < ESIR_NodeKind.BinaryExprConcat || kind > ESIR_NodeKind.BinaryExprLogicalOr)
            throw new ArgumentOutOfRangeException (nameof (kind), "Kind is not a simple binary expression.");

        var node = ESIR_NodeCache.Shared.TryGetNode (kind, left, right, out var hash);
        if (node is not null)
            return (ESIR_SimpleBinaryExpression) node;

        var ret = new ESIR_SimpleBinaryExpression (kind, left, right);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_AssignExpression AssignmentExpression (ESIR_Expression assignee, ESIR_Expression value) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.AssignExpression, assignee, value, out var hash);
        if (node is not null)
            return (ESIR_AssignExpression) node;

        var ret = new ESIR_AssignExpression (assignee, value);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_UnaryExpression UnaryExpression (ESIR_NodeKind kind, ESIR_Expression inner) {
        if (kind < ESIR_NodeKind.UnaryNegative || kind > ESIR_NodeKind.UnaryPostDecrement)
            throw new ArgumentOutOfRangeException (nameof (kind), "Kind is not an unary expression.");

        var node = ESIR_NodeCache.Shared.TryGetNode (kind, inner, out var hash);
        if (node is not null)
            return (ESIR_UnaryExpression) node;

        var ret = new ESIR_UnaryExpression (kind, inner);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_LiteralExpression LiteralTrueExpression ()
        => LiteralExpressionInternal (ESIR_NodeKind.LiteralTrue, null);
    public static ESIR_LiteralExpression LiteralFalseExpression ()
        => LiteralExpressionInternal (ESIR_NodeKind.LiteralFalse, null);

    public static ESIR_LiteralExpression LiteralExpression (ESIR_NodeKind kind, ESIR_ValueNode value) {
        if (kind < ESIR_NodeKind.LiteralInt || kind > ESIR_NodeKind.LiteralChar)
            throw new ArgumentOutOfRangeException (nameof (kind), "Kind is not an literal expression.");

        return LiteralExpressionInternal (kind, value);
    }
    private static ESIR_LiteralExpression LiteralExpressionInternal (ESIR_NodeKind kind, ESIR_ValueNode? value) {
        var node = ESIR_NodeCache.Shared.TryGetNode (kind, value, out var hash);
        if (node is not null)
            return (ESIR_LiteralExpression) node;

        var ret = new ESIR_LiteralExpression (kind, value);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_NullLiteralExpression NullLiteralExpression (ESIR_TypeNode type) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.LiteralNull, type, out var hash);
        if (node is not null)
            return (ESIR_NullLiteralExpression) node;

        var ret = new ESIR_NullLiteralExpression (type);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_StringConstantExpression StringConstantExpression (int index)
        => StringConstantExpression (ValueNode (index));
    private static ESIR_StringConstantExpression StringConstantExpression (ESIR_ValueNode index) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.StringConstant, index, out var hash);
        if (node is not null)
            return (ESIR_StringConstantExpression) node;

        var ret = new ESIR_StringConstantExpression (index);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_StaticVariableExpression StaticVariableExpression (ArrayPointer<byte> name)
        => StaticVariableExpression (ValueNode (name));
    private static ESIR_StaticVariableExpression StaticVariableExpression (ESIR_ValueNode name) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.StaticVariableExpression, name, out var hash);
        if (node is not null)
            return (ESIR_StaticVariableExpression) node;

        var ret = new ESIR_StaticVariableExpression (name);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_ArgumentExpression ArgumentExpression (int index)
        => ArgumentExpression (ValueNode (index));
    private static ESIR_ArgumentExpression ArgumentExpression (ESIR_ValueNode index) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ArgumentExpression, index, out var hash);
        if (node is not null)
            return (ESIR_ArgumentExpression) node;

        var ret = new ESIR_ArgumentExpression (index);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_LocalValueExpression LocalValueExpression (int index)
        => LocalValueExpression (ValueNode (index));
    private static ESIR_LocalValueExpression LocalValueExpression (ESIR_ValueNode index) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.LocalValueExpression, index, out var hash);
        if (node is not null)
            return (ESIR_LocalValueExpression) node;

        var ret = new ESIR_LocalValueExpression (index);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_DefaultValueExpression DefaultValueExpression (ESIR_TypeNode type) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.DefaultValueExpression, type, out var hash);
        if (node is not null)
            return (ESIR_DefaultValueExpression) node;

        var ret = new ESIR_DefaultValueExpression (type);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_MemberAccessExpression MemberAccessExpression (ESIR_Expression parent, ArrayPointer<byte> name)
        => MemberAccessExpression (parent, ValueNode (name));
    private static ESIR_MemberAccessExpression MemberAccessExpression (ESIR_Expression parent, ESIR_ValueNode name) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.MemberAccessExpression, parent, name, out var hash);
        if (node is not null)
            return (ESIR_MemberAccessExpression) node;

        var ret = new ESIR_MemberAccessExpression (parent, name);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_FunctionCallExpression FunctionCallExpression (ArrayPointer<byte> name, ESIR_List<ESIR_ArgumentValue> arguments)
        => FunctionCallExpression (ValueNode (name), arguments);
    private static ESIR_FunctionCallExpression FunctionCallExpression (ESIR_ValueNode name, ESIR_List<ESIR_ArgumentValue> arguments) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.FunctionCallExpression, name, arguments, out var hash);
        if (node is not null)
            return (ESIR_FunctionCallExpression) node;

        var ret = new ESIR_FunctionCallExpression (name, arguments);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_IndexingExpression IndexingExpression (ESIR_Expression indexedExpr, ESIR_List<ESIR_Expression> ranks) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.IndexingExpression, indexedExpr, ranks, out var hash);
        if (node is not null)
            return (ESIR_IndexingExpression) node;

        var ret = new ESIR_IndexingExpression (indexedExpr, ranks);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_NewObjectExpression NewObjectExpression (ESIR_TypeNode type) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.NewObjectExpression, type, out var hash);
        if (node is not null)
            return (ESIR_NewObjectExpression) node;

        var ret = new ESIR_NewObjectExpression (type);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_NewArrayExpression NewArrayExpression (ESIR_TypeNode elemType, ESIR_List<ESIR_Expression> indices) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.NewArrayExpression, elemType, indices, out var hash);
        if (node is not null)
            return (ESIR_NewArrayExpression) node;

        var ret = new ESIR_NewArrayExpression (elemType, indices);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_CastExpression CastExpression (ESIR_Expression expression, ESIR_TypeNode destType) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.CastExpression, expression, destType, out var hash);
        if (node is not null)
            return (ESIR_CastExpression) node;

        var ret = new ESIR_CastExpression (expression, destType);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    public static ESIR_ConditionalExpression ConditionalExpression (ESIR_Expression condition, ESIR_Expression left, ESIR_Expression right) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ConditionalExpression, condition, left, right, out var hash);
        if (node is not null)
            return (ESIR_ConditionalExpression) node;

        var ret = new ESIR_ConditionalExpression (condition, left, right);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }

    private static ESIR_ExpressionList ExpressionList (ESIR_List<ESIR_Expression> exprs) {
        var node = ESIR_NodeCache.Shared.TryGetNode (ESIR_NodeKind.ExpressionList, exprs, out var hash);
        if (node is not null)
            return (ESIR_ExpressionList) node;

        var ret = new ESIR_ExpressionList (exprs);
        if (hash >= 0)
            ESIR_NodeCache.Shared.AddNode (ret, hash);

        return ret;
    }
}
