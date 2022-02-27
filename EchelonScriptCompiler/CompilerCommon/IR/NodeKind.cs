/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScriptCompiler.CompilerCommon.IR;

public enum ESIR_NodeKind {
    None,

    ValueInt,
    ValueUInt,
    ValueFloat32,
    ValueFloat64,
    ValueIdentifier,
    ValueString,
    ValuePointer,

    List,

    TypeNode,

    StaticVariable,
    Struct,
    Class,
    Function,

    ArgumentDefinition,
    ArgumentValue,

    Field,
    StaticField,

    #region Expressions

    ErrorExpression,

    AssignExpression,

    #region Binary

    BinaryExprConcat,

    BinaryExprPower,

    BinaryExprMultiply,
    BinaryExprDivide,
    BinaryExprModulo,

    BinaryExprAdd,
    BinaryExprSubtract,

    BinaryExprShiftLeft,
    BinaryExprShiftRight,
    BinaryExprShiftRightUnsigned,

    BinaryExprLesserThan,
    BinaryExprGreaterThan,
    BinaryExprLesserThanEqual,
    BinaryExprGreaterThanEqual,

    BinaryExprEquals,
    BinaryExprNotEquals,

    BinaryExprBitAnd,
    BinaryExprBitOr,
    BinaryExprBitXor,

    BinaryExprLogicalAnd,
    BinaryExprLogicalOr,

    #endregion

    #region Unary

    UnaryNegative,
    UnaryLogicalNot,
    UnaryBitNot,
    UnaryDereference,

    UnaryPreIncrement,
    UnaryPreDecrement,
    UnaryPostIncrement,
    UnaryPostDecrement,

    #endregion

    #region Literals

    LiteralTrue,
    LiteralFalse,

    LiteralInt,
    LiteralFloat,
    LiteralChar,
    LiteralNull,

    #endregion

    StringConstant,
    StaticVariableExpression,
    ArgumentExpression,
    LocalValueExpression,
    DefaultValueExpression,

    MemberAccessExpression,

    FunctionCallExpression,
    VirtualCallExpression,
    IndexingExpression,
    NewObjectExpression,
    NewArrayExpression,

    CastExpression,

    ConditionalExpression,

    ExpressionList,

    #endregion

    #region Statements

    BlockStatement,
    LabeledStatement,

    ConditionalStatement,
    SwitchStatement,

    BreakStatement,
    ContinueStatement,

    GotoLabelStatement,
    GotoCaseStatement,

    ReturnStatement,

    LoopStatement,

    ExpressionStatement,

    #endregion

    #region Attributes

    TraceDataAttribute,
    FunctionDataAttribute,

    #endregion
}

public static partial class ESIR_Utils {
    public static bool IsBinaryComparison (this ESIR_NodeKind kind) {
        return kind switch {
            ESIR_NodeKind.BinaryExprLesserThan => true,
            ESIR_NodeKind.BinaryExprGreaterThan => true,
            ESIR_NodeKind.BinaryExprLesserThanEqual => true,
            ESIR_NodeKind.BinaryExprGreaterThanEqual => true,

            ESIR_NodeKind.BinaryExprEquals => true,
            ESIR_NodeKind.BinaryExprNotEquals => true,

            _ => false,
        };
    }
}
