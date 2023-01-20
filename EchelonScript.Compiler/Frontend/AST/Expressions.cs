/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.CompilerCommon;

namespace EchelonScript.Compiler.Frontend.AST;

public enum SimpleBinaryExprType {
    Concatenation,

    Power,

    Multiply,
    Divide,
    Modulo,

    Add,
    Subtract,

    ShiftLeft,
    ShiftRight,
    ShiftRightUnsigned,

    LesserThan,
    GreaterThan,
    LesserThanEqual,
    GreaterThanEqual,

    Equals,
    NotEquals,

    BitAnd,
    BitXor,
    BitOr,

    LogicalAnd,
    LogicalOr,

    #region Assignment

    TAG_AssignExpr_Start,

    Assign,

    AssignAdd,
    AssignSubtract,

    AssignMultiply,
    AssignDivide,
    AssignModulo,
    AssignPower,

    AssignBitAnd,
    AssignBitOr,
    AssignXor,

    AssignConcatenate,

    AssignShiftLeft,
    AssignShiftRight,
    AssignShiftRightUnsigned,

    TAG_AssignExpr_End

    #endregion
}

public enum SimpleUnaryExprType {
    Positive,
    Negative,
    LogicalNot,
    BitNot,
    Dereference,
}

public static class ES_AstExtensions {
    public static bool IsAssignment (this SimpleBinaryExprType op)
        => op >= SimpleBinaryExprType.TAG_AssignExpr_Start && op <= SimpleBinaryExprType.TAG_AssignExpr_End;

    public static bool IsLogical (this SimpleBinaryExprType op)
        => op == SimpleBinaryExprType.LogicalAnd || op == SimpleBinaryExprType.LogicalOr;

    public static bool IsBitShift (this SimpleBinaryExprType op) {
        return op switch {
            SimpleBinaryExprType.ShiftLeft => true,
            SimpleBinaryExprType.ShiftRight => true,
            SimpleBinaryExprType.ShiftRightUnsigned => true,
            SimpleBinaryExprType.AssignShiftLeft => true,
            SimpleBinaryExprType.AssignShiftRight => true,
            SimpleBinaryExprType.AssignShiftRightUnsigned => true,

            _ => false,
        };
    }

    public static bool IsComparison (this SimpleBinaryExprType op) {
        return op switch {
            SimpleBinaryExprType.LesserThan => true,
            SimpleBinaryExprType.GreaterThan => true,
            SimpleBinaryExprType.LesserThanEqual => true,
            SimpleBinaryExprType.GreaterThanEqual => true,
            SimpleBinaryExprType.Equals => true,
            SimpleBinaryExprType.NotEquals => true,

            _ => false,
        };
    }
}

public abstract class ES_AstExpression : ES_AstNode {
    public ES_AstExpression (int nothing) : base (1) { }
}

public sealed class ES_AstEmptyErrorExpression : ES_AstExpression {
    private int errStartPos;
    private int errEndPos;
    public override ES_AstNodeBounds NodeBounds => new (errStartPos, errEndPos);

    public ES_AstEmptyErrorExpression (int pos) : base (1) => errStartPos = errEndPos = pos;

    public ES_AstEmptyErrorExpression (int startPos, int endPos) : base (1) {
        errStartPos = startPos;
        errEndPos = endPos;
    }
}

public abstract class ES_AstBinaryExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Left?.NodeBounds.StartPos ?? Right?.NodeBounds.StartPos ?? 0,
        EndPos = Right?.NodeBounds.EndPos ?? Left?.NodeBounds.EndPos ?? 0,
    };

    public ES_AstExpression Left;
    public ES_AstExpression Right;

    public ES_AstBinaryExpression (ES_AstExpression left, ES_AstExpression right) : base (1) {
        Left = left;
        Right = right;
    }
}

#region Primary expressions

public sealed class ES_AstParenthesisExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression Inner;

    public ES_AstParenthesisExpression (
        ES_AstExpression inner,
        EchelonScriptToken openTk, EchelonScriptToken closeTk
    ) : base (1) {
        Inner = inner;

        bounds = new (openTk.TextStartPos, closeTk.TextEndPos);
    }
}

public sealed class ES_AstIntegerLiteralExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Token.TextStartPos,
        EndPos = Token.TextEndPos,
    };

    public readonly bool? Signed;
    public readonly ES_IntSize? Size;
    public readonly bool HexBin;
    public readonly ulong Value;

    public readonly EchelonScriptToken Token;

    public ES_AstIntegerLiteralExpression (bool? isSigned, ES_IntSize? size, bool hexBin, ulong value, EchelonScriptToken tk) : base (1) {
        Signed = isSigned;
        Size = size;
        HexBin = hexBin;
        Value = value;

        Token = tk;
    }
}

public sealed class ES_AstBooleanLiteralExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Token.TextStartPos,
        EndPos = Token.TextEndPos,
    };

    public readonly bool Value;

    public readonly EchelonScriptToken Token;

    public ES_AstBooleanLiteralExpression (bool value, EchelonScriptToken tk) : base (1) {
        Value = value;

        Token = tk;
    }
}

public sealed class ES_AstFloatLiteralExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Token.TextStartPos,
        EndPos = Token.TextEndPos,
    };

    public readonly bool IsFloat;
    public readonly float ValueFloat;
    public readonly double ValueDouble;

    public readonly EchelonScriptToken Token;

    public ES_AstFloatLiteralExpression (float value, EchelonScriptToken tk) : base (1) {
        IsFloat = true;
        ValueFloat = value;

        Token = tk;
    }

    public ES_AstFloatLiteralExpression (double value, EchelonScriptToken tk) : base (1) {
        IsFloat = false;
        ValueDouble = value;

        Token = tk;
    }
}

public sealed class ES_AstStringLiteralExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Token.TextStartPos,
        EndPos = Token.TextEndPos,
    };

    public readonly bool Verbatim;
    //public readonly string ValueUTF16;
    //public readonly int [] ValueUTF32;

    public readonly EchelonScriptToken Token;

    public ES_AstStringLiteralExpression (bool verbatim, EchelonScriptToken tk) : base (1) {
        Verbatim = verbatim;
        /*ValueUTF16 = tk.DecodedStringUTF16;
        ValueUTF32 = tk.DecodedStringUTF32;*/

        Token = tk;
    }
}

public sealed class ES_AstCharLiteralExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Token.TextStartPos,
        EndPos = Token.TextEndPos,
    };

    public readonly int Value;
    public readonly EchelonScriptToken Token;

    public ES_AstCharLiteralExpression (int value, EchelonScriptToken tk) : base (1) {
        Value = value;

        Token = tk;
    }
}

public sealed class ES_AstNullLiteralExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Token.TextStartPos,
        EndPos = Token.TextEndPos,
    };

    public readonly EchelonScriptToken Token;

    public ES_AstNullLiteralExpression (EchelonScriptToken tk) : base (1) => Token = tk;
}

public sealed class ES_AstNameExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Value.TextStartPos,
        EndPos = Value.TextEndPos,
    };

    public readonly EchelonScriptToken Value;

    public ES_AstNameExpression (EchelonScriptToken value) : base (1) => Value = value;
}

public sealed class ES_AstFullyQualifiedNameExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Namespace.NodeBounds.StartPos,
        EndPos = Identifier.TextEndPos,
    };

    public readonly ES_AstNamespaceIdentifier Namespace;
    public readonly EchelonScriptToken Identifier;

    public ES_AstFullyQualifiedNameExpression (ES_AstNamespaceIdentifier ns, EchelonScriptToken id) : base (1) {
        Namespace = ns;
        Identifier = id;
    }
}

public sealed class ES_AstMemberAccessExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = Parent?.NodeBounds.StartPos ?? OpToken.TextStartPos,
        EndPos = Member?.TextEndPos ?? OpToken.TextEndPos,
    };

    public ES_AstExpression Parent;
    public EchelonScriptToken? Member;
    public EchelonScriptToken OpToken;

    public ES_AstMemberAccessExpression (ES_AstExpression parent, EchelonScriptToken? member, EchelonScriptToken opToken) : base (1) {
        Parent = parent;
        Member = member;

        OpToken = opToken;
    }
}

public sealed class ES_AstFunctionCallExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression FunctionExpression;
    public ES_AstFunctionCallArgument [] Arguments;

    public ES_AstFunctionCallExpression (
        ES_AstExpression innerExpr, ES_AstFunctionCallArgument [] args,
        EchelonScriptToken closeParenTk
    ) : base (1) {
        FunctionExpression = innerExpr;
        Arguments = args;

        bounds = new (innerExpr.NodeBounds.StartPos, closeParenTk.TextEndPos);
    }
}

public sealed class ES_AstFunctionCallArgument : ES_AstNode {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_ArgumentKind ArgType;
    public ES_AstExpression ValueExpression;

    public ES_AstFunctionCallArgument (
        ES_ArgumentKind type, ES_AstExpression valExpr,
        EchelonScriptToken startTk
    ) : base (1) {
        ArgType = type;
        ValueExpression = valExpr;

        bounds = new (startTk.TextStartPos, valExpr?.NodeBounds.EndPos ?? startTk.TextEndPos);
    }
}

public sealed class ES_AstIndexingExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression IndexedExpression;
    public ES_AstExpression? [] DimensionExpressions;

    public ES_AstIndexingExpression (
        ES_AstExpression indexedExpr, ES_AstExpression? [] dims, int endPos
    ) : base (1) {
        IndexedExpression = indexedExpr;
        DimensionExpressions = dims;

        bounds = new (indexedExpr.NodeBounds.StartPos, endPos);
    }
}

public sealed class ES_AstNewObjectExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstTypeDeclaration? TypeDeclaration;
    public ES_AstFunctionCallArgument [] Arguments;

    public ES_AstNewObjectExpression (
        ES_AstTypeDeclaration? typeDecl, ES_AstFunctionCallArgument [] args,
        EchelonScriptToken newStartTk, EchelonScriptToken closeParenTk
    ) : base (1) {
        TypeDeclaration = typeDecl;
        Arguments = args;

        bounds = new (newStartTk.TextStartPos, closeParenTk.TextEndPos);
    }
}

public sealed class ES_AstNewArrayExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstTypeDeclaration? ElementType;
    public ES_AstExpression? [] Dimensions;

    public ES_AstNewArrayExpression (
        ES_AstTypeDeclaration? typeDecl, ES_AstExpression? [] dims,
        EchelonScriptToken newStartTk, int endPos
    ) : base (1) {
        ElementType = typeDecl;
        Dimensions = dims;

        bounds = new (newStartTk.TextStartPos, endPos);
    }
}

#endregion

public sealed class ES_AstIncDecExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds {
        get {
            if (Postfix) {
                return new ES_AstNodeBounds {
                    StartPos = Inner?.NodeBounds.StartPos ?? OperatorToken.TextStartPos,
                    EndPos = OperatorToken.TextEndPos,
                };
            } else {
                return new ES_AstNodeBounds {
                    StartPos = OperatorToken.TextStartPos,
                    EndPos = Inner?.NodeBounds.EndPos ?? OperatorToken.TextEndPos,
                };
            }
        }
    }

    public readonly EchelonScriptToken OperatorToken;
    public readonly bool Decrement;
    public readonly bool Postfix;

    public ES_AstExpression Inner;

    public ES_AstIncDecExpression (
        EchelonScriptToken opToken,
        bool decrement, bool postfix, ES_AstExpression inner
    ) : base (1) {
        OperatorToken = opToken;
        Decrement = decrement;
        Postfix = postfix;

        Inner = inner;
    }
}

#region Unary expressions

public sealed class ES_AstSimpleUnaryExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = OperatorToken.TextStartPos,
        EndPos = Inner?.NodeBounds.EndPos ?? OperatorToken.TextEndPos,
    };

    public readonly EchelonScriptToken OperatorToken;
    public readonly SimpleUnaryExprType ExpressionType;
    public ES_AstExpression Inner;

    public ES_AstSimpleUnaryExpression (
        EchelonScriptToken opToken, SimpleUnaryExprType exprType,
        ES_AstExpression inner
    ) : base (1) {
        OperatorToken = opToken;
        ExpressionType = exprType;
        Inner = inner;
    }
}

public sealed class ES_AstCastExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => new () {
        StartPos = bounds.StartPos,
        EndPos = InnerExpression?.NodeBounds.EndPos ?? bounds.EndPos,
    };
    private ES_AstNodeBounds bounds;

    public ES_AstNodeBounds CastBounds => bounds;

    public ES_AstTypeDeclaration? DestinationType;
    public ES_AstExpression InnerExpression;

    public ES_AstCastExpression (
        ES_AstTypeDeclaration? destType, ES_AstExpression innerExpr,
        EchelonScriptToken castStartTk, EchelonScriptToken closeParenTk
    ) : base (1) {
        DestinationType = destType;
        InnerExpression = innerExpr;

        bounds = new (castStartTk.TextStartPos, closeParenTk.TextEndPos);
    }
}

#endregion

public sealed class ES_AstSimpleBinaryExpression : ES_AstBinaryExpression {
    public readonly SimpleBinaryExprType ExpressionType;
    public readonly EchelonScriptToken OperatorToken;

    public ES_AstSimpleBinaryExpression (
        SimpleBinaryExprType exprType, ES_AstExpression left, ES_AstExpression right,
        EchelonScriptToken opToken
    ) : base (left, right) {
        ExpressionType = exprType;
        OperatorToken = opToken;
    }
}

public sealed class ES_AstConditionalExpression : ES_AstExpression {
    public override ES_AstNodeBounds NodeBounds => bounds;
    private ES_AstNodeBounds bounds;

    public ES_AstExpression Condition;
    public ES_AstExpression Then;
    public ES_AstExpression Else;

    public ES_AstConditionalExpression (
        ES_AstExpression condExpr, ES_AstExpression thenExpr, ES_AstExpression elseExpr
    ) : base (1) {
        Condition = condExpr;
        Then = thenExpr;
        Else = elseExpr;

        bounds = new (Condition.NodeBounds.StartPos, Else.NodeBounds.EndPos);
    }
}
