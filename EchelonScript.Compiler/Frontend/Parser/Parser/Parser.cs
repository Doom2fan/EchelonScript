/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using ChronosLib.Pooled;
using EchelonScript.Common;
using EchelonScript.Common.Data.Types;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.AST;
using EchelonScript.Compiler.Frontend.Parser.Tokenizer;

namespace EchelonScript.Compiler.Frontend.Parser;

public struct ES_AggregateModifiers {
    public ES_AccessModifier? AccessModifier;
    public bool? Static;
    public bool? Const;
    public ES_VirtualnessModifier? VirtualnessModifier;

    public EchelonScriptToken? DocComment;

    public void ResetToNull () {
        AccessModifier = null;
        Static = null;
        Const = null;
        VirtualnessModifier = null;

        DocComment = null;
    }

    public void CopyDefaultsToUndefined (ES_AggregateModifiers defaults) {
        if (AccessModifier == null)
            AccessModifier = defaults.AccessModifier;

        if (Static == null)
            Static = defaults.Static;

        if (Const == null)
            Const = defaults.Const;

        if (VirtualnessModifier == null)
            VirtualnessModifier = defaults.VirtualnessModifier;
    }

    public bool AnyUndefined ()
        => !AccessModifier.HasValue || !Static.HasValue || !Const.HasValue || !VirtualnessModifier.HasValue;

    public bool AnySet ()
        => AccessModifier.HasValue || Static.HasValue || Const.HasValue || VirtualnessModifier.HasValue;
}

public sealed partial class EchelonScriptParser : IDisposable {
    #region ================== Static properties

    public static string [] PrimitiveTypes { get; private set; }
    public static string [] Keywords { get; private set; }

    private static Dictionary<EchelonScriptTokenType, List<IPrefixExpressionParselet>> PrefixExprParsers { get; set; }
    private static Dictionary<EchelonScriptTokenType, List<IPostfixExpressionParselet>> PostfixExprParsers { get; set; }

    #endregion

    #region ================== Instance fields

    private List<EchelonScriptErrorMessage> errorsList;
    private EchelonScriptTokenizer tokenizer;
    private ReadOnlyMemory<char> fileName;
    private ReadOnlyMemory<char> sourceText;

    #endregion

    #region ================== Instance properties

    public IReadOnlyList<EchelonScriptErrorMessage> Errors => errorsList;

    #endregion

    #region ================== Constructors

    #region Util functions

    static void AddPrefixParselet (EchelonScriptTokenType tokenType, IPrefixExpressionParselet parselet) {
        if (!PrefixExprParsers.ContainsKey (tokenType))
            PrefixExprParsers.Add (tokenType, new List<IPrefixExpressionParselet> ());

        PrefixExprParsers [tokenType].Add (parselet);
    }

    static void AddPostfixParselet (EchelonScriptTokenType tokenType, IPostfixExpressionParselet parselet) {
        if (!PostfixExprParsers.ContainsKey (tokenType))
            PostfixExprParsers.Add (tokenType, new List<IPostfixExpressionParselet> ());

        PostfixExprParsers [tokenType].Add (parselet);
    }

    static void AddSimpleBinaryExpr (ExpressionPrecedence precedence, EchelonScriptTokenType tokenType, SimpleBinaryExprType exprType) {
        Debug.Assert (!exprType.IsAssignment ());

        if (!PostfixExprParsers.ContainsKey (tokenType))
            PostfixExprParsers.Add (tokenType, new List<IPostfixExpressionParselet> ());

        PostfixExprParsers [tokenType].Add (new SimpleBinaryExpressionParselet (precedence, tokenType, exprType));
    }

    static void AddAssignExpr (ExpressionPrecedence precedence, EchelonScriptTokenType tokenType, SimpleBinaryExprType exprType) {
        Debug.Assert (exprType.IsAssignment ());

        if (!PostfixExprParsers.ContainsKey (tokenType))
            PostfixExprParsers.Add (tokenType, new List<IPostfixExpressionParselet> ());

        PostfixExprParsers [tokenType].Add (new AssignExpressionParselet (precedence, tokenType, exprType));
    }

    static void AddSimpleUnaryExpr (ExpressionPrecedence precedence, EchelonScriptTokenType tokenType, SimpleUnaryExprType exprType) {
        if (!PrefixExprParsers.ContainsKey (tokenType))
            PrefixExprParsers.Add (tokenType, new List<IPrefixExpressionParselet> ());

        PrefixExprParsers [tokenType].Add (new SimpleUnaryExpressionParselet (precedence, tokenType, exprType));
    }

    #endregion

    static EchelonScriptParser () {
        // Primitive types
        using var primitiveTypesList = new StructPooledList<string> (CL_ClearMode.Auto);
        var primitiveTypesConstants = typeof (ES_PrimitiveTypeConsts).GetFields (BindingFlags.Public | BindingFlags.Static);

        foreach (var primitiveConst in primitiveTypesConstants) {
            if (primitiveConst.IsLiteral && !primitiveConst.IsInitOnly && primitiveConst.FieldType == typeof (string))
                primitiveTypesList.Add ((primitiveConst.GetValue (null) as string)!);
        }

        PrimitiveTypes = primitiveTypesList.ToArray ();

        // Keywords
        using var keywordsList = new StructPooledList<string> (CL_ClearMode.Auto);
        var keywordsConstants = typeof (ES_Keywords).GetFields (BindingFlags.Public | BindingFlags.Static);

        foreach (var keywordConst in keywordsConstants) {
            if (keywordConst.IsLiteral && !keywordConst.IsInitOnly && keywordConst.FieldType == typeof (string))
                keywordsList.Add ((keywordConst.GetValue (null) as string)!);
        }

        Keywords = keywordsList.ToArray ();

        // Parselets
        PrefixExprParsers = new Dictionary<EchelonScriptTokenType, List<IPrefixExpressionParselet>> ();
        PostfixExprParsers = new Dictionary<EchelonScriptTokenType, List<IPostfixExpressionParselet>> ();

        var incDecParselet = new IncDecExpressionParselet ();

        #region Primary expressions

        AddPrefixParselet (EchelonScriptTokenType.ParenOpen, new ParenthesisExpressionParselet ());
        var literalExprParselet = new LiteralExpressionParselet ();
        {
            AddPrefixParselet (EchelonScriptTokenType.DecIntegerLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.HexIntegerLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.BinIntegerLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.FloatLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.RegularStringLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.VerbatimStringLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.CharacterLiteral, literalExprParselet);
            AddPrefixParselet (EchelonScriptTokenType.Identifier, literalExprParselet);
        }
        AddPrefixParselet (EchelonScriptTokenType.Identifier, new NewExpressionParselet ());
        AddPrefixParselet (EchelonScriptTokenType.Identifier, new NullExpressionParselet ());
        AddPrefixParselet (EchelonScriptTokenType.Identifier, new FullyQualifiedNameExpressionParselet ());
        AddPrefixParselet (EchelonScriptTokenType.Identifier, new NameExpressionParselet ());
        AddPostfixParselet (EchelonScriptTokenType.Dot, new MemberAccessExpressionParselet ());
        AddPostfixParselet (EchelonScriptTokenType.ParenOpen, new FunctionCallExpressionParselet ());
        AddPostfixParselet (EchelonScriptTokenType.BracketOpen, new IndexingExpressionParselet ());
        AddPostfixParselet (EchelonScriptTokenType.PlusPlus, incDecParselet);
        AddPostfixParselet (EchelonScriptTokenType.MinusMinus, incDecParselet);

        #endregion

        #region Unary expressions

        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Plus, SimpleUnaryExprType.Positive);
        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Minus, SimpleUnaryExprType.Negative);
        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Bang, SimpleUnaryExprType.LogicalNot);
        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Tilde, SimpleUnaryExprType.BitNot);
        AddSimpleUnaryExpr (ExpressionPrecedence.Unary, EchelonScriptTokenType.Asterisk, SimpleUnaryExprType.Dereference);
        AddPrefixParselet (EchelonScriptTokenType.PowerOp, new DoubleDereferenceExpressionParselet (ExpressionPrecedence.Unary));
        AddPrefixParselet (EchelonScriptTokenType.PlusPlus, incDecParselet);
        AddPrefixParselet (EchelonScriptTokenType.MinusMinus, incDecParselet);
        AddPrefixParselet (EchelonScriptTokenType.Identifier, new CastExpressionParselet ());

        #endregion

        #region Binary ops

        AddSimpleBinaryExpr (ExpressionPrecedence.Concatenation, EchelonScriptTokenType.DotDot, SimpleBinaryExprType.Concatenation);

        AddSimpleBinaryExpr (ExpressionPrecedence.Exponentiation, EchelonScriptTokenType.PowerOp, SimpleBinaryExprType.Power);

        AddSimpleBinaryExpr (ExpressionPrecedence.Multiplicative, EchelonScriptTokenType.Asterisk, SimpleBinaryExprType.Multiply);
        AddSimpleBinaryExpr (ExpressionPrecedence.Multiplicative, EchelonScriptTokenType.Divide, SimpleBinaryExprType.Divide);
        AddSimpleBinaryExpr (ExpressionPrecedence.Multiplicative, EchelonScriptTokenType.Modulo, SimpleBinaryExprType.Modulo);

        AddSimpleBinaryExpr (ExpressionPrecedence.Additive, EchelonScriptTokenType.Plus, SimpleBinaryExprType.Add);
        AddSimpleBinaryExpr (ExpressionPrecedence.Additive, EchelonScriptTokenType.Minus, SimpleBinaryExprType.Subtract);

        AddSimpleBinaryExpr (ExpressionPrecedence.Shift, EchelonScriptTokenType.ShiftLeft, SimpleBinaryExprType.ShiftLeft);
        AddSimpleBinaryExpr (ExpressionPrecedence.Shift, EchelonScriptTokenType.ShiftRight, SimpleBinaryExprType.ShiftRight);
        AddSimpleBinaryExpr (ExpressionPrecedence.Shift, EchelonScriptTokenType.ShiftRightU, SimpleBinaryExprType.ShiftRightUnsigned);

        AddSimpleBinaryExpr (ExpressionPrecedence.Comparison, EchelonScriptTokenType.LesserThan, SimpleBinaryExprType.LesserThan);
        AddSimpleBinaryExpr (ExpressionPrecedence.Comparison, EchelonScriptTokenType.GreaterThan, SimpleBinaryExprType.GreaterThan);
        AddSimpleBinaryExpr (ExpressionPrecedence.Comparison, EchelonScriptTokenType.LesserThanEq, SimpleBinaryExprType.LesserThanEqual);
        AddSimpleBinaryExpr (ExpressionPrecedence.Comparison, EchelonScriptTokenType.GreaterThanEq, SimpleBinaryExprType.GreaterThanEqual);

        AddSimpleBinaryExpr (ExpressionPrecedence.Equality, EchelonScriptTokenType.EqualsEquals, SimpleBinaryExprType.Equals);
        AddSimpleBinaryExpr (ExpressionPrecedence.Equality, EchelonScriptTokenType.NotEquals, SimpleBinaryExprType.NotEquals);

        AddSimpleBinaryExpr (ExpressionPrecedence.BitAnd, EchelonScriptTokenType.And, SimpleBinaryExprType.BitAnd);

        AddSimpleBinaryExpr (ExpressionPrecedence.BitXor, EchelonScriptTokenType.Xor, SimpleBinaryExprType.BitXor);

        AddSimpleBinaryExpr (ExpressionPrecedence.BitOr, EchelonScriptTokenType.BitOr, SimpleBinaryExprType.BitOr);

        AddSimpleBinaryExpr (ExpressionPrecedence.ConditionalAnd, EchelonScriptTokenType.AndAnd, SimpleBinaryExprType.LogicalAnd);

        AddSimpleBinaryExpr (ExpressionPrecedence.ConditionalOr, EchelonScriptTokenType.OrOr, SimpleBinaryExprType.LogicalOr);

        #region Assignments

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.Equals, SimpleBinaryExprType.Assign);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.PlusEq, SimpleBinaryExprType.AssignAdd);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.MinusEq, SimpleBinaryExprType.AssignSubtract);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.MultiplyEq, SimpleBinaryExprType.AssignMultiply);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.DivideEq, SimpleBinaryExprType.AssignDivide);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ModuloEq, SimpleBinaryExprType.AssignModulo);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.PowerOpEq, SimpleBinaryExprType.AssignPower);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.AndEq, SimpleBinaryExprType.AssignBitAnd);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.BitOrEq, SimpleBinaryExprType.AssignBitOr);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.XorEq, SimpleBinaryExprType.AssignXor);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ConcatEq, SimpleBinaryExprType.AssignConcatenate);

        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ShiftLeftEq, SimpleBinaryExprType.AssignShiftLeft);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ShiftRightEq, SimpleBinaryExprType.AssignShiftRight);
        AddAssignExpr (ExpressionPrecedence.Assignment, EchelonScriptTokenType.ShiftRightUEq, SimpleBinaryExprType.AssignShiftRightUnsigned);

        #endregion

        #endregion

        AddPostfixParselet (EchelonScriptTokenType.Question, new ConditionalExpressionParselet ());

        // Shrink the lists to fit
        foreach (var kvp in PrefixExprParsers)
            kvp.Value.Capacity = kvp.Value.Count;
        foreach (var kvp in PostfixExprParsers)
            kvp.Value.Capacity = kvp.Value.Count;
    }

    public EchelonScriptParser (List<EchelonScriptErrorMessage> errList) {
        errorsList = errList;
        tokenizer = new EchelonScriptTokenizer (errorsList);
    }

    #endregion

    #region ================== Instance methods

    private void CheckDisposed () {
        if (IsDisposed)
            throw new ObjectDisposedException (nameof (EchelonScriptParser));
    }

    public void Reset () {
        CheckDisposed ();

        tokenizer.Reset ();
        fileName = null;
        sourceText = null;
    }

    public ES_AbstractSyntaxTree ParseCode (ReadOnlyMemory<char> name, ReadOnlyMemory<char> codeData) {
        CheckDisposed ();

        Reset ();
        fileName = name;
        sourceText = codeData;
        tokenizer.SetSource (name, codeData);

        var astTree = ParseCodeUnit ();

        return astTree;
    }

    #endregion

    #region ================== IDisposable Support

    public bool IsDisposed { get; private set; }

    ~EchelonScriptParser () {
        if (!IsDisposed)
            DoDispose ();
    }

    private void DoDispose () {
        if (IsDisposed)
            return;

        tokenizer?.Dispose ();

        sourceText = null;

        IsDisposed = true;
    }

    public void Dispose () {
        GC.SuppressFinalize (this);
        DoDispose ();
    }

    #endregion
}
