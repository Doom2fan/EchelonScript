/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Text;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using ChronosLib.Pooled;
using EchelonScriptCommon;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCommon.Utilities;
using EchelonScriptCompiler;
using EchelonScriptCompiler.CompilerCommon.IR;
using EchelonScriptCompiler.Data;
using ICSharpCode.AvalonEdit.Document;

namespace TestSuiteWPF.Tests;

/// <summary>
/// Interaction logic for IRTest.xaml
/// </summary>
public partial class IRTest : UserControl {
    protected enum MessageType {
        Error,
        Warning,
        Message,
    }

    protected struct CompilerMessage {
        public string Type { get; }
        public string Message { get; }

        public int StartPos { get; }
        public int Length { get; }

        public string FileName { get; }
        public int Line { get; }
        public int Column { get; }

        public CompilerMessage (string type, EchelonScriptErrorMessage msg) {
            Type = type;
            Message = msg.Message;

            StartPos = msg.StartPos;
            Length = msg.Length;

            FileName = msg.FileName;
            Line = msg.Line;
            Column = msg.Column;
        }
    }

    #region ================== Instance fields

    protected ObservableCollection<CompilerMessage> errList;
    protected TextMarkerService textMarkerService;

    System.Globalization.CultureInfo cultureInfo;

    ES_IdentifierPool idPool;
    EchelonScript_Compiler compiler;
    EchelonScriptEnvironment env;

    #endregion

    #region ================== Constructors

    public IRTest () {
        InitializeComponent ();

        textMarkerService = new TextMarkerService (codeText);

        errList = new ObservableCollection<CompilerMessage> ();

        errorsList.ItemsSource = errList;

        cultureInfo = new System.Globalization.CultureInfo (System.Globalization.CultureInfo.CurrentCulture.Name);
        cultureInfo.NumberFormat.NumberDecimalSeparator = ".";
        cultureInfo.NumberFormat.NumberGroupSeparator = "'";

        idPool = new ();
        compiler = EchelonScript_Compiler.Create ();
    }

    #endregion

    #region ================== Instance methods

    private void DisplaySquiggly (int column, int lineNumber, int length, string message, Color col) {
        if (lineNumber >= 1 && lineNumber <= codeText.Document.LineCount) {
            var offset = codeText.Document.GetOffset (new TextLocation (lineNumber, column));
            var endOffset = offset + length;

            if (endOffset < 0)
                endOffset = codeText.Document.TextLength;

            if (length < 2)
                length = Math.Min (2, codeText.Document.TextLength - offset);

            textMarkerService.Create (offset, length, tm => {
                tm.MarkerColor = col;
                tm.ToolTip = message;
            });
        }
    }

    private static TreeViewItem AddNodeToTree (string nodeText, TreeViewItem parentItem) {
        var thisItem = new TreeViewItem ();
        parentItem.Items.Add (thisItem);
        thisItem.Header = nodeText;

        return thisItem;
    }

    private unsafe void AddTypeToTree (ES_TypeInfo* typeData, TreeViewItem parentItem) {
        string typeType = null;

        typeType = typeData->TypeTag switch {
            ES_TypeTag.Void => "Void",
            ES_TypeTag.Bool => "Bool",
            ES_TypeTag.Int => "Int",
            ES_TypeTag.Float => "Float",
            ES_TypeTag.Enum => "Enum",

            ES_TypeTag.Struct => "Struct",
            ES_TypeTag.Class => "Class",
            ES_TypeTag.Interface => "Interface",

            ES_TypeTag.Function => "Prototype",

            ES_TypeTag.Reference => "Reference",
            ES_TypeTag.Array => "Array",

            ES_TypeTag.Const => "Const",
            ES_TypeTag.Immutable => "Immutable",

            _ => "[UNRECOGNIZED]",
        };
        var typeNode = AddNodeToTree ($"{typeType} {GetTypeName (typeData, false)}", parentItem);

        AddNodeToTree ($"Runtime size: {typeData->RuntimeSize}", typeNode);
        AddNodeToTree ($"Fully qualified name: {GetTypeName (typeData, true)}", typeNode);
        AddNodeToTree ($"Source unit: {typeData->SourceUnitString}", typeNode);

        if (typeData->TypeTag == ES_TypeTag.Function) {
            var funcData = (ES_FunctionPrototypeData*) typeData;

            AddNodeToTree ($"Return type: {GetTypeName (funcData->ReturnType, true)}", typeNode);

            var argsListNode = AddNodeToTree ($"Arguments list", typeNode);
            foreach (var arg in funcData->ArgumentsList.Span) {
                var argType = arg.ArgType.ToString ();
                string argTypeName;

                if (arg.ValueType != null)
                    argTypeName = GetTypeName (arg.ValueType, true);
                else
                    argTypeName = "[NULL]";

                AddNodeToTree ($"{argType} {argTypeName}", argsListNode);
            }
        }
    }

    private unsafe void AddFunctionToTree (ES_FunctionData* functionData, TreeViewItem parentItem) {
        var typeNode = AddNodeToTree ($"Function {functionData->Name.TypeName.GetCharsSpan ()}", parentItem);

        AddNodeToTree ($"Fully qualified name: {functionData->Name.GetNameAsTypeString ()}", typeNode);
        AddNodeToTree ($"Source unit: {functionData->SourceUnitString}", typeNode);

        var protoData = functionData->FunctionType;
        AddNodeToTree ($"Return type: {GetTypeName (protoData->ReturnType, true)}", typeNode);

        var argsListNode = AddNodeToTree ($"Arguments list", typeNode);
        var reqArgsCount = protoData->ArgumentsList.Length - functionData->OptionalArgsCount;
        for (var i = 0; i < protoData->ArgumentsList.Length; i++) {
            var argProto = protoData->ArgumentsList.Span [i];
            var argData = functionData->Arguments.Span [i];

            var argType = string.Empty;
            var argTypeName = "[NULL]";
            var argName = argData.Name.GetCharsSpan ();
            var argDef = string.Empty;

            if (argProto.ArgType != ES_ArgumentType.Normal)
                argType = $"{argProto.ArgType} ";

            if (argProto.ValueType != null)
                argTypeName = GetTypeName (argProto.ValueType, true);

            if (i >= reqArgsCount)
                argDef = " = [...]";

            AddNodeToTree ($"{argType}{argName} {argTypeName}{argDef}", argsListNode);
        }
    }

    private unsafe string GetTypeName (ESIR_TypeNode type, bool fullyQualified)
        => type is not null ? GetTypeName (type.Pointer, fullyQualified) : "[TYPE NODE NULL]";
    private unsafe string GetTypeName (ES_TypeInfo* type, bool fullyQualified)
        => env!.GetNiceTypeNameString (type, fullyQualified);
    private static unsafe string GetString (ES_Identifier id) => id.GetCharsSpan ().GetPooledString ();

    private unsafe string GetArgString (int i, ESIR_ArgumentDefinition arg) {
        if (arg.ArgType == ES_ArgumentType.Normal)
            return $"{i} : {GetTypeName (arg.ValueType, true)}";

        return $"{i} : {arg.ArgType} {GetTypeName (arg.ValueType, true)}";
    }
    private unsafe string GetLocalValueString (int i, ESIR_TypeNode valType) => $"{i} : {GetTypeName (valType, true)}";

    private string GetExprLabel (ESIR_Expression node) {
        Debug.Assert (node is not null);
        if (node is null)
            return "[EXPR NODE NULL]";

        switch (node.Kind) {
            case ESIR_NodeKind.ErrorExpression: return "[ERROR]";

            case ESIR_NodeKind.AssignExpression: return "[Assign]";

            #region Binary

            case ESIR_NodeKind.BinaryExprConcat: return "[Concat]";

            case ESIR_NodeKind.BinaryExprPower: return "[Exponentiate]";

            case ESIR_NodeKind.BinaryExprMultiply: return "[Multiply]";
            case ESIR_NodeKind.BinaryExprDivide: return "[Divide]";
            case ESIR_NodeKind.BinaryExprModulo: return "[Modulo]";

            case ESIR_NodeKind.BinaryExprAdd: return "[Add]";
            case ESIR_NodeKind.BinaryExprSubtract: return "[Subtract]";

            case ESIR_NodeKind.BinaryExprShiftLeft: return "[Shift left]";
            case ESIR_NodeKind.BinaryExprShiftRight: return "[Shift right]";
            case ESIR_NodeKind.BinaryExprShiftRightUnsigned: return "[Unsigned shift right]";

            case ESIR_NodeKind.BinaryExprLesserThan: return "[Lesser than]";
            case ESIR_NodeKind.BinaryExprGreaterThan: return "[Greater than]";
            case ESIR_NodeKind.BinaryExprLesserThanEqual: return "[Lesser than-equals]";
            case ESIR_NodeKind.BinaryExprGreaterThanEqual: return "[Greater than-equals]";

            case ESIR_NodeKind.BinaryExprEquals: return "[Equals]";
            case ESIR_NodeKind.BinaryExprNotEquals: return "[Not equal]";

            case ESIR_NodeKind.BinaryExprBitAnd: return "[AND]";
            case ESIR_NodeKind.BinaryExprBitOr: return "[OR]";
            case ESIR_NodeKind.BinaryExprBitXor: return "[XOR]";

            case ESIR_NodeKind.BinaryExprLogicalAnd: return "[Logical AND]";
            case ESIR_NodeKind.BinaryExprLogicalOr: return "[Logical OR]";

            #endregion

            #region Unary

            case ESIR_NodeKind.UnaryNegative: return "[Negative]";
            case ESIR_NodeKind.UnaryLogicalNot: return "[Logical negation]";
            case ESIR_NodeKind.UnaryBitNot: return "[NOT]";
            case ESIR_NodeKind.UnaryDereference: return "[Dereference]";

            case ESIR_NodeKind.UnaryPreIncrement: return $"[Pre-increment]";
            case ESIR_NodeKind.UnaryPreDecrement: return $"[Pre-decrement]";
            case ESIR_NodeKind.UnaryPostIncrement: return $"[Post-increment]";
            case ESIR_NodeKind.UnaryPostDecrement: return $"[Post-decrement]";

            #endregion

            #region Literals

            case ESIR_NodeKind.LiteralTrue:
            case ESIR_NodeKind.LiteralFalse:
            case ESIR_NodeKind.LiteralInt:
            case ESIR_NodeKind.LiteralFloat:
            case ESIR_NodeKind.LiteralChar:
            case ESIR_NodeKind.LiteralNull:
                return GetNiceExpr (node);

            #endregion

            #region Values

            case ESIR_NodeKind.StringConstant:
            case ESIR_NodeKind.StaticVariableExpression:
            case ESIR_NodeKind.ArgumentExpression:
            case ESIR_NodeKind.LocalValueExpression:
                return GetNiceExpr (node);

            case ESIR_NodeKind.DefaultValueExpression: return "[default]";

            #endregion

            case ESIR_NodeKind.MemberAccessExpression: return "[Member access]";

            case ESIR_NodeKind.FunctionCallExpression: return "[Function call]";

            case ESIR_NodeKind.VirtualCallExpression: return "[Virtual call]";

            case ESIR_NodeKind.IndexingExpression: return "[Indexing]";

            case ESIR_NodeKind.NewObjectExpression: return "[New obj]";

            case ESIR_NodeKind.NewArrayExpression: return "[New array]";

            case ESIR_NodeKind.CastExpression: return "[Cast]";

            case ESIR_NodeKind.ConditionalExpression: return "[Conditional]";

            case ESIR_NodeKind.ExpressionList: return "[ExpressionList]";

            default:
                Debug.Fail ("IR node invalid or not implemented.");
                return "[invalid]";
        }
    }

    private string GetNiceExpr (ESIR_Expression node) {
        Debug.Assert (node is not null);
        if (node is null)
            return "[EXPR NODE NULL]";

        switch (node.Kind) {
            case ESIR_NodeKind.ErrorExpression: return "[ERROR]";

            case ESIR_NodeKind.AssignExpression when node is ESIR_AssignExpression assignExpr:
                return $"{GetExprLabel (assignExpr.Assignee)} = {GetExprLabel (assignExpr.Value)}";

            #region Binary

            case ESIR_NodeKind.BinaryExprConcat when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} .. {GetExprLabel (binaryExpr.ExprRight)}";

            case ESIR_NodeKind.BinaryExprPower when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} ** {GetExprLabel (binaryExpr.ExprRight)}";

            case ESIR_NodeKind.BinaryExprMultiply when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} * {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprDivide when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} / {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprModulo when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} % {GetExprLabel (binaryExpr.ExprRight)}";

            case ESIR_NodeKind.BinaryExprAdd when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} + {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprSubtract when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} - {GetExprLabel (binaryExpr.ExprRight)}";

            case ESIR_NodeKind.BinaryExprShiftLeft when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} << {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprShiftRight when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} >> {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprShiftRightUnsigned when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} >>> {GetExprLabel (binaryExpr.ExprRight)}";

            case ESIR_NodeKind.BinaryExprLesserThan when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} < {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprGreaterThan when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} > {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprLesserThanEqual when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} <= {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprGreaterThanEqual when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} >= {GetExprLabel (binaryExpr.ExprRight)}";

            case ESIR_NodeKind.BinaryExprEquals when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} == {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprNotEquals when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} != {GetExprLabel (binaryExpr.ExprRight)}";

            case ESIR_NodeKind.BinaryExprBitAnd when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} & {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprBitOr when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} | {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprBitXor when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} ^ {GetExprLabel (binaryExpr.ExprRight)}";

            case ESIR_NodeKind.BinaryExprLogicalAnd when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} && {GetExprLabel (binaryExpr.ExprRight)}";
            case ESIR_NodeKind.BinaryExprLogicalOr when node is ESIR_SimpleBinaryExpression binaryExpr:
                return $"{GetExprLabel (binaryExpr.ExprLeft)} || {GetExprLabel (binaryExpr.ExprRight)}";

            #endregion

            #region Unary

            case ESIR_NodeKind.UnaryNegative when node is ESIR_UnaryExpression unaryExpr:
                return $"-{GetExprLabel (unaryExpr.ExprInner)}";
            case ESIR_NodeKind.UnaryLogicalNot when node is ESIR_UnaryExpression unaryExpr:
                return $"!{GetExprLabel (unaryExpr.ExprInner)}";
            case ESIR_NodeKind.UnaryBitNot when node is ESIR_UnaryExpression unaryExpr:
                return $"~{GetExprLabel (unaryExpr.ExprInner)}";
            case ESIR_NodeKind.UnaryDereference when node is ESIR_UnaryExpression unaryExpr:
                return $"*{GetExprLabel (unaryExpr.ExprInner)}";

            case ESIR_NodeKind.UnaryPreIncrement when node is ESIR_UnaryExpression unaryExpr:
                return $"++{GetExprLabel (unaryExpr.ExprInner)}";
            case ESIR_NodeKind.UnaryPreDecrement when node is ESIR_UnaryExpression unaryExpr:
                return $"--{GetExprLabel (unaryExpr.ExprInner)}";
            case ESIR_NodeKind.UnaryPostIncrement when node is ESIR_UnaryExpression unaryExpr:
                return $"{GetExprLabel (unaryExpr.ExprInner)}++";
            case ESIR_NodeKind.UnaryPostDecrement when node is ESIR_UnaryExpression unaryExpr:
                return $"{GetExprLabel (unaryExpr.ExprInner)}--";

            #endregion

            #region Literals

            case ESIR_NodeKind.LiteralTrue: return "True";
            case ESIR_NodeKind.LiteralFalse: return "False";

            case ESIR_NodeKind.LiteralInt when node is ESIR_LiteralExpression litExpr:
                return litExpr.Value.Kind == ESIR_NodeKind.ValueUInt
                    ? $"{litExpr.Value.GetUInt ()}u"
                    : $"{litExpr.Value.GetInt  ()}i";
            case ESIR_NodeKind.LiteralFloat when node is ESIR_LiteralExpression litExpr:
                return litExpr.Value.Kind == ESIR_NodeKind.ValueFloat32
                    ? $"{litExpr.Value.GetFloat32 ()}f32"
                    : $"{litExpr.Value.GetFloat64 ()}f64";
            case ESIR_NodeKind.LiteralChar when node is ESIR_LiteralExpression litExpr:
                return $"'{(char) litExpr.Value.GetInt ()}'";
            case ESIR_NodeKind.LiteralNull when node is ESIR_NullLiteralExpression:
                return "[null]";

            #endregion

            #region Values

            case ESIR_NodeKind.StringConstant when node is ESIR_StringConstantExpression stringConstExpr:
                return $"str{stringConstExpr.Index}";

            case ESIR_NodeKind.StaticVariableExpression when node is ESIR_StaticVariableExpression staticVarExpr:
                return $"[SVar {GetString (staticVarExpr.Name)}]";

            case ESIR_NodeKind.ArgumentExpression when node is ESIR_ArgumentExpression argExpr:
                return $"arg{argExpr.Index}";

            case ESIR_NodeKind.LocalValueExpression when node is ESIR_LocalValueExpression localExpr:
                return $"local{localExpr.Index}";

            case ESIR_NodeKind.DefaultValueExpression when node is ESIR_DefaultValueExpression:
                return "[default]";

            #endregion

            case ESIR_NodeKind.MemberAccessExpression when node is ESIR_MemberAccessExpression accessExpr:
                return $"{GetExprLabel (accessExpr.ExprParent)}.{GetString (accessExpr.Name)}";

            case ESIR_NodeKind.FunctionCallExpression when node is ESIR_FunctionCallExpression callExpr:
                return $"{GetString (callExpr.Name)}{(callExpr.Arguments.Elements.Length > 0 ? " ([...])" : "")}";

            case ESIR_NodeKind.IndexingExpression when node is ESIR_IndexingExpression indexExpr:
                return $"{GetExprLabel (indexExpr.IndexedExpr)} [{new string (',', indexExpr.Indices.Elements.Length - 1)}]";

            case ESIR_NodeKind.NewObjectExpression when node is ESIR_NewObjectExpression newObjExpr:
                return $"new ({GetTypeName (newObjExpr.Type, true)})";

            case ESIR_NodeKind.NewArrayExpression when node is ESIR_NewArrayExpression newArrExpr:
                return $"array ({GetTypeName (newArrExpr.ElementType, true)} [{new string (',', newArrExpr.Ranks.Elements.Length - 1)}])";

            case ESIR_NodeKind.CastExpression when node is ESIR_CastExpression castExpr:
                return $"cast ({GetExprLabel (castExpr.Expression)} -> {GetTypeName (castExpr.DestType, true)})";

            case ESIR_NodeKind.ConditionalExpression when node is ESIR_ConditionalExpression condExpr:
                return $"{GetExprLabel (condExpr.Condition)} ? {GetExprLabel (condExpr.ThenExpression)} : {GetExprLabel (condExpr.ElseExpression)}";

            case ESIR_NodeKind.ExpressionList when node is ESIR_ExpressionList exprList:
                if (exprList.Expressions.Elements.Length == 1)
                    return GetNiceExpr (exprList.Expressions.Elements [0]);
                else
                    return "[ExpressionList]";

            default:
                Debug.Fail ("IR node invalid or not implemented.");
                return "[invalid]";
        }
    }

    private void AddBinaryExprToTree (string name, string op, ESIR_SimpleBinaryExpression expr, TreeViewItem parentItem) {
        var concatItem = AddNodeToTree ($"{name}: {GetNiceExpr (expr.ExprLeft)} {op} {GetNiceExpr (expr.ExprRight)}", parentItem);
        AddIRNodeToTree (expr.ExprLeft, concatItem);
        AddIRNodeToTree (expr.ExprRight, concatItem);
    }

    private void AddUnaryExprToTree (string name, string op, ESIR_UnaryExpression expr, TreeViewItem parentItem, bool postfix = false) {
        var niceInner = GetNiceExpr (expr.ExprInner);
        var concatItem = AddNodeToTree ($"{name}: {(!postfix ? op : niceInner)}{(!postfix ? niceInner : op)}", parentItem);
        AddIRNodeToTree (expr.ExprInner, concatItem);
    }

    private void AddExprStmtToTree (ESIR_Expression expr, TreeViewItem parentItem) {
        var exprItem = AddNodeToTree ($"Expression {GetNiceExpr (expr)}", parentItem);
        AddIRNodeToTree (expr, exprItem);
    }

    private unsafe void AddIRNodeToTree (ESIR_Node node, TreeViewItem parentItem) {
        Debug.Assert (node is not null);
        if (node is null)
            return;

        switch (node.Kind) {
            case ESIR_NodeKind.ValueInt:
            case ESIR_NodeKind.ValueUInt:
            case ESIR_NodeKind.ValueFloat32:
            case ESIR_NodeKind.ValueFloat64:
            case ESIR_NodeKind.ValueIdentifier:
            case ESIR_NodeKind.ValueString:
            case ESIR_NodeKind.ValuePointer:
            case ESIR_NodeKind.List:
            case ESIR_NodeKind.TypeNode:
                Debug.Fail ("These nodes shouldn't be added directly.");
                break;

            case ESIR_NodeKind.StaticVariable when node is ESIR_StaticVariable staticVar:
                AddNodeToTree ($"{staticVar.Name.GetCharsSpan ()} : {GetTypeName (staticVar.Type, true)}", parentItem);
                break;


            case ESIR_NodeKind.Struct when node is ESIR_Struct structDef: {
                var structItem = AddNodeToTree ($"{GetTypeName (structDef.Type, true)}", parentItem);
                AddNodeToTree ($"Size: {structDef.Type->RuntimeSize}", structItem);

                foreach (var member in structDef.Members.Elements)
                    AddIRNodeToTree (member, structItem);

                break;
            }

            case ESIR_NodeKind.Function when node is ESIR_Function funcDef: {
                var structItem = AddNodeToTree ($"{GetString (funcDef.Name)}", parentItem);
                AddNodeToTree ($"Return type: {GetTypeName (funcDef.ReturnType, true)}", structItem);

                var argsItem = AddNodeToTree ("Arguments", structItem);
                var localsItem = AddNodeToTree ("Locals", structItem);
                var bodyItem = AddNodeToTree ("Function body", structItem);

                var argsCount = 0;
                foreach (var arg in funcDef.Arguments.Elements)
                    AddNodeToTree (GetArgString (argsCount++, arg), argsItem);

                var localsCount = 0;
                foreach (var localVal in funcDef.LocalValues.Elements)
                    AddNodeToTree (GetLocalValueString (localsCount++, localVal), localsItem);

                foreach (var stmt in funcDef.Statements.Elements)
                    AddIRNodeToTree (stmt, bodyItem);

                break;
            }

            case ESIR_NodeKind.ArgumentValue when node is ESIR_ArgumentValue argVal:
                if (argVal.ArgType == ES_ArgumentType.Normal)
                    AddNodeToTree ($"{GetNiceExpr (argVal.Expression)}", parentItem);
                else
                    AddNodeToTree ($"{argVal.ArgType} {GetNiceExpr (argVal.Expression)}", parentItem);
                break;

            case ESIR_NodeKind.Class:
                Debug.Fail ("No IR node yet.");
                break;

            case ESIR_NodeKind.Field when node is ESIR_Field field: {
                var fieldItem = AddNodeToTree ($"{GetString (field.Name)} : {GetTypeName (field.Type, true)}", parentItem);
                AddNodeToTree ($"Offset: {field.Offset}", fieldItem);
                break;
            }

            case ESIR_NodeKind.StaticField when node is ESIR_StaticField staticField:
                AddNodeToTree ($"{GetString (staticField.Name)} : {GetString (staticField.StaticVariable)}", parentItem);
                break;

            #region Expressions

            case ESIR_NodeKind.ErrorExpression:
                AddNodeToTree ("[ERROR]", parentItem);
                break;

            case ESIR_NodeKind.AssignExpression when node is ESIR_AssignExpression assignExpr: {
                var assignItem = AddNodeToTree ($"Assign: {GetNiceExpr (assignExpr.Assignee)} = {GetNiceExpr (assignExpr.Value)}", parentItem);
                AddIRNodeToTree (assignExpr.Assignee, assignItem);
                AddIRNodeToTree (assignExpr.Value, assignItem);
                break;
            }

            #region Binary

            case ESIR_NodeKind.BinaryExprConcat when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Concatenate", "..", binaryExpr, parentItem);
                break;
            }

            case ESIR_NodeKind.BinaryExprPower when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Exponentiate", "**", binaryExpr, parentItem);
                break;
            }

            case ESIR_NodeKind.BinaryExprMultiply when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Multiply", "*", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprDivide when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Divide", "/", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprModulo when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Modulo", "%", binaryExpr, parentItem);
                break;
            }

            case ESIR_NodeKind.BinaryExprAdd when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Add", "+", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprSubtract when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Subtract", "-", binaryExpr, parentItem);
                break;
            }

            case ESIR_NodeKind.BinaryExprShiftLeft when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Left shift", "<<", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprShiftRight when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Right shift", ">>", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprShiftRightUnsigned when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Unsigned right shift", ">>>", binaryExpr, parentItem);
                break;
            }

            case ESIR_NodeKind.BinaryExprLesserThan when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Lesser than", "<", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprGreaterThan when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Greater than", ">", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprLesserThanEqual when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Lesser than-equals", "<=", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprGreaterThanEqual when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Greater than-equals", ">=", binaryExpr, parentItem);
                break;
            }

            case ESIR_NodeKind.BinaryExprEquals when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Equals", "==", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprNotEquals when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Not equal", "!=", binaryExpr, parentItem);
                break;
            }

            case ESIR_NodeKind.BinaryExprBitAnd when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("AND", "&", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprBitOr when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("OR", "|", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprBitXor when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("XOR", "^", binaryExpr, parentItem);
                break;
            }

            case ESIR_NodeKind.BinaryExprLogicalAnd when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Logical and", "&&", binaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.BinaryExprLogicalOr when node is ESIR_SimpleBinaryExpression binaryExpr: {
                AddBinaryExprToTree ("Logical or", "||", binaryExpr, parentItem);
                break;
            }

            #endregion

            #region Unary

            case ESIR_NodeKind.UnaryNegative when node is ESIR_UnaryExpression unaryExpr: {
                AddUnaryExprToTree ("Negative", "-", unaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.UnaryLogicalNot when node is ESIR_UnaryExpression unaryExpr: {
                AddUnaryExprToTree ("Logical negation", "!", unaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.UnaryBitNot when node is ESIR_UnaryExpression unaryExpr: {
                AddUnaryExprToTree ("NOT", "~", unaryExpr, parentItem);
                break;
            }
            case ESIR_NodeKind.UnaryDereference when node is ESIR_UnaryExpression unaryExpr: {
                AddUnaryExprToTree ("Dereference", "*", unaryExpr, parentItem);
                break;
            }

            case ESIR_NodeKind.UnaryPreIncrement when node is ESIR_UnaryExpression unaryExpr: {
                AddUnaryExprToTree ("Pre-increment", "++", unaryExpr, parentItem, false);
                break;
            }
            case ESIR_NodeKind.UnaryPreDecrement when node is ESIR_UnaryExpression unaryExpr: {
                AddUnaryExprToTree ("Pre-decrement", "--", unaryExpr, parentItem, false);
                break;
            }
            case ESIR_NodeKind.UnaryPostIncrement when node is ESIR_UnaryExpression unaryExpr: {
                AddUnaryExprToTree ("Post-increment", "++", unaryExpr, parentItem, true);
                break;
            }
            case ESIR_NodeKind.UnaryPostDecrement when node is ESIR_UnaryExpression unaryExpr: {
                AddUnaryExprToTree ("Post-decrement", "--", unaryExpr, parentItem, true);
                break;
            }

            #endregion

            #region Literals

            case ESIR_NodeKind.LiteralTrue:
                AddNodeToTree ("True", parentItem);
                break;
            case ESIR_NodeKind.LiteralFalse:
                AddNodeToTree ("False", parentItem);
                break;

            case ESIR_NodeKind.LiteralInt when node is ESIR_LiteralExpression litExpr:
                AddNodeToTree ($"Int {litExpr.Value.GetInt ()}", parentItem);
                break;
            case ESIR_NodeKind.LiteralFloat when node is ESIR_LiteralExpression litExpr: {
                if (litExpr.Value.Kind == ESIR_NodeKind.ValueFloat32)
                    AddNodeToTree ($"Float {litExpr.Value.GetFloat32 ()}", parentItem);
                else
                    AddNodeToTree ($"Float {litExpr.Value.GetFloat64 ()}", parentItem);
                break;
            }
            case ESIR_NodeKind.LiteralChar when node is ESIR_LiteralExpression litExpr:
                AddNodeToTree ($"Char {(char) litExpr.Value.GetInt ()}", parentItem);
                break;
            case ESIR_NodeKind.LiteralNull when node is ESIR_NullLiteralExpression nullExpr:
                AddNodeToTree ($"Null {GetTypeName (nullExpr.Type, true)}", parentItem);
                break;

            #endregion

            #region Values

            case ESIR_NodeKind.StringConstant when node is ESIR_StringConstantExpression stringConstExpr: {
                AddNodeToTree ($"String constant #{stringConstExpr.Index}", parentItem);
                break;
            }

            case ESIR_NodeKind.StaticVariableExpression when node is ESIR_StaticVariableExpression staticVarExpr: {
                AddNodeToTree ($"Static var {GetString (staticVarExpr.Name)}", parentItem);
                break;
            }

            case ESIR_NodeKind.ArgumentExpression when node is ESIR_ArgumentExpression argExpr: {
                AddNodeToTree ($"Arg #{argExpr.Index}", parentItem);
                break;
            }

            case ESIR_NodeKind.LocalValueExpression when node is ESIR_LocalValueExpression localExpr: {
                AddNodeToTree ($"Local #{localExpr.Index}", parentItem);
                break;
            }

            case ESIR_NodeKind.DefaultValueExpression when node is ESIR_DefaultValueExpression defValExpr: {
                AddNodeToTree ($"Defaults {GetTypeName (defValExpr.Type, true)}", parentItem);
                break;
            }

            #endregion

            case ESIR_NodeKind.MemberAccessExpression when node is ESIR_MemberAccessExpression accessExpr: {
                var nameStr = GetString (accessExpr.Name);
                var accessItem = AddNodeToTree ($"Member access: {GetNiceExpr (accessExpr.ExprParent)}.{nameStr}", parentItem);
                AddIRNodeToTree (accessExpr.ExprParent, accessItem);
                AddNodeToTree (nameStr, accessItem);

                break;
            }

            case ESIR_NodeKind.FunctionCallExpression when node is ESIR_FunctionCallExpression callExpr: {
                var funcName = GetString (callExpr.Name);
                var callItem = AddNodeToTree ($"Function call {funcName}", parentItem);
                foreach (var arg in callExpr.Arguments.Elements)
                    AddIRNodeToTree (arg, callItem);

                break;
            }

            case ESIR_NodeKind.IndexingExpression when node is ESIR_IndexingExpression indexExpr: {
                var sb = new StringBuilder ();
                sb.Append ("Index ");
                sb.Append (GetNiceExpr (indexExpr.IndexedExpr));

                sb.Append (" [");
                var firstDim = false;
                foreach (var index in indexExpr.Indices.Elements) {
                    if (firstDim)
                        firstDim = false;
                    else
                        sb.Append (", ");
                    sb.Append (GetNiceExpr (index));
                }
                sb.Append (']');

                var indexItem = AddNodeToTree (sb.ToString (), parentItem);
                AddIRNodeToTree (indexExpr.IndexedExpr, indexItem);
                var dimsItem = AddNodeToTree ("Dimensions", indexItem);

                foreach (var index in indexExpr.Indices.Elements)
                    AddIRNodeToTree (index, dimsItem);

                break;
            }

            case ESIR_NodeKind.NewObjectExpression when node is ESIR_NewObjectExpression newObjExpr: {
                AddNodeToTree ($"New {GetTypeName (newObjExpr.Type, true)}", parentItem);
                break;
            }

            case ESIR_NodeKind.NewArrayExpression when node is ESIR_NewArrayExpression newArrExpr: {
                var dimsCount = newArrExpr.Ranks.Elements.Length;
                var newItem = AddNodeToTree ($"New {GetTypeName (newArrExpr.ElementType, true)} [{new string (',', dimsCount - 1)}]", parentItem);

                foreach (var dim in newArrExpr.Ranks.Elements)
                    AddIRNodeToTree (dim, newItem);

                break;
            }

            case ESIR_NodeKind.CastExpression when node is ESIR_CastExpression castExpr: {
                var castItem = AddNodeToTree ($"Cast ({GetTypeName (castExpr.DestType, true)})", parentItem);
                AddIRNodeToTree (castExpr.Expression, castItem);

                break;
            }

            case ESIR_NodeKind.ConditionalExpression when node is ESIR_ConditionalExpression condExpr: {
                var niceCond = GetNiceExpr (condExpr.Condition);
                var niceTrueExpr = GetNiceExpr (condExpr.ThenExpression);
                var niceFalseExpr = GetNiceExpr (condExpr.ElseExpression);

                var condItem = AddNodeToTree ($"Conditional: {niceCond} ? {niceTrueExpr} : {niceFalseExpr}", parentItem);
                AddIRNodeToTree (condExpr.Condition, condItem);
                AddIRNodeToTree (condExpr.ThenExpression, condItem);
                AddIRNodeToTree (condExpr.ElseExpression, condItem);

                break;
            }

            case ESIR_NodeKind.ExpressionList when node is ESIR_ExpressionList exprList: {
                var listItem = AddNodeToTree ("Expression list", parentItem);
                foreach (var expr in exprList.Expressions.Elements)
                    AddIRNodeToTree (expr, listItem);

                break;
            }

            #endregion

            #region Statements

            case ESIR_NodeKind.BlockStatement when node is ESIR_BlockStatement blockStmt: {
                var blockItem = AddNodeToTree ("Block", parentItem);
                foreach (var stmt in blockStmt.Statements.Elements)
                    AddIRNodeToTree (stmt, blockItem);

                break;
            }
            case ESIR_NodeKind.LabeledStatement when node is ESIR_LabeledStatement labelStmt: {
                AddNodeToTree ($"Label {GetString (labelStmt.Label)}:", parentItem);
                AddIRNodeToTree (labelStmt.Statement, parentItem);
                break;
            }

            case ESIR_NodeKind.ConditionalStatement when node is ESIR_ConditionalStatement condStmt: {
                var ifItem = AddNodeToTree ($"If ({GetNiceExpr (condStmt.Condition)})", parentItem);
                var condItem = AddNodeToTree ("Condition", ifItem);
                AddIRNodeToTree (condStmt.Condition, condItem);

                AddIRNodeToTree (condStmt.Then, ifItem);
                if (condStmt.Else is not null)
                    AddIRNodeToTree (condStmt.Else, ifItem);

                break;
            }
            case ESIR_NodeKind.SwitchStatement:
                AddNodeToTree ("Switch [NO NODE IMPLEMENTED]", parentItem);
                break;

            case ESIR_NodeKind.BreakStatement when node is ESIR_BreakStatement breakStmt: {
                if (breakStmt.Label is null) {
                    AddNodeToTree ("Break", parentItem);
                    break;
                }

                AddNodeToTree ($"Break {GetString (breakStmt.Label.Value)}", parentItem);
                break;
            }
            case ESIR_NodeKind.ContinueStatement when node is ESIR_ContinueStatement continueStmt: {
                if (continueStmt.Label is null) {
                    AddNodeToTree ("Continue", parentItem);
                    break;
                }

                AddNodeToTree ($"Continue {GetString (continueStmt.Label.Value)}", parentItem);
                break;
            }

            case ESIR_NodeKind.GotoLabelStatement when node is ESIR_GotoLabelStatement gotoStmt: {
                AddNodeToTree ($"Goto {GetString (gotoStmt.Label)}", parentItem);
                break;
            }
            case ESIR_NodeKind.GotoCaseStatement when node is ESIR_GotoCaseStatement gotoCaseStmt: {
                var caseStr = gotoCaseStmt.CaseExpr is null ? "default" : $"case {GetNiceExpr (gotoCaseStmt.CaseExpr)}";
                AddNodeToTree ($"Goto {caseStr}", parentItem);
                break;
            }

            case ESIR_NodeKind.ReturnStatement when node is ESIR_ReturnStatement retStmt: {
                if (retStmt.ReturnExpr is null) {
                    AddNodeToTree ("Return void", parentItem);
                    break;
                }

                var retItem = AddNodeToTree ($"Return {GetNiceExpr (retStmt.ReturnExpr)}", parentItem);
                AddIRNodeToTree (retStmt.ReturnExpr, retItem);
                break;
            }

            case ESIR_NodeKind.LoopStatement when node is ESIR_LoopStatement loopStmt: {
                var loopItem = AddNodeToTree ("Loop", parentItem);

                var condExprs = loopStmt.ConditionExpr.Elements;
                if (condExprs.Length > 0) {
                    foreach (var expr in condExprs [..^1])
                        AddExprStmtToTree (expr, parentItem);

                    var condItem = AddNodeToTree ($"Condition {GetNiceExpr (condExprs [^1])}", loopItem);
                    AddIRNodeToTree (condExprs [^1], condItem);
                }

                var iterExprs = loopStmt.IterationExpr.Elements;
                if (iterExprs.Length > 0) {
                    var iterItem = AddNodeToTree ("Iterators", loopItem);

                    foreach (var expr in iterExprs)
                        AddExprStmtToTree (expr, iterItem);
                }

                var bodyItem = AddNodeToTree ("Body", loopItem);
                AddIRNodeToTree (loopStmt.Body, bodyItem);

                break;
            }

            case ESIR_NodeKind.ExpressionStatement when node is ESIR_ExpressionStatement exprStmt: {
                AddExprStmtToTree (exprStmt.Expression, parentItem);
                break;
            }

            #endregion

            default:
                Debug.Fail ("IR node invalid or not implemented.");
                AddNodeToTree ("IR node invalid or not implemented.", parentItem);
                break;
        }
    }

    private unsafe void CompileCode () {
        using var d = Dispatcher.DisableProcessing ();

        var code = codeText.Text;

        using var codeUnits = new StructPooledList<(ReadOnlyMemory<char>, ReadOnlyMemory<char>)> (CL_ClearMode.Auto) {
            ("Buffer".AsMemory (), code.AsMemory ())
        };

        env?.Dispose ();
        compiler.Reset ();
        compiler.Setup (idPool, out var envOut);
        env = envOut;
        compiler.AddTranslationUnit ("MainUnit", codeUnits.Span);

        var irTree = compiler.CompileIR ();

        textMarkerService.Clear ();
        symbolsTreeView.Items.Clear ();

        errList.Clear ();
        foreach (var error in compiler.Errors) {
            errList.Add (new CompilerMessage ("Error", error));

            if (error.Length == 0 || (error.StartPos + error.Length > codeText.Text.Length))
                continue;

            DisplaySquiggly (error.Column, error.Line, error.Length, error.Message, Colors.Red);
        }

        foreach (var warn in compiler.Warnings) {
            errList.Add (new CompilerMessage ("Warn", warn));

            if (warn.Length == 0 || (warn.StartPos + warn.Length > codeText.Text.Length))
                continue;

            DisplaySquiggly (warn.Column, warn.Line, warn.Length, warn.Message, Colors.Orange);
        }

        foreach (var info in compiler.InfoMessages) {
            errList.Add (new CompilerMessage ("Info", info));

            if (info.Length == 0 || (info.StartPos + info.Length > codeText.Text.Length))
                continue;

            DisplaySquiggly (info.Column, info.Line, info.Length, info.Message, Colors.CornflowerBlue);
        }

        var rootItem = new TreeViewItem {
            Header = "IR Tree",
            IsExpanded = true
        };
        symbolsTreeView.Items.Add (rootItem);

        if (errList.Count > 0 || irTree is null)
            return;

        var staticVarsItem = AddNodeToTree ("Static variables", rootItem);
        var funcsItem = AddNodeToTree ("Functions", rootItem);
        var structsItem = AddNodeToTree ("Structs", rootItem);

        foreach (var var in irTree.StaticVariables.Elements)
            AddIRNodeToTree (var, staticVarsItem);

        foreach (var func in irTree.Functions.Elements)
            AddIRNodeToTree (func, funcsItem);

        foreach (var structDef in irTree.Structs.Elements)
            AddIRNodeToTree (structDef, structsItem);
    }

    private static string GetExceptionString (EchelonScriptException e) {
        Console.Write ("EchelonScript error: ");
        Console.WriteLine (e.Message);
        foreach (var line in e.GetESStackTrace (false)) {
            Console.Write ("  ");
            Console.WriteLine (line);
        }

        return e.Message;
    }

    #endregion

    #region ================== Event handlers

    private void codeText_TextChanged (object sender, EventArgs e) {
        if (compileOnEditCheckbox.IsChecked == true)
            CompileCode ();
    }

    private void CompileButton_Click (object sender, System.Windows.RoutedEventArgs e) => CompileCode ();

    private void errorsList_MouseDoubleClick (object sender, MouseButtonEventArgs e) {
        if (errorsList.SelectedItem is not CompilerMessage)
            return;

        var error = (CompilerMessage) errorsList.SelectedItem;
        codeText.Focus ();
        codeText.Select (error.StartPos, error.Length);
        codeText.ScrollTo (error.Line, error.Column);
    }

    private void symbolsTreeView_ClickItem (object sender, MouseButtonEventArgs e) {
        if (e.ClickCount < 2)
            return;

        var selectedItem = (TreeViewItem) symbolsTreeView.SelectedItem;
        if (selectedItem == null)
            return;


    }

    #endregion
}
