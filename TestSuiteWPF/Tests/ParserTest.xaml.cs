/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

using System;
using System.Text;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using EchelonScriptCompiler.Parser;
using ICSharpCode.AvalonEdit.Document;

namespace TestSuiteWPF.Tests {
    /// <summary>
    /// Interaction logic for TokenizerTest.xaml
    /// </summary>
    public partial class ParserTest : UserControl {
        #region ================== Instance fields

        protected EchelonScriptParser parser;
        protected TextMarkerService textMarkerService;

        #endregion

        #region ================== Constructors

        public ParserTest () {
            InitializeComponent ();

            parser = new EchelonScriptParser ();
            textMarkerService = new TextMarkerService (codeText);
        }

        #endregion

        #region ================== Instance methods

        private void DisplayError (int column, int lineNumber, int length, string message) {
            if (lineNumber >= 1 && lineNumber <= codeText.Document.LineCount) {
                int offset = codeText.Document.GetOffset (new TextLocation (lineNumber, column));
                int endOffset = offset + length;

                if (endOffset < 0)
                    endOffset = codeText.Document.TextLength;

                if (length < 2)
                    length = Math.Min (2, codeText.Document.TextLength - offset);

                textMarkerService.Create (offset, length, tm => {
                    tm.MarkerColor = Colors.Red;
                    tm.ToolTip = message;
                });
            }
        }

        #endregion

        #region ================== Event handlers

        private void codeText_TextChanged (object sender, EventArgs e) {
            string code = codeText.Text;

            textMarkerService.Clear ();
            errorsList.Items.Clear ();
            astTreeView.Items.Clear ();

            parser.Reset ();
            var astTree = parser.ParseCode (code.AsMemory ());

            foreach (var error in parser.Errors) {
                errorsList.Items.Add ($"Line {error.Line}, column {error.Column}: {error.Message}");

                if (error.Length == 0 || (error.StartPos + error.Length > codeText.Text.Length))
                    continue;

                DisplayError (error.Column, error.Line, error.Length, error.Message);
            }

            astTreeView.Items.Clear ();

            var rootItem = new TreeViewItem ();
            rootItem.Header = "Code Unit";
            rootItem.IsExpanded = true;
            astTreeView.Items.Add (rootItem);

            foreach (var import in astTree.ImportStatements)
                AddAstNodeToTree (import, rootItem);
            foreach (var alias in astTree.TypeAliases)
                AddAstNodeToTree (alias, rootItem);
            foreach (var nm in astTree.Namespaces)
                AddAstNodeToTree (nm, rootItem);
        }

        private TreeViewItem AddNodeToTree (string nodeText, TreeViewItem parentItem) {
            var thisItem = new TreeViewItem ();
            parentItem.Items.Add (thisItem);
            thisItem.Header = nodeText;
            thisItem.IsExpanded = true;

            return thisItem;
        }

        private void AddAstNodeToTree (ES_AstTreeNode node, TreeViewItem parentItem) {
            if (node == null)
                return;

            switch (node) {
                case ES_AstNamespace namespaceDef: {
                    var thisItem = AddNodeToTree ($"Namespace \"{namespaceDef.NamespaceName}\"", parentItem);
                    thisItem.Tag = namespaceDef.NamespaceName;

                    foreach (var content in namespaceDef.Contents)
                        AddAstNodeToTree (content, thisItem);
                    break;
                }

                #region Aggregates

                case ES_AstClassDefinition classDef: {
                    var thisItem = AddNodeToTree ($"Class \"{classDef.Name.Text}\"", parentItem);
                    thisItem.Tag = classDef.Name;

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (classDef.AccessModifier.ToString (), modifiersList);
                    if (classDef.Static)
                        AddNodeToTree ("Static", modifiersList);

                    if (classDef.InheritanceList != null && classDef.InheritanceList.Length > 0) {
                        var inheritanceList = AddNodeToTree ("Inheritance list", thisItem);

                        foreach (var type in classDef.InheritanceList) {
                            var typeItem = AddNodeToTree (type.ToString (), inheritanceList);
                            typeItem.Tag = type;
                        }
                    }

                    if (classDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
                        docComNode.Tag = classDef.DocComment.Value;
                        AddNodeToTree (classDef.DocComment.Value.Text.Span.ToString (), docComNode);
                    }

                    if (classDef.Contents != null && classDef.Contents.Length > 0) {
                        var contentsList = AddNodeToTree ("Contents", thisItem);

                        foreach (var childNode in classDef.Contents)
                            AddAstNodeToTree (childNode, contentsList);
                    }
                    break;
                }

                case ES_AstStructDefinition structDef: {
                    var thisItem = AddNodeToTree ($"Struct \"{structDef.Name.Text}\"", parentItem);
                    thisItem.Tag = structDef.Name;

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (structDef.AccessModifier.ToString (), modifiersList);
                    if (structDef.Static)
                        AddNodeToTree ("Static", modifiersList);

                    if (structDef.InterfacesList != null && structDef.InterfacesList.Length > 0) {
                        var inheritanceList = AddNodeToTree ("Interfaces list", thisItem);

                        foreach (var type in structDef.InterfacesList) {
                            var typeItem = AddNodeToTree (type.ToString (), inheritanceList);
                            typeItem.Tag = type;
                        }
                    }

                    if (structDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
                        docComNode.Tag = structDef.DocComment.Value;
                        AddNodeToTree (structDef.DocComment.Value.Text.Span.ToString (), docComNode);
                    }

                    if (structDef.Contents != null && structDef.Contents.Length > 0) {
                        var contentsList = AddNodeToTree ("Contents", thisItem);

                        foreach (var childNode in structDef.Contents)
                            AddAstNodeToTree (childNode, contentsList);
                    }
                    break;
                }

                case ES_AstMemberVarDefinition memberVarDef: {
                    var thisItem = AddNodeToTree ($"Field {memberVarDef.Name.Text} ({memberVarDef.ValueType})", parentItem);
                    thisItem.Tag = memberVarDef.Name;

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (memberVarDef.AccessModifier.ToString (), modifiersList);
                    if (memberVarDef.Static)
                        AddNodeToTree ("Static", modifiersList);

                    if (memberVarDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
                        docComNode.Tag = memberVarDef.DocComment.Value;
                        AddNodeToTree (memberVarDef.DocComment.Value.Text.Span.ToString (), docComNode);
                    }

                    if (memberVarDef.InitializationExpression != null) {
                        var initializer = AddNodeToTree ("Initialization expression", thisItem);

                        AddAstNodeToTree (memberVarDef.InitializationExpression, initializer);
                    }

                    break;
                }

                #endregion

                case ES_AstEnumDefinition enumDef: {
                    TreeViewItem thisItem;
                    if (enumDef.BaseType == null)
                        thisItem = AddNodeToTree ($"Enum {enumDef.Name.Text}", parentItem);
                    else
                        thisItem = AddNodeToTree ($"Enum {enumDef.Name.Text} ({enumDef.BaseType.Value})", parentItem);

                    thisItem.Tag = enumDef.Name;

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (enumDef.AccessModifier.ToString (), modifiersList);

                    if (enumDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
                        docComNode.Tag = enumDef.DocComment.Value;
                        AddNodeToTree (enumDef.DocComment.Value.Text.Span.ToString (), docComNode);
                    }

                    var membersList = AddNodeToTree ("Members", thisItem);
                    if (enumDef.MembersList != null) {
                        foreach (var member in enumDef.MembersList) {
                            var memberItem = AddNodeToTree (member.Item1.Text.ToString (), membersList);
                            memberItem.Tag = member.Item1;
                            if (member.Item2 != null)
                                AddAstNodeToTree (member.Item2, memberItem);
                        }
                    }

                    break;
                }

                case ES_AstFunctionDefinition funcDef: {
                    var thisItem = AddNodeToTree ($"Function {funcDef.Name.Text} ({funcDef.ReturnType})", parentItem);
                    thisItem.Tag = funcDef.Name;

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (funcDef.AccessModifier.ToString (), modifiersList);
                    if (funcDef.Static)
                        AddNodeToTree ("Static", modifiersList);
                    if (funcDef.Const)
                        AddNodeToTree ("Const", modifiersList);

                    if (funcDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
                        docComNode.Tag = funcDef.DocComment.Value;
                        AddNodeToTree (funcDef.DocComment.Value.Text.Span.ToString (), docComNode);
                    }

                    var argsList = AddNodeToTree ("Arguments list", thisItem);
                    var statements = AddNodeToTree ("Body", thisItem);

                    foreach (var arg in funcDef.ArgumentsList) {
                        string text;

                        switch (arg.ArgType) {
                            case ES_ArgumentType.Normal:
                                text = $"{arg.ValueType} {arg.Name.Text}";
                                break;
                            case ES_ArgumentType.Ref:
                                text = $"ref {arg.ValueType} {arg.Name.Text}";
                                break;
                            case ES_ArgumentType.In:
                                text = $"in {arg.ValueType} {arg.Name.Text}";
                                break;
                            case ES_ArgumentType.Out:
                                text = $"out {arg.ValueType} {arg.Name.Text}";
                                break;

                            default:
                                throw new NotImplementedException ();
                        }

                        var argItem = AddNodeToTree (text, argsList);
                        argItem.Tag = arg.Name;

                        if (arg.DefaultExpression != null)
                            AddAstNodeToTree (arg.DefaultExpression, argItem);
                    }

                    if (funcDef.StatementsList != null) {
                        foreach (var statement in funcDef.StatementsList)
                            AddAstNodeToTree (statement, statements);
                    }

                    break;
                }

                #region Statements

                case ES_AstLabeledStatement labeledStatement: {
                    var thisItem = AddNodeToTree ($"{labeledStatement.LabelName}:", parentItem);
                    AddAstNodeToTree (labeledStatement.Statement, parentItem);
                    thisItem.Tag = labeledStatement.LabelName;
                    break;
                }

                case ES_AstBlockStatement blockStatement: {
                    var thisItem = AddNodeToTree ("Statement block", parentItem);

                    if (blockStatement.Statements != null) {
                        foreach (var statement in blockStatement.Statements)
                            AddAstNodeToTree (statement, thisItem);
                    }
                    break;
                }

                case ES_AstEmptyStatement emptyStatement: {
                    AddNodeToTree ("Empty statement", parentItem);
                    break;
                }

                #region Symbol definition

                case ES_AstTypeAlias alias: {
                    var thisItem = AddNodeToTree ($"Type alias {alias.AliasName.Text} = {alias.OriginalName}", parentItem);
                    thisItem.Tag = alias.AliasName;
                    break;
                }

                case ES_AstImportStatement importStatement: {
                    var thisItem = AddNodeToTree ($"Import {importStatement.NamespaceName}", parentItem);
                    thisItem.Tag = importStatement.NamespaceName;

                    if (importStatement.ImportedNames != null && importStatement.ImportedNames.Length > 0) {
                        foreach (var symbol in importStatement.ImportedNames)
                            AddNodeToTree (symbol.Text.ToString (), thisItem);
                    }

                    break;
                }

                case ES_AstLocalVarDefinition localVarDefStatement: {
                    if (localVarDefStatement.Variables == null)
                        break;

                    string baseText = $"{(localVarDefStatement.UsingVar ? "using " : "")}{localVarDefStatement.ValueType?.ToString () ?? "var"}";
                    foreach (var variable in localVarDefStatement.Variables) {
                        string exprText = variable.InitializationExpression != null ? " = [...]" : "";

                        var varNode = AddNodeToTree ($"{baseText} {variable.Name}{exprText}", parentItem);
                        varNode.Tag = variable.Name;

                        AddAstNodeToTree (variable.InitializationExpression, varNode);
                    }

                    break;
                }

                #endregion

                #region Jumps

                case ES_AstConditionalStatement condStatement: {
                    var thisItem = AddNodeToTree ("if (...)", parentItem);

                    var conditionItem = AddNodeToTree ("Condition", thisItem);
                    var thenItem = AddNodeToTree ("Then", thisItem);

                    AddAstNodeToTree (condStatement.ConditionExpression, conditionItem);
                    AddAstNodeToTree (condStatement.ThenStatement, thenItem);

                    if (condStatement.ElseStatement != null) {
                        var elseItem = AddNodeToTree ("Else", thisItem);
                        AddAstNodeToTree (condStatement.ElseStatement, elseItem);
                    }

                    break;
                }

                case ES_AstSwitchStatement switchStatement: {
                    var thisItem = AddNodeToTree ("switch (...) [NOT IMPLEMENTED]", parentItem);

                    break;
                }

                case ES_AstBreakStatement breakStatement: {
                    var itemText = breakStatement.LabelName != null ? "break" : $"break {breakStatement.LabelName}";
                    var thisItem = AddNodeToTree (itemText, parentItem);
                    if (breakStatement.LabelName != null)
                        thisItem.Tag = breakStatement.LabelName;

                    break;
                }

                case ES_AstContinueStatement continueStatement: {
                    var itemText = continueStatement.LabelName != null ? "continue" : $"continue {continueStatement.LabelName}";
                    var thisItem = AddNodeToTree (itemText, parentItem);
                    if (continueStatement.LabelName != null)
                        thisItem.Tag = continueStatement.LabelName;

                    break;
                }

                case ES_AstGotoLabelStatement gotoLabelStatement: {
                    var thisItem = AddNodeToTree ($"goto {gotoLabelStatement.LabelName}", parentItem);
                    thisItem.Tag = gotoLabelStatement.LabelName;

                    break;
                }

                case ES_AstGotoCaseStatement gotoCaseStatement: {
                    var thisItem = AddNodeToTree (gotoCaseStatement.CaseExpression != null ? "goto case (...)" : "goto default", parentItem);
                    AddAstNodeToTree (gotoCaseStatement.CaseExpression, thisItem);

                    break;
                }

                case ES_AstReturnStatement returnStatement: {
                    var thisItem = AddNodeToTree (returnStatement.ReturnExpression != null ? "return (...)" : "return", parentItem);
                    AddAstNodeToTree (returnStatement.ReturnExpression, thisItem);

                    break;
                }

                #endregion

                #region Loops

                case ES_AstLoopStatement loopStatement: {
                    string itemString;
                    bool forLoop = false;

                    if (
                        loopStatement.InitializationStatement != null ||
                        loopStatement.ConditionExpression == null ||
                        loopStatement.IterationExpressions != null
                    ) {
                        string initString = loopStatement.InitializationStatement != null ? "..." : "";
                        string condString = loopStatement.ConditionExpression != null ? " ..." : "";
                        string iterString = loopStatement.IterationExpressions != null ? " ..." : "";

                        itemString = $"for ({initString};{condString};{iterString})";
                        forLoop = true;
                    } else
                        itemString = "while (...)";

                    var thisItem = AddNodeToTree (itemString, parentItem);

                    if (forLoop) {
                        if (loopStatement.InitializationStatement != null) {
                            var initItem = AddNodeToTree ("Initialization", thisItem);
                            AddAstNodeToTree (loopStatement.InitializationStatement, initItem);
                        }

                        if (loopStatement.ConditionExpression != null) {
                            var condItem = AddNodeToTree ("Condition", thisItem);
                            AddAstNodeToTree (loopStatement.ConditionExpression, condItem);
                        }

                        if (loopStatement.IterationExpressions != null) {
                            var iterItem = AddNodeToTree ("Iteration", thisItem);

                            foreach (var expr in loopStatement.IterationExpressions)
                                AddAstNodeToTree (expr, iterItem);
                        }
                    } else {
                        var condItem = AddNodeToTree ("Condition", thisItem);
                        AddAstNodeToTree (loopStatement.ConditionExpression, condItem);
                    }

                    var bodyItem = AddNodeToTree ("Body", thisItem);
                    AddAstNodeToTree (loopStatement.LoopBody, bodyItem);

                    break;
                }

                #endregion

                case ES_AstExpressionStatement exprStatement: {
                    AddAstNodeToTree (exprStatement.Expression, parentItem);
                    break;
                }

                #endregion

                #region Expressions

                #region Primary expressions

                case ES_AstParenthesisExpression parenthesisExpr: {
                    var thisItem = AddNodeToTree ($"()", parentItem);
                    AddAstNodeToTree (parenthesisExpr.Inner, thisItem);
                    break;
                }

                case ES_AstIntegerLiteralExpression intLiteralExpr: {
                    var thisItem = AddNodeToTree ($"Int literal {intLiteralExpr.Value}{(intLiteralExpr.Unsigned ? "u" : "")}{(intLiteralExpr.Long ? "L" : "")}", parentItem);
                    thisItem.Tag = intLiteralExpr.Token;

                    break;
                }

                case ES_AstFloatLiteralExpression floatLiteralExpr: {
                    string nodeText;
                    if (floatLiteralExpr.IsFloat)
                        nodeText = $"Float literal {floatLiteralExpr.ValueFloat}f";
                    else
                        nodeText = $"Float literal {floatLiteralExpr.ValueDouble}d";

                    var thisItem = AddNodeToTree (nodeText, parentItem);
                    thisItem.Tag = floatLiteralExpr.Token;

                    break;
                }

                case ES_AstStringLiteralExpression strLiteralExpr: {
                    string nodeText;
                    if (!strLiteralExpr.Verbatim)
                        nodeText = $"String literal \"{strLiteralExpr.ValueUTF16}\"";
                    else
                        nodeText = $"Verbatim string literal \"{strLiteralExpr.ValueUTF16}\"";

                    var thisItem = AddNodeToTree (nodeText, parentItem);
                    thisItem.Tag = strLiteralExpr.Token;

                    break;
                }

                case ES_AstCharLiteralExpression charLiteralExpr: {
                    Span<char> charBuf = stackalloc char [2];

                    var rune = new Rune (charLiteralExpr.Value);
                    rune.EncodeToUtf16 (charBuf);

                    var thisItem = AddNodeToTree ($"Char literal '{charBuf.ToString ()}'", parentItem);
                    thisItem.Tag = charLiteralExpr.Token;

                    break;
                }

                case ES_AstNameExpression nameExpr: {
                    var thisItem = AddNodeToTree ($"Name \"{nameExpr.Value.Text}\"", parentItem);
                    thisItem.Tag = nameExpr.Value;
                    break;
                }

                case ES_AstFunctionCallExpression funcCallExpr: {
                    var thisItem = AddNodeToTree ("Function call", parentItem);
                    var funcExpr = AddNodeToTree ("Expression", thisItem);
                    var argsList = AddNodeToTree ("Arguments", thisItem);

                    AddAstNodeToTree (funcCallExpr.FunctionExpression, funcExpr);
                    AddArgumentsListToTree (funcCallExpr.Arguments, argsList);

                    break;
                }

                case ES_AstIndexingExpression indexingExpr: {
                    var thisItem = AddNodeToTree ("Indexing", parentItem);
                    var indexedExpr = AddNodeToTree ("Expression", thisItem);
                    var ranksList = AddNodeToTree ("Ranks", thisItem);

                    AddAstNodeToTree (indexingExpr.IndexedExpression, indexedExpr);
                    foreach (var rank in indexingExpr.RankExpressions)
                        AddAstNodeToTree (rank, ranksList);

                    break;
                }

                case ES_AstNewExpression newExpr: {
                    string thisItemText;

                    if (newExpr.Arguments.Length < 1)
                        thisItemText = $"new {newExpr.TypeDeclaration} ()";
                    else
                        thisItemText = $"new {newExpr.TypeDeclaration} ([...])";

                    var thisItem = AddNodeToTree (thisItemText, parentItem);
                    AddArgumentsListToTree (newExpr.Arguments, thisItem);

                    break;
                }

                #endregion

                case ES_AstIncDecExpression incDecExpr: {
                    string sideText = incDecExpr.Postfix ? "Postfix" : "Prefix";
                    string opText = !incDecExpr.Decrement ? "Increment" : "Decrement";
                    var thisItem = AddNodeToTree ($"{sideText} {opText}", parentItem);
                    AddAstNodeToTree (incDecExpr.Inner, thisItem);

                    break;
                }

                #region Unary expressions

                case ES_AstSimpleUnaryExpression simpleUnaryExpr: {
                    string opText;
                    switch (simpleUnaryExpr.ExpressionType) {
                        case SimpleUnaryExprType.Positive:
                            opText = "Positive";
                            break;
                        case SimpleUnaryExprType.Negative:
                            opText = "Negative";
                            break;
                        case SimpleUnaryExprType.LogicalNot:
                            opText = "Logical NOT";
                            break;
                        case SimpleUnaryExprType.BitNot:
                            opText = "Bitwise NOT";
                            break;

                        default:
                            throw new NotImplementedException ();
                    }
                    var thisItem = AddNodeToTree (opText, parentItem);
                    AddAstNodeToTree (simpleUnaryExpr.Inner, thisItem);

                    break;
                }

                case ES_AstCastExpression castExpr: {
                    var thisItem = AddNodeToTree ($"Cast ({castExpr.DestinationType})", parentItem);
                    AddAstNodeToTree (castExpr.InnerExpression, thisItem);

                    break;
                }

                #endregion

                case ES_AstSimpleBinaryExpression simpleBinaryExpr: {
                    string opText;
                    switch (simpleBinaryExpr.ExpressionType) {
                        case SimpleBinaryExprType.MemberAccess:
                            opText = "Member access";
                            break;

                        case SimpleBinaryExprType.Power:
                            opText = "Exponentiation";
                            break;

                        case SimpleBinaryExprType.Multiply:
                            opText = "Multiplication";
                            break;
                        case SimpleBinaryExprType.Divide:
                            opText = "Division";
                            break;
                        case SimpleBinaryExprType.Modulo:
                            opText = "Modulo";
                            break;

                        case SimpleBinaryExprType.Add:
                            opText = "Addition";
                            break;
                        case SimpleBinaryExprType.Subtract:
                            opText = "Subtraction";
                            break;

                        case SimpleBinaryExprType.ShiftLeft:
                            opText = "Bitshift left";
                            break;
                        case SimpleBinaryExprType.ShiftRight:
                            opText = "Bitshift right";
                            break;
                        case SimpleBinaryExprType.ShiftRightUnsigned:
                            opText = "Bitshift right unsigned";
                            break;

                        case SimpleBinaryExprType.LesserThan:
                            opText = "Lesser than";
                            break;
                        case SimpleBinaryExprType.GreaterThan:
                            opText = "Greater than";
                            break;
                        case SimpleBinaryExprType.LesserThanEqual:
                            opText = "Lesser than or equals";
                            break;
                        case SimpleBinaryExprType.GreaterThanEqual:
                            opText = "Greater than or equals";
                            break;

                        case SimpleBinaryExprType.Equals:
                            opText = "Equals";
                            break;
                        case SimpleBinaryExprType.NotEquals:
                            opText = "Not equals";
                            break;

                        case SimpleBinaryExprType.BitAnd:
                            opText = "Bitwise AND";
                            break;
                        case SimpleBinaryExprType.BitXor:
                            opText = "Bitwise XOR";
                            break;
                        case SimpleBinaryExprType.BitOr:
                            opText = "Bitwise OR";
                            break;

                        case SimpleBinaryExprType.LogicalAnd:
                            opText = "Logical AND";
                            break;
                        case SimpleBinaryExprType.LogicalOr:
                            opText = "Logical OR";
                            break;

                        #region Assignment

                        case SimpleBinaryExprType.Assign:
                            opText = "Assignment";
                            break;

                        case SimpleBinaryExprType.AssignAdd:
                            opText = "Assign-Addition";
                            break;
                        case SimpleBinaryExprType.AssignSubtract:
                            opText = "Assign-Subtraction";
                            break;

                        case SimpleBinaryExprType.AssignMultiply:
                            opText = "Assign-Multiplication";
                            break;
                        case SimpleBinaryExprType.AssignDivide:
                            opText = "Assign-Division";
                            break;
                        case SimpleBinaryExprType.AssignModulo:
                            opText = "Assign-Modulo";
                            break;
                        case SimpleBinaryExprType.AssignPower:
                            opText = "Assign-Exponentiation";
                            break;

                        case SimpleBinaryExprType.AssignBitAnd:
                            opText = "Assign-Bitwise AND";
                            break;
                        case SimpleBinaryExprType.AssignBitOr:
                            opText = "Assign-Bitwise OR";
                            break;
                        case SimpleBinaryExprType.AssignXor:
                            opText = "Assign-Bitwise XOR";
                            break;

                        case SimpleBinaryExprType.AssignTilde:
                            opText = "Assign-Concatenation";
                            break;

                        case SimpleBinaryExprType.AssignShiftLeft:
                            opText = "Assign-Shift left";
                            break;
                        case SimpleBinaryExprType.AssignShiftRight:
                            opText = "Assign-Shift right";
                            break;
                        case SimpleBinaryExprType.AssignShiftRightUnsigned:
                            opText = "Assign-Shift right unsigned";
                            break;

                        #endregion

                        default:
                            throw new NotImplementedException ();
                    }
                    var thisItem = AddNodeToTree (opText, parentItem);
                    AddAstNodeToTree (simpleBinaryExpr.Left, thisItem);
                    AddAstNodeToTree (simpleBinaryExpr.Right, thisItem);

                    break;
                }

                #endregion

                default:
                    throw new NotImplementedException ();
            }
        }

        private void AddArgumentsListToTree (ES_AstFunctionCallArgument [] argsList, TreeViewItem parentItem) {
            foreach (var arg in argsList) {
                string typeString;

                switch (arg.ArgType) {
                    case ES_ArgumentType.Normal:
                        typeString = "By-value";
                        break;
                    case ES_ArgumentType.Ref:
                        typeString = "By-ref";
                        break;
                    case ES_ArgumentType.Out:
                        typeString = "Out";
                        break;

                    case ES_ArgumentType.In:
                        throw new Exception ("What? This shouldn't happen.");

                    default:
                        throw new NotImplementedException ();
                }

                var argItem = AddNodeToTree (typeString, parentItem);
                AddAstNodeToTree (arg.ValueExpression, argItem);
            }
        }

        private void errorsList_MouseDoubleClick (object sender, MouseButtonEventArgs e) {
            if (errorsList.SelectedIndex < 0 || errorsList.SelectedIndex > parser.Errors.Count)
                return;

            var error = parser.Errors [errorsList.SelectedIndex];
            codeText.Focus ();
            codeText.Select (error.StartPos, error.Length);
        }

        private void astTreeView_ClickItem (object sender, MouseButtonEventArgs e) {
            if (e.ClickCount < 2)
                return;

            var selectedItem = (TreeViewItem) astTreeView.SelectedItem;
            if (selectedItem == null)
                return;

            if (selectedItem.Tag is EchelonScriptToken?) {
                var token = selectedItem.Tag as EchelonScriptToken?;

                e.Handled = true;
                codeText.Focus ();
                codeText.Select (token.Value.TextStartPos, token.Value.Text.Length);
                codeText.UpdateLayout ();
                codeText.ScrollTo (token.Value.TextLine, token.Value.TextColumn);
            } else if (selectedItem.Tag is ES_AstDottableIdentifier) {
                var id = selectedItem.Tag as ES_AstDottableIdentifier;

                if (id.Parts == null || id.Parts.Length < 1)
                    return;

                int startPos = int.MaxValue;
                int endPos = int.MinValue;

                foreach (var token in id.Parts) {
                    startPos = Math.Min (startPos, token.TextStartPos);
                    endPos = Math.Max (endPos, token.TextStartPos + token.Text.Length);
                }

                e.Handled = true;
                codeText.Focus ();
                codeText.Select (startPos, endPos - startPos);
                codeText.UpdateLayout ();
                codeText.ScrollTo (id.Parts [0].TextLine, id.Parts [0].TextColumn);
            }
        }

        #endregion
    }
}
