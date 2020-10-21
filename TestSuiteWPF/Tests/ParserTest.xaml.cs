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

            return thisItem;
        }

        private void AddAstNodeToTree (ES_AstTreeNode node, TreeViewItem parentItem) {
            switch (node) {
                case ES_AstNamespace nm: {
                    var thisItem = AddNodeToTree ($"Namespace \"{nm.NamespaceName}\"", parentItem);

                    foreach (var content in nm.Contents)
                        AddAstNodeToTree (content, thisItem);
                    break;
                }

                case ES_AstClassDefinition classDef: {
                    var thisItem = AddNodeToTree ($"Class \"{classDef.Name.Text}\"", parentItem);

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (classDef.AccessModifier.ToString (), modifiersList);
                    if (classDef.Static)
                        AddNodeToTree ("Static", modifiersList);

                    if (classDef.InheritanceList != null && classDef.InheritanceList.Length > 0) {
                        var inheritanceList = AddNodeToTree ("Inheritance list", thisItem);

                        foreach (var types in classDef.InheritanceList)
                            AddNodeToTree (types.ToString (), inheritanceList);
                    }

                    if (classDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
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
                    var thisItem = AddNodeToTree ($"Class \"{structDef.Name.Text}\"", parentItem);

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (structDef.AccessModifier.ToString (), modifiersList);
                    if (structDef.Static)
                        AddNodeToTree ("Static", modifiersList);

                    if (structDef.InterfacesList != null && structDef.InterfacesList.Length > 0) {
                        var inheritanceList = AddNodeToTree ("Interfaces list", thisItem);

                        foreach (var types in structDef.InterfacesList)
                            AddAstNodeToTree (types, inheritanceList);
                    }

                    if (structDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
                        AddNodeToTree (structDef.DocComment.Value.Text.Span.ToString (), docComNode);
                    }

                    if (structDef.Contents != null && structDef.Contents.Length > 0) {
                        var contentsList = AddNodeToTree ("Contents", thisItem);

                        foreach (var childNode in structDef.Contents)
                            AddAstNodeToTree (childNode, contentsList);
                    }
                    break;
                }

                case ES_AstEnumDefinition enumDef: {
                    TreeViewItem thisItem;
                    if (enumDef.BaseType == null)
                        thisItem = AddNodeToTree ($"Enum {enumDef.Name.Text}", parentItem);
                    else
                        thisItem = AddNodeToTree ($"Enum {enumDef.Name.Text} ({enumDef.BaseType.Value})", parentItem);

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (enumDef.AccessModifier.ToString (), modifiersList);

                    if (enumDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
                        AddNodeToTree (enumDef.DocComment.Value.Text.Span.ToString (), docComNode);
                    }

                    var membersList = AddNodeToTree ("Members", thisItem);
                    if (enumDef.MembersList != null) {
                        foreach (var member in enumDef.MembersList) {
                            var memberItem = AddNodeToTree (member.Item1.Text.ToString (), membersList);
                            if (member.Item2 != null)
                                AddAstNodeToTree (member.Item2, memberItem);
                        }
                    }

                    break;
                }

                case ES_AstImportStatement importStatement: {
                    var thisItem = AddNodeToTree ($"Import {importStatement.NamespaceName}", parentItem);

                    if (importStatement.ImportedNames != null && importStatement.ImportedNames.Length > 0) {
                        foreach (var symbol in importStatement.ImportedNames)
                            AddNodeToTree (symbol.Text.ToString (), thisItem);
                    }

                    break;
                }

                case ES_AstTypeAlias alias: {
                    AddNodeToTree ($"Type alias {alias.AliasName.Text} = {alias.OriginalName}", parentItem);
                    break;
                }

                case ES_AstFunctionDefinition funcDef: {
                    var thisItem = AddNodeToTree ($"Function {funcDef.Name.Text} ({funcDef.ReturnType})", parentItem);

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (funcDef.AccessModifier.ToString (), modifiersList);
                    if (funcDef.Static)
                        AddNodeToTree ("Static", modifiersList);
                    if (funcDef.Const)
                        AddNodeToTree ("Const", modifiersList);

                    if (funcDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
                        AddNodeToTree (funcDef.DocComment.Value.Text.Span.ToString (), docComNode);
                    }

                    var argsList = AddNodeToTree ("Arguments list", thisItem);
                    var statements = AddNodeToTree ("Body", thisItem);

                    foreach (var arg in funcDef.ArgumentsList) {
                        string text;

                        switch (arg.Mode) {
                            case ES_AstFunctionArgument.ArgumentMode.Normal:
                                text = $"{arg.ValueType} {arg.Name.Text}";
                                break;
                            case ES_AstFunctionArgument.ArgumentMode.Ref:
                                text = $"ref {arg.ValueType} {arg.Name.Text}";
                                break;
                            case ES_AstFunctionArgument.ArgumentMode.In:
                                text = $"in {arg.ValueType} {arg.Name.Text}";
                                break;
                            case ES_AstFunctionArgument.ArgumentMode.Out:
                                text = $"out {arg.ValueType} {arg.Name.Text}";
                                break;

                            default:
                                throw new NotImplementedException ();
                        }

                        var argItem = AddNodeToTree (text, argsList);

                        if (arg.DefaultExpression != null)
                            AddAstNodeToTree (arg.DefaultExpression, argItem);
                    }

                    if (funcDef.StatementsList != null) {
                        foreach (var statement in funcDef.StatementsList)
                            AddAstNodeToTree (statement, statements);
                    }

                    break;
                }

                case ES_AstMemberVarDefinition memberVarDef: {
                    var thisItem = AddNodeToTree ($"Field {memberVarDef.Name.Text} ({memberVarDef.ValueType})", parentItem);

                    var modifiersList = AddNodeToTree ("Access modifiers", thisItem);
                    AddNodeToTree (memberVarDef.AccessModifier.ToString (), modifiersList);
                    if (memberVarDef.Static)
                        AddNodeToTree ("Static", modifiersList);

                    if (memberVarDef.DocComment != null) {
                        var docComNode = AddNodeToTree ("Documentation comment", thisItem);
                        AddNodeToTree (memberVarDef.DocComment.Value.Text.Span.ToString (), docComNode);
                    }

                    if (memberVarDef.InitializationExpression != null) {
                        var initializer = AddNodeToTree ("Initialization expression", thisItem);

                        AddAstNodeToTree (memberVarDef.InitializationExpression, initializer);
                    }

                    break;
                }

                default:
                    throw new NotImplementedException ();
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
            var token = (EchelonScriptToken?) selectedItem.Tag;
            if (token == null)
                return;

            e.Handled = true;
            codeText.Focus ();
            codeText.Select (token.Value.TextStartPos, token.Value.Text.Length);
            codeText.UpdateLayout ();
            codeText.ScrollTo (token.Value.TextLine, token.Value.TextColumn);
        }

        #endregion
    }
}
