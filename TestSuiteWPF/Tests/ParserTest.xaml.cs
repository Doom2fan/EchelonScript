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

                #endregion

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
