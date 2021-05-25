/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using ChronosLib.Pooled;
using EchelonScriptCompiler;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using ICSharpCode.AvalonEdit.Document;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace TestSuiteWPF.Tests {
    /// <summary>
    /// Interaction logic for CompilerTest.xaml
    /// </summary>
    public partial class FrontendTest : UserControl {
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

            public int Line { get; }
            public int Column { get; }

            public CompilerMessage (string type, EchelonScriptErrorMessage msg) {
                Type = type;
                Message = msg.Message;

                StartPos = msg.StartPos;
                Length = msg.Length;

                Line = msg.Line;
                Column = msg.Column;
            }
        }

        #region ================== Instance fields

        protected ObservableCollection<CompilerMessage> errList;
        protected TextMarkerService textMarkerService;

        protected EchelonScript_Compiler compiler;

        #endregion

        #region ================== Constructors

        public FrontendTest () {
            InitializeComponent ();

            textMarkerService = new TextMarkerService (codeText);

            compiler = new EchelonScript_Compiler ();
            errList = new ObservableCollection<CompilerMessage> ();

            errorsList.ItemsSource = errList;
        }

        #endregion

        #region ================== Instance methods

        private void DisplaySquiggly (int column, int lineNumber, int length, string message, Color col) {
            if (lineNumber >= 1 && lineNumber <= codeText.Document.LineCount) {
                int offset = codeText.Document.GetOffset (new TextLocation (lineNumber, column));
                int endOffset = offset + length;

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

        #endregion

        #region ================== Event handlers

        private unsafe void codeText_TextChanged (object sender, EventArgs e) {
            string code = codeText.Text;

            using var codeUnits = new StructPooledList<ReadOnlyMemory<char>> (CL_ClearMode.Auto);
            codeUnits.Add (code.AsMemory ());

            compiler.Reset ();
            compiler.Setup (out var env);
            compiler.AddTranslationUnit ("MainUnit", codeUnits.Span);
            compiler.Compile ();

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

            var rootItem = new TreeViewItem ();
            rootItem.Header = "Code Unit";
            rootItem.IsExpanded = true;
            symbolsTreeView.Items.Add (rootItem);

            foreach (var nm in env.Namespaces) {
                var namespaceNode = AddNodeToTree (nm.Value.NamespaceNameString, rootItem);
                namespaceNode.IsExpanded = true;

                foreach (var typeDataPtr in nm.Value.TypeSpan)
                    AddTypeToTree (typeDataPtr.Address, namespaceNode);
            }
        }

        private TreeViewItem AddNodeToTree (string nodeText, TreeViewItem parentItem) {
            var thisItem = new TreeViewItem ();
            parentItem.Items.Add (thisItem);
            thisItem.Header = nodeText;

            return thisItem;
        }

        private unsafe void AddTypeToTree (ES_TypeInfo* typeData, TreeViewItem parentItem) {
            string typeType = null;

            switch (typeData->TypeTag) {
                case ES_TypeTag.Class: typeType = "Class"; break;
                case ES_TypeTag.Struct: typeType = "Struct"; break;
                case ES_TypeTag.Interface: typeType = "Interface"; break;
                case ES_TypeTag.Array: typeType = "Array"; break;
                case ES_TypeTag.Reference: typeType = "Reference"; break;
                case ES_TypeTag.Function: typeType = "Function"; break;

                default: typeType = "[UNRECOGNIZED]"; break;
            }

            var typeNode = AddNodeToTree ($"{typeType} {typeData->TypeNameString}", parentItem);

            AddNodeToTree ($"Runtime size: {typeData->RuntimeSize}", typeNode);
            AddNodeToTree ($"Fully qualified name: {typeData->FullyQualifiedNameString}", typeNode);
            AddNodeToTree ($"Source unit: {typeData->SourceUnitString}", typeNode);

            if (typeData->TypeTag == ES_TypeTag.Function) {
                return;
                var funcData = (ES_FunctionPrototypeData*) typeData;

                if (funcData->ReturnType != null)
                    AddNodeToTree ($"Return type: {funcData->ReturnType->FullyQualifiedNameString}", typeNode);
                else
                    AddNodeToTree ($"Return type: void", typeNode);

                var argsListNode = AddNodeToTree ($"Arguments list", typeNode);
                foreach (var arg in funcData->ArgumentsList.Span) {
                    string argType = arg.ArgType.ToString ();
                    string argTypeName;

                    if (arg.ValueType != null)
                        argTypeName = arg.ValueType->FullyQualifiedNameString;
                    else
                        argTypeName = "[NULL]";

                    AddNodeToTree ($"{argType} {argTypeName}", argsListNode);
                }
            }
        }

        private void errorsList_MouseDoubleClick (object sender, MouseButtonEventArgs e) {
            if (errorsList.SelectedItem is not CompilerMessage)
                return;

            var error = (CompilerMessage) errorsList.SelectedItem;
            codeText.Focus ();
            codeText.Select (error.StartPos, error.Length);
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
}
