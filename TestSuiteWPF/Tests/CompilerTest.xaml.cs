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
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using ChronosLib.Pooled;
using EchelonScriptCompiler;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Data.Types;
using ICSharpCode.AvalonEdit.Document;

namespace TestSuiteWPF.Tests {
    /// <summary>
    /// Interaction logic for CompilerTest.xaml
    /// </summary>
    public partial class CompilerTest : UserControl {
        #region ================== Instance fields

        protected IReadOnlyList<EchelonScriptErrorMessage> errList;
        protected TextMarkerService textMarkerService;

        protected EchelonScript_Compiler compiler;

        #endregion

        #region ================== Constructors

        public CompilerTest () {
            InitializeComponent ();

            textMarkerService = new TextMarkerService (codeText);

            compiler = new EchelonScript_Compiler ();
            errList = compiler.Errors;
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

        private unsafe void codeText_TextChanged (object sender, EventArgs e) {
            string code = codeText.Text;

            using var codeUnits = new StructPooledList<ReadOnlyMemory<char>> (CL_ClearMode.Auto);
            codeUnits.Add (code.AsMemory ());

            compiler.Reset ();
            compiler.Setup (out var env);
            compiler.AddTranslationUnit ("MainUnit", codeUnits.Span);
            compiler.Compile ();

            

            textMarkerService.Clear ();
            errorsList.Items.Clear ();
            symbolsTreeView.Items.Clear ();

            for (int i = 0; i < errList.Count; i++) {
                var error = errList [i];

                errorsList.Items.Add ($"Line {error.Line}, column {error.Column}: {error.Message}");

                if (error.Length == 0 || (error.StartPos + error.Length > codeText.Text.Length))
                    continue;

                DisplayError (error.Column, error.Line, error.Length, error.Message);
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
                case ES_TypeTag.Pointer: typeType = "Pointer"; break;

                default: typeType = "[UNRECOGNIZED]"; break;
            }

            var typeNode = AddNodeToTree ($"{typeType} {typeData->TypeNameString}", parentItem);

            AddNodeToTree ($"Runtime size: {typeData->RuntimeSize}", typeNode);
            AddNodeToTree ($"Fully qualified name: {typeData->FullyQualifiedNameString}", typeNode);
            AddNodeToTree ($"Source unit: {typeData->SourceUnitString}", typeNode);
        }

        private void errorsList_MouseDoubleClick (object sender, MouseButtonEventArgs e) {
            if (errorsList.SelectedIndex < 0 || errorsList.SelectedIndex > errList.Count)
                return;

            var error = errList [errorsList.SelectedIndex];
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
