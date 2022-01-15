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
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using EchelonScriptCompiler.CompilerCommon;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend.Parser;
using ICSharpCode.AvalonEdit.Document;

namespace TestSuiteWPF.Tests {
    /// <summary>
    /// Interaction logic for TokenizerTest.xaml
    /// </summary>
    public partial class TokenizerTest : UserControl {
        #region ================== Instance fields

        protected List<EchelonScriptErrorMessage> errors;
        protected EchelonScriptTokenizer tokenizer;
        protected TextMarkerService textMarkerService;

        #endregion

        #region ================== Constructors

        public TokenizerTest () {
            InitializeComponent ();

            errors = new List<EchelonScriptErrorMessage> ();
            tokenizer = new EchelonScriptTokenizer (errors);
            textMarkerService = new TextMarkerService (codeText);
        }

        #endregion

        #region ================== Instance methods

        private void DisplayError (int column, int lineNumber, string message) {
            if (lineNumber >= 1 && lineNumber <= codeText.Document.LineCount) {
                int offset = codeText.Document.GetOffset (new TextLocation (lineNumber, column));
                int endOffset = TextUtilities.GetNextCaretPosition (codeText.Document, offset, LogicalDirection.Forward, CaretPositioningMode.WordBorderOrSymbol);

                if (endOffset < 0)
                    endOffset = codeText.Document.TextLength;
                int length = endOffset - offset;

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
            errors.Clear ();
            tokenizer.SetSource ("Buffer".AsMemory (), code.AsMemory ());

            textMarkerService.Clear ();
            errorsList.Items.Clear ();
            tokensTree.Items.Clear ();

            EchelonScriptToken tok = new EchelonScriptToken () { Type = EchelonScriptTokenType.Invalid };
            EchelonScriptToken? docComment;
            while (tok.Type != EchelonScriptTokenType.EOF) {
                (tok, docComment) = tokenizer.NextToken ();

                var tokItem = new TreeViewItem ();
                tokItem.Header = $"{tok.Type} ({tok.TextLine}:{tok.TextColumn})";
                if (tok.Text.Length > 0)
                    tokItem.ToolTip = $"Text: \"{tok.Text}\"";
                tokItem.Tag = tok;

                if (docComment != null) {
                    var docItem = new TreeViewItem ();
                    docItem.Header = $"{docComment?.Type} ({docComment?.TextLine}:{docComment?.TextColumn})";
                    docItem.ToolTip = $"Text: \"{docComment?.Text}\"";
                    docItem.Tag = docComment.Value;
                    tokItem.Items.Add (docItem);
                }

                tokensTree.Items.Add (tokItem);
            }
            foreach (var error in errors) {
                errorsList.Items.Add ($"Line {error.Line}, column {error.Column}: {error.Message}");

                if (error.Length == 0 || (error.StartPos + error.Length >= codeText.Text.Length))
                    continue;

                DisplayError (error.Column, error.Line, error.Message);
            }
        }

        private void errorsList_MouseDoubleClick (object sender, MouseButtonEventArgs e) {
            if (errorsList.SelectedIndex < 0 || errorsList.SelectedIndex > errors.Count)
                return;

            var error = errors [errorsList.SelectedIndex];
            if (error.Length == 0)
                return;

            codeText.Focus ();
            codeText.Select (error.StartPos, error.Length);
        }

        private void tokensTree_ClickItem (object sender, MouseButtonEventArgs e) {
            if (e.ClickCount < 2)
                return;

            var selectedItem = (TreeViewItem) tokensTree.SelectedItem;
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
