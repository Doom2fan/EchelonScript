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
using System.Collections.Generic;
using System.Windows.Controls;
using System.Windows.Documents;
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
