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
using System.Collections.Immutable;
using System.Data.Common;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using EchelonScript.Compiler;
using EchelonScript.Compiler.CompilerCommon;
using EchelonScript.Compiler.Data;
using EchelonScript.Compiler.Frontend.Parser.Tokenizer;
using ICSharpCode.AvalonEdit.Document;

namespace TestSuiteWPF.Tests;

/// <summary>
/// Interaction logic for TokenizerTest.xaml
/// </summary>
public partial class TokenizerTest : UserControl {
    #region ================== Instance fields

    protected List<ES_Diagnostic> diagnostics;
    protected TextMarkerService textMarkerService;

    #endregion

    public TokenizerTest () {
        InitializeComponent ();

        diagnostics = new List<ES_Diagnostic> ();
        textMarkerService = new TextMarkerService (codeText);
    }

    #region ================== Instance methods

    private void DisplayDiagnostic (int startPos, int endPos, string message) {
        if (startPos > endPos || startPos < 0 || endPos > codeText.Text.Length)
            return;

        textMarkerService.Create (startPos, endPos - startPos, tm => {
            tm.MarkerColor = Colors.Red;
            tm.ToolTip = message;
        });
    }

    #endregion

    #region ================== Event handlers

    private void codeText_TextChanged (object sender, EventArgs e) {
        using var d = Dispatcher.DisableProcessing ();

        var context = new ES_CompilerContext () {
            ReportDiagnostic = diagnostics.Add
        };
        using var tokenizer = new ES_Tokenizer ();

        diagnostics.Clear ();
        context.SourceMap.AddFile ("<anon>", "Buffer", codeText.Text.ToImmutableArray ());
        tokenizer.SetSource (context, context.SourceMap.EnumerateFiles ().First ().Span);

        textMarkerService.Clear ();
        diagList.Items.Clear ();
        tokensTree.Items.Clear ();

        var tok = new ES_Token () { Type = ES_TokenType.Invalid };
        while (tok.Type != ES_TokenType.EOF) {
            tok = tokenizer.NextToken ();
            var location = context.SourceMap.GetLocation (tok.TextSpan);

            var tokItem = new TreeViewItem {
                Header = $"{tok.Type} ({location.Line}:{location.Column})",
                ToolTip = tok.TextSpan.Length > 0 ? $"Text: \"{context.SourceMap.GetText (tok.TextSpan)}\"" : null,
                Tag = location
            };

            var leadingTriviaItem = new TreeViewItem { Header = "Leading trivia:", };
            var trailingTriviaItem = new TreeViewItem { Header = "Trailing trivia:", };
            tokItem.Items.Add (leadingTriviaItem);
            tokItem.Items.Add (trailingTriviaItem);

            foreach (var trivia in tok.LeadingTrivia) {
                var triviaLoc = context.SourceMap.GetLocation (trivia.TextSpan);
                leadingTriviaItem.Items.Add (new TreeViewItem {
                    Header = $"{trivia.Type} ({triviaLoc.Line}:{triviaLoc.Column})",
                    ToolTip = trivia.TextSpan.Length > 0 ? $"Text: \"{context.SourceMap.GetText (trivia.TextSpan)}\"" : null,
                    Tag = triviaLoc
                });
            }
            foreach (var trivia in tok.TrailingTrivia) {
                var triviaLoc = context.SourceMap.GetLocation (trivia.TextSpan);
                trailingTriviaItem.Items.Add (new TreeViewItem {
                    Header = $"{trivia.Type} ({triviaLoc.Line}:{triviaLoc.Column})",
                    ToolTip = trivia.TextSpan.Length > 0 ? $"Text: \"{context.SourceMap.GetText (trivia.TextSpan)}\"" : null,
                    Tag = triviaLoc
                });
            }

            tokensTree.Items.Add (tokItem);
        }

        foreach (var diag in diagnostics) {
            var message = diag.GetMessage ();
            if (diag.Location is null) {
                diagList.Items.Add (message);
                continue;
            }

            var location = diag.Location.Value;
            diagList.Items.Add ($"Line {location.Line}, column {location.Column}: {message}");

            if (location.Length == 0 || location.EndPos >= codeText.Text.Length)
                continue;

            DisplayDiagnostic (location.StartPos, location.EndPos, message);
        }
    }

    private void diagList_MouseDoubleClick (object sender, MouseButtonEventArgs e) {
        if (diagList.SelectedIndex < 0 || diagList.SelectedIndex > diagnostics.Count)
            return;

        var diag = diagnostics [diagList.SelectedIndex];
        if (diag.Location == null || diag.Location.Value.Length < 1)
            return;

        var location = diag.Location.Value;
        codeText.Focus ();
        codeText.Select (location.StartPos, location.Length);
        codeText.ScrollTo (location.Line, location.Column);
    }

    private void tokensTree_ClickItem (object sender, MouseButtonEventArgs e) {
        if (e.ClickCount < 2)
            return;

        var selectedItem = (TreeViewItem) tokensTree.SelectedItem;
        if (selectedItem is null || selectedItem.Tag is null)
            return;
        var location = (SourceLocation) selectedItem.Tag;

        codeText.Focus ();
        codeText.Select (location.StartPos, location.Length);
        codeText.UpdateLayout ();
        codeText.ScrollTo (location.Line, location.Column);

        e.Handled = true;
    }

    #endregion
}
