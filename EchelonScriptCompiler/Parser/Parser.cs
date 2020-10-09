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
using System.Linq;
using EchelonScriptCompiler.Utilities;

namespace EchelonScriptCompiler.Parser {
    public class EchelonScriptParser : IDisposable {
        #region ================== Static properties

        public static string [] PrimitiveTypes { get; private set; }
        public static string [] Keywords { get; private set; }

        #endregion

        #region ================== Instance fields

        protected List<EchelonScriptErrorMessage> errorsList;
        protected EchelonScriptTokenizer tokenizer;

        #endregion

        #region ================== Instance properties

        public IReadOnlyList<EchelonScriptErrorMessage> Errors { get => errorsList; }

        #endregion

        #region ================== Constructors

        static EchelonScriptParser () {
            PrimitiveTypes = new string [] {
                "int64" , "int32"  , "int16"  , "int8" ,
                "uint64", "uint32" , "uint16" , "uint8",
                "char"  , "float32", "float64", "bool" ,
                "object",
            };

            Keywords = new string [] {
                "using",
                "alias",
                "namespace",

                "var",
                "new",
                "cast",

                "const",
                "immutable",

                "public",
                "protected",
                "internal",
                "private",

                "static",

                "class",
                "struct",
                "enum",

                "if",
                "while",
                "for",
                "return",
            };
        }

        public EchelonScriptParser () {
            errorsList = new List<EchelonScriptErrorMessage> ();
            tokenizer = new EchelonScriptTokenizer (errorsList);
        }

        #endregion

        #region ================== Instance methods

        #region Public methods

        public void Reset () {
            CheckDisposed ();

            errorsList.Clear ();
            tokenizer.Reset ();
        }

        public ES_AstTree ParseCode (ReadOnlyMemory<char> codeData) {
            CheckDisposed ();

            Reset ();
            tokenizer.SetSource (codeData);

            var astTree = ParseCodeUnit ();

            return astTree;
        }

        #endregion

        #region Parsing methods

        protected ES_AstTree ParseCodeUnit () {
            using var importsList = new StructPooledList<ES_AstImportStatement> (ClearMode.Auto);
            using var aliasesList = new StructPooledList<ES_AstTypeAlias> (ClearMode.Auto);
            using var namespacesList = new StructPooledList<ES_AstNamespace> (ClearMode.Auto);

            var tkPair = tokenizer.PeekNextToken ();
            for (; tkPair.tk.Type != EchelonScriptTokenType.EOF; tkPair = tokenizer.PeekNextToken ()) {
                if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
                    tkPair = tokenizer.NextToken ();
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected \"using\", \"alias\" or \"namespace\", got {tkPair.tk.Text}."));
                    continue;
                }

                var textSpan = tkPair.tk.Text.Span;
                if (textSpan.Equals ("using", StringComparison.Ordinal)) {
                    var importStatement = ParseImportStatement ();
                    if (importStatement != null)
                        importsList.Add (importStatement);
                } else if (textSpan.Equals ("alias", StringComparison.Ordinal)) {
                    var aliasStatement = ParseAliasStatement ();
                    if (aliasStatement != null)
                        aliasesList.Add (aliasStatement);
                } else if (textSpan.Equals ("namespace", StringComparison.Ordinal)) {
                    var namespaceNode = ParseNamespace ();
                    if (namespaceNode != null)
                        namespacesList.Add (namespaceNode);
                } else {
                    tokenizer.NextToken ();
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected \"using\", \"alias\" or \"namespace\", got \"{tkPair.tk.Text}\"."));
                }
            }

            return new ES_AstTree () {
                ImportStatements = importsList.ToArray (),
                TypeAliases = aliasesList.ToArray (),
                Namespaces = namespacesList.ToArray (),
            };
        }

        protected enum EnsureTokenResult {
            Correct,
            WrongType,
            WrongText,
        }

        protected EnsureTokenResult EnsureToken (EchelonScriptToken tk, EchelonScriptTokenType type, string text) {
            if (tk.Type != type)
                return EnsureTokenResult.WrongType;

            if (text != null && !tk.Text.Span.Equals (text, StringComparison.Ordinal))
                return EnsureTokenResult.WrongText;

            return EnsureTokenResult.Correct;
        }

        protected EnsureTokenResult EnsureTokenPeek (EchelonScriptTokenType type, string text) {
            var tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type != type)
                return EnsureTokenResult.WrongType;

            if (text != null && !tkPair.tk.Text.Span.Equals (text, StringComparison.Ordinal))
                return EnsureTokenResult.WrongText;

            return EnsureTokenResult.Correct;
        }

        protected ReadOnlyMemory<char>? ParseIdentifier () {
            var tkPair = tokenizer.NextToken ();

            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected an identifier, got \"{tkPair.tk.Text}\"."));
                return null;
            }

            return tkPair.tk.Text;
        }

        protected ReadOnlyMemory<char>? ParseUserIdentifier () {
            var tkPair = tokenizer.NextToken ();

            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected an identifier, got \"{tkPair.tk.Text}\"."));
                return null;
            }

            var idSpan = tkPair.tk.Text.Span;
            foreach (var keyword in Keywords) {
                if (idSpan.Equals (keyword, StringComparison.Ordinal)) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, "User identifiers cannot be named the same as a keyword."));
                    return null;
                }
            }
            foreach (var keyword in PrimitiveTypes) {
                if (idSpan.Equals (keyword, StringComparison.Ordinal)) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, "User identifiers cannot be named the same as a primitive type."));
                    return null;
                }
            }

            return tkPair.tk.Text;
        }

        protected ES_AstDottableIdentifier ParseDottableIdentifier () {
            if (EnsureTokenPeek (EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            using var partsList = new StructPooledList<ReadOnlyMemory<char>> (ClearMode.Auto);

            var id = ParseUserIdentifier ();
            if (id != null)
                partsList.Add (id.Value);

            var tkPair = tokenizer.PeekNextToken ();
            while (tkPair.tk.Type == EchelonScriptTokenType.Dot) {
                tokenizer.NextToken ();

                id = ParseUserIdentifier ();
                if (id != null)
                    partsList.Add (id.Value);

                tkPair = tokenizer.PeekNextToken ();
            }

            return new ES_AstDottableIdentifier () { Parts = partsList.ToArray () };
        }

        protected ES_AstNamespace ParseNamespace () {
            if (EnsureToken (tokenizer.NextToken ().tk, EchelonScriptTokenType.Identifier, "namespace") != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected an identifier, got \"{tkPair.tk.Text}\"."));
                return null;
            }

            var namespaceName = ParseDottableIdentifier ();
            var namespaceContents = new StructPooledList<ES_AstTreeNode> (ClearMode.Auto);

            tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected '=', got \"{tkPair.tk.Text}\"."));
                return null;
            }

            for (tkPair = tokenizer.PeekNextToken (); tkPair.tk.Type != EchelonScriptTokenType.EOF; tkPair = tokenizer.PeekNextToken ()) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                    tokenizer.NextToken ();
                    break;
                } else {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected a keyword or '}}', got \"{tkPair.tk.Text}\"."));
                    tokenizer.NextToken ();
                }
            }

            return new ES_AstNamespace () { NamespaceName = namespaceName, Contents = namespaceContents.ToArray () };
        }

        #region Statements

        protected ES_AstImportStatement ParseImportStatement () {
            if (EnsureToken (tokenizer.NextToken ().tk, EchelonScriptTokenType.Identifier, "using") != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected an identifier, got \"{tkPair.tk.Text}\"."));
                return null;
            }

            var namespaceName = ParseDottableIdentifier ();
            using var importedNamesList = new StructPooledList<ReadOnlyMemory<char>> (ClearMode.Auto);

            tkPair = tokenizer.PeekNextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.Colon) {
                tokenizer.NextToken ();

                var name = ParseUserIdentifier ();
                if (name != null)
                    importedNamesList.Add (name.Value);

                while (true) {
                    tkPair = tokenizer.PeekNextToken ();

                    if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
                        tokenizer.NextToken ();
                        break;
                    } else if (tkPair.tk.Type != EchelonScriptTokenType.Comma) {
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected ',' or ';', got \"{tkPair.tk.Text}\"."));
                        break;
                    } else
                        tokenizer.NextToken ();

                    name = ParseUserIdentifier ();

                    if (name != null)
                        importedNamesList.Add (name.Value);
                }
            } else if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
                tokenizer.NextToken ();
            } else
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected ',' or ';', got \"{tkPair.tk.Text}\"."));

            return new ES_AstImportStatement () { NamespaceName = namespaceName, ImportedNames = importedNamesList.ToArray () };
        }

        protected ES_AstTypeAlias ParseAliasStatement () {
            if (EnsureToken (tokenizer.NextToken ().tk, EchelonScriptTokenType.Identifier, "alias") != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var aliasName = ParseUserIdentifier ();
            if (aliasName == null)
                return null;

            var tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Equals, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected '=', got \"{tkPair.tk.Text}\"."));
                return null;
            }

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected an identifier, got \"{tkPair.tk.Text}\"."));
                return null;
            }
            var originalName = ParseDottableIdentifier ();

            tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, $"Expected ';', got \"{tkPair.tk.Text}\"."));

            return new ES_AstTypeAlias () { AliasName = aliasName.Value, OriginalName = originalName };
        }

        #endregion

        #endregion

        #region Protected methods

        protected void CheckDisposed () {
            if (IsDisposed)
                throw new ObjectDisposedException (null);
        }

        #endregion

        #endregion

        #region ================== IDisposable Support

        public bool IsDisposed { get; private set; }

        protected virtual void DoDispose () {
            if (!IsDisposed) {

                IsDisposed = true;
            }
        }

        public void Dispose () {
            DoDispose ();
        }

        #endregion
    }
}
