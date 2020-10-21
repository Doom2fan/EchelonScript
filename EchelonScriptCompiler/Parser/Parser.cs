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
using System.Reflection;
using EchelonScriptCompiler.Utilities;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Parser {
    public struct ES_AggregateModifiers {
        public ES_AccessModifier? AccessModifier;
        public bool? Static;
        public bool? Const;

        public EchelonScriptToken? DocComment;

        public void ResetToNull () {
            AccessModifier = null;
            Static = null;
            Const = null;

            DocComment = null;
        }

        public void CopyDefaultsToUndefined (ES_AggregateModifiers defaults) {
            if (AccessModifier == null)
                AccessModifier = defaults.AccessModifier;

            if (Static == null)
                Static = defaults.Static;

            if (Const == null)
                Const = defaults.Const;
        }

        public bool AnyUndefined () {
            return !AccessModifier.HasValue || !Static.HasValue || !Const.HasValue;
        }

        public bool AnySet () {
            return AccessModifier.HasValue || Static.HasValue || Const.HasValue;
        }
    }

    public static class ES_PrimitiveTypes {
        public const string Object = "object";
        public const string Bool = "bool";

        public const string Int64 = "int64";
        public const string Int32 = "int32";
        public const string Int16 = "int16";
        public const string Int8  = "int8";

        public const string UInt64 = "int64";
        public const string UInt32 = "int32";
        public const string UInt16 = "int16";
        public const string UInt8  = "int8";

        public const string Float32 = "float32";
        public const string Float64 = "float64";

        public const string String = "string";
        public const string Char = "char";
    }

    public static class ES_Keywords {
        public const string Using = "using";
        public const string Alias = "alias";
        public const string Namespace = "namespace";

        public const string Var = "var";
        public const string New = "new";
        public const string Cast = "cast";

        public const string Const = "const";
        public const string Immutable = "immutable";

        public const string Public = "public";
        public const string Protected = "protected";
        public const string Internal = "internal";
        public const string Private = "private";

        public const string Static = "static";

        public const string Class = "class";
        public const string Struct = "struct";
        public const string Enum = "enum";

        public const string If = "if";
        public const string While = "while";
        public const string For = "for";
        public const string Return = "return";
    }

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
            // Primitive types
            using var primitiveTypesList = new StructPooledList<string> (ClearMode.Auto);
            var primitiveTypesConstants = typeof (ES_PrimitiveTypes).GetFields (BindingFlags.Public | BindingFlags.Static);

            foreach (var primitiveConst in primitiveTypesConstants) {
                if (primitiveConst.IsLiteral && !primitiveConst.IsInitOnly)
                    primitiveTypesList.Add (primitiveConst.GetValue (null) as string);
            }

            PrimitiveTypes = primitiveTypesList.ToArray ();

            // Keywords
            using var keywordsList = new StructPooledList<string> (ClearMode.Auto);
            var keywordsConstants = typeof (ES_Keywords).GetFields (BindingFlags.Public | BindingFlags.Static);

            foreach (var keywordConst in keywordsConstants) {
                if (keywordConst.IsLiteral && !keywordConst.IsInitOnly)
                    keywordsList.Add (keywordConst.GetValue (null) as string);
            }

            Keywords = keywordsList.ToArray ();
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

        #region Utilities

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

        protected bool CheckNoAccessModifierErrors (EchelonScriptToken tk, ES_AggregateModifiers currentModifiers) {
            if (currentModifiers.Static == true) {
                errorsList.Add (new EchelonScriptErrorMessage (tk, ES_Errors.AccessBeforeStorage));
                return false;
            }
            if (currentModifiers.AccessModifier != null) {
                errorsList.Add (new EchelonScriptErrorMessage (tk, ES_Errors.MultipleAccessMods));
                return false;
            }

            return true;
        }

        protected void ParseAggregate_SetDocComment (ref ES_AggregateModifiers curMods, EchelonScriptToken? newDocCom) {
            if (newDocCom == null)
                return;

            if (curMods.AnySet ()) {
                errorsList.Add (new EchelonScriptErrorMessage (newDocCom.Value, ES_Errors.UnexpectedDocComment));
                return;
            }

            curMods.DocComment = newDocCom;
        }

        protected bool IsIntegerLiteral (EchelonScriptToken tk) {
            return (
                tk.Type == EchelonScriptTokenType.DecIntegerLiteral ||
                tk.Type == EchelonScriptTokenType.BinIntegerLiteral ||
                tk.Type == EchelonScriptTokenType.HexIntegerLiteral 
            );
        }

        protected bool IsFloatLiteral (EchelonScriptToken tk) {
            return (
                tk.Type == EchelonScriptTokenType.FloatLiteral ||
                tk.Type == EchelonScriptTokenType.DecIntegerLiteral
            );
        }

        protected bool IsKeyword (EchelonScriptToken tk) {
            var idSpan = tk.Text.Span;

            foreach (var keyword in Keywords) {
                if (idSpan.Equals (keyword, StringComparison.Ordinal))
                    return true;
            }

            return false;
        }

        protected bool IsPrimitiveType (EchelonScriptToken tk) {
            var idSpan = tk.Text.Span;

            foreach (var type in PrimitiveTypes) {
                if (idSpan.Equals (type, StringComparison.Ordinal))
                    return true;
            }

            return false;
        }

        #endregion

        #region Basic data

        protected EchelonScriptToken? ParseIdentifier () {
            var tkPair = tokenizer.NextToken ();

            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedIdentifier (tkPair.tk)));
                return null;
            }

            return tkPair.tk;
        }

        protected bool ParseUserIdentifier (out EchelonScriptToken token) {
            var tkPair = tokenizer.NextToken ();
            token = tkPair.tk;

            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedIdentifier (tkPair.tk)));
                return false;
            }

            if (IsKeyword (tkPair.tk)) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.InvalidUserIdentifier));
                return false;
            }

            if (IsPrimitiveType (tkPair.tk)) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.InvalidUserIdentifier));
                return false;
            }

            return true;
        }

        protected ES_AstDottableIdentifier ParseDottableIdentifier () {
            if (EnsureTokenPeek (EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            using var partsList = new StructPooledList<EchelonScriptToken> (ClearMode.Auto);

            var id = ParseIdentifier ();
            if (id != null)
                partsList.Add (id.Value);

            var tkPair = tokenizer.PeekNextToken ();
            while (tkPair.tk.Type == EchelonScriptTokenType.Dot) {
                tokenizer.NextToken ();

                id = ParseIdentifier ();
                if (id != null)
                    partsList.Add (id.Value);

                tkPair = tokenizer.PeekNextToken ();
            }

            return new ES_AstDottableIdentifier () { Parts = partsList.ToArray () };
        }

        protected ES_AstDottableIdentifier ParseDottableUserIdentifier () {
            var ret = ParseDottableIdentifier ();

            foreach (var part in ret.Parts) {
                if (IsKeyword (part))
                    errorsList.Add (new EchelonScriptErrorMessage (part, ES_Errors.InvalidUserIdentifier));

                if (IsPrimitiveType (part))
                    errorsList.Add (new EchelonScriptErrorMessage (part, ES_Errors.InvalidUserIdentifier));
            }

            return ret;
        }

        protected ES_AstTypeDeclaration ParseTypeDeclaration () {
            ES_AstTypeDeclaration innerDecl = null;

            while (true) {
                var tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                    bool isConst = tkPair.tk.Text.Span.Equals (ES_Keywords.Const, StringComparison.Ordinal);
                    bool isImmutable = tkPair.tk.Text.Span.Equals (ES_Keywords.Immutable, StringComparison.Ordinal);

                    if (isConst || isImmutable) {
                        tokenizer.NextToken ();

                        tkPair = tokenizer.NextToken ();
                        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("'('", tkPair.tk)));

                        var innerType = ParseTypeDeclaration ();

                        tkPair = tokenizer.PeekNextToken ();
                        if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("')'", tkPair.tk)));
                        else
                            tokenizer.NextToken ();

                        var typeDecl = new ES_AstTypeDeclaration_Basic ();
                        typeDecl.Inner = innerType;

                        if (isConst) typeDecl.Type = ES_AstTypeDeclaration_Basic.DeclType.Const;
                        else if (isImmutable) typeDecl.Type = ES_AstTypeDeclaration_Basic.DeclType.Immutable;

                        innerDecl = typeDecl;
                    } else if (innerDecl == null) {
                        var dottableId = ParseDottableIdentifier ();
                        innerDecl = new ES_AstTypeDeclaration_TypeName { TypeName = dottableId, };
                    } else
                        break;
                } else if (innerDecl == null && tkPair.tk.Type == EchelonScriptTokenType.ParenOpen) {
                    tokenizer.NextToken ();

                    var innerType = ParseTypeDeclaration ();

                    tkPair = tokenizer.PeekNextToken ();
                    if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenClose, null) != EnsureTokenResult.Correct)
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("')'", tkPair.tk)));
                    else
                        tokenizer.NextToken ();

                    innerDecl = innerType;
                } else if (tkPair.tk.Type == EchelonScriptTokenType.Asterisk) {
                    if (innerDecl == null) {
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenUnexpectedToken (tkPair.tk)));
                        break;
                    }

                    innerDecl = new ES_AstTypeDeclaration_Basic {
                        Type = ES_AstTypeDeclaration_Basic.DeclType.Pointer,
                        Inner = innerDecl,
                    };
                } else if (tkPair.tk.Type == EchelonScriptTokenType.Question) {
                    if (innerDecl == null) {
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenUnexpectedToken (tkPair.tk)));
                        break;
                    }

                    innerDecl = new ES_AstTypeDeclaration_Basic {
                        Type = ES_AstTypeDeclaration_Basic.DeclType.Nullable,
                        Inner = innerDecl,
                    };
                } else if (tkPair.tk.Type == EchelonScriptTokenType.BracketOpen) {
                    if (innerDecl == null) {
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenUnexpectedToken (tkPair.tk)));
                        break;
                    }

                    using var ranks = new StructPooledList<ES_AstExpression?> (ClearMode.Auto);

                    ES_AstExpression lastDim = null;
                    while (true) {
                        tkPair = tokenizer.PeekNextToken ();

                        if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                            break;
                        }

                        if (tkPair.tk.Type == EchelonScriptTokenType.BracketClose) {
                            tokenizer.NextToken ();

                            ranks.Add (lastDim);
                            lastDim = null;

                            break;
                        } else if (tkPair.tk.Type == EchelonScriptTokenType.Comma) {
                            tokenizer.NextToken ();

                            ranks.Add (lastDim);
                            lastDim = null;
                        } else
                            lastDim = ParseExpression ();
                    }

                    innerDecl = new ES_AstTypeDeclaration_Array {
                        Inner = innerDecl,
                        Dimensions = ranks.ToArray (),
                    };
                } else
                    break;
            }

            return innerDecl;
        }

        #endregion

        protected ES_AstTree ParseCodeUnit () {
            using var importsList = new StructPooledList<ES_AstImportStatement> (ClearMode.Auto);
            using var aliasesList = new StructPooledList<ES_AstTypeAlias> (ClearMode.Auto);
            using var namespacesList = new StructPooledList<ES_AstNamespace> (ClearMode.Auto);

            while (true) {
                var tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF)
                    break;

                if (tkPair.tk.Type != EchelonScriptTokenType.Identifier) {
                    tkPair = tokenizer.NextToken ();
                    var tokenText = StringPool.Shared.GetOrAdd (tkPair.tk.Text.Span);
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.CodeUnit_UnexpectedToken.Replace ("{0}", tokenText)));
                    continue;
                }

                var textSpan = tkPair.tk.Text.Span;
                if (textSpan.Equals ("using", StringComparison.Ordinal)) {
                    if (namespacesList.Count > 0)
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.ImportAfterNamespace));

                    var importStatement = ParseImportStatement ();
                    if (importStatement != null)
                        importsList.Add (importStatement);
                } else if (textSpan.Equals ("alias", StringComparison.Ordinal)) {
                    if (namespacesList.Count > 0)
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.AliasAfterNamespace));

                    var aliasStatement = ParseAliasStatement ();
                    if (aliasStatement != null)
                        aliasesList.Add (aliasStatement);
                } else if (textSpan.Equals ("namespace", StringComparison.Ordinal)) {
                    var namespaceNode = ParseNamespace ();
                    if (namespaceNode != null)
                        namespacesList.Add (namespaceNode);
                } else {
                    tokenizer.NextToken ();
                    var tokenText = StringPool.Shared.GetOrAdd (tkPair.tk.Text.Span);
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.CodeUnit_UnexpectedToken.Replace ("{0}", tokenText)));
                }
            }

            return new ES_AstTree () {
                Valid = errorsList.Count == 0,
                ImportStatements = importsList.ToArray (),
                TypeAliases = aliasesList.ToArray (),
                Namespaces = namespacesList.ToArray (),
            };
        }

        protected ES_AstNamespace ParseNamespace () {
            if (EnsureToken (tokenizer.NextToken ().tk, EchelonScriptTokenType.Identifier, ES_Keywords.Namespace) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedIdentifier (tkPair.tk)));
                return null;
            }

            var defaultModifiers = new ES_AggregateModifiers {
                AccessModifier = ES_AccessModifier.Private,
                Static = false,
                Const = false,
            };

            var namespaceName = ParseDottableUserIdentifier ();
            var namespaceContents = ParseAggregateOrNamespace (null, defaultModifiers, true);

            return new ES_AstNamespace () { NamespaceName = namespaceName, Contents = namespaceContents };
        }

        #region Aggregates

        protected ES_AstTreeNode [] ParseAggregateOrNamespace (
            ES_AggregateModifiers? baseModifiersArg, ES_AggregateModifiers defaultModifiers, bool isNamespace
        ) {
            ES_AggregateModifiers baseModifiers;
            if (baseModifiersArg == null) {
                baseModifiers = new ES_AggregateModifiers ();
                baseModifiers.ResetToNull ();
            } else
                baseModifiers = baseModifiersArg.Value;

            var tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("'{'", tkPair.tk)));
                return Array.Empty<ES_AstTreeNode> ();
            }

            using var contents = new StructPooledList<ES_AstTreeNode> (ClearMode.Auto);
            ES_AggregateModifiers currentModifiers = new ES_AggregateModifiers ();
            currentModifiers.ResetToNull ();

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                bool abortLoop = false;

                if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                    tokenizer.NextToken ();
                    if (currentModifiers.AnySet ())
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedAggregateContent (tkPair.tk)));

                    break;
                } else if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                    var textSpan = tkPair.tk.Text.Span;
                    var textString = StringPool.Shared.GetOrAdd (textSpan);

                    switch (textString) {
                        /* Access modifiers */
                        case ES_Keywords.Public:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                            tokenizer.NextToken ();

                            if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                                currentModifiers.AccessModifier = ES_AccessModifier.Public;
                            break;
                        case ES_Keywords.Protected:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                            tokenizer.NextToken ();
                            if (isNamespace) {
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.InvalidAccessModForNamespaceContent));
                                break;
                            }

                            tkPair = tokenizer.PeekNextToken ();
                            if (tkPair.tk.Type == EchelonScriptTokenType.Identifier && tkPair.tk.Text.Span.Equals ("internal", StringComparison.Ordinal)) {
                                tokenizer.NextToken ();

                                if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                                    currentModifiers.AccessModifier = ES_AccessModifier.ProtectedInternal;
                            } else {
                                if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                                    currentModifiers.AccessModifier = ES_AccessModifier.Protected;
                            }
                            break;
                        case ES_Keywords.Internal:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                            tokenizer.NextToken ();

                            if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                                currentModifiers.AccessModifier = ES_AccessModifier.Internal;
                            break;
                        case ES_Keywords.Private:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                            tokenizer.NextToken ();
                            if (isNamespace) {
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.InvalidAccessModForNamespaceContent));
                                break;
                            } else if (CheckNoAccessModifierErrors (tkPair.tk, currentModifiers) && CheckNoAccessModifierErrors (tkPair.tk, baseModifiers))
                                currentModifiers.AccessModifier = ES_AccessModifier.Private;
                            break;

                        /* Other modifiers */
                        case ES_Keywords.Static:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                            tokenizer.NextToken ();

                            if (currentModifiers.Static == true)
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.ElementAlreadyStatic));

                            currentModifiers.Static = true;
                            break;
                        case ES_Keywords.Const:
                            if (tokenizer.PeekNextToken (1).tk.Type != EchelonScriptTokenType.ParenOpen) {
                                ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);
                                tokenizer.NextToken ();

                                if (currentModifiers.Const == true)
                                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.ElementAlreadyConst));

                                currentModifiers.Const = true;
                                break;
                            } else
                                goto ParseFunctionOrVariable;

                        case ES_Keywords.Namespace:
                            abortLoop = true;
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.NamespaceMissingBrace));
                            break;

                        case ES_Keywords.Class:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                            currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                            contents.Add (ParseClass (currentModifiers, true));

                            currentModifiers.ResetToNull ();
                            break;

                        case ES_Keywords.Struct:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                            currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                            contents.Add (ParseStruct (currentModifiers, true));

                            currentModifiers.ResetToNull ();
                            break;

                        case ES_Keywords.Enum:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                            currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                            contents.Add (ParseEnum (currentModifiers, true));

                            currentModifiers.ResetToNull ();
                            break;

                        case ES_Keywords.Immutable:
                            goto ParseFunctionOrVariable;

                        default:
                            goto ParseFunctionOrVariable;

                        ParseFunctionOrVariable:
                            ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                            currentModifiers.CopyDefaultsToUndefined (baseModifiers);
                            currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                            if (!isNamespace)
                                contents.AddRange (ParseFunctionOrVariable (currentModifiers));
                            else
                                contents.Add (ParseFunction (currentModifiers, null));

                            currentModifiers.ResetToNull ();
                            break;
                    }
                } else {
                    if (tkPair.tk.Type == EchelonScriptTokenType.ParenOpen) {
                        ParseAggregate_SetDocComment (ref currentModifiers, tkPair.doc);

                        currentModifiers.CopyDefaultsToUndefined (defaultModifiers);
                        if (!isNamespace)
                            contents.AddRange (ParseFunctionOrVariable (currentModifiers));
                        else
                            contents.Add (ParseFunction (currentModifiers, null));

                        currentModifiers.ResetToNull ();
                    } else {
                        if (!isNamespace && tkPair.tk.Type == EchelonScriptTokenType.BraceOpen && currentModifiers.AnySet ()) {
                            currentModifiers.CopyDefaultsToUndefined (baseModifiers);

                            var newContents = ParseAggregateOrNamespace (currentModifiers, defaultModifiers, false);
                            contents.AddRange (newContents);

                            currentModifiers.ResetToNull ();
                        } else {
                            tokenizer.NextToken ();

                            if (!isNamespace)
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("a keyword, function definition, variable definition or '}'", tkPair.tk)));
                            else
                                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("a keyword, function definition or '}'", tkPair.tk)));
                        }
                    }
                }

                if (abortLoop)
                    break;
            }

            return contents.ToArray ();
        }

        protected ES_AstDottableIdentifier [] ParseInheritanceList () {
            var tkPair = tokenizer.PeekNextToken ();

            if (tkPair.tk.Type != EchelonScriptTokenType.Colon)
                return null;
            tokenizer.NextToken ();

            using var listContents = new StructPooledList<ES_AstDottableIdentifier> (ClearMode.Auto);

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedIdentifier (tkPair.tk)));
                else
                    listContents.Add (ParseDottableUserIdentifier ());

                tkPair = tokenizer.PeekNextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.Comma)
                    tokenizer.NextToken ();
                else
                    break;
            }

            return listContents.ToArray ();
        }

        protected ES_AstClassDefinition ParseClass (ES_AggregateModifiers classModifiers, bool parseStartKeyword) {
            // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
            if (classModifiers.AnyUndefined ())
                throw new ArgumentException ("Modifiers set contains undefined values.", nameof (classModifiers));

            var defaultModifiers = new ES_AggregateModifiers {
                AccessModifier = ES_AccessModifier.Private,
                Static = false,
                Const = false,
            };

            // Parse the 'class' keyword.
            if (parseStartKeyword) {
                var kwTkPair = tokenizer.NextToken ();
                if (EnsureToken (kwTkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.Class) != EnsureTokenResult.Correct)
                    errorsList.Add (new EchelonScriptErrorMessage (kwTkPair.tk, ES_Errors.GenExpectedXGotY ($"\"{ES_Keywords.Class}\"", kwTkPair.tk)));
            }

            // Parse the class' name.
            var nameTkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (nameTkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                errorsList.Add (new EchelonScriptErrorMessage (nameTkPair.tk, ES_Errors.GenExpectedIdentifier (nameTkPair.tk)));
            else
                tokenizer.NextToken ();

            if (classModifiers.Const == true)
                errorsList.Add (new EchelonScriptErrorMessage (nameTkPair.tk, ES_Errors.ConstOnlyOnFunctions));
            classModifiers.Const = false;

            // Error out if this is a static class and it has an inheritance list.
            var colonTkPair = tokenizer.PeekNextToken ();
            if (classModifiers.Static == true && colonTkPair.tk.Type == EchelonScriptTokenType.Colon)
                errorsList.Add (new EchelonScriptErrorMessage (colonTkPair.tk, ES_Errors.InheritanceOnStaticClass));

            // Parse the inheritance list.
            var inheritanceList = ParseInheritanceList ();
            if (inheritanceList == null)
                inheritanceList = Array.Empty<ES_AstDottableIdentifier> ();

            // Parse the class' contents.
            var contents = ParseAggregateOrNamespace (null, defaultModifiers, false);

            // Create the class AST node.
            var classDef = new ES_AstClassDefinition {
                DocComment = classModifiers.DocComment,

                AccessModifier = classModifiers.AccessModifier.Value,
                Static = classModifiers.Static.Value,

                Name = nameTkPair.tk,
                InheritanceList = inheritanceList,

                Contents = contents,
            };

            return classDef;
        }

        protected ES_AstStructDefinition ParseStruct (ES_AggregateModifiers structModifiers, bool parseStartKeyword) {
            // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
            if (structModifiers.AnyUndefined ())
                throw new ArgumentException ("Modifiers set contains undefined values.", nameof (structModifiers));

            var defaultModifiers = new ES_AggregateModifiers {
                AccessModifier = ES_AccessModifier.Private,
                Static = false,
                Const = false,
            };

            // Parse the 'struct' keyword.
            if (parseStartKeyword) {
                var kwTkPair = tokenizer.NextToken ();
                if (EnsureToken (kwTkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.Struct) != EnsureTokenResult.Correct)
                    errorsList.Add (new EchelonScriptErrorMessage (kwTkPair.tk, ES_Errors.GenExpectedXGotY ($"\"{ES_Keywords.Struct}\"", kwTkPair.tk)));
            }

            // Parse the struct's name.
            var nameTkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (nameTkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct)
                errorsList.Add (new EchelonScriptErrorMessage (nameTkPair.tk, ES_Errors.GenExpectedIdentifier (nameTkPair.tk)));
            else
                tokenizer.NextToken ();

            if (structModifiers.Const == true)
                errorsList.Add (new EchelonScriptErrorMessage (nameTkPair.tk, ES_Errors.ConstOnlyOnFunctions));
            structModifiers.Const = false;

            // Error out if this is a static struct and it has an inheritance list.
            var colonTkPair = tokenizer.PeekNextToken ();
            if (structModifiers.Static == true && colonTkPair.tk.Type == EchelonScriptTokenType.Colon)
                errorsList.Add (new EchelonScriptErrorMessage (colonTkPair.tk, ES_Errors.InheritanceOnStaticStruct));

            // Parse the inheritance list.
            var interfacesList = ParseInheritanceList ();
            if (interfacesList == null)
                interfacesList = Array.Empty<ES_AstDottableIdentifier> ();

            // Parse the struct's contents
            var contents = ParseAggregateOrNamespace (null, defaultModifiers, false);

            // Create the struct AST node.
            var structDef = new ES_AstStructDefinition {
                DocComment = structModifiers.DocComment,

                AccessModifier = structModifiers.AccessModifier.Value,
                Static = structModifiers.Static.Value,

                Name = nameTkPair.tk,
                InterfacesList = interfacesList,

                Contents = contents,
            };

            return structDef;
        }

        #endregion

        protected ES_AstEnumDefinition ParseEnum (ES_AggregateModifiers enumModifiers, bool parseStartKeyword) {
            // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
            if (enumModifiers.AnyUndefined ())
                throw new ArgumentException ("Modifiers set contains undefined values.", nameof (enumModifiers));

            // Parse the 'enum' keyword.
            if (parseStartKeyword) {
                var kwTkPair = tokenizer.NextToken ();
                if (EnsureToken (kwTkPair.tk, EchelonScriptTokenType.Identifier, ES_Keywords.Enum) != EnsureTokenResult.Correct)
                    errorsList.Add (new EchelonScriptErrorMessage (kwTkPair.tk, ES_Errors.GenExpectedXGotY ($"\"{ES_Keywords.Enum}\"", kwTkPair.tk)));
            }

            // Parse the enum's name.
            ParseUserIdentifier (out var nameToken);

            if (enumModifiers.Const == true)
                errorsList.Add (new EchelonScriptErrorMessage (nameToken, ES_Errors.ConstOnlyOnFunctions));
            enumModifiers.Const = false;

            // Parse the enum's type, if any.
            EchelonScriptToken? baseType = null;
            if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.Colon) {
                tokenizer.NextToken ();

                baseType = ParseIdentifier ();
            }

            // Parse the opening brace.
            {
                var openBraceTkPair = tokenizer.NextToken ();
                if (EnsureToken (openBraceTkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct)
                    errorsList.Add (new EchelonScriptErrorMessage (openBraceTkPair.tk, ES_Errors.GenExpectedXGotY ("'{'", openBraceTkPair.tk)));
            }

            // Parse the enum's members
            using var membersList = new StructPooledList<(EchelonScriptToken, ES_AstExpression)> (ClearMode.Auto);

            bool expectingEnd = false;
            while (true) {
                var tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                    tokenizer.NextToken ();
                    break;
                } else if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                    ParseUserIdentifier (out var memberName);
                    ES_AstExpression assignExpr = null;

                    // Error out if the previous member had no comma.
                    if (expectingEnd)
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("'}'", tkPair.tk)));

                    // Read the value expression
                    tkPair = tokenizer.PeekNextToken ();
                    if (tkPair.tk.Type == EchelonScriptTokenType.Equals) {
                        tokenizer.NextToken ();

                        assignExpr = ParseExpression ();
                    }

                    // Check for a comma.
                    tkPair = tokenizer.PeekNextToken ();
                    if (tkPair.tk.Type == EchelonScriptTokenType.Comma)
                        tokenizer.NextToken ();
                    else
                        expectingEnd = true;

                    membersList.Add ((memberName, assignExpr));
                } else {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("a keyword or '}'", tkPair.tk)));
                    tokenizer.NextToken ();
                }
            }

            // Create the enum AST node.
            var enumDef = new ES_AstEnumDefinition {
                AccessModifier = enumModifiers.AccessModifier.Value,
                DocComment = enumModifiers.DocComment,

                Name = nameToken,
                BaseType = baseType,

                MembersList = membersList.ToArray (),
            };

            return enumDef;
        }

        protected ES_AstTreeNode [] ParseFunctionOrVariable (ES_AggregateModifiers modifiers) {
            var typeDecl = ParseTypeDeclaration ();

            var nextTkPair = tokenizer.PeekNextToken (1);
            if (nextTkPair.tk.Type == EchelonScriptTokenType.ParenOpen)
                return new ES_AstTreeNode [] { ParseFunction (modifiers, typeDecl) };
            else
                return ParseMemberVar (modifiers, typeDecl);
        }

        protected ES_AstFunctionDefinition ParseFunction (ES_AggregateModifiers funcModifiers, ES_AstTypeDeclaration returnType) {
            // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
            if (funcModifiers.AnyUndefined ())
                throw new ArgumentException ("Modifiers set contains undefined values.", nameof (funcModifiers));

            if (returnType == null)
                returnType = ParseTypeDeclaration ();

            ParseUserIdentifier (out var functionName);

            var argumentsList = ParseArgumentsList ();

            var statements = ParseStatementsBlock ();

            var functionDef = new ES_AstFunctionDefinition {
                AccessModifier = funcModifiers.AccessModifier.Value,
                DocComment = funcModifiers.DocComment,

                Static = funcModifiers.Static.Value,
                Const = funcModifiers.Const.Value,

                Name = functionName,
                ReturnType = returnType,
                ArgumentsList = argumentsList,

                StatementsList = statements,
            };

            return functionDef;
        }

        protected ES_AstMemberVarDefinition [] ParseMemberVar (ES_AggregateModifiers varModifiers, ES_AstTypeDeclaration valueType) {
            // Check if all the modifiers are defined/set. (If not, someone forgot to fill them with the defaults.)
            if (varModifiers.AnyUndefined ())
                throw new ArgumentException ("Modifiers set contains undefined values.", nameof (varModifiers));

            if (valueType == null)
                valueType = ParseTypeDeclaration ();

            using var variables = new StructPooledList<ES_AstMemberVarDefinition> (ClearMode.Auto);

            while (true) {
                var tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                ParseUserIdentifier (out var functionName);
                ES_AstExpression expression = null;

                if (varModifiers.Const == true)
                    errorsList.Add (new EchelonScriptErrorMessage (functionName, ES_Errors.ConstOnlyOnFunctions));
                varModifiers.Const = false;

                tkPair = tokenizer.PeekNextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.Equals) {
                    tokenizer.NextToken ();

                    expression = ParseExpression ();
                }

                var memberVarDef = new ES_AstMemberVarDefinition {
                    AccessModifier = varModifiers.AccessModifier.Value,
                    DocComment = varModifiers.DocComment,

                    Static = varModifiers.Static.Value,

                    Name = functionName,
                    ValueType = valueType,
                    InitializationExpression = expression,
                };
                variables.Add (memberVarDef);

                tkPair = tokenizer.PeekNextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.Comma)
                    tokenizer.NextToken ();
                else
                    break;
            }

            var semicolonTkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (semicolonTkPair.tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (new EchelonScriptErrorMessage (semicolonTkPair.tk, ES_Errors.GenExpectedXGotY ("';'", semicolonTkPair.tk)));
            else
                tokenizer.NextToken ();

            return variables.ToArray ();
        }

        protected ES_AstFunctionArgument [] ParseArgumentsList () {
            var tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.ParenOpen, null) != EnsureTokenResult.Correct)
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("'('", tkPair.tk)));

            if (tokenizer.PeekNextToken ().tk.Type == EchelonScriptTokenType.ParenClose) {
                tokenizer.NextToken ();
                return Array.Empty<ES_AstFunctionArgument> ();
            }

            using var argsList = new StructPooledList<ES_AstFunctionArgument> (ClearMode.Auto);

            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                var mode = ES_AstFunctionArgument.ArgumentMode.Normal;

                if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                    var textSpan = tkPair.tk.Text.Span;
                    if (textSpan.Equals ("ref", StringComparison.Ordinal))
                        mode = ES_AstFunctionArgument.ArgumentMode.Ref;
                    else if (textSpan.Equals ("in", StringComparison.Ordinal))
                        mode = ES_AstFunctionArgument.ArgumentMode.In;
                    else if (textSpan.Equals ("out", StringComparison.Ordinal))
                        mode = ES_AstFunctionArgument.ArgumentMode.Out;

                    if (mode != ES_AstFunctionArgument.ArgumentMode.Normal) {
                        tokenizer.NextToken ();
                        tkPair = tokenizer.PeekNextToken ();

                        if (tkPair.tk.Type != EchelonScriptTokenType.Identifier)
                            errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedIdentifier (tkPair.tk)));
                    }
                }

                var argumentType = ParseTypeDeclaration ();

                tkPair = tokenizer.NextToken ();
                if (tkPair.tk.Type != EchelonScriptTokenType.Identifier)
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedIdentifier (tkPair.tk)));

                var argumentName = tkPair.tk;
                ES_AstExpression argumentExpr = null;

                tkPair = tokenizer.PeekNextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.Equals) {
                    tokenizer.NextToken ();

                    argumentExpr = ParseExpression ();
                }

                var functionArgDef = new ES_AstFunctionArgument {
                    Mode = mode,
                    ValueType = argumentType,
                    Name = argumentName,
                    DefaultExpression = argumentExpr,
                };
                argsList.Add (functionArgDef);

                tkPair = tokenizer.NextToken ();
                if (tkPair.tk.Type == EchelonScriptTokenType.ParenClose)
                    break;
                else if (tkPair.tk.Type != EchelonScriptTokenType.Comma)
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("',' or ')'", tkPair.tk)));
            }

            return argsList.ToArray ();
        }

        protected ES_AstStatement [] ParseStatementsBlock () {
            var tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.BraceOpen, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("'{'", tkPair.tk)));
                return null;
            }

            using var contents = new StructPooledList<ES_AstStatement> (ClearMode.Auto);
            ES_AggregateModifiers currentModifiers = new ES_AggregateModifiers ();
            currentModifiers.ResetToNull ();

            // TODO: Actually implement statement blocks
            while (true) {
                tkPair = tokenizer.PeekNextToken ();

                if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                    errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                    break;
                }

                if (tkPair.tk.Type == EchelonScriptTokenType.BraceClose) {
                    tokenizer.NextToken ();
                    if (currentModifiers.AnySet ())
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedAggregateContent (tkPair.tk)));

                    break;
                } else if (tkPair.tk.Type == EchelonScriptTokenType.Identifier) {
                    var textSpan = tkPair.tk.Text.Span;
                    var textString = StringPool.Shared.GetOrAdd (textSpan);

                    tokenizer.NextToken ();
                } else {
                    //errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("a keyword or '}'", tkPair.tk)));
                    tokenizer.NextToken ();
                }
            }

            return contents.ToArray ();
        }

        #region Statements

        protected ES_AstImportStatement ParseImportStatement () {
            if (EnsureToken (tokenizer.NextToken ().tk, EchelonScriptTokenType.Identifier, ES_Keywords.Using) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            var tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedIdentifier (tkPair.tk)));
                return null;
            }

            var namespaceName = ParseDottableUserIdentifier ();
            using var importedNamesList = new StructPooledList<EchelonScriptToken> (ClearMode.Auto);

            tkPair = tokenizer.PeekNextToken ();
            if (tkPair.tk.Type == EchelonScriptTokenType.Colon) {
                tokenizer.NextToken ();

                if (ParseUserIdentifier (out var name))
                    importedNamesList.Add (name);

                while (true) {
                    tkPair = tokenizer.PeekNextToken ();

                    if (tkPair.tk.Type == EchelonScriptTokenType.EOF) {
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.UnexpectedEOF));
                        break;
                    }

                    if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
                        tokenizer.NextToken ();
                        break;
                    } else if (tkPair.tk.Type != EchelonScriptTokenType.Comma) {
                        errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("',' or ';'", tkPair.tk)));
                        break;
                    } else
                        tokenizer.NextToken ();

                    if (ParseUserIdentifier (out name))
                        importedNamesList.Add (name);
                }
            } else if (tkPair.tk.Type == EchelonScriptTokenType.Semicolon) {
                tokenizer.NextToken ();
            } else
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("',' or ';'", tkPair.tk)));

            return new ES_AstImportStatement () { NamespaceName = namespaceName, ImportedNames = importedNamesList.ToArray () };
        }

        protected ES_AstTypeAlias ParseAliasStatement () {
            if (EnsureToken (tokenizer.NextToken ().tk, EchelonScriptTokenType.Identifier, ES_Keywords.Alias) != EnsureTokenResult.Correct)
                throw new Exception ("The calling function must check for the correct initial token first.");

            if (!ParseUserIdentifier (out var aliasName))
                return null;

            var tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Equals, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("'='", tkPair.tk)));
                return null;
            }

            tkPair = tokenizer.PeekNextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Identifier, null) != EnsureTokenResult.Correct) {
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedIdentifier (tkPair.tk)));
                return null;
            }
            var originalName = ParseDottableUserIdentifier ();

            tkPair = tokenizer.NextToken ();
            if (EnsureToken (tkPair.tk, EchelonScriptTokenType.Semicolon, null) != EnsureTokenResult.Correct)
                errorsList.Add (new EchelonScriptErrorMessage (tkPair.tk, ES_Errors.GenExpectedXGotY ("';'", tkPair.tk)));

            return new ES_AstTypeAlias () { AliasName = aliasName, OriginalName = originalName };
        }

        #endregion

        #region Expressions

        public ES_AstExpression ParseExpression () {
            //throw new NotImplementedException ("Expression parsing is not implemented yet");
            return null;
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
                tokenizer.Dispose ();

                IsDisposed = true;
            }
        }

        public void Dispose () {
            DoDispose ();
        }

        #endregion
    }
}
