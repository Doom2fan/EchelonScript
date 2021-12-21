﻿/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Text;
using System.Linq;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using ChronosLib.Pooled;
using CommunityToolkit.HighPerformance.Buffers;
using EchelonScriptCommon.Data.Types;
using EchelonScriptCompiler;
using EchelonScriptCompiler.Backends.LLVMBackend;
using EchelonScriptCompiler.Backends.RoslynBackend;
using EchelonScriptCompiler.Data;
using EchelonScriptCompiler.Frontend;
using ICSharpCode.AvalonEdit.Document;

namespace TestSuiteWPF.Tests {
    /// <summary>
    /// Interaction logic for CompilerTest.xaml
    /// </summary>
    public partial class CompilerTest : UserControl {
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

        System.Globalization.CultureInfo cultureInfo;

        EchelonScriptEnvironment env;
        Random rand = new Random ();

        #endregion

        #region ================== Constructors

        public CompilerTest () {
            InitializeComponent ();

            textMarkerService = new TextMarkerService (codeText);

            errList = new ObservableCollection<CompilerMessage> ();

            errorsList.ItemsSource = errList;

            cultureInfo = new System.Globalization.CultureInfo (System.Globalization.CultureInfo.CurrentCulture.Name);
            cultureInfo.NumberFormat.NumberDecimalSeparator = ".";
            cultureInfo.NumberFormat.NumberGroupSeparator = "'";

            AddCompiler ("Frontend only", EchelonScript_Compiler.Create ());
            AddCompiler ("LLVM", EchelonScript_Compiler.Create<LLVMCompilerBackend> ());
            AddCompiler ("Roslyn", EchelonScript_Compiler.Create<RoslynCompilerBackend> ());

            compilerComboBox.SelectedIndex = 0;
        }

        #endregion

        #region ================== Instance methods

        private void AddCompiler (string name, EchelonScript_Compiler compiler) {
            var comboBoxItem = new ComboBoxItem ();
            comboBoxItem.Content = name;
            comboBoxItem.Tag = compiler;

            compilerComboBox.Items.Add (comboBoxItem);
        }

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

        private TreeViewItem AddNodeToTree (string nodeText, TreeViewItem parentItem) {
            var thisItem = new TreeViewItem ();
            parentItem.Items.Add (thisItem);
            thisItem.Header = nodeText;

            return thisItem;
        }

        private unsafe void AddTypeToTree (ES_TypeInfo* typeData, TreeViewItem parentItem) {
            string typeType = null;

            switch (typeData->TypeTag) {
                case ES_TypeTag.Void: typeType = "Void"; break;
                case ES_TypeTag.Bool: typeType = "Bool"; break;
                case ES_TypeTag.Int: typeType = "Int"; break;
                case ES_TypeTag.Float: typeType = "Float"; break;

                case ES_TypeTag.Function: typeType = "Prototype"; break;
                case ES_TypeTag.Struct: typeType = "Struct"; break;
                case ES_TypeTag.Class: typeType = "Class"; break;
                case ES_TypeTag.Enum: typeType = "Enum"; break;
                case ES_TypeTag.Interface: typeType = "Interface"; break;

                case ES_TypeTag.Reference: typeType = "Reference"; break;
                case ES_TypeTag.Const: typeType = "Const"; break;
                case ES_TypeTag.Immutable: typeType = "Immutable"; break;
                case ES_TypeTag.Array: typeType = "Array"; break;

                default: typeType = "[UNRECOGNIZED]"; break;
            }

            var typeNode = AddNodeToTree ($"{typeType} {typeData->Name.TypeNameString}", parentItem);

            AddNodeToTree ($"Runtime size: {typeData->RuntimeSize}", typeNode);
            AddNodeToTree ($"Fully qualified name: {typeData->Name.GetNameAsTypeString ()}", typeNode);
            AddNodeToTree ($"Source unit: {typeData->SourceUnitString}", typeNode);

            if (typeData->TypeTag == ES_TypeTag.Function) {
                var funcData = (ES_FunctionPrototypeData*) typeData;

                AddNodeToTree ($"Return type: {funcData->ReturnType->Name.GetNameAsTypeString ()}", typeNode);

                var argsListNode = AddNodeToTree ($"Arguments list", typeNode);
                foreach (var arg in funcData->ArgumentsList.Span) {
                    string argType = arg.ArgType.ToString ();
                    string argTypeName;

                    if (arg.ValueType != null)
                        argTypeName = arg.ValueType->Name.GetNameAsTypeString ();
                    else
                        argTypeName = "[NULL]";

                    AddNodeToTree ($"{argType} {argTypeName}", argsListNode);
                }
            }
        }

        private unsafe void AddFunctionToTree (ES_FunctionData* functionData, TreeViewItem parentItem) {
            var typeNode = AddNodeToTree ($"Function {functionData->Name.TypeNameString}", parentItem);

            AddNodeToTree ($"Fully qualified name: {functionData->Name.GetNameAsTypeString ()}", typeNode);
            AddNodeToTree ($"Source unit: {functionData->SourceUnitString}", typeNode);

            var protoData = functionData->FunctionType;
            AddNodeToTree ($"Return type: {protoData->ReturnType->Name.GetNameAsTypeString ()}", typeNode);

            var argsListNode = AddNodeToTree ($"Arguments list", typeNode);
            var reqArgsCount = protoData->ArgumentsList.Length - functionData->OptionalArgsCount;
            for (int i = 0; i < protoData->ArgumentsList.Length; i++) {
                var argProto = protoData->ArgumentsList.Span [i];
                var argData = functionData->Arguments.Span [i];

                var argType = string.Empty;
                var argTypeName = "[NULL]";
                var argName = StringPool.Shared.GetOrAdd (argData.Name.Span, Encoding.ASCII);
                var argDef = string.Empty;

                if (argProto.ArgType != ES_ArgumentType.Normal)
                    argType = $"{argProto.ArgType} ";

                if (argProto.ValueType != null)
                    argTypeName = argProto.ValueType->Name.GetNameAsTypeString ();

                if (i >= reqArgsCount)
                    argDef = " = [...]";

                AddNodeToTree ($"{argType}{argName} {argTypeName}{argDef}", argsListNode);
            }
        }

        public delegate int Int32Int32Int32Delegate (int a, int b);
        public delegate float FloatFloatFloatDelegate (float a, float b);

        private unsafe void CompileCode () {
            var code = codeText.Text;

            using var codeUnits = new StructPooledList<ReadOnlyMemory<char>> (CL_ClearMode.Auto);
            codeUnits.Add (code.AsMemory ());

            var compiler = (compilerComboBox.SelectedItem as ComboBoxItem)!.Tag as EchelonScript_Compiler;
            var frontendOnly = !compiler.HasBackend;

            env?.Dispose ();
            compiler.Reset ();
            compiler.Setup (out env);
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

            if (errList.Count > 0) {
                env?.Dispose ();
                env = null;
                return;
            }

            foreach (var namespaceKVP in env.Namespaces) {
                var namespaceData = namespaceKVP.Value;
                var namespaceNode = AddNodeToTree (namespaceData.NamespaceNameString, rootItem);
                namespaceNode.IsExpanded = true;

                foreach (var typeDataPtr in namespaceData.Types)
                    AddTypeToTree (typeDataPtr.Address, namespaceNode);

                foreach (var funcKVP in namespaceData.Functions)
                    AddFunctionToTree (funcKVP.Value.Address, namespaceNode);
            }

            if (frontendOnly) {
                env.Dispose ();
                env = null;
                return;
            }

            if (runOnCompile.IsChecked == true)
                RunCode ();
        }

        private unsafe void RunCode () {
            if (env is null)
                return;

            var newErrList = errList.Where (msg => msg.Type != "Output").ToArray ();
            errList.Clear ();
            foreach (var err in newErrList)
                errList.Add (err);

            var testI32Name = "testI32";
            var testF32Name = "testF32";

            var idMain = env.IdPool.GetIdentifier ("main");
            var idTestI32 = env.IdPool.GetIdentifier (testI32Name);
            var idTestF32 = env.IdPool.GetIdentifier (testF32Name);

            var idInt32 = env.IdPool.GetIdentifier (ES_PrimitiveTypes.GetIntName (ES_IntSize.Int32, false));
            var typeInt32 = env.GetFullyQualifiedType (env.GlobalTypesNamespace, idInt32);

            var idFloat32 = env.IdPool.GetIdentifier (ES_PrimitiveTypes.GetFloatName (ES_FloatSize.Single));
            var typeFloat32 = env.GetFullyQualifiedType (env.GlobalTypesNamespace, idFloat32);

            Debug.Assert (typeInt32 is not null);
            Debug.Assert (typeFloat32 is not null);

            foreach (var namespaceKVP in env.Namespaces) {
                var namespaceData = namespaceKVP.Value;

                if (namespaceData.Functions.TryGetValue (idMain, out var func)) {
                    var funcType = func.Address->FunctionType;

                    if (funcType->ReturnType->TypeTag == ES_TypeTag.Void && funcType->ArgumentsList.Length == 0) {
                        // TODO: Add stuff here
                    }
                }

                if (namespaceData.Functions.TryGetValue (idTestI32, out func)) {
                    var funcType = func.Address->FunctionType;

                    bool matchSig = false;

                    Int32Int32Int32Delegate fp = null;
                    if (funcType->ReturnType == typeInt32 && funcType->ArgumentsList.Length == 2 &&
                        funcType->ArgumentsList.Span [0].ValueType == typeInt32 && funcType->ArgumentsList.Span [0].ArgType == ES_ArgumentType.Normal &&
                        funcType->ArgumentsList.Span [1].ValueType == typeInt32 && funcType->ArgumentsList.Span [1].ArgType == ES_ArgumentType.Normal) {
                        fp = env.GetFunctionDelegate<Int32Int32Int32Delegate> (func);
                        matchSig = true;
                    }

                    if (fp != null) {
                        var a = rand.Next (int.MinValue, int.MaxValue);
                        var b = rand.Next (int.MinValue, int.MaxValue);
                        int ret = fp (a, b);

                        var infoStr = string.Format (cultureInfo, $"{testI32Name} ({{0:n0}}, {{1:n0}}) returned {{2:n0}}.", a, b, ret);
                        errList.Add (new CompilerMessage ("Output", new EchelonScriptErrorMessage (infoStr, 0, 0, 0, 0)));
                    } else if (matchSig)
                        errList.Add (new CompilerMessage ("Output", new EchelonScriptErrorMessage ($"{testI32Name} had a null function pointer.", 0, 0, 0, 0)));
                    else
                        errList.Add (new CompilerMessage ("Output", new EchelonScriptErrorMessage ($"{testI32Name} didn't match signature.", 0, 0, 0, 0)));
                }

                if (namespaceData.Functions.TryGetValue (idTestF32, out func)) {
                    var funcType = func.Address->FunctionType;

                    bool matchSig = false;

                    FloatFloatFloatDelegate fp = null;
                    if (funcType->ReturnType == typeFloat32 && funcType->ArgumentsList.Length == 2 &&
                        funcType->ArgumentsList.Span [0].ValueType == typeFloat32 && funcType->ArgumentsList.Span [0].ArgType == ES_ArgumentType.Normal &&
                        funcType->ArgumentsList.Span [1].ValueType == typeFloat32 && funcType->ArgumentsList.Span [1].ArgType == ES_ArgumentType.Normal) {
                        fp = env.GetFunctionDelegate<FloatFloatFloatDelegate> (func);
                        matchSig = true;
                    }

                    if (fp != null) {
                        var a = (float) (rand.NextDouble () * int.MaxValue);
                        var b = (float) (rand.NextDouble () * int.MaxValue);
                        float ret = fp (a, b);

                        var infoStr = string.Format (cultureInfo, $"{testF32Name} ({{0:n}}, {{1:n}}) returned {{2:n}}.", a, b, ret);
                        errList.Add (new CompilerMessage ("Output", new EchelonScriptErrorMessage (infoStr, 0, 0, 0, 0)));
                    } else if (matchSig)
                        errList.Add (new CompilerMessage ("Output", new EchelonScriptErrorMessage ($"{testF32Name} had a null function pointer.", 0, 0, 0, 0)));
                    else
                        errList.Add (new CompilerMessage ("Output", new EchelonScriptErrorMessage ($"{testF32Name} didn't match signature.", 0, 0, 0, 0)));
                }
            }
        }

        #endregion

        #region ================== Event handlers

        private void codeText_TextChanged (object sender, EventArgs e) {
            if (compileOnEditCheckbox.IsChecked == true)
                CompileCode ();
        }

        private void CompileButton_Click (object sender, System.Windows.RoutedEventArgs e) => CompileCode ();

        private void RunButton_Click (object sender, System.Windows.RoutedEventArgs e) => RunCode ();

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
