﻿/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Controls;
using EchelonScript.Common;
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.Exporting;
using EchelonScript.Common.GarbageCollection;
using static TerraFX.Interop.Mimalloc;

namespace TestSuiteWPF.Tests;

[ES_ExportStruct (Namespace = "NativeTests.Export", Name = "LinkedList")]
internal partial struct Struct_TreeTest {
    private struct ExportDefinition {
        [ES_ExportField (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public ES_Object<Struct_TreeTest> Left;
        [ES_ExportField (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public ES_Object<Struct_TreeTest> Right;

        public Struct_ValueTypeTest Value;
    }

    public Struct_TreeTest (int val, ES_Object<Struct_TreeTest> left, ES_Object<Struct_TreeTest> right) {
        Value.Value = val;
        Left = left;
        Right = right;
    }
}

[ES_ExportStruct (Namespace = "NativeTests.Export", Name = "LinkedList")]
internal partial struct Struct_ValueTypeTest {
    private struct ExportDefinition {
        [ES_ExportField (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public ES_Object<Struct_TreeTest> A;
        public ES_Object<Struct_TreeTest> B;

        [ES_ExportField (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public int Value;
    }
}

/// <summary>
/// Interaction logic for ExportAndGCTest.xaml
/// </summary>
public unsafe partial class ExportAndGCTest : UserControl {
    private ES_Object<Struct_TreeTest>* TreePointer;
    private ES_ObjectAddress** Roots;

    private void InitTree () {
        if (TreePointer != null)
            return;

        TreePointer = mi_malloc_tp<ES_Object<Struct_TreeTest>> ();
        *TreePointer = ES_Object<Struct_TreeTest>.Null;

        Roots = (ES_ObjectAddress**) mi_malloc_tp<nint> ();
        Roots [0] = (ES_ObjectAddress*) TreePointer;

        ES_GarbageCollector.AddRoots (Roots, 1);
    }
    private ES_MethodTable* GetTreeMethodTable () => ES_TypeTable.GetNativeType<Struct_TreeTest>();
    private ES_Object<Struct_TreeTest> AddNode (int val, ES_Object<Struct_TreeTest> left, ES_Object<Struct_TreeTest> right, bool pinned = false)
        => ES_GarbageCollector.AllocObject<Struct_TreeTest> (new (val, left, right), pinned);

    private ES_Object<Struct_TreeTest> GenerateTree () =>
        AddNode (1,
            AddNode (2,
                AddNode (4,
                    AddNode (7,
                        ES_Object<Struct_TreeTest>.Null,
                        ES_Object<Struct_TreeTest>.Null
                    ),
                    ES_Object<Struct_TreeTest>.Null
                ),
                AddNode (5,
                    ES_Object<Struct_TreeTest>.Null,
                    ES_Object<Struct_TreeTest>.Null
                )
            ),
            AddNode (3,
                AddNode (6,
                    AddNode (8,
                        ES_Object<Struct_TreeTest>.Null,
                        ES_Object<Struct_TreeTest>.Null
                    ),
                    AddNode (9,
                        ES_Object<Struct_TreeTest>.Null,
                        ES_Object<Struct_TreeTest>.Null
                    )
                ),
                ES_Object<Struct_TreeTest>.Null
            ),
            pinned: true
        );

    public ExportAndGCTest () {
        InitializeComponent ();
    }

    private void TestExports_Click (object sender, System.Windows.RoutedEventArgs e) {
        InitTree ();

        try {
            var mTable = GetTreeMethodTable ();

            if (mTable->RuntimeSize != sizeof (Struct_TreeTest)) {
                resultsTextBox.Text = $"Exports test failed: RuntimeSize ({mTable->RuntimeSize}) != struct size ({sizeof (Struct_TreeTest)})";
                return;
            }

            resultsTextBox.Text = $"Exports test succeeded";
        } catch (Exception ex) {
            resultsTextBox.Text = $"Exports test failed: Exception\n{ex.Message}\n{ex.StackTrace}";
        }
    }

    private void TestConsistency_Click (object sender, System.Windows.RoutedEventArgs e) {
        static IEnumerable<int> Preorder (Struct_TreeTest node) {
            yield return node.Value.Value;
            if (!node.Left.IsNull ())
                foreach (var value in Preorder (node.Left.Value))
                    yield return value;
            if (!node.Right.IsNull ())
                foreach (var value in Preorder (node.Right.Value))
                    yield return value;
        }

        static IEnumerable<int> Inorder (Struct_TreeTest node) {
            if (!node.Left.IsNull ())
                foreach (var value in Inorder (node.Left.Value))
                    yield return value;
            yield return node.Value.Value;
            if (!node.Right.IsNull ())
                foreach (var value in Inorder (node.Right.Value))
                    yield return value;
        }

        static IEnumerable<int> Postorder (Struct_TreeTest node) {
            if (!node.Left.IsNull ())
                foreach (var value in Postorder (node.Left.Value))
                    yield return value;
            if (!node.Right.IsNull ())
                foreach (var value in Postorder (node.Right.Value))
                    yield return value;
            yield return node.Value.Value;
        }

        static IEnumerable<int> LevelOrder (ES_Object<Struct_TreeTest> thisNode) {
            var queue = new Queue<ES_Object<Struct_TreeTest>> ();
            queue.Enqueue (thisNode);
            while (queue.Any ()) {
                var node = queue.Dequeue ();
                yield return node.Value.Value.Value;
                if (!node.Value.Left.IsNull ())
                    queue.Enqueue (node.Value.Left);
                if (!node.Value.Right.IsNull ())
                    queue.Enqueue (node.Value.Right);
            }
        }

        InitTree ();

        try {
            ref ES_Object<Struct_TreeTest> tree = ref *TreePointer;
            tree = GenerateTree ();
            ES_GarbageCollector.PerformCollection (-1, ES_GarbageCollector.CollectionMode.Forced);

            resultsTextBox.Text = $@"Preorder:    {string.Join (" ", Preorder (tree.Value))}
Inorder:     {string.Join (" ", Inorder (tree.Value))}
Postorder:   {string.Join (" ", Postorder (tree.Value))}
Level-order: {string.Join (" ", LevelOrder (tree))}";
            ES_GarbageCollector.PerformCollection (-1, ES_GarbageCollector.CollectionMode.Forced);
        } catch (Exception ex) {
            resultsTextBox.Text = $"Consistency test failed: Exception\n{ex.Message}\n{ex.StackTrace}";
        }
    }
}
