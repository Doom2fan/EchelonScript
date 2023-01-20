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
using System.Linq;
using System.Windows.Controls;
using EchelonScript.Common;
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.GarbageCollection;
using TerraFX.Interop;

namespace TestSuiteWPF.Tests;

[ES_ExportStruct (new [] { "NativeTests.Export" }, "LinkedList")]
internal struct Struct_TreeTest {
    public ES_Object<Struct_TreeTest> Left;
    public ES_Object<Struct_TreeTest> Right;

    public int Value;

    public Struct_TreeTest (int val, ES_Object<Struct_TreeTest> left, ES_Object<Struct_TreeTest> right) {
        Value = val;
        Left = left;
        Right = right;
    }
}

/// <summary>
/// Interaction logic for ExportAndGCTest.xaml
/// </summary>
public unsafe partial class ExportAndGCTest : UserControl {
    private static ES_Object<Struct_TreeTest>* TreePointer;

    private static ES_MethodTable* GetTreeMethodTable () => ES_NativeTypeTable.GetNativeType<Struct_TreeTest>().MethodTable;
    private static ES_Object<Struct_TreeTest> AddNode (int val, ES_Object<Struct_TreeTest> left, ES_Object<Struct_TreeTest> right, bool pinned = false)
        => ES_GarbageCollector.AllocObject<Struct_TreeTest> (GetTreeMethodTable (), pinned, new (val, left, right));

    private static ES_Object<Struct_TreeTest> GenerateTree () =>
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
        TreePointer = Mimalloc.mi_malloc_tp<ES_Object<Struct_TreeTest>> ();
        ES_GarbageCollector.AddRoots ((ES_ObjectAddress**) TreePointer, 1);
        try {
            var mTable = GetTreeMethodTable ();

            if (mTable->RuntimeSize != sizeof (Struct_TreeTest)) {
                resultsTextBox.Text = $"Exports test failed: RuntimeSize ({mTable->RuntimeSize}) != struct size ({sizeof (Struct_TreeTest)})";
                return;
            }
        } catch (Exception ex) {
            resultsTextBox.Text = $"Exports test failed: Exception\n{ex.Message}\n{ex.StackTrace}";
        }
    }

    private void TestConsistency_Click (object sender, System.Windows.RoutedEventArgs e) {
        static IEnumerable<int> Preorder (Struct_TreeTest node) {
            yield return node.Value;
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
            yield return node.Value;
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
            yield return node.Value;
        }

        static IEnumerable<int> LevelOrder (ES_Object<Struct_TreeTest> thisNode) {
            var queue = new Queue<ES_Object<Struct_TreeTest>> ();
            queue.Enqueue (thisNode);
            while (queue.Any ()) {
                var node = queue.Dequeue ();
                yield return node.Value.Value;
                if (!node.Value.Left.IsNull ())
                    queue.Enqueue (node.Value.Left);
                if (!node.Value.Right.IsNull ())
                    queue.Enqueue (node.Value.Right);
            }
        }

        try {
            ref ES_Object<Struct_TreeTest> tree = ref *TreePointer;
            tree = GenerateTree ();
            ES_GarbageCollector.PerformCollection (-1, ES_GarbageCollector.CollectionMode.Forced);

            Console.WriteLine ($"Preorder:    {Preorder (tree.Value)}");
            Console.WriteLine ($"Inorder:     {Inorder (tree.Value)}");
            Console.WriteLine ($"Postorder:   {Postorder (tree.Value)}");
            Console.WriteLine ($"Level-order: {LevelOrder (tree)}");
            ES_GarbageCollector.PerformCollection (-1, ES_GarbageCollector.CollectionMode.Forced);
        } catch (Exception ex) {
            resultsTextBox.Text = $"Consistency test failed: Exception\n{ex.Message}\n{ex.StackTrace}";
        }
    }
}
