/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Analyzers.CSharpExporting;

namespace EchelonScript.UnitTests;

[UsesVerify]
public class ExportGeneratorSnapshotTests {
    [Fact]
    public Task GeneratesExportsCorrectly () {
        var source = @"
[ES_ExportStruct (new [] { ""NativeTests.Export"" }, ""BinaryTree"")]
internal partial struct BinaryTree {
    [ES_ExportFieldAttribute (""Left"", ES_AccessModifiers.Public, ES_Constness.Mutable)]
    public partial ref ES_Object<BinaryTree> Left { get; }
    [ES_ExportFieldAttribute (ES_AccessModifiers.Public, ES_Constness.Mutable)]
    public partial ref ES_Object<BinaryTree> Right { get; }

    [ES_ExportFieldAttribute (ES_AccessModifiers.Public, ES_Constness.Mutable)]
    public partial ref ES_Object<SomeValue> ValuePointer { get; }
    [ES_ExportFieldAttribute (ES_AccessModifiers.Public, ES_Constness.Mutable)]
    public partial ref SomeValue ValueDirect { get; }

    private partial ref ES_Object<SomeValue> HiddenValuePointer { get; }
    private partial ref SomeValue HiddenValue { get; }

    public BinaryTree (int val, ES_Object<BinaryTree> left, ES_Object<BinaryTree> right) {
        Value = val;
        Left = left;
        Right = right;

        ValuePointer = default;
        ValueDirect = default;

        HiddenValuePointer = default;
        HiddenValue = default;
    }
}

[ES_ExportStruct (new [] { ""NativeTests.Export"" }, ""BinaryTree"")]
internal partial struct SomeValue {
    [ES_ExportFieldAttribute (ES_AccessModifiers.Public, ES_Constness.Mutable)]
    public partial ref int Value { get; }

    [ES_ExportFieldAttribute (""SomeBinaryTree"", ES_AccessModifiers.Public, ES_Constness.Mutable)]
    public partial ref ES_Object<BinaryTree> BinaryTree { get; }
}
";

        return TestHelper.VerifyCSharp<ES_ExportGenerator> (source);
    }
}
