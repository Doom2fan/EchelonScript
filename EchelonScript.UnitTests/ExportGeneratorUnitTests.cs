/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using EchelonScript.Analyzers.CSharpExporting;
using EchelonScript.Analyzers.CSharpExporting.Internal;
using Microsoft.CodeAnalysis;
using static EchelonScript.UnitTests.TestHelper;

namespace EchelonScript.UnitTests;

[UsesVerify]
public class ExportGeneratorSnapshotTests {
    [Fact]
    public Task GeneratesExportsCorrectly () {
        var source = @"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct (Namespace = ""NativeTests.Export"", Name = ""BinaryTree"")]
public partial struct BinaryTree {
    private struct ExportDefinition {
        [ES_ExportFieldAttribute (Name = ""Left"", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public ES_Object<BinaryTree> Left;
        [ES_ExportFieldAttribute (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public ES_Object<BinaryTree> Right;

        [ES_ExportFieldAttribute (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public ES_Object<SomeValue> ValuePointer;
        [ES_ExportFieldAttribute (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public SomeValue ValueDirect;

        private ES_Object<SomeValue> HiddenValuePointer;
        private SomeValue HiddenValue;
    }
}

[ES_ExportStruct (Namespace = ""NativeTests.Export"")]
public partial struct SomeValue {
    private struct ExportDefinition {
        [ES_ExportFieldAttribute (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public int Value;

        [ES_ExportFieldAttribute (Name = ""SomeBinaryTree"", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public ES_Object<BinaryTree> BinaryTree;
    }
}
";

        return VerifyCSharp<ES_ExportGenerator> (source, (compilation, driver) => {
            Assert.Empty (compilation.GetDiagnostics ().Where (diag => !diag.IsWarningAsError && diag.Severity == DiagnosticSeverity.Error));
            Assert.Empty (driver.GetRunResult ().Diagnostics);
        });
    }

    [Fact]
    public void TestError_StructNotPartial () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public struct [|0:Test1|] {{
    private partial struct ExportDefinition {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.StructNotPartial) });
    }
}
