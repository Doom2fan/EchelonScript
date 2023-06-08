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
using static EchelonScript.UnitTests.TestHelper;

namespace EchelonScript.UnitTests;

[UsesVerify]
public class ErrorAnalyzerUnitTests {
    [Fact]
    public Task TestDiagnostic_ReferenceUsedOutsideExport () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

public class TestClass {{
    public ES_Object<int> [|0:Foo|];
    public ES_Array1D<int> [|1:Bar|];

    public ES_Object<int> [|2:FooProp|] {{ get; set; }}
    public ES_Array1D<int> [|3:BarProp|] {{ get; set; }}
}}
";

        return TestCSharp<ES_ErrorAnalyzer> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ReferenceUsedOutsideExport),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.ReferenceUsedOutsideExport),
            DiagnosticResult.FromDescriptor (2, DiagnosticDescriptors.ReferenceUsedOutsideExport),
            DiagnosticResult.FromDescriptor (3, DiagnosticDescriptors.ReferenceUsedOutsideExport),
        });
    }

    [Fact]
    public Task TestDiagnostic_ExportUsedAsValueTypeOutsideExport () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct TestStruct {{
    private partial struct ExportDefinition {{
        public ES_Object<int> Foo;
        public ES_Array1D<int> Bar;
    }}
}}

public class TestClass {{
    public TestStruct [|0:Foo|];
    public TestStruct [|1:Foo|] {{ get; set; }}
}}
";

        return TestCSharp<ES_ErrorAnalyzer> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ExportUsedAsValueTypeOutsideExport),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.ExportUsedAsValueTypeOutsideExport),
        });
    }

    [Fact]
    public Task TestDiagnostic_DefinitionStructReferenced () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct TestStruct {{
    // Technically the definition struct being public is is an error, but we need this to test it being used outside the export.
    public partial struct ExportDefinition {{
    }}

    public static Foo () {{
        var asd = new [|0:ExportDefinition|] ();
    }}
}}

public class TestClass {{
    public TestStruct.[|1:ExportDefinition|] Foo;
}}
";

        return TestCSharp<ES_ErrorAnalyzer> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DefinitionStructReferenced),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.DefinitionStructReferenced),
        });
    }

    [Fact]
    public Task TestDiagnostic_ClassUsedAsAValueType () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;
using EchelonScript.Common.GarbageCollection;

[ES_ExportClass]
public partial struct TestExport {{
    private partial struct ExportDefinition {{
        public int Foo;
    }}
}}

[ES_ExportStruct]
public partial struct TestExport2 {{
    private partial struct ExportDefinition {{
        public TestExport [|0:Foo|];
    }}
}}

public class TestClass {{
    public ES_Object<TestExport> TestMethodValid (ES_Object<TestExport> testArg, ref TestExport testArg2) {{
        ES_Object<TestExport> a = default;
        var b = default (ES_Object<TestExport>);
        var c = new ES_Object<TestExport> ();
        a = ES_GarbageCollector.AllocObject<TestExport> (false);
        a.Value = default;
        var z = (ref TestExport a) => {{ }};
        z (ref a.Value);

        TestMethodValid (c, ref b.Value);

        return c;
    }}

    public TestExport [|1:TestMethodInvalid|] (TestExport [|2:testArg|]) {{
        TestExport [|3:a|] = default;
        var [|4:b|] = default (TestExport);
        var [|5:c|] = new TestExport ();
        var [|6:d|] = [|7:new TestExport [1]|];
        var [|8:z|] = [|9:ES_GarbageCollector.AllocArray<TestExport> (new ES_ArrayIndex [] {{ 1 }}, false)|];

        return c;
    }}
}}
";

        return TestCSharp<ES_ErrorAnalyzer> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ClassUsedAsAValueType),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.ClassUsedAsAValueType),
            DiagnosticResult.FromDescriptor (2, DiagnosticDescriptors.ClassUsedAsAValueType),
            DiagnosticResult.FromDescriptor (3, DiagnosticDescriptors.ClassUsedAsAValueType),
            DiagnosticResult.FromDescriptor (4, DiagnosticDescriptors.ClassUsedAsAValueType),
            DiagnosticResult.FromDescriptor (5, DiagnosticDescriptors.ClassUsedAsAValueType),
            DiagnosticResult.FromDescriptor (6, DiagnosticDescriptors.ClassUsedAsAValueType),
            DiagnosticResult.FromDescriptor (7, DiagnosticDescriptors.ClassUsedAsAValueType),
            DiagnosticResult.FromDescriptor (8, DiagnosticDescriptors.ClassUsedAsAValueType),
            DiagnosticResult.FromDescriptor (9, DiagnosticDescriptors.ClassUsedAsAValueType),
        });
    }
}
