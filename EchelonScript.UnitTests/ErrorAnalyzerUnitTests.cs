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
    public Task TestDiagnostic_NonStructExported () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public class [|0:Test1|] {{ }}
";

        return TestCSharp<ES_ErrorAnalyzer> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.NonStructExported), });
    }

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
}
