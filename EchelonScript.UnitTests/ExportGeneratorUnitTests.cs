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
public class ExportGeneratorUnitTests {
    [Fact]
    public void TestDiagnostic_StructNotPartial () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public struct [|0:Test1|] {{
    private partial struct ExportDefinition {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.StructNotPartial), });
    }

#if false // Ref structs can't have attributes, so this error isn't even output. Leaving here just in case that ever changes.
    [Fact]
    public void TestDiagnostic_StructIsRef () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public ref partial struct [|0:Test1|] {{
    private partial struct ExportDefinition {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.StructIsRef), });
    }
#endif

    [Fact]
    public void TestDiagnostic_StructIsGeneric () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct [|0:Test1|]<T> {{
    private partial struct ExportDefinition {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.StructIsGeneric), });
    }

    [Fact]
    public void TestDiagnostic_StructNotUnmanaged_InstanceMemberInAggregate () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct [|0:Test1|] {{
    private partial struct ExportDefinition {{ }}

    public string [|1:Foo|];
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.StructNotUnmanaged),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.InstanceMemberInAggregate),
        });
    }

    [Fact]
    public void TestDiagnostic_DefinitionStructMissing () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct [|0:Test1|] {{
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DefinitionStructMissing), });
    }

    [Fact]
    public void TestDiagnostic_DefinitionTypeNotAStruct_DefinitionStructNotUnmanaged () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private partial class [|0:ExportDefinition|] {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DefinitionTypeNotAStruct),
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DefinitionStructNotUnmanaged),
        });
    }

    [Fact]
    public void TestDiagnostic_DefinitionStructNotPartial () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private struct [|0:ExportDefinition|] {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DefinitionStructNotPartial), });
    }

    [Fact]
    public void TestDiagnostic_DefinitionStructIsGeneric () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct [|0:ExportDefinition|]<T> {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DefinitionStructIsGeneric), });
    }

    [Fact]
    public void TestDiagnostic_DefinitionStructNotPrivate () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    public partial struct [|0:ExportDefinition|] {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DefinitionStructNotPrivate), });
    }

    [Fact]
    public void TestDiagnostic_DefinitionStructDeclaredInMultiplePlaces () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct [|0:ExportDefinition|] {{ }}
}}

partial struct Test1 {{
    partial struct [|1:ExportDefinition|] {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DefinitionStructDeclaredInMultiplePlaces),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.DefinitionStructDeclaredInMultiplePlaces),
        });
    }

    [Fact]
    public void TestDiagnostic_DefinitionStructIsRef () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private ref partial struct [|0:ExportDefinition|] {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DefinitionStructIsRef), });
    }

    [Fact]
    public void TestDiagnostic_ExportedTypeNestedInGeneric () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

public partial class Foo<T> {{
    [ES_ExportStruct]
    public partial struct [|0:Test1|] {{
        private partial struct ExportDefinition {{ }}
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ExportedTypeNestedInGeneric), });
    }

    [Fact]
    public void TestDiagnostic_ExportedTypeNestedInNonPartial () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

public class Foo {{
    [ES_ExportStruct]
    public partial struct [|0:Test1|] {{
        private partial struct ExportDefinition {{ }}
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ExportedTypeNestedInNonPartial), });
    }

    [Fact]
    public void TestDiagnostic_StaticMemberInDefStruct () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct ExportDefinition {{
        public static bool [|0:Foo|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.StaticMemberInDefStruct), });
    }

    [Fact]
    public void TestDiagnostic_ExportedMemberIsReadonly () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct ExportDefinition {{
        private readonly int [|0:Foo|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ExportedMemberIsReadonly), });
    }

    [Fact]
    public void TestDiagnostic_RefMembersNotAllowed () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

public ref struct RefStruct {{ }}

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct ExportDefinition {{
        private RefStruct [|0:Foo|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.RefMembersNotAllowed),
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DisallowedTypeInField_NotExportOrPrimitive),
        });
    }

#if false // TODO: Implement this test once methods are implemented.
    [Fact]
    public void TestDiagnostic_RefReturnNotAllowed () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct ExportDefinition {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.RefReturnNotAllowed), });
    }
#endif

    [Fact]
    public void TestDiagnostic_ReferenceTypesNotAllowed () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct [|1:ExportDefinition|] {{
        private string [|0:Foo|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ReferenceTypesNotAllowed),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.DefinitionStructNotUnmanaged),
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DisallowedTypeInField_NotExportOrPrimitive),
        });
    }

    [Fact]
    public void TestDiagnostic_ManagedTypesNotAllowed () {
        var source = @$"
using System.Runtime.InteropServices;
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

public struct ManagedStruct {{
    string Foo;
}}

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct [|1:ExportDefinition|] {{
        private ManagedStruct [|0:Foo|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.DefinitionStructNotUnmanaged),
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ManagedTypesNotAllowed),
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DisallowedTypeInField_NotExportOrPrimitive),
        });
    }

    [Fact]
    public void TestDiagnostic_DisallowedTypeInField () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

public struct UnexportedType {{ }}

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct ExportDefinition {{
        public UnexportedType [|0:Foo|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DisallowedTypeInField_NotExportOrPrimitive), });
    }

    [Fact]
    public void TestDiagnostic_InvalidNamespace () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct (Namespace = ""1ASD"")]
public partial struct [|0:Test1|] {{
    private partial struct ExportDefinition {{ }}
}}

[ES_ExportStruct (Namespace = ""ASD@::ASD!#"")]
public partial struct [|1:Test2|] {{
    private partial struct ExportDefinition {{ }}
}}

[ES_ExportStruct (Namespace = ""@ASD::!ASD#"")]
public partial struct [|2:Test3|] {{
    private partial struct ExportDefinition {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.InvalidNamespace),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.InvalidNamespace),
            DiagnosticResult.FromDescriptor (2, DiagnosticDescriptors.InvalidNamespace),
        });
    }

    [Fact]
    public void TestDiagnostic_InvalidTypeName () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct (Name = ""1ASD"")]
public partial struct [|0:Test1|] {{
    private partial struct ExportDefinition {{ }}
}}

[ES_ExportStruct (Name = ""ASD@::ASD!#"")]
public partial struct [|1:Test2|] {{
    private partial struct ExportDefinition {{ }}
}}

[ES_ExportStruct (Name = ""@ASD::!ASD#"")]
public partial struct [|2:Test3|] {{
    private partial struct ExportDefinition {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.InvalidTypeName),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.InvalidTypeName),
            DiagnosticResult.FromDescriptor (2, DiagnosticDescriptors.InvalidTypeName),
        });
    }

    [Fact]
    public void TestDiagnostic_InvalidAutoTypeName () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct [|0:Test1_·ÈÌÛ˙‡ËÏÚ˘„ı‚ÍÓÙ˚Á|] {{
    private partial struct ExportDefinition {{ }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.InvalidAutoTypeName), });
    }

    [Fact]
    public void TestDiagnostic_InvalidFieldName () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct ExportDefinition {{
        [ES_ExportField (Name = ""234"", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        private int [|0:Foo|];
        [ES_ExportField (Name = ""@#4"", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        private int [|1:Bar|];
        [ES_ExportField (Name = ""ASDASD@"", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        private int [|2:Baz|];
        [ES_ExportField (Name = ""ASDASD"", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        private int Foo2;
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.InvalidFieldName),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.InvalidFieldName),
            DiagnosticResult.FromDescriptor (2, DiagnosticDescriptors.InvalidFieldName),
        });
    }

    [Fact]
    public void TestDiagnostic_InvalidAutoFieldName () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Test1 {{
    private partial struct ExportDefinition {{
        [ES_ExportField (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        private int [|0:Test1_·ÈÌÛ˙‡ËÏÚ˘„ı‚ÍÓÙ˚Á|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] { DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.InvalidAutoFieldName), });
    }

    [Fact]
    public void TestDiagnostic_ProtectedMemberInStruct_ProtectedMemberInSealedClass () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct TestStruct {{
    private partial struct ExportDefinition {{
        [ES_ProtectedField]
        public ES_Object<int> [|0:Foo|];
    }}
}}

[ES_ExportClass (Sealed = true)]
public partial struct TestClass {{
    private partial struct ExportDefinition {{
        [ES_ProtectedField]
        public ES_Object<int> [|1:Foo|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ProtectedMemberInStruct),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.ProtectedMemberInSealedClass),
        });
    }

    [Fact]
    public void TestDiagnostic_DuplicateFieldExportName () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct TestStruct {{
    private partial struct ExportDefinition {{
        [ES_ExportField (Name = ""Foo"", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        private int Foo;
        [ES_ExportField (Name = ""Foo"", AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        private int [|0:Bar|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.DuplicateFieldExportName),
        });
    }

    [Fact]
    public void TestDiagnostic_FieldNameUsedInBase_FieldExportNameUsedInBase () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportClass (Namespace = ""NativeTests.Export"")]
public partial struct ClassDef {{
    private partial struct ExportDefinition {{
        [ES_ExportFieldAttribute (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public int Value;
    }}
}}

[ES_ExportClass (Namespace = ""NativeTests.Export"", ParentClass = typeof (ClassDef))]
public partial struct SubClassDef {{
    private partial struct ExportDefinition {{
        [ES_ExportFieldAttribute (AccessModifier = ES_AccessModifier.Public, Constness = ES_Constness.Mutable)]
        public int [|0:Value|];
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.FieldNameUsedInBase),
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.FieldExportNameUsedInBase),
        });
    }

    [Fact]
    public void TestDiagnostic_InheritsFromSealedType () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportClass (Sealed = true)]
public partial struct ClassDef {{
    private partial struct ExportDefinition {{
    }}
}}

[ES_ExportClass (ParentClass = typeof (ClassDef))]
public partial struct [|0:SubClassDef|] {{
    private partial struct ExportDefinition {{
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.InheritsFromSealedType),
        });
    }

    [Fact]
    public void TestDiagnostic_InheritanceCycle () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportClass (ParentClass = typeof (Bar))]
public partial struct Foo {{
    private partial struct ExportDefinition {{
    }}
}}

[ES_ExportClass (ParentClass = typeof (Foo))]
public partial struct [|0:Bar|] {{
    private partial struct ExportDefinition {{
    }}
}}

[ES_ExportClass (ParentClass = typeof (Baz))]
public partial struct [|1:Baz|] {{
    private partial struct ExportDefinition {{
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.InheritanceCycle),
            DiagnosticResult.FromDescriptor (1, DiagnosticDescriptors.InheritanceCycle),
        });
    }

    [Fact]
    public void TestDiagnostic_ClassInheritsNonClass () {
        var source = @$"
using EchelonScript.Common;
using EchelonScript.Common.Exporting;

[ES_ExportStruct]
public partial struct Foo {{
    private partial struct ExportDefinition {{
    }}
}}

[ES_ExportClass (ParentClass = typeof (Foo))]
public partial struct [|0:Bar|] {{
    private partial struct ExportDefinition {{
    }}
}}
";

        TestCSharp<ES_ExportGenerator> (source, new [] {
            DiagnosticResult.FromDescriptor (0, DiagnosticDescriptors.ClassInheritsNonClass),
        });
    }
}
