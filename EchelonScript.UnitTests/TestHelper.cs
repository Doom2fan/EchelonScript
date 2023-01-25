/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Runtime.CompilerServices;
using EchelonScript.Common.Exporting;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace EchelonScript.UnitTests;

public static class TestHelper {
    [ModuleInitializer]
    public static void ModuleInitializer () {
        VerifySourceGenerators.Enable ();
    }

    public static Task VerifyCSharp<T> (string source) where T : IIncrementalGenerator, new () {
        var syntaxTree = CSharpSyntaxTree.ParseText (source);
        var compilation = CSharpCompilation.Create (
            assemblyName: "Tests",
            syntaxTrees: new [] { syntaxTree },
            references: Basic.Reference.Assemblies.Net70.References.All.Union (new [] {
                MetadataReference.CreateFromFile (typeof (TestHelper).Assembly.Location),
                MetadataReference.CreateFromFile (typeof (IES_ExportedType).Assembly.Location),
            }),
            options: new (
                OutputKind.DynamicallyLinkedLibrary,
                reportSuppressedDiagnostics: false,
                allowUnsafe: true,
                assemblyIdentityComparer: DesktopAssemblyIdentityComparer.Default
            )
        );

        var generator = new T ();
        var driver = (GeneratorDriver) CSharpGeneratorDriver.Create (generator);

        driver = driver.RunGenerators (compilation);

        return Verify (driver).UseDirectory ("Snapshots");
    }
}
