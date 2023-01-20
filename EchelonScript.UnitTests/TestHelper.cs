/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace EchelonScript.UnitTests;

public static class TestHelper {
    public static Task VerifyCSharp<T> (string source) where T : IIncrementalGenerator, new () {
        // Parse the provided string into a C# syntax tree
        var syntaxTree = CSharpSyntaxTree.ParseText (source);

        // Create a Roslyn compilation for the syntax tree.
        var compilation = CSharpCompilation.Create (
            assemblyName: "Tests",
            syntaxTrees: new [] { syntaxTree }
        );

        // Create an instance of the incremental source generator
        var generator = new T ();

        // The GeneratorDriver is used to run the generator against a compilation.
        var driver = (GeneratorDriver) CSharpGeneratorDriver.Create (generator);

        // Run the source generator!
        driver = driver.RunGenerators (compilation);

        // Use verify to snapshot test the source generator output!
        return Verify (driver);
    }
}
