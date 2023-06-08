/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Collections.Immutable;
using System.Runtime.CompilerServices;
using EchelonScript.Common.Exporting;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;

namespace EchelonScript.UnitTests;

internal static partial class TestHelper {
    [ModuleInitializer]
    public static void ModuleInitializer () {
        VerifySourceGenerators.Enable ();
    }

    public static IEnumerable<PortableExecutableReference> GetAnalyzerReferences (bool includeFramework) {
        var assemblies = includeFramework ? Basic.Reference.Assemblies.Net70.References.All : Enumerable.Empty<PortableExecutableReference> ();
        return assemblies.Union (new [] {
            MetadataReference.CreateFromFile (typeof (TestHelper).Assembly.Location),
            MetadataReference.CreateFromFile (typeof (IES_ExportedType).Assembly.Location),
        });
    }

    public static bool ContainsDiagnostic<T> (T diagnostics, DiagnosticDescriptor diagDesc) where T : IEnumerable<Diagnostic> {
        foreach (var diag in diagnostics) {
            if (diag.Descriptor.Category == diagDesc.Category && diag.Id == diagDesc.Id)
                return true;
        }

        return false;
    }

    public static bool HasDiagnostics (this GeneratorDriverRunResult runResult, DiagnosticDescriptor expectedDiagnostic)
        => HasDiagnostics (runResult, new [] { expectedDiagnostic });

    public static bool HasDiagnostics (this GeneratorDriverRunResult runResult, DiagnosticDescriptor [] expectedDiagnostics) {
        foreach (var diag in runResult.Diagnostics) {
            foreach (var diagDesc in expectedDiagnostics) {
                if (diag.Id == diagDesc.Id && diag.Severity == diagDesc.DefaultSeverity)
                    return true;
            }
        }

        return false;
    }

    public static CSharpCompilation CompileCSharp (string source) {
        return CSharpCompilation.Create (
            assemblyName: "Tests",
            syntaxTrees: new [] { CSharpSyntaxTree.ParseText (source) },
            references: GetAnalyzerReferences (true),
            options: new (
                OutputKind.DynamicallyLinkedLibrary,
                reportSuppressedDiagnostics: false,
                allowUnsafe: true,
                assemblyIdentityComparer: DesktopAssemblyIdentityComparer.Default
            )
        );
    }

    private static void VerifyDiagnostics (MarkupData markupData, ReadOnlySpan<DiagnosticResult> expectedDiagnostics, ImmutableArray<Diagnostic> diagnostics) {
        var diagnosticMatched = new bool [diagnostics.Length];
        foreach (var expectedDiag in expectedDiagnostics) {
            var diagLoc = markupData.DiagnosticLocations [expectedDiag.LocationIndex];

            var notFound = true;
            for (var i = 0; i < diagnostics.Length; i++) {
                var diag = diagnostics [i];

                // Check expected diagnostic data.
                if (diag.Id != expectedDiag.Id)
                    continue;
                if (diag.DefaultSeverity != expectedDiag.Severity)
                    continue;
                // Check expected diagnostic location.
                static bool CheckLocation (DiagnosticResultLocation diagLoc, Location location) {
                    var locSpan = location.SourceSpan;
                    var locStartLinePos = location.GetLineSpan ().StartLinePosition;

                    if (locSpan.Start != diagLoc.StartIndex || locSpan.Length != diagLoc.Length)
                        return false;
                    if (locStartLinePos.Line != diagLoc.Line || locStartLinePos.Character != diagLoc.Column)
                        return false;

                    return true;
                }
                if (!CheckLocation (diagLoc, diag.Location) && !diag.AdditionalLocations.Any (loc => CheckLocation (diagLoc, loc)))
                    continue;

                notFound = false;
                diagnosticMatched [i] = true;
                break;
            }

            Assert.False (notFound, $"Expected diagnostic (#{expectedDiag.LocationIndex}) '{expectedDiag.Id}' with severity {expectedDiag.Severity} at line {diagLoc.Line + 1}, column {diagLoc.Column + 1} is not present.");
        }

        for (var i = 0; i < diagnostics.Length; i++) {
            if (diagnosticMatched [i])
                continue;

            var diag = diagnostics [i];
            var loc = diag.Location.GetLineSpan ().StartLinePosition;
            Assert.Fail (@$"Unexpected diagnostic '{diag.Id}' with severity {diag.Severity} at line {loc.Line + 1}, column {loc.Character + 1}: ""{diag.GetMessage ()}"".");
        }
    }

    /*
     * Analyzers
     */
    public static CompilationWithAnalyzers CompileCSharpAndAnalyze<T> (string source) where T : DiagnosticAnalyzer, new()
        => CompileCSharp (source).WithAnalyzers (ImmutableArray.Create<DiagnosticAnalyzer> (new T ()));

    private async static Task<CompilationWithAnalyzers> TestCSharp_Internal<T> (string markupSource, DiagnosticResult []? expectedDiagnostics, Action<CompilationWithAnalyzers>? userActions)
        where T : DiagnosticAnalyzer, new() {
        var markupData = ProcessMarkup (markupSource);

        // Validate first before we do anything.
        if (expectedDiagnostics is not null) {
            foreach (var expectedDiag in expectedDiagnostics) {
                if (!markupData.DiagnosticLocations.ContainsKey (expectedDiag.LocationIndex))
                    throw new Exception ($"Non-existent diagnostic index #{expectedDiag.LocationIndex}.");
            }
        }

        var analyzerArr = ImmutableArray.Create<DiagnosticAnalyzer> (new T ());
        var bareCompilation = CompileCSharp (markupData.ProcessedCode);
        var compilation = bareCompilation.WithAnalyzers (analyzerArr);

        foreach (var diag in bareCompilation.GetDiagnostics ()) {
            if (diag.Severity != DiagnosticSeverity.Error || diag.IsSuppressed)
                continue;

            var loc = diag.Location.GetLineSpan ().StartLinePosition;
            Assert.Fail (@$"Unexpected compiler diagnostic '{diag.Id}' with severity {diag.Severity} at line {loc.Line + 1}, column {loc.Character + 1}: ""{diag.GetMessage ()}"".");
        }

        userActions?.Invoke (compilation);

        if (expectedDiagnostics is null)
            return compilation;

        var diagnostics = await compilation.GetAnalyzerDiagnosticsAsync (analyzerArr, CancellationToken.None);
        VerifyDiagnostics (markupData, expectedDiagnostics, diagnostics);

        return compilation;
    }

    // Test diagnostics
    public static Task TestCSharp<T> (string markupSource, DiagnosticResult [] expectedDiagnostics, Action<CompilationWithAnalyzers>? userActions = null)
        where T : DiagnosticAnalyzer, new() => TestCSharp_Internal<T> (markupSource, expectedDiagnostics, userActions);

    public static Task TestCSharp<T> (string markupSource, Action<CompilationWithAnalyzers>? testActions = null)
        where T : DiagnosticAnalyzer, new() => TestCSharp_Internal<T> (markupSource, null, testActions);

    /*
     * Incremental generators
     */
    public static (CSharpCompilation, GeneratorDriver) CompileCSharpAndGenerate<T> (string source) where T : IIncrementalGenerator, new() {
        var compilation = CompileCSharp (source);

        var generator = new T ();
        var driver = (GeneratorDriver) CSharpGeneratorDriver.Create (generator);

        return (compilation, driver.RunGenerators (compilation));
    }

    private static (Compilation, GeneratorDriver) TestCSharp_Internal<T> (string markupSource, DiagnosticResult []? expectedDiagnostics, Action<Compilation, GeneratorDriver>? userActions)
        where T : IIncrementalGenerator, new() {
        var markupData = ProcessMarkup (markupSource);

        // Validate first before we do anything.
        if (expectedDiagnostics is not null) {
            foreach (var expectedDiag in expectedDiagnostics) {
                if (!markupData.DiagnosticLocations.ContainsKey (expectedDiag.LocationIndex))
                    throw new Exception ($"Non-existent diagnostic index #{expectedDiag.LocationIndex}.");
            }
        }

        var compilation = CompileCSharp (markupData.ProcessedCode);
        var generator = new T ();
        var driver = (GeneratorDriver) CSharpGeneratorDriver.Create (generator);

        foreach (var diag in compilation.GetDiagnostics ()) {
            if (diag.Severity != DiagnosticSeverity.Error || diag.IsSuppressed)
                continue;

            var loc = diag.Location.GetLineSpan ().StartLinePosition;
            Assert.Fail (@$"Unexpected compiler diagnostic '{diag.Id}' with severity {diag.Severity} at line {loc.Line + 1}, column {loc.Character + 1}: ""{diag.GetMessage ()}"".");
        }

        driver = driver.RunGenerators (compilation);
        userActions?.Invoke (compilation, driver);

        if (expectedDiagnostics is null)
            return (compilation, driver);

        var diagnostics = driver.GetRunResult ().Diagnostics;
        var diagnosticMatched = new bool [diagnostics.Length];
        foreach (var expectedDiag in expectedDiagnostics) {
            var diagLoc = markupData.DiagnosticLocations [expectedDiag.LocationIndex];

            var notFound = true;
            for (var i = 0; i < diagnostics.Length; i++) {
                var diag = diagnostics [i];

                // Check expected diagnostic data.
                if (diag.Id != expectedDiag.Id)
                    continue;
                if (diag.DefaultSeverity != expectedDiag.Severity)
                    continue;
                // Check expected diagnostic location.
                static bool CheckLocation (DiagnosticResultLocation diagLoc, Location location) {
                    var locSpan = location.SourceSpan;
                    var locStartLinePos = location.GetLineSpan ().StartLinePosition;

                    if (locSpan.Start != diagLoc.StartIndex || locSpan.Length != diagLoc.Length)
                        return false;
                    if (locStartLinePos.Line != diagLoc.Line || locStartLinePos.Character != diagLoc.Column)
                        return false;

                    return true;
                }
                if (!CheckLocation (diagLoc, diag.Location) && !diag.AdditionalLocations.Any (loc => CheckLocation (diagLoc, loc)))
                    continue;

                notFound = false;
                diagnosticMatched [i] = true;
                break;
            }

            Assert.False (notFound, $"Expected diagnostic '{expectedDiag.Id}' with severity {expectedDiag.Severity} at line {diagLoc.Line + 1}, column {diagLoc.Column + 1} is not present.");
        }

        for (var i = 0; i < diagnostics.Length; i++) {
            if (diagnosticMatched [i])
                continue;

            var diag = diagnostics [i];
            var loc = diag.Location.GetLineSpan ().StartLinePosition;
            Assert.Fail (@$"Unexpected diagnostic '{diag.Id}' with severity {diag.Severity} at line {loc.Line + 1}, column {loc.Character + 1}: ""{diag.GetMessage ()}"".");
        }

        return (compilation, driver);
    }

    private static Task VerifyCSharp_Internal<T> (string source, DiagnosticResult []? expectedDiagnostics, Action<Compilation, GeneratorDriver>? userActions = null)
        where T : IIncrementalGenerator, new() {
        var (compilation, driver) = TestCSharp_Internal<T> (source, expectedDiagnostics, userActions);

        return Verify (driver).UseDirectory ("Snapshots");
    }

    // Test diagnostics
    public static void TestCSharp<T> (string markupSource, DiagnosticResult [] expectedDiagnostics, Action<Compilation, GeneratorDriver>? userActions = null)
        where T : IIncrementalGenerator, new() => TestCSharp_Internal<T> (markupSource, expectedDiagnostics, userActions);

    public static void TestCSharp<T> (string markupSource, Action<Compilation, GeneratorDriver>? testActions = null)
        where T : IIncrementalGenerator, new() => TestCSharp_Internal<T> (markupSource, null, testActions);

    // Verify generated code
    public static Task VerifyCSharp<T> (string markupSource, DiagnosticResult [] expectedDiagnostics, Action<Compilation, GeneratorDriver>? userActions = null)
            where T : IIncrementalGenerator, new() => VerifyCSharp_Internal<T> (markupSource, expectedDiagnostics, userActions);

    public static Task VerifyCSharp<T> (string source, Action<Compilation, GeneratorDriver>? userActions = null)
        where T : IIncrementalGenerator, new() => VerifyCSharp_Internal<T> (source, null, userActions);
}
