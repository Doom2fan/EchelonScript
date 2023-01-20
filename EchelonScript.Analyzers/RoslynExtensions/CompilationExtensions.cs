/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Diagnostics;
using Microsoft.CodeAnalysis;

namespace EchelonScript.Analyzers.RoslynExtensions;

internal static partial class CompilationExtensions {
    public static INamedTypeSymbol? GetAnalyzerTypeByMetadataName (this Compilation compilation, string fullyQualifiedMetadataName)
        => GetBestTypeByMetadataName (compilation, fullyQualifiedMetadataName);
}

// THE FOLLOWING LICENSE APPLIES ONLY TO THE FOLLOWING CODE:
/*
 * The MIT License (MIT)
 *
 * Copyright (c) .NET Foundation and Contributors
 *
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

internal static partial class CompilationExtensions {
    // From https://github.com/dotnet/roslyn/blob/main/src/Workspaces/SharedUtilitiesAndExtensions/Compiler/Core/Extensions/CompilationExtensions.cs
    /// <summary>
    /// Gets a type by its metadata name to use for code analysis within a <see cref="Compilation"/>. This method
    /// attempts to find the "best" symbol to use for code analysis, which is the symbol matching the first of the
    /// following rules.
    ///
    /// <list type="number">
    ///   <item><description>
    ///     If only one type with the given name is found within the compilation and its referenced assemblies, that
    ///     type is returned regardless of accessibility.
    ///   </description></item>
    ///   <item><description>
    ///     If the current <paramref name="compilation"/> defines the symbol, that symbol is returned.
    ///   </description></item>
    ///   <item><description>
    ///     If exactly one referenced assembly defines the symbol in a manner that makes it visible to the current
    ///     <paramref name="compilation"/>, that symbol is returned.
    ///   </description></item>
    ///   <item><description>
    ///     Otherwise, this method returns <see langword="null"/>.
    ///   </description></item>
    /// </list>
    /// </summary>
    /// <param name="compilation">The <see cref="Compilation"/> to consider for analysis.</param>
    /// <param name="fullyQualifiedMetadataName">The fully-qualified metadata type name to find.</param>
    /// <returns>The symbol to use for code analysis; otherwise, <see langword="null"/>.</returns>
    public static INamedTypeSymbol? GetBestTypeByMetadataName (this Compilation compilation, string fullyQualifiedMetadataName) {
        INamedTypeSymbol? type = null;

        foreach (var currentType in compilation.GetTypesByMetadataName (fullyQualifiedMetadataName)) {
            if (ReferenceEquals (currentType.ContainingAssembly, compilation.Assembly)) {
                Debug.Assert (type is null);
                return currentType;
            }

            switch (currentType.GetResultantVisibility ()) {
                case SymbolVisibility.Public:
                case SymbolVisibility.Internal when currentType.ContainingAssembly.GivesAccessTo (compilation.Assembly):
                    break;

                default:
                    continue;
            }

            if (type is object) {
                // Multiple visible types with the same metadata name are present
                return null;
            }

            type = currentType;
        }

        return type;
    }
}
