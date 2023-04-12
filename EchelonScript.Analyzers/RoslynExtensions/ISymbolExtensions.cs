/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System.Text;
using Microsoft.CodeAnalysis;

namespace EchelonScript.Analyzers.RoslynExtensions;

internal static partial class ISymbolExtensions {
    public static string GetFullNamespace (this ISymbol symbol) {
        while (symbol != null && symbol.Kind != SymbolKind.Namespace)
            symbol = symbol.ContainingSymbol;

        if (symbol == null)
            return string.Empty;

        var sb = new StringBuilder ();
        var first = true;
        while (symbol != null && symbol.Kind == SymbolKind.Namespace) {
            if (!first)
                sb.Insert (0, '.');
            sb.Insert (0, symbol.Name);
            first = false;

            symbol = symbol.ContainingSymbol;
            if ((symbol as INamespaceSymbol)?.IsGlobalNamespace == true)
                break;
        }

        return sb.ToString ();
    }
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

internal enum SymbolVisibility {
    Public,
    Internal,
    Private,
}

internal static partial class ISymbolExtensions {
    // From https://github.com/dotnet/roslyn/blob/main/src/Workspaces/SharedUtilitiesAndExtensions/Compiler/Core/Extensions/ISymbolExtensions.cs
    public static SymbolVisibility GetResultantVisibility (this ISymbol symbol) {
        // Start by assuming it's visible.
        var visibility = SymbolVisibility.Public;

        switch (symbol.Kind) {
            case SymbolKind.Alias:
                // Aliases are uber private.  They're only visible in the same file that they
                // were declared in.
                return SymbolVisibility.Private;
        }

        while (symbol != null && symbol.Kind != SymbolKind.Namespace) {
            switch (symbol.DeclaredAccessibility) {
                // If we see anything private, then the symbol is private.
                case Accessibility.NotApplicable:
                case Accessibility.Private:
                    return SymbolVisibility.Private;

                // If we see anything internal, then knock it down from public to
                // internal.
                case Accessibility.Internal:
                case Accessibility.ProtectedAndInternal:
                    visibility = SymbolVisibility.Internal;
                    break;

                    // For anything else (Public, Protected, ProtectedOrInternal), the
                    // symbol stays at the level we've gotten so far.
            }

            symbol = symbol.ContainingSymbol;
        }

        return visibility;
    }
}
