/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using EchelonScript.Compiler.CompilerCommon;

namespace EchelonScript.Compiler.Data;

public ref struct SourceData {
    public ReadOnlySpan<char> Code;
    public ReadOnlyMemory<char> FileName;
}

public enum ES_DiagnosticSeverity {
    Info,
    Warning,
    Error,
}

public class ES_DiagnosticDescriptor {
    #region ================== Instance properties

    /// <summary>An unique id for the diagnostic.</summary>
    public string Id { get; private init; }
    /// <summary>The category of the diagnostic.</summary>
    public string Category { get; private init; }

    /// <summary>A short message describing the diagnostic.</summary>
    public string Title { get; private init; }
    /// <summary>An optional longer message for the diagnostic.</summary>
    public string? Description { get; private init; }
    /// <summary>A message string.
    /// Passed as the first argument to <see cref="string.Format(string, object[])"/> when creating a diagnostic.</summary>
    public string MessageFormat { get; private init; }

    /// <summary>The diagnostic's severity.</summary>
    public ES_DiagnosticSeverity DefaultSeverity { get; private init; }

    /// <summary>Whether the user can change the diagnostic's severity or disable it.</summary>
    public bool IsConfigurable { get; private init; }
    /// <summary>Whether the diagnostic is enabled by default.</summary>
    public bool IsEnabledByDefault { get; private init; }

    #endregion

    public ES_DiagnosticDescriptor (
        string id,
        string title,
        string messageFormat,
        string category,
        ES_DiagnosticSeverity severity,
        bool isEnabledByDefault,

        string? description = null,
        bool isConfigurable = true

    ) {
        if (!isConfigurable && !isEnabledByDefault)
            throw new ArgumentException ("Descriptor cannot be both non-configurable and disabled by default.");

        Id = id;
        Category = category;

        Title = title;
        Description = description;
        MessageFormat = messageFormat;

        DefaultSeverity = severity;

        IsConfigurable = isConfigurable;
        IsEnabledByDefault = isEnabledByDefault;
    }

    #region ================== Static methods

    public ES_Diagnostic Create (params object? []? messageArgs) => ES_Diagnostic.Create (this, messageArgs);

    public ES_Diagnostic Create (SourceLocation? location, params object? []? messageArgs)
        => ES_Diagnostic.Create (this, location, messageArgs);

    public ES_Diagnostic Create (
        SourceLocation? location,
        ImmutableArray<SourceLocation> additionalLocations,
        params object? []? messageArgs
    ) => ES_Diagnostic.Create (this, location, additionalLocations, messageArgs);

    #endregion
}

public struct ES_Diagnostic {
    #region ================== Instance properties

    private object? [] messageArgs { get; init; }

    /// <summary>The diagnostic's descriptor.</summary>
    public ES_DiagnosticDescriptor Descriptor;

    /// <summary>The diagnostic's severity.</summary>
    public ES_DiagnosticSeverity Severity { get; private init; }
    /// <summary>The diagnostic's default severity.</summary>
    public readonly ES_DiagnosticSeverity DefaultSeverity => Descriptor.DefaultSeverity;

    /// <summary>The diagnostic's id.</summary>
    public readonly string Id => Descriptor.Id;
    /// <summary>The diagnostic's arguments.</summary>
    public readonly IReadOnlyList<object?> Arguments => messageArgs;

    /// <summary>The diagnostic's location.</summary>
    public SourceLocation? Location { get; private init; }
    /// <summary>Any additional locations for the diagnostic.</summary>
    public ImmutableArray<SourceLocation> AdditionalLocations { get; private init; }

    /// <summary>Whether the diagnostic was suppressed.</summary>
    public bool IsSuppressed { get; private init; }

    /// <summary>Whether the diagnostic is a warning treated as an error.</summary>
    public readonly bool IsWarningAsError
        => DefaultSeverity == ES_DiagnosticSeverity.Warning && Severity == ES_DiagnosticSeverity.Error;

    #endregion

    #region ================== Instance methods

    public ES_Diagnostic WithSeverity (ES_DiagnosticSeverity severity) => this with { Severity = severity, };

    public ES_Diagnostic WithSuppression (bool isSuppressed) => this with { IsSuppressed = isSuppressed, };

    public readonly string GetMessage (IFormatProvider? formatProvider = null) {
        if (messageArgs.Length < 1)
            return Descriptor.MessageFormat;

        try {
            return string.Format (formatProvider, Descriptor.MessageFormat, messageArgs);
        } catch (Exception) {
            return Descriptor.MessageFormat;
        }
    }

    #endregion

    #region ================== Static methods

    public static ES_Diagnostic Create (
        ES_DiagnosticDescriptor descriptor,
        SourceLocation? location,
        ImmutableArray<SourceLocation> additionalLocations,
        params object? []? messageArgs
    ) {
        return new ES_Diagnostic () with {
            messageArgs = messageArgs ?? Array.Empty<object?> (),

            Descriptor = descriptor,
            Severity = descriptor.DefaultSeverity,

            Location = location,
            AdditionalLocations = additionalLocations,

            IsSuppressed = false,
        };
    }

    public static ES_Diagnostic Create (
        ES_DiagnosticDescriptor descriptor,
        SourceLocation? location,
        params object? []? messageArgs
    ) => Create (descriptor, location, ImmutableArray<SourceLocation>.Empty, messageArgs);

    public static ES_Diagnostic Create (
        ES_DiagnosticDescriptor descriptor,
        params object? []? messageArgs
    ) => Create (descriptor, null, ImmutableArray<SourceLocation>.Empty, messageArgs);

    #endregion
}
